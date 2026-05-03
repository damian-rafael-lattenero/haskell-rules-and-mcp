{
  description = "haskell-flows — agent-first MCP server for property-driven Haskell development";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        # ----------------------------------------------------------------
        # Toolchain pin — kept compatible with the CI matrix in
        # `.github/workflows/haskell-ci.yml` (GHC 9.10.1 + GHC 9.12.2).
        # `nixos-24.05` ships GHC 9.8 by default; we pick the closest
        # available compiler from the haskellPackages set. The MCP
        # tolerates any GHC 9.6+ that cabal accepts, and CI is the
        # authoritative version gate — Nix is the "I want a
        # reproducible local shell" path.
        # ----------------------------------------------------------------
        ghc = pkgs.haskell.compiler.ghc98;
        cabal = pkgs.cabal-install;

        # Quality-gate tools pinned via Nix when available. Versions
        # may lag the cabal.project.freeze; the MCP's resolution chain
        # (host PATH → bundled → auto-download) keeps working even
        # when the Nix-shipped binary is older.
        hlint = pkgs.haskellPackages.hlint;
        fourmolu = pkgs.haskellPackages.fourmolu;
        ormolu = pkgs.haskellPackages.ormolu;
        hls = pkgs.haskell-language-server.override {
          supportedGhcVersions = [ "98" ];
        };

        node = pkgs.nodejs_22;

        # Common toolchain — included by every devShell variant.
        coreTools = [
          ghc
          cabal
          hlint
          pkgs.git
          pkgs.gh
          pkgs.jq
          pkgs.curl
        ];

        # Developer-comfort extras (formatters, IDE, scripting). NOT in
        # the CI shell because they balloon download size and the gates
        # don't need them.
        devExtras = [
          fourmolu
          ormolu
          hls
          node
        ];
      in
      {
        # ----------------------------------------------------------------
        # Default dev shell — full toolchain for local hacking.
        #
        # Usage:
        #   nix develop          # enters a shell with everything
        #   nix develop .#ci     # enters the CI-equivalent slim shell
        # ----------------------------------------------------------------
        devShells.default = pkgs.mkShell {
          name = "haskell-flows-dev";
          packages = coreTools ++ devExtras;

          shellHook = ''
            echo "haskell-flows dev shell (full)"
            echo "  ghc:      $(ghc --numeric-version)"
            echo "  cabal:    $(cabal --numeric-version)"
            echo "  hlint:    $(hlint --version | head -1)"
            echo "  fourmolu: $(fourmolu --version | head -1)"
            echo "  ormolu:   $(ormolu --version | head -1)"
            echo "  hls:      $(haskell-language-server-wrapper --version | head -1)"
            echo ""
            echo "Build the MCP:    cd mcp-server-haskell && cabal build"
            echo "Run unit tests:   cd mcp-server-haskell && cabal test haskell-flows-mcp-test"
            echo "Run e2e tests:    cd mcp-server-haskell && cabal test haskell-flows-mcp-e2e"
            echo ""
            echo "Re-warm fixture:  cd mcp-server-haskell/test-e2e/Fixtures/Baseline && cabal build"
          '';
        };

        # ----------------------------------------------------------------
        # CI-equivalent slim shell — mirrors what the GitHub Actions
        # workflow expects on PATH. Use this for local CI repro:
        #
        #   nix develop .#ci -c bash -lc \
        #     'cd mcp-server-haskell && cabal build all && cabal test all'
        #
        # Drops formatter + HLS + node since CI doesn't need them.
        # Smaller closure = faster `nix develop .#ci` first run.
        # ----------------------------------------------------------------
        devShells.ci = pkgs.mkShell {
          name = "haskell-flows-ci";
          packages = coreTools;

          shellHook = ''
            echo "haskell-flows CI-equivalent shell"
            echo "  ghc:    $(ghc --numeric-version)"
            echo "  cabal:  $(cabal --numeric-version)"
            echo "  hlint:  $(hlint --version | head -1)"
          '';
        };

        # ----------------------------------------------------------------
        # Optional: a `pre-warmed` package output for users who want
        # Nix to pre-build the cabal-store closure for the project.
        #
        # Build with:    nix build .#pre-warmed
        # Result symlink: ./result -> /nix/store/<hash>-baseline-deps
        #
        # Doesn't replace cabal in the dev loop; useful when sharing a
        # warm closure across machines via cachix.org or Nix-store
        # NAR copy.
        # ----------------------------------------------------------------
        packages = {
          inherit hlint fourmolu ormolu hls;
          default = self.devShells.${system}.default;
        };

        # ----------------------------------------------------------------
        # Cachix integration (opt-in, not configured in this flake):
        #
        #   1. Create a binary cache at https://app.cachix.org
        #   2. `cachix authtoken <token>` (locally or in CI secrets)
        #   3. Push:  nix build .#default && cachix push <name> ./result
        #   4. Pull:  cachix use <name>
        #
        # When set up, every `nix develop` invocation across machines
        # pulls pre-built closures from the cache rather than rebuilding
        # GHC + libraries locally — the same kind of "pre-warmed
        # instance" effect as the GHCR Docker image, content-addressable
        # rather than tag-based.
        # ----------------------------------------------------------------
      });
}
