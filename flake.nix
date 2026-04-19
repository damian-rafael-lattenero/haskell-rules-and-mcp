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

        # Haskell tooling the MCP expects at the versions pinned in the
        # manifest. `haskell.compiler.ghc98` is the closest stable GHC on
        # nixos-24.05; the MCP tolerates any GHC 9.6+ that cabal accepts.
        ghc = pkgs.haskell.compiler.ghc98;
        cabal = pkgs.cabal-install;

        # The four optional quality-gate tools. Nix versions may lag behind
        # the manifest's pinned versions; when they diverge, the MCP's
        # resolution chain (host PATH → bundled → auto-download) keeps
        # working. Nix is the "I want reproducibility" path, not the only
        # one.
        hlint = pkgs.haskellPackages.hlint;
        fourmolu = pkgs.haskellPackages.fourmolu;
        ormolu = pkgs.haskellPackages.ormolu;
        hls = pkgs.haskell-language-server.override {
          supportedGhcVersions = [ "98" ];
        };

        node = pkgs.nodejs_22;
      in
      {
        # `nix develop` lands you in a shell with everything you need to
        # rebuild the MCP from source and run the Haskell playground.
        devShells.default = pkgs.mkShell {
          name = "haskell-flows-dev";
          packages = [
            ghc
            cabal
            hlint
            fourmolu
            ormolu
            hls
            node
            pkgs.git
            pkgs.gh
            pkgs.jq
          ];

          shellHook = ''
            echo "haskell-flows dev shell"
            echo "  ghc:      $(ghc --numeric-version)"
            echo "  cabal:    $(cabal --numeric-version)"
            echo "  hlint:    $(hlint --version | head -1)"
            echo "  fourmolu: $(fourmolu --version | head -1)"
            echo "  ormolu:   $(ormolu --version | head -1)"
            echo "  hls:      $(haskell-language-server-wrapper --version | head -1)"
            echo "  node:     $(node --version)"
            echo ""
            echo "Build the MCP:   cd mcp-server && npm install && npm run build"
            echo "Run tests:       cd mcp-server && npm run test:all"
          '';
        };

        # Expose the toolchain individually so CI can run
        # `nix build .#hlint` etc. without entering the dev shell.
        packages = {
          inherit hlint fourmolu ormolu hls;
          default = self.devShells.${system}.default;
        };
      });
}
