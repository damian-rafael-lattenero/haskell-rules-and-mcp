# CI Performance — Five Optimizations

The CI pipeline went through a perf-batch in May 2026 that landed
**four** optimizations and **scoped** the fifth for future work. This
document is the reference for what each one does, what it's worth,
and how to extend or undo it.

## TL;DR

| # | Optimization | Status | Win |
|---|---|---|---|
| 1 | Pre-warmed E2E fixture | ✅ landed | ~1 min/cell warm |
| 2 | Hyper-local quick wins | ✅ landed | ~5-8 min/push |
| 3 | Pre-baked Docker image | ✅ image published, **not yet wired** | ~3 min cold-start when wired |
| 4 | Nix devShell extension | ✅ landed | reproducibility, +cachix opt-in |
| 5 | Self-hosted runner pool | 📋 documented, not implemented | requires infra |

## #1 — Pre-warmed E2E fixture

`mcp-server-haskell/test-e2e/Fixtures/Baseline/` is a minimal cabal
project containing the union of common E2E dependencies (QuickCheck,
aeson, text, containers, vector). The CI workflow builds it once
before the E2E pool starts:

```yaml
- name: Pre-warm E2E fixture deps
  working-directory: mcp-server-haskell/test-e2e/Fixtures/Baseline
  run: cabal build
```

`E2E.Fixture.copyBaselineInto` is the opt-in helper for scenarios
that want to skip the `ghc_project create + ghc_deps add QuickCheck`
round-trip and start from a pre-resolved cabal layout.

**Net**: ~2-4s saved per scenario × ~24 scenarios per shard cell
= ~1 minute saved per CI cell on warm runs, larger on cold.

## #2 — Hyper-local quick wins

Three structural changes inside `haskell-ci.yml`:

1. **Combined build step**: `cabal build all --only-dependencies` →
   `cabal build all` was a noop split. Single-step now uses
   `--keep-going` so multiple errors per push surface in one cell.
2. **`package-quality` job**: `Build haddocks`, `Cabal check`, `SDist
   round-trip` previously ran 11x per push. Now run once on a
   canonical cell.
3. **`cabal test --keep-going`**: surfaces all failing scenarios per
   shard instead of stopping at the first.

Aggregator now requires `[build-and-test, hlint, package-quality]`.

## #3 — Pre-baked Docker image

`.docker/Dockerfile.ci` is a multi-stage image that bakes:

* GHC + cabal at matrix versions (9.10.1, 9.12.2)
* hlint binary on PATH
* `~/.cabal/store/` pre-populated with the freeze closure
* The E2E Baseline fixture pre-built

The image is published to GHCR by `build-ci-image.yml`:

```
ghcr.io/<owner>/haskell-flows-ci:<ghc>-<freeze-hash>
ghcr.io/<owner>/haskell-flows-ci:<ghc>-latest
```

**To wire the image into the main CI** (do this once first build is green):

```yaml
# in .github/workflows/haskell-ci.yml under build-and-test.steps:
runs-on: ubuntu-latest
container:
  image: ghcr.io/${{ github.repository_owner }}/haskell-flows-ci:${{ matrix.ghc }}-latest
```

Then drop the `Set up GHC`, `Restore cabal store cache`, `Install
HLint`, and `Pre-warm E2E fixture deps` steps — the image already has
all four.

## #4 — Nix devShell extension

`flake.nix` now exposes:

* `devShells.default` — full toolchain (ghc, cabal, hlint, fourmolu,
  ormolu, hls, node)
* `devShells.ci` — slim toolchain mirroring CI's PATH
* `packages.*` — individual package outputs

To enable shared caching across machines (the third "pre-warmed
instance" path alongside `~/.cabal/store/` and the GHCR image):

```bash
# 1. Create cache: https://app.cachix.org → New Binary Cache
# 2. Auth locally:
cachix authtoken <token>
# 3. Push your local closure:
nix build .#default && cachix push <cache-name> ./result
# 4. Other machines pull on first `nix develop`:
cachix use <cache-name>
```

Add `cachix/cachix-action@v15` to CI workflows to populate the cache
from CI builds.

## #5 — Self-hosted runner pool (not yet implemented)

The user's vision: "40-50 instances Docker que ya están corriendo y
cada test agarra una y se ejecuta ahí". Concretely, this is a
**self-hosted runner pool** with persistent state.

### Requirements

* **Cloud account** (AWS/GCP/Azure or even a beefy bare-metal box)
* **Pool size**: ~10-50 ec2.t3.medium-equivalent runners
* **AMI**: Ubuntu 22.04 with GHC + cabal + ~/.cabal/store/ pre-warmed
* **GitHub registration**: each runner registers with the org via
  `actions-runner` daemon

### Stack options (in increasing complexity)

#### Option A — Static EC2 fleet (~$30/mo for 5 runners)

```hcl
# terraform/main.tf — sketch only, NOT production-ready
resource "aws_launch_template" "haskell_runner" {
  name          = "haskell-flows-runner"
  image_id      = "ami-XXXXXXX"  # build via Packer
  instance_type = "t3.medium"
  user_data     = base64encode(<<-EOT
    #!/bin/bash
    cd /home/ubuntu/actions-runner
    ./config.sh --url https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp \
                --token $${GH_REGISTRATION_TOKEN} --labels self-hosted,haskell-warm \
                --replace --unattended
    ./run.sh &
    EOT
  )
}

resource "aws_autoscaling_group" "haskell_runners" {
  desired_capacity = 5
  min_size         = 5
  max_size         = 5
  launch_template { id = aws_launch_template.haskell_runner.id }
}
```

Then in CI:

```yaml
runs-on: [self-hosted, haskell-warm]
```

#### Option B — Ephemeral via `actions-runner-controller` on Kubernetes

`actions-runner-controller` (ARC) on EKS/GKE — runners spin up on
demand, terminate after job. Better isolation, more complex to
operate. Worth it past ~30 runners.

#### Option C — Buildkite or CircleCI

Native runner pool concept; pay per minute used. Skip GitHub Actions
infrastructure altogether.

### When to pull the trigger

Self-hosted is worth it when:

1. You're hitting the 2000-min/month free tier hard (private repos)
2. macOS runners are queuing because of the org-wide 5-concurrent cap
3. The dev cycle is dominated by CI wait time (>10 min average)
4. You want to amortize a giant one-time setup cost (e.g. preloading
   a large hoogle database) across hundreds of runs/week

For the current haskell-flows traffic (~5-10 pushes/day to master),
the GHCR image (#3) gets you most of the way without infra burden.
Revisit when push volume hits ~30/day or macOS queueing becomes a
sustained pain point.

## Measuring impact

Baseline (pre-batch, May 2):
* ubuntu shard cell: ~6-13 min
* macOS shard cell: ~9-13 min
* Total CI wall-clock to green: ~13 min

Current (post-batch, May 3):
* TBD after image-wire commit. Expected: ~7-9 min wall-clock.

Run `gh run view <id>` and sum job durations to verify.
