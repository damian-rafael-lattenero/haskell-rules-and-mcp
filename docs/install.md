# Installing haskell-flows

Three install paths, in increasing order of self-sufficiency. Pick the
one that fits your setup.

Minimum assumption for all three: **a working GHC + cabal**. If you
don't have that, install [GHCup](https://www.haskell.org/ghcup/) first
— it provisions GHC, cabal, and HLS with one script.

---

## 1. From a release binary (fastest)

Download the pre-built binary matching your OS from
<https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases/latest>:

| Asset                                   | Platform          |
|-----------------------------------------|-------------------|
| `haskell-flows-mcp-linux-x86_64`        | Linux             |
| `haskell-flows-mcp-darwin-arm64`        | macOS Apple Silicon |

```bash
# Linux example
curl -LO https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases/latest/download/haskell-flows-mcp-linux-x86_64
chmod +x haskell-flows-mcp-linux-x86_64
sudo mv haskell-flows-mcp-linux-x86_64 /usr/local/bin/haskell-flows-mcp
haskell-flows-mcp --help   # sanity check
```

Binaries are dynamically linked. If you see a libgmp / libffi error on
Linux, either install the system package or fall back to method 2.

---

## 2. From Hackage (once published)

```bash
cabal update
cabal install haskell-flows-mcp --install-method=copy --installdir=~/.local/bin
```

Adds `haskell-flows-mcp` to `~/.local/bin`. Make sure that directory
is on your `PATH`:

```bash
export PATH="$HOME/.local/bin:$PATH"
```

---

## 3. From source (offline / dev)

```bash
git clone https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp
cd haskell-rules-and-mcp/mcp-server-haskell
cabal install exe:haskell-flows-mcp --install-method=copy --installdir=~/.local/bin
```

Same outcome as method 2, but builds the exact source in the checkout
— useful when you want to run a branch or a local patch.

---

## Wiring it into an MCP client

Every MCP client (Claude Code, Cursor, any other) reads a JSON
config. Copy the project's [`.mcp.example.json`](../.mcp.example.json)
and edit paths:

```json
{
  "mcpServers": {
    "haskell-flows": {
      "command": "haskell-flows-mcp",
      "env": {
        "HASKELL_PROJECT_DIR": "/absolute/path/to/your/haskell/project"
      }
    }
  }
}
```

### Client-specific notes

**Claude Code (CLI).** Save the config as `.mcp.json` at your repo
root; Claude Code auto-discovers it. Verify with `/mcp` in the REPL
and run `ghc_session(action="status")` to confirm the server is
alive.

**Cursor / VS Code MCP extensions.** Config path depends on the
extension. Consult its docs, but the JSON shape above is universal.

### Environment variables

| Variable                  | Required | Default                        |
|---------------------------|----------|--------------------------------|
| `HASKELL_PROJECT_DIR`     | no       | Current working directory      |

`HASKELL_PROJECT_DIR` is the root of the Haskell project the server
operates on. If unset, the server uses the directory it was invoked
from — usually the MCP client's CWD, which is the repo root for Claude
Code.

---

## Verifying the install

Once the client is wired, ask the agent to run:

```
ghc_toolchain_status
```

That returns a structured inventory of every external binary the
server can delegate to (ghc, cabal, hlint, fourmolu, ormolu, hoogle,
hls). Missing tools are reported as `available: false`; install them
as needed with `cabal install <tool>`.

For a deeper smoke test:

```
ghc_session(action="status")
ghc_load(module_path="src/Main.hs")    # replace with a real module
```

If both succeed, the install is working end-to-end.

---

## Upgrading

Method 1 (release binary): re-download and replace `/usr/local/bin/haskell-flows-mcp`.

Methods 2 & 3: `cabal update && cabal install haskell-flows-mcp --overwrite-policy=always`.

Release notes for every version are on the
[GitHub releases page](https://github.com/damian-rafael-lattenero/haskell-rules-and-mcp/releases).

---

## Uninstalling

Whichever method you used, the binary lives wherever you put it:

```bash
rm /usr/local/bin/haskell-flows-mcp   # or ~/.local/bin/haskell-flows-mcp
```

Remove the `haskell-flows` entry from your MCP client's config to stop
the auto-start.
