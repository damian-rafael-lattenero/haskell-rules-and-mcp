#!/usr/bin/env python3
"""Cold-start benchmark: GhcSession (Phase-2 in-process) vs Session (legacy).
Each call spawns a fresh MCP server so we measure true cold-start time.
"""
import subprocess
import time
import json
import os
import sys

BIN = "/Users/dlattenero/haskell-rules-and-mcp/clever-tesla-f860bc/mcp-server-haskell/dist-newstyle/build/aarch64-osx/ghc-9.12.2/haskell-flows-mcp-0.1.0.0/x/haskell-flows-mcp/build/haskell-flows-mcp/haskell-flows-mcp"
PROJECT = "/tmp/bench-project"

def bench(tool, args, warmup_first=True):
    """Spawn a fresh server, send initialize + one tool call, time the response."""
    env = os.environ.copy()
    env["HASKELL_PROJECT_DIR"] = PROJECT
    env["PATH"] = "/Users/dlattenero/.ghcup/bin:/Users/dlattenero/.cabal/bin:" + env["PATH"]

    messages = [
        {"jsonrpc": "2.0", "id": 1, "method": "initialize",
         "params": {"protocolVersion": "2024-11-05", "capabilities": {},
                    "clientInfo": {"name": "bench", "version": "0"}}},
        {"jsonrpc": "2.0", "method": "notifications/initialized", "params": {}},
        {"jsonrpc": "2.0", "id": 2, "method": "tools/call",
         "params": {"name": tool, "arguments": args}},
    ]

    start = time.monotonic()
    proc = subprocess.Popen([BIN], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                            stderr=subprocess.DEVNULL, env=env, cwd=PROJECT)

    # Send all messages at once
    payload = "".join(json.dumps(m) + "\n" for m in messages).encode()
    try:
        proc.stdin.write(payload)
        proc.stdin.flush()
    except BrokenPipeError:
        pass

    # Read responses line-by-line until we see id=2
    result = None
    while True:
        line = proc.stdout.readline()
        if not line:
            break
        try:
            resp = json.loads(line.decode())
        except json.JSONDecodeError:
            continue
        if resp.get("id") == 2:
            result = resp
            break

    elapsed = time.monotonic() - start

    # Clean shutdown
    try:
        proc.stdin.close()
    except Exception:
        pass
    proc.terminate()
    try:
        proc.wait(timeout=3)
    except subprocess.TimeoutExpired:
        proc.kill()
        proc.wait()

    return elapsed, result

def fmt(label, t, result):
    ms = t * 1000
    marker = ""
    if result is None:
        marker = "  (no response received)"
    elif "error" in result:
        marker = f"  (error: {str(result.get('error'))[:40]})"
    print(f"  {label:<40s} {ms:8.0f} ms{marker}")

def main():
    print("=== Cold-start benchmark (fresh MCP server per call) ===")
    print(f"Project: {PROJECT}")
    print(f"Binary:  {os.path.basename(BIN)} ({os.path.getsize(BIN) // (1024*1024)} MB)")
    print()
    print("# Warmup (one hit so cabal planning + package db cache settle)")
    tw, _ = bench("ghc_type", {"expression": "1"})
    print(f"  warmup ghc_type                         {tw*1000:8.0f} ms")
    print()
    print("# 3 repeats of each tool — each spawns a fresh MCP server")
    print()
    trials = 3
    for label, tool, args in [
        ("Phase-2 in-process · ghc_type",     "ghc_type",     {"expression": "map"}),
        ("Phase-2 in-process · ghc_complete", "ghc_complete", {"prefix": "fold"}),
        ("Phase-2 in-process · ghc_imports",  "ghc_imports",  {}),
        ("Phase-2 in-process · ghc_goto",     "ghc_goto",     {"name": "double"}),
        ("Legacy ghci subprocess · ghc_eval", "ghc_eval",     {"expression": "1 + 2"}),
        ("Legacy ghci subprocess · ghc_quickcheck", "ghc_quickcheck", {"expression": "\\x -> x == (x :: Int)"}),
    ]:
        times = []
        for _ in range(trials):
            t, _ = bench(tool, args)
            times.append(t * 1000)
        best = min(times)
        worst = max(times)
        avg = sum(times) / len(times)
        print(f"  {label:<42s} best={best:6.0f} ms · avg={avg:6.0f} ms · worst={worst:6.0f} ms")

if __name__ == "__main__":
    main()
