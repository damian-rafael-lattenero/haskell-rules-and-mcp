#!/usr/bin/env python3
"""Dogfood driver daemon: holds ONE haskell-flows-mcp stdio server alive
and serves tool calls from a FIFO, logging per-call metrics.

Usage:
  mcpd.py daemon <binary> <project_dir> <spool_dir> &
  mcpd.py call   <spool_dir> <tool> <json-args> [timeout_s]

FIFO protocol (one line per call):
  <tool>\t<json-args>\t<resp-file>
The daemon writes the unwrapped tool payload (pretty JSON) to <resp-file>
and appends "<seq> <tool> <latency_s> <wall>" to <spool>/metrics.log.
"""
import json, os, subprocess, sys, threading, time, queue

def daemon(binary, project_dir, spool):
    os.makedirs(spool, exist_ok=True)
    env = dict(os.environ)
    env["HASKELL_PROJECT_DIR"] = project_dir
    proc = subprocess.Popen([binary], stdin=subprocess.PIPE,
                            stdout=subprocess.PIPE, stderr=open(f"{spool}/server.stderr.log", "ab"),
                            env=env)
    responses = queue.Queue()

    def reader():
        for line in proc.stdout:
            line = line.strip()
            if not line:
                continue
            try:
                responses.put(json.loads(line))
            except json.JSONDecodeError:
                with open(f"{spool}/raw.log", "ab") as f:
                    f.write(b"BADLINE " + line + b"\n")
    threading.Thread(target=reader, daemon=True).start()

    next_id = [0]

    def rpc(method, params=None, notify=False):
        next_id[0] += 1
        msg = {"jsonrpc": "2.0", "method": method}
        if params is not None:
            msg["params"] = params
        if not notify:
            msg["id"] = next_id[0]
        proc.stdin.write((json.dumps(msg) + "\n").encode())
        proc.stdin.flush()
        if notify:
            return None
        deadline = time.time() + 600
        while time.time() < deadline:
            try:
                r = responses.get(timeout=1)
            except queue.Empty:
                if proc.poll() is not None:
                    return {"fatal": "server exited", "code": proc.returncode}
                continue
            if r.get("id") == next_id[0]:
                return r
            # stale/unsolicited — drop
        return {"fatal": "rpc timeout"}

    # ---- handshake ----
    init = rpc("initialize", {"protocolVersion": "2024-11-05",
                              "capabilities": {},
                              "clientInfo": {"name": "dogfood-driver", "version": "1.0"}})
    with open(f"{spool}/initialize.json", "w") as f:
        json.dump(init, f, indent=2)
    instr = ""
    try:
        instr = init["result"]["instructions"]
    except Exception:
        pass
    with open(f"{spool}/instructions.txt", "w") as f:
        f.write(instr)
    rpc("notifications/initialized", notify=True)
    tlist = rpc("tools/list")
    with open(f"{spool}/tools.json", "w") as f:
        json.dump(tlist, f, indent=2)
    with open(f"{spool}/daemon.ready", "w") as f:
        f.write(str(time.time()))

    seq = 0
    # O_RDWR so the daemon holds its own write end: the FIFO never
    # hits EOF when a caller closes its side (EOF killed daemon v1).
    if not os.path.exists(f"{spool}/cmd.fifo"):
        os.mkfifo(f"{spool}/cmd.fifo")
    fifo_fd = os.open(f"{spool}/cmd.fifo", os.O_RDWR)
    pending = b""
    while True:
        chunk = os.read(fifo_fd, 65536)
        if not chunk:
            time.sleep(0.05)
            continue
        pending += chunk
        while b"\n" in pending:
            raw, pending = pending.split(b"\n", 1)
            parts = raw.decode().split("\t")
            if len(parts) != 3:
                continue
            tool, args_json, resp_file = parts
            seq += 1
            t0 = time.time()
            r = rpc("tools/call", {"name": tool,
                                   "arguments": json.loads(args_json) if args_json else {}})
            latency = time.time() - t0
            payload = r.get("result", r)
            unwrapped = payload
            try:
                unwrapped = json.loads(payload["content"][0]["text"])
            except Exception:
                pass
            with open(resp_file, "w") as f:
                json.dump(unwrapped, f, indent=2)
            with open(f"{spool}/metrics.log", "a") as f:
                f.write(f"{seq}\t{tool}\t{latency:.2f}\t{time.time():.0f}\n")

def call(spool, tool, args_json, timeout_s):
    resp = f"{spool}/resp.{time.time_ns()}.json"
    with open(f"{spool}/cmd.fifo", "w") as fifo:
        fifo.write(f"{tool}\t{args_json}\t{resp}\n")
    deadline = time.time() + timeout_s
    while time.time() < deadline:
        if os.path.exists(resp):
            time.sleep(0.05)
            with open(resp) as f:
                print(f.read(), end="")
            os.unlink(resp)
            return 0
        time.sleep(0.1)
    print(json.dumps({"driver": "TIMEOUT", "tool": tool, "timeout_s": timeout_s}))
    return 1

def callfile(spool, tool, path, timeout_s):
    """Args JSON from a file — lets callers embed big sources cleanly."""
    with open(path) as f:
        return call(spool, tool, f.read(), timeout_s)

def writemodule(spool, module, src_path, timeout_s):
    """ghc_module(action=write) with the full module source from a file."""
    with open(src_path) as f:
        src = f.read()
    args = json.dumps({"action": "write", "module": module, "code": src})
    return call(spool, "ghc_module", args, timeout_s)

if __name__ == "__main__":
    if sys.argv[1] == "daemon":
        daemon(sys.argv[2], sys.argv[3], sys.argv[4])
    elif sys.argv[1] == "callfile":
        sys.exit(callfile(sys.argv[2], sys.argv[3], sys.argv[4],
                          float(sys.argv[5]) if len(sys.argv) > 5 else 180.0))
    elif sys.argv[1] == "write-module":
        sys.exit(writemodule(sys.argv[2], sys.argv[3], sys.argv[4],
                             float(sys.argv[5]) if len(sys.argv) > 5 else 180.0))
    else:
        sys.exit(call(sys.argv[2], sys.argv[3], sys.argv[4],
                      float(sys.argv[5]) if len(sys.argv) > 5 else 180.0))
