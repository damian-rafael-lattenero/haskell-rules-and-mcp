#!/usr/bin/env python3
"""hangwatch — run a command under a hang watchdog. Never use the user as the timeout.

Usage:
  hangwatch.py LABEL CMD [ENV=VAL ...] [STALL_S=secs] [TOTAL_S=secs]

Runs CMD in a new process group with the KEY=VAL env vars exported (STALL_S and
TOTAL_S are consumed by the watchdog itself). Output streams to /tmp/hangwatch-LABEL.log.
Verdicts:
  DONE          — process exited 0 within budget
  EXIT-n        — process exited non-zero
  HANG(stall)   — log did not grow for STALL_S seconds
  HANG(budget)  — TOTAL_S elapsed
On HANG the whole process group is SIGKILLed and the last log lines (the "last
step") are printed. Exit code 0 iff DONE.
"""
import os
import signal
import subprocess
import sys
import time

POLL_S = 2.0


def tail_of(path, lines=60):
    try:
        with open(path, "rb") as f:
            chunks = f.read()[-64 * 1024:].decode("utf-8", "replace")
        return "\n".join(chunks.splitlines()[-lines:])
    except OSError:
        return "(no output)"


def main():
    if len(sys.argv) < 3:
        print(__doc__)
        sys.exit(2)
    label, cmd = sys.argv[1], sys.argv[2]
    env = dict(os.environ)
    stall_s, total_s = 180.0, 2400.0
    for kv in sys.argv[3:]:
        key, _, val = kv.partition("=")
        if key == "STALL_S":
            stall_s = float(val)
        elif key == "TOTAL_S":
            total_s = float(val)
        else:
            env[key] = val

    log_path = "/tmp/hangwatch-%s.log" % label
    log = open(log_path, "wb")
    start = time.time()
    proc = subprocess.Popen(
        [cmd], env=env, stdout=log, stderr=subprocess.STDOUT,
        stdin=subprocess.DEVNULL, start_new_session=True,
    )
    print("hangwatch[%s] pid=%d log=%s (STALL_S=%g TOTAL_S=%g)"
          % (label, proc.pid, log_path, stall_s, total_s))
    sys.stdout.flush()

    size, last_growth, verdict = 0, start, None
    while verdict is None:
        time.sleep(POLL_S)
        rc = proc.poll()
        try:
            cur = os.path.getsize(log_path)
        except OSError:
            cur = size
        now = time.time()
        if cur > size:
            size, last_growth = cur, now
        if rc is not None:
            verdict = "DONE" if rc == 0 else "EXIT-%d" % rc
        elif now - last_growth > stall_s:
            verdict = "HANG(stall)"
        elif now - start > total_s:
            verdict = "HANG(budget)"

    if verdict.startswith("HANG"):
        try:
            os.killpg(os.getpgid(proc.pid), signal.SIGKILL)
        except (ProcessLookupError, PermissionError):
            pass
    log.close()

    print("VEREDICTO: %s (%.0fs)" % (verdict, time.time() - start))
    print("── tail de %s ──" % log_path)
    print(tail_of(log_path))
    sys.exit(0 if verdict == "DONE" else 1)


if __name__ == "__main__":
    main()
