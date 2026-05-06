#!/usr/bin/env python3
import argparse
import re
from collections import OrderedDict

LINE_RE = re.compile(
    r'^\s*([^,]+)\s*,\s*tp=(\d+)\s*,\s*ts=(\d+)\s*,\s*user="((?:[^"\\]|\\.)*)"\s*,\s*bot=(True|False)\s*$'
)

def esc(s: str) -> str:
    return s.replace("\\", "\\\\").replace('"', '\\"')

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("input")
    ap.add_argument("output")
    ap.add_argument("--use-tp", action="store_true",
                    help="group by tp instead of ts")
    args = ap.parse_args()

    groups = OrderedDict()  # key -> [event lines]

    with open(args.input, "r", encoding="utf-8") as f:
        for ln, raw in enumerate(f, 1):
            line = raw.strip()
            if not line:
                continue
            m = LINE_RE.match(line)
            if not m:
                raise ValueError(f"Bad line {ln}: {line}")

            pred, tp, ts, user, bot = m.groups()
            key = int(tp) if args.use_tp else int(ts)
            evt = f'{pred}("{esc(user)}",{bot})'

            groups.setdefault(key, []).append(evt)

    with open(args.output, "w", encoding="utf-8") as out:
        for t, evts in sorted(groups.items()):
            out.write(f"@{t}\n")
            for e in evts:
                out.write(e + "\n")
            out.write("\n")

if __name__ == "__main__":
    main()
