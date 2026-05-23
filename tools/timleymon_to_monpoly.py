import csv
import re
import sys
from collections import defaultdict


def parse_value(s):
    s = s.strip()
    if len(s) >= 2 and s[0] == '"' and s[-1] == '"':
        return s[1:-1]
    return s


def parse_line(line):
    # Use csv module to handle quoted fields with commas
    reader = csv.reader([line], skipinitialspace=True)
    fields = next(reader)
    if not fields:
        return None

    pred_name = fields[0].strip()
    ts = None
    args = []

    for f in fields[1:]:
        if "=" not in f:
            continue
        key, _, value = f.partition("=")
        key = key.strip()
        value = parse_value(value)
        if key == "tp":
            continue               
        elif key == "ts":
            ts = int(value)
        else:
            args.append(value)

    if ts is None:
        return None
    return pred_name, ts, args


def main():
    if len(sys.argv) < 2:
        sys.exit(1)

    events_by_ts = defaultdict(list)

    with open(sys.argv[1], "r", encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            parsed = parse_line(line)
            if parsed is None:
                continue
            pred, ts, args = parsed
            events_by_ts[ts].append((pred, args))


    for ts in sorted(events_by_ts):
        parts = [f"@{ts}"]
        for pred, args in events_by_ts[ts]:
            args_str = ", ".join(args)
            parts.append(f"{pred}({args_str})")
        print(" ".join(parts))


if __name__ == "__main__":
    main()
