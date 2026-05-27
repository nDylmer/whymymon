#!/usr/bin/env python3
import argparse
import re
import sys


VERDICT_RE = re.compile(r"\([^()]*\)")


def count_verdicts(lines):
    total = 0
    per_line = []

    for line_no, line in enumerate(lines, start=1):
        _, sep, verdict_part = line.partition(":")
        if not sep:
            verdict_part = line

        count = len(VERDICT_RE.findall(verdict_part))
        if count:
            per_line.append((line_no, count))
            total += count

    return total, per_line


def main():
    parser = argparse.ArgumentParser(
        description="Count parenthesized verdict assignments in monitor output."
    )
    parser.add_argument("file", nargs="?", help="Input file. Reads stdin if omitted.")
    parser.add_argument(
        "--per-line",
        action="store_true",
        help="Also print the count found on each non-empty verdict line.",
    )
    args = parser.parse_args()

    if args.file:
        with open(args.file, "r", encoding="utf-8") as f:
            total, per_line = count_verdicts(f)
    else:
        total, per_line = count_verdicts(sys.stdin)

    if args.per_line:
        for line_no, count in per_line:
            print(f"line {line_no}: {count}")

    print(total)


if __name__ == "__main__":
    main()
