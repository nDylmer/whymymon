#!/usr/bin/env python3
import sys

for ts, line in enumerate(sys.stdin, start=1):
    line = line.rstrip('\n')
    if line:
        print(f"{line},{ts}")
