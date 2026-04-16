#!/usr/bin/env python3
import re
import sys

for line in sys.stdin:
    line = re.sub(r"(x\d+=)([^,\n']+)", r"\1'\2'", line)
    print(line, end="")
