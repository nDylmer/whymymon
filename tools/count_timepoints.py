import sys

total = 0

for line in open(sys.argv[1]):
    timepoints = line.split()[-1]

    if "-" in timepoints:
        start, end = map(int, timepoints.split("-"))
        total += end - start + 1
    else:
        total += 1

print(total)
