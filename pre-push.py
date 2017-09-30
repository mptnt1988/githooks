#!/usr/bin/env python3

import sys

print(sys.argv)
print(input().split())
sys.stdin = open("/dev/tty", "r")
X = input("Do you want to continue? (yes/...)")
if X == "yes":  # Type 'yes' and enter
    print("ok, done")
else:
    print("Not ok, ending...")
