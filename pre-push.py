#!/usr/bin/env python3

import sys

print(sys.argv)
print(input().split())
sys.stdin = open("/dev/tty", "r")
X = input("Do you want to continue?")
print("ok, done")
