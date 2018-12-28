#!/usr/bin/python3
import string
import random
abet = string.ascii_lowercase
MAX = 20000
print(MAX)
s = ''.join([random.choice(abet) for _ in range(MAX)])
print(s)
