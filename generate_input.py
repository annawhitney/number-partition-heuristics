import random

rands = [random.randrange(1, 10**12) for _ in range(100)]

for rand in rands:
    print rand
