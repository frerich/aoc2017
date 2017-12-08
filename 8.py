from collections import defaultdict
import sys

def step(registers, reg, op, value, _, left, condition, right):
    if eval("registers['%s'] %s %s" % (left, condition, right)):
        registers[reg] += int(value) if op == 'inc' else -int(value)
    return max(registers.values())

def maxRegisterValues(lines):
    registers = defaultdict(int)
    return [step(registers, *line.split()) for line in lines]

if __name__ == "__main__":
    maxima = maxRegisterValues(sys.stdin.readlines())
    print(maxima[-1])
    print(max(maxima))
