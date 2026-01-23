# Micro-benchmark para tree-walker

def fib(n):
    if n < 2:
        return n
    return fib(n - 1) + fib(n - 2)


# Func call + recursion
x = fib(35)

# Loop + arithmetic + locals
s = 0
for i in [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]:
    for j in [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]:
        s = s + i * j

# String concat
st = ""
for i in [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]:
    st = st + "a"

print(x)
print(s)
print(st)
