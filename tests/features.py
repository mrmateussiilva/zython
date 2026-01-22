x = 10
if x > 5:
    print("x is greater than 5")
else:
    print("x is small")

i = 0
while i < 3:
    print(i)
    i = i + 1


def add(a, b):
    return a + b


print(add(5, 7))


def fib(n):
    if n < 2:
        return n
    return fib(n - 1) + fib(n - 2)


print(fib(10))
