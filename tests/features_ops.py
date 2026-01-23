i = 0
i += 5
print(i)
i -= 2
print(i)
i *= 3
print(i)
i /= 3
print(i)

l = [10]
l[0] += 5
print(l[0])

class A:
    def __init__(self):
        self.x = 100

a = A()
a.x += 50
print(a.x)
