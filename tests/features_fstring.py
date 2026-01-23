name = "Zython"
v = 42
print(f"Hello {name}, value is {v} and {v+1}")

l = [10, 20]
print(f"List item: {l[0]}")

class A:
    def __init__(self):
        self.x = 99
a = A()
print(f"Obj: {a.x}")
