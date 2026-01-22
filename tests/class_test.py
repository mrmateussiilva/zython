class Greeter:
    def __init__(self, name):
        self.name = name

    def greet(self):
        print("Hello, " + self.name)


def message(msg):
    print(msg)


c = 0
while c < 10:
    print(c)
    c = c + 1

# g = Greeter("World")
# g.greet()

# g.name = "Zython"
# g.greet()
