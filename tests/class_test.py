class Greeter:
    def __init__(self, name):
        self.name = name

    def greet(self):
        print("Hello, " + self.name)


g = Greeter("World")
g.greet()

g.name = "Zython"
g.greet()
