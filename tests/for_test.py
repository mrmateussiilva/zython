# Teste de for loop
nums = [1, 2, 3]
total = 0
for n in nums:
    total = total + n
print(total)

names = ["Alice", "Bob"]
for name in names:
    print("Hello, " + name + "!")

# Lista vazia
empty = []
for x in empty:
    print("Should not print this")

# Escopo da variável de loop
val = 100
for v in [1, 2]:
    val = val + v
print(val) # Should be 103 (100 + 1 + 2)
