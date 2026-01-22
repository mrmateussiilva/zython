# Teste básico de listas
nums = [1, 2, 3]
print(nums)

# Acesso por índice
print(nums[0])
print(nums[2])

# Modificação
nums[1] = 99
print(nums)

# Append
nums.append(4)
print(nums)

# Lista de Tipos mistos
mixed = [1, "dois", True, [10, 20]]
print(mixed)
print(mixed[3])
print(mixed[3][1])

# Modificar aninhado
mixed[3][0] = 999
print(mixed)
