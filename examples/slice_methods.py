nums = [1, 2, 3, 4, 5]
print(nums[1:4])
print(nums[:2])
print(nums[2:])
print(nums[-3:-1])

s = "AbCde"
print(s[1:4])
print(s.lower())
print(s.upper())

nums.pop()
nums.extend([6, 7])
print(nums)

m = {"a": 1}
print(m.get("a"))
print(m.get("b"))
print(m.get("b", 9))
