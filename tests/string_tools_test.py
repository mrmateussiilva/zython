text = "  hello world  "
cleaned = text.strip()
print(cleaned)
parts = cleaned.split(" ")
print(parts[0])
print(parts[1])

csv = "a,b,c"
items = csv.split(",")
print(len(items))
print(items[1])
