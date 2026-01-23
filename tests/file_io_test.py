f = open("tests/test_file.txt", "w")
f.write("line1\nline2")
f.close()

f2 = open("tests/test_file.txt", "r")
content = f2.read()
f2.close()

print(content)
lines = content.split("\n")
print(len(lines))
print(lines[0])
