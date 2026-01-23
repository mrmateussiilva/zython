print("Start")

try:
    print("In try")
    raise "Something went wrong"
    print("Unreachable")
except:
    print("Caught exception!")

print("End")

def fail():
    raise "Error in function"

try:
    fail()
except:
    print("Caught function error")

print("Done")