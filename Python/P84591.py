def absValue(x):
    if x < 0: 
        x *= -1
    return x

def power(x,p):
    y = 1
    for i in range(0,p):
        y = x*y
    return y
    
def squareRoot(x):
    for i in range(1,x):
        if i*i >= x:
            return i
    
def isPrime(x):
    for i in range(2,squareRoot(x)):
        if x%i == 0:
            return False
    return True

def slowFib(n):
    if n == 0:
        return 0
    elif n == 1:
        return 1
    else:
        return slowFib(n-1) + slowFib(n-2)
    
    
def quickFib(n):
    fib0 = 0
    fib1 = 1
    for i in range(1,n):
        aux = fib1
        fib1 = fib0 + fib1
        fib0 = aux
    if n == 0: fib1 = 0
    return fib1

print(quickFib(0))
