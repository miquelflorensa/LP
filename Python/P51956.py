def average(L):
    ave = 0
    for i in L: ave += i
    ave /= len(L)
    return ave

print(average([1,2,3,4]))
