import math
def num_factors(n):
    lim =  math.ceil(math.sqrt(n))
    i = 1
    count = 0
    while i<lim :
        if n % i == 0:
            count += 2
        i = i + 1
    return count

def find_triangular_number(factor_limit):
    i = 2
    n = 1 + i
    factors = num_factors(n+i)
    while factors < factor_limit:
        i = i + 1
        n = n + i
        factors = num_factors(n)
    return n

triangular = find_triangular_number(500)

print triangular
