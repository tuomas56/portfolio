import sys
import math
import itertools

def subst(f, n):
    return sum(a*b**n for (a, b) in f)

def verify_n_equals_1(f, d, file):
    if subst(f, 1) % d != 0:
        raise RuntimeError()

def find_subidentity(f):
    elim_exp = f[-1][1]
    f_prime = [(a*(b - elim_exp), b) for (a, b) in f[:-1]]
    return (elim_exp, f_prime)

def prove_induction_hypothesis(f, d, n, file):
    (multiplier, f_prime) = find_subidentity(f)
    prove(f_prime, d, n + 1, file)

def verify_final(f, d, file):
    mul = f[0][0] * f[0][1]
    if mul % d != 0:
        raise RuntimeError()

def prove(f, d, n = 0, file=sys.stdout):
    f.sort(key=lambda x:x[1], reverse=True)
    if len(f) == 1:
        verify_final(f, d, file)
    else:
        verify_n_equals_1(f, d, file)
        prove_induction_hypothesis(f, d, n, file)

identities = []
n = 0
i = 0

for perm in itertools.product(itertools.product(range(2, 10), range(1000, 1020)), itertools.product(range(2, 10), range(3200, 3400))):
    if i % 1500 == 0:
        print(perm)
    i += 1
    for divisor in range(600, 700):
        n += 1
        try:
            perm = list(perm)
            if perm[0][1] == perm[1][1]: continue
            if perm[0][0] % divisor == 0 or perm[1][0] % divisor == 0: continue
            if perm[0][0] == perm[0][1] or perm[1][0] == perm[1][1]: continue
            if divisor % perm[0][0] == 0 or divisor % perm[1][0] == 0: continue
            if perm[0][0] == perm[1][1] or perm[1][0] == perm[0][1]: continue
            if perm[0][0] == perm[1][0]: continue
            if (perm[0][0] * perm[0][1]) % divisor == 0: continue
            if (perm[1][0] * perm[1][1]) % divisor == 0: continue
            prove(list(perm), divisor)
            identities.append((perm, divisor))
        except RuntimeError:
            continue

print(print(identities), len(identities), n, len(identities)/n)
