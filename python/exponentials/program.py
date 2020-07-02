import sys
import math
import itertools

def pretty(f):
    return " + ".join("%s\\cdot%s^n" % z for z in f)

def subst(f, n):
    return sum(a*b**n for (a, b) in f)

def verify_n_equals_1(f, d, file):
    a = subst(f, 1)
    if a % d == 0:
        print("\nFor $P(1)$: $$%s = %s \\cdot %s$$" % (a, a // d, d), file=file)
    else:
        raise RuntimeError("ERROR: Identity is false! %s is not divisible by %s." % (a, d))
        sys.exit(0)

def find_subidentity(f):
    elim_exp = f[-1][1]
    f_prime = [(a*(b - elim_exp), b) for (a, b) in f[:-1]]
    return (elim_exp, f_prime)

def prove_induction_hypothesis(f, d, n, file):
    (multiplier, f_prime) = find_subidentity(f)
    print("\nFor $P(n + 1)$: $$f(n + 1) = %s\\cdot f(n) + %s$$" % (multiplier, pretty(f_prime)), file=file)
    print("Thus $P(n) \\rightarrow P(n + 1)$ by lemma %s." % (n + 1), file=file)
    print("Thus $P(n)$ for $n > 0$ by induction.", file=file)
    prove(f_prime, d, n + 1, file)

def verify_final(f, d, file):
    mul = f[0][0] * f[0][1]
    if mul % d == 0:
        print("\nFor $P(n)$: $$%s = %s \\cdot %s$$" % (mul, mul // d, d), file=file)
    else:
        raise RuntimeError("ERROR: Identity is false or unprovable! %s is not divisible by %s." % (f[0][0], d))

def prove(f, d, n = 0, file=sys.stdout):
    if n > 0:
        print("\n\n###Lemma %s" % n, file=file)
    else:
        print("##Theorem", file=file)
    print("$$f(n) = %s$$ is divisible by $%s$." % (pretty(f), d), file=file)
    print("####Proof", file=file)
    print("Let $P(n)$ be the proposition that $$f(n) = %s$$ is divisible by $%s$." % (pretty(f), d), file=file)
    if len(f) == 1:
        verify_final(f, d, file)
    else:
        verify_n_equals_1(f, d, file)
        prove_induction_hypothesis(f, d, n, file)
    if n == 0:
        print("\n\nQED.", file=file)

prove([(1, 2903), (-1, 803), (-1, 464), (1, 261)], 1897, file=open("proof.md", 'w'))
