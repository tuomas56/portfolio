use crate::util;
use rug::Integer;

pub fn exp(mut a: usize, mut b: usize, n: usize) -> usize {
    if n == 1 {
        0
    } else {
        let mut res = 1;
        a = a % n;
        while b > 0 {
            if b % 2 == 1 {
                res = ((res as u128 * a as u128) % n as u128) as usize;
            }
            b >>= 1;
            a = ((a as u128 * a as u128) % n as u128) as usize;
        }
        res
    }
}

pub fn inv(a: usize, p: usize) -> usize {
    exp(a, p - 2, p)
}

pub fn mul(a: usize, b: usize, n: usize) -> usize {
    ((a as u128 * b as u128) % n as u128) as usize
}

pub fn add(a: usize, b: usize, p: usize) -> usize {
    (((a as u128) + (b as u128)) % p as u128) as usize
}

pub fn neg(a: usize, p: usize) -> usize {
    p - (a % p)
}

pub fn quadratic_residue(a: usize, p: usize) -> bool {
    if a < 2 {
        true
    } else {
        exp(a, (p - 1)/2, p) == 1
    }
}

pub fn sqrt(n: usize, p: usize) -> Option<usize> {
    if n < 2 || p == 2 {
        return Some(n);
    }

    if !quadratic_residue(n, p) {
        return None;
    }

    let (q, s) = util::odd_fact(p);

    if s == 1 {
        return Some(exp(n, (p + 1)/4, p));
    }

    let mut z = 2;
    while quadratic_residue(z, p) {
        z += 1;
    }

    let mut m = s;
    let mut c = exp(z, q, p);
    let mut t = exp(n, q, p);
    let mut r = exp(n, (q + 1)/2, p);

    while t != 1 {
        let mut i = 0;
        let mut tmp = t;

        while tmp != 1 {
            tmp = mul(tmp, tmp, p);
            i += 1;
        }
        
        let b = exp(c, 1 << m - i - 1, p);
        m = i;
        c = mul(b, b, p);
        t = mul(t, c, p);
        r = mul(r, b, p);
    }

    Some(r)
}