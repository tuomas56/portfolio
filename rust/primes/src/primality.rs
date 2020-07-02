use crate::{util, modular};
use rug::Integer;

const PRECOMPUTED: [usize; 6] = [2, 3, 5, 7, 11, 13];
const PRECOMPUTED_BOUND: usize = 17 * 17 - 1;

pub fn strong_pseudoprime(n: usize, a: usize) -> bool {
    let (d, s) = util::odd_fact(n);

    let mut x = modular::exp(a, d, n);

    if x == 1 || x == n - 1 {
        return true;
    }

    for _ in 1..s {
        x = modular::mul(x, x, n);
        if x == n - 1 {
            return true;
        }
    }

    false
}



pub fn strong_pseudoprime_int(n: Integer, a: Integer) -> bool {
    fn odd_fact(mut n: Integer) -> (Integer, usize) {
        n -= 1;
        let mut r = 0;
        while n.is_even() {
            n >>= 1;
            r += 1;
        }
        (n, r)
    }
    
    let (d, s) = odd_fact(n.clone());
    
    let mut x = a.pow_mod(&d, &n).unwrap();

    let nm1 = n.clone() - 1;

    if x == 1 || x == nm1 {
        return true;
    }

    for _ in 1..s {
        x.pow_mod_mut(&Integer::from(2), &n).unwrap();
        if x == nm1 {
            return true;
        }
    }

    false
}

pub fn is_prime_int(n: Integer) -> bool {
    if n.is_even() {
        n == 2
    } else if n < 3317044064679887385961981u128 {
        for &base in &[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41] {
            if !strong_pseudoprime_int(n.clone(), Integer::from(base)) {
                return false;
            }
        }

        true
    } else {
        for base in 0..((693 * n.significant_bits()) / 500) {
            if is_prime(base as usize) {
                if !strong_pseudoprime_int(n.clone(), Integer::from(base)) {
                    return false;
                }
            }
        }

        true
    }
}

pub fn is_prime(n: usize) -> bool {
    for &prime in &PRECOMPUTED {
        if n % prime == 0 {
            return n == prime;
        }
    }
    
    if n <= PRECOMPUTED_BOUND {
        true
    } else {
        for &base in &[2, 325, 9375, 28178, 450775, 9780504, 1795265022] {
            if !strong_pseudoprime(n, base) {
                return false;
            }
        }
        true
    }
}
