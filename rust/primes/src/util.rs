pub fn odd_fact(n: usize) -> (usize, usize) {
    let mut n = n - 1;
    let mut r = 0;
    while n & 1 == 0 {
        n >>= 1;
        r += 1;
    }
    (n, r)
}

pub fn isqrt(n: usize) -> usize {
    if n < 2 {
        n
    } else {
        (n as f64).sqrt() as usize
    }
}

pub fn is_square(mut n: usize) -> bool {
    const MASK: usize = 0xC840C04048404040;
    if MASK.overflowing_shl(n as u32).0 & 0x8000000000000000 == 0 {
        return false;
    }

    let ntz = n.trailing_zeros();
    if (ntz & 1) != 0 {
        return false;
    }
    n >>= ntz;
    
    if (n & 7) != 1 {
        return false;
    }

    let tst = (n as f64).sqrt() as usize;
    tst * tst == n
}