#[derive(Debug)]
pub struct SimpleSieve {
    valid_til: usize,
    arr: Vec<bool>
}

#[derive(Debug, Copy, Clone)]
pub enum NoSuchPrime {
    IntervalTooSmall,
    NotFinishedSieving
}

impl SimpleSieve {
    pub fn new() -> SimpleSieve {
        SimpleSieve {
            valid_til: 2,
            arr: vec![false, false, true]
        }
    }

    pub fn with_capacity(n: usize) -> SimpleSieve {
        SimpleSieve {
            valid_til: 2,
            arr: {
                let mut arr = vec![true; n + 1];
                arr[0] = false;
                arr[1] = false;
                arr
            }
        }
    }

    pub fn resize(&mut self, n: usize) {
        self.valid_til = self.valid_til.min(n);
        self.arr.resize(n + 1, true);
    }

    pub fn pi(&self, n: usize) -> Result<usize, NoSuchPrime> {
        if self.valid_til >= n {
            if self.valid_til == self.arr.len() - 1 {
                Ok(self.primes().take_while(|p| *p <= n).count())
            } else {
                Err(NoSuchPrime::NotFinishedSieving)
            }
        } else {
            Err(NoSuchPrime::IntervalTooSmall)
        }
    }

    pub fn nth(&self, n: usize) -> Result<usize, NoSuchPrime> {
        if self.valid_til == self.arr.len() - 1 {
            self.primes().nth(n - 1).ok_or(NoSuchPrime::IntervalTooSmall)
        } else {
            self.primes().nth(n - 1).ok_or(NoSuchPrime::NotFinishedSieving)
        }
    }

    pub fn find_below(&self, n: usize) -> Result<usize, NoSuchPrime> {
        if self.valid_til >= n {
            Ok(self.primes().take_while(|p| *p <= n).last().unwrap())
        } else if self.arr.len() <= n {
            Err(NoSuchPrime::IntervalTooSmall)
        } else {
            Err(NoSuchPrime::NotFinishedSieving)
        }
    }

    fn is_prime(&self, n: usize) -> Result<bool, NoSuchPrime> {
        if self.valid_til >= n {
            Ok(self.arr[n])
        } else if self.arr.len() > n{
            Err(NoSuchPrime::NotFinishedSieving)
        } else {
            Err(NoSuchPrime::IntervalTooSmall)
        }
    }

    pub fn primes<'s>(&'s self) -> SimpleSievePrimes<'s> {
        SimpleSievePrimes::new(self)
    }

    pub fn sieve(&mut self) {
        if self.valid_til == self.arr.len() - 1 {
            return;
        }

        for p in 0..=self.valid_til {
            if self.arr[p] {
                let mut k = (if (self.valid_til + 1) % p == 0 {
                    self.valid_til + 1
                } else {
                    self.valid_til + 1 + p - ((self.valid_til + 1) % p)
                }).max(p * p);
                while k < self.arr.len() {
                    self.arr[k] = false;
                    k += p;
                }
            }
        }

        let mut i = self.valid_til + 1;

        while i * i < self.arr.len() {
            if self.arr[i] {
                let mut k = i * i;
                while k < self.arr.len() {
                    self.arr[k] = false;
                    k += i; 
                }
            }

            i += 1;
        }

        self.valid_til = self.arr.len() - 1;
    }
}

#[derive(Clone)]
pub struct SimpleSievePrimes<'s> {
    sieve: &'s SimpleSieve,
    idx: usize
}

impl<'s> SimpleSievePrimes<'s> {
    fn new(sieve: &'s SimpleSieve) -> SimpleSievePrimes<'s> {
        SimpleSievePrimes { sieve, idx: 0 }
    }
}

impl<'s> Iterator for SimpleSievePrimes<'s> {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        while self.idx <= self.sieve.valid_til && !self.sieve.arr[self.idx] {
            self.idx += 1;
        }

        if self.idx > self.sieve.valid_til {
            None
        } else {
            self.idx += 1;
            Some(self.idx - 1)
        }
    }
}