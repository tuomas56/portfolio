use std::collections::HashMap;
use std::sync::Arc;
use rug::Integer;
use crate::modular;

#[derive(Debug, Clone)]
pub struct Polynomial {
    pub a: Integer,
    pub b: Integer,
    pub isqrta: Integer
}

#[derive(Debug, Copy, Clone)]
pub struct FactorBaseEntry {
    pub value: usize,
    pub log2: usize,
    pub sqrt: usize
}

#[derive(Debug, Clone)]
pub struct Relation {
    pub sqrt: Integer,
    pub evec: Vec<usize>
}

#[derive(Debug, Clone)]
pub struct SieveInformation {
    pub n: Integer,
    pub interval: usize,
    pub hlog2n: usize,
    pub leniency: usize,
    pub factor_base: Vec<FactorBaseEntry>,
    pub large_prime_bound: Integer
}

#[derive(Debug, Clone)]
pub struct SieveResults {
    pub poly: Polynomial,
    pub full_relations: Vec<Relation>,
    pub partial_relations: HashMap<Integer, Relation>,
    pub initial: usize,
    pub combined: usize
}

pub struct QuadraticSieve {
    poly: Polynomial,
    info: Arc<SieveInformation>,
    data: Vec<usize>,
    start_value: usize
}

impl QuadraticSieve {
    pub fn new(poly: Polynomial, info: Arc<SieveInformation>) -> QuadraticSieve {
        let data = vec![0; info.interval];

        QuadraticSieve {
            poly, info, data, start_value: 0
        }
    }

    pub fn clear(&mut self) {
        self.start_value += self.info.interval;
        self.data.clear();
        self.data.resize(self.info.interval, 0);
    }

    pub fn sieve(&mut self) {
        for p in &self.info.factor_base {
            let amodp = (self.poly.a.clone() % (p.value as u64)).to_usize_wrapping();
            let bmodp = (self.poly.b.clone() % (p.value as u64)).to_usize_wrapping();
            let bmodp = modular::add(self.start_value, bmodp, p.value);
            let ainv = modular::inv(amodp, p.value);
            let offset = modular::mul(ainv, modular::add(p.sqrt, modular::neg(bmodp, p.value), p.value), p.value);

            let mut i = offset;
            while i < self.data.len() {
                self.data[i] += p.log2;
                i += p.value;
            }

            if p.value > 2 {
                let offset = modular::mul(ainv, modular::neg(modular::add(p.sqrt, bmodp, p.value), p.value), p.value);

                let mut i = offset;
                while i < self.data.len() {
                    self.data[i] += p.log2;
                    i += p.value;
                }
            }
        }
    }

    pub fn collect(&mut self, res: &mut SieveResults) {
        let test_value = self.info.hlog2n + 64 - (self.info.interval + self.start_value).leading_zeros() as usize - self.info.leniency;
        for x in 0..self.data.len() {
            if self.data[x] >= test_value {
                let mut sqrt = &self.poly.a * Integer::from(x) + &self.poly.b + (self.start_value as u64);
                let mut fxa = (sqrt.clone().square() - &self.info.n) / &self.poly.a;
                sqrt *= &self.poly.isqrta;
                let mut evec = Vec::new();
                for p in &self.info.factor_base {
                    let mut count = 0;
                    let p = Integer::from(p.value);
                    loop {
                        let (quot, rem) = fxa.clone().div_rem(p.clone());
                        if rem == 0 {
                            count += 1;
                            fxa = quot;
                        } else {
                            break;
                        }
                    }
                    evec.push(count);
                }

                if fxa == 1 {
                    res.full_relations.push(Relation {
                        sqrt, evec
                    });
                    res.initial += 1;
                } else if fxa <= self.info.large_prime_bound {
                    if let Some(other) = res.partial_relations.get(&fxa) {
                        if let Ok(ifxa) = fxa.invert(&self.info.n) {
                            let sqrt = (sqrt * &other.sqrt) * ifxa;
                            for (i, e) in evec.iter_mut().enumerate() {
                                *e += other.evec[i];
                            }
                            res.full_relations.push(Relation {
                                sqrt, evec
                            });
                            res.combined += 1;
                        }
                    } else {
                        res.partial_relations.insert(fxa, Relation {
                            sqrt, evec
                        });
                    }
                }
            }
        }
    }
}