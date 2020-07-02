#![feature(vec_remove_item)]

extern crate rug;
extern crate indicatif;

mod util;
mod modular;
mod generator;
mod primality;
mod sieve;
mod gaussian;

use rug::Integer;
use std::sync::Arc;
use std::collections::HashMap;

fn trial_division(primes: &generator::SimpleSieve, mut n: Integer) -> (Integer, Vec<Integer>) {
    println!("trial division to 26 bits:");
    let mut res = Vec::new();
    let max = n.to_usize().map(|n| util::isqrt(n).min(1 << 26)).unwrap_or(1 << 26);
    for k in primes.primes() {
        if k > max {
            break;
        }

        while n.clone() % (k as u64) == 0 {
            println!("factor: {}", k);
            n /= k as u64;
            res.push(Integer::from(k));
        }
    }
    
    (n, res)
}

fn factor(primes: &generator::SimpleSieve, n: Integer) -> Vec<Integer> {
    if primality::is_prime_int(n.clone()) {
        println!("miller test => true");
        println!("factor: {}", n);
        return vec![n];
    } else {
        println!("miller test => false");
    }

    let (n, mut resp) = trial_division(primes, n);

    if primality::is_prime_int(n.clone()) {
        println!("miller test => true");
        println!("factor: {}", n);
        resp.push(n);
        return resp;
    } else {
        println!("miller test => false");
    }

    println!("quadratic sieve on {}:", n);
    let mut pgen = generator::SimpleSieve::with_capacity(100000);
    pgen.sieve();
    let mut factor_base = Vec::new();
    for prime in pgen.primes() {
        let nmodp = (n.clone() % (prime as u64)).to_usize_wrapping();
        if let Some(sqrt) = modular::sqrt(nmodp, prime) {
            factor_base.push(sieve::FactorBaseEntry {
                log2: (prime as f64).log2() as usize,
                value: prime, sqrt
            });
        }
    }
    //println!("fb: {}", factor_base.len());

    let hlog2n = (n.significant_bits()/2) as usize;
    let sqrtn = n.clone().sqrt();
    let leniency = 64 - factor_base.last().unwrap().value.leading_zeros() as usize;
    let large_prime_bound = Integer::from(factor_base.last().unwrap().value).square();
    let info = sieve::SieveInformation {
        n: n.clone(), hlog2n, interval: 20000,
        leniency, factor_base: factor_base.clone(),
        large_prime_bound
    };

    let poly = sieve::Polynomial { a: Integer::from(1), b: sqrtn, isqrta: Integer::from(1).sqrt().invert(&n).unwrap() };
    let mut sieve = sieve::QuadraticSieve::new(poly.clone(), Arc::new(info));
    let mut res = sieve::SieveResults {
        poly: poly.clone(),
        full_relations: Vec::new(),
        partial_relations: HashMap::new(),
        initial: 0, combined: 0
    };

    let bar = indicatif::ProgressBar::new((factor_base.len() + factor_base.len()/10) as u64);
    bar.set_style(indicatif::ProgressStyle::default_bar()
    .template("[{elapsed_precise}] {bar:40.cyan/blue} {pos:>7}/{len:7} {msg}")
    .progress_chars("##-"));
    bar.set_message("sieving");
    let mut last = 0;
    while res.full_relations.len() <= factor_base.len() + factor_base.len()/10 {
        sieve.sieve();
        sieve.collect(&mut res);
        sieve.clear();
        bar.inc((res.full_relations.len() - last) as u64);
        last = res.full_relations.len();
    }
    bar.finish_and_clear();

    //println!("{} relations = {} full + {} combined", res.full_relations.len(), res.initial, res.combined);

    if res.full_relations.len() > factor_base.len() {
        let mat = gaussian::MatrixF2::new(&res.full_relations);
        //println!("formed {}x{} matrix", mat.rows.len(), mat.rows[0].len() * 64);
        let nsp = mat.nullsp();
        //println!("found {} dependencies", nsp.len());
        
        for (i, v) in nsp.iter().enumerate() {
            let mut y = Integer::from(1);
            let mut evec = vec![0; factor_base.len()];
            for (i, b) in v.iter().enumerate() {
                if *b {
                    for (j, k) in res.full_relations[i].evec.iter().enumerate() {
                        evec[j] += k;
                    }
                    y *= &res.full_relations[i].sqrt;
                }
            }

            let mut x = Integer::from(1);
            for (j, p) in factor_base.iter().enumerate() {
                for _ in 0..(evec[j]/2) {
                    x *= p.value as u64;
                }
            }

            assert!(x.clone().square() % &n == y.clone().square() % &n);

            let z = (x + y).gcd(&n);
            if z != 1 && z != n { 
                let mut o1 = factor(primes, z.clone());
                let mut o2 = factor(primes, n / z);
                resp.append(&mut o1);
                resp.append(&mut o2);
                break;
            }
        }
    }

    resp
}

fn main() {
    println!("generating trial division primes");
    let mut primes = generator::SimpleSieve::with_capacity(1 << 26);
    primes.sieve();
    for s in std::env::args().skip(1) {
        let n = Integer::from_str_radix(s.as_str(), 10).unwrap();
        let mut res = factor(&primes, n.clone());
        println!("---> {} = {}", n, res.drain(..).map(|k| format!("{}", k)).collect::<Vec<_>>().join(" * "));
    }
}
