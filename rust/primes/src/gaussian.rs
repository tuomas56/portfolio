use std::collections::{HashMap, HashSet};
use crate::sieve;
use indicatif;

#[derive(Debug)]
pub struct MatrixF2 {
    pub rows: Vec<Vec<u64>>,
    alen: usize
}

impl MatrixF2 {
    pub fn new(relations: &Vec<sieve::Relation>) -> MatrixF2 {
        let mut rows = Vec::new();
        for i in 0..relations[0].evec.len() {
            let mut row = Vec::new();
            let mut k = 0;
            while k < relations.len() {
                let mut bucket = 0u64;
                for j in (0..64).rev() {
                    if k < relations.len() {
                        bucket |= ((relations[k].evec[i] & 1) as u64) << j;
                    }
                    k += 1;
                }
                row.push(bucket);
            }
            rows.push(row);
        }
        MatrixF2 { rows, alen: relations.len() }
    }

    fn swap(&mut self, i: usize, j: usize) {
        self.rows.swap(i, j);
    }

    fn add(&mut self, i: usize, j: usize) {
        for k in 0..self.rows[0].len() {
            self.rows[j][k] ^= self.rows[i][k];
        }
    }

    fn elem(&self, i: usize, j: usize, k: usize) -> bool {
        (self.rows[i][j] >> (63 - k)) & 1 != 0
    }

    pub fn print(&self) {
        println!("[");
        for row in &self.rows {
            for bucket in row {
                print!("{:064b}", bucket);
            }
            println!("");
        }
        println!("]");
    }

    pub fn rref(&mut self) {
        let bar = indicatif::ProgressBar::new(self.rows.len() as u64);
        bar.set_style(indicatif::ProgressStyle::default_bar()
           .template("[{elapsed_precise}] {bar:40.cyan/blue} {pos:>7}/{len:7} {msg}")
           .progress_chars("##-"));
        bar.set_message("gaussian elimination");
        let mut row = 0;
        let mut bucket = 0;
        let mut place = 0;

        
        while row < self.rows.len() && bucket < self.rows[0].len() {
            for i in row..self.rows.len() {
                if self.elem(i, bucket, place) {
                    self.swap(row, i);
                    for j in 0..self.rows.len() {
                        if j != row && self.elem(j, bucket, place) {
                            self.add(row, j);
                        }
                    }
                    row += 1;
                    bar.inc(1);
                    break;
                }
            }

            if place == 63 {
                place = 0;
                bucket += 1;
            } else {
                place += 1;
            }
        }
        bar.finish_and_clear();
    }

    pub fn nullsp(mut self) -> HashSet<Vec<bool>> {
        self.rref();

        let mut free = (0..(self.rows[0].len() * 64)).collect::<Vec<_>>();
        let mut basic = HashMap::new();
        let mut bucket = 0;
        let mut place = 0;

        for i in 0..self.rows.len() {
            while bucket < self.rows[i].len() {
                if self.elem(i, bucket, place) {
                    let col = bucket * 64 + place;
                    basic.insert(col, i);
                    free.remove_item(&col);
                    
                    if place == 63 {
                        place = 0;
                        bucket += 1;
                    } else {
                        place += 1;
                    }
                    break;
                } else {
                    if place == 63 {
                        place = 0;
                        bucket += 1;
                    } else {
                        place += 1;
                    }
                }
            }
        }

        let bar = indicatif::ProgressBar::new(free.len() as u64);
        bar.set_style(indicatif::ProgressStyle::default_bar()
           .template("[{elapsed_precise}] {bar:40.cyan/blue} {pos:>7}/{len:7} {msg}")
           .progress_chars("##-"));
        bar.set_message("null space");
        let mut basis = HashSet::new();
        for &v in &free {
            let mut bvec = Vec::new();
            for bucket in 0..self.rows[0].len() {
                for place in 0..64 {
                    let col = bucket * 64 + place;
                    if col == v {
                        bvec.push(true);
                    } else if free.contains(&col) {
                        bvec.push(false);
                    } else {
                        bvec.push(self.elem(basic[&col], v >> 6, v & 63));
                    }
                }
            }
            bvec.truncate(self.alen);
            bar.inc(1);
            basis.insert(bvec);
        }
        bar.finish_and_clear();

        basis
    }
}