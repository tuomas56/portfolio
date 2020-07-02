use rand;
use std::{ops, convert};

#[derive(Debug)]
pub struct ExpFilter<T: ops::AddAssign + ops::Sub<Output=T> + ops::Mul<Output=T> + convert::From<f32> + Copy> {
    value: T, alpha: f32
}

impl<T: ops::AddAssign + ops::Sub<Output=T> + ops::Mul<Output=T> + convert::From<f32> + Copy> ExpFilter<T> {
    pub fn new(value: T, alpha: f32) -> ExpFilter<T> {
        ExpFilter { value, alpha }
    }

    pub fn update(&mut self, new: T, dt: f64) {
        self.value += T::from(1.0 - (1.0 - self.alpha).powf(60.0 * dt as f32)) * (new - self.value);
    }

    pub fn value(&self) -> T {
        self.value
    }

    pub fn value_mut(&mut self) -> &mut T {
        &mut self.value
    }
}

pub struct ParticleSystem {
    pub particles: Vec<(f32, f32, f32, f32, f32, f32, f32, f32, f32, f32, f32)>
}

impl ParticleSystem {
    pub fn update(&mut self, dt: f64) {
        let dt = dt as f32;

        for i in 0..self.particles.len() {
            self.particles[i].0 += dt * self.particles[i].2;
            self.particles[i].1 += dt * self.particles[i].3;
            self.particles[i].3 += dt * self.particles[i].8;
            self.particles[i].7 += dt;
        } 

        for i in (0..self.particles.len()).rev() {
            if self.particles[i].7 > self.particles[i].10 {
                self.particles.remove(i);
            }
        }
    }

    pub fn draw(&self, window: &mut px::Window) {
        for p in self.particles.iter() {
            window.fill(p.4, p.5, p.6, 1.0 - p.7/p.10);
            window.no_stroke();
            window.rect(p.0 - 2.5, p.1 - 2.5, p.9, p.9);
        }
    }

    pub fn spawn(&mut self, n: usize, x: (f32, f32), y: (f32, f32), a: (f32, f32), nx: f32, ny: f32, v: f32, g: f32, cl: (f32, f32, f32), sz: f32, tm: f32, p: f32) {
        self.particles.reserve(n);
        for _ in 0..n {
            let xp = rand::random::<f32>() * (x.1 - x.0) + x.0;
            let yp = rand::random::<f32>() * (y.1 - y.0) + y.0;
            let theta = rand::random::<f32>() * a.0 + (3.141592/2.0 - a.1);
            let c = theta.cos();
            let s = theta.sin();
            let vx =  c * nx + s * ny;
            let vy = -s * nx + c * ny;
            let vl = rand::random::<f32>() * v * (1.0 - p) + p * v;
            self.particles.push((xp, yp, vx * vl, vy * vl, cl.0, cl.1, cl.2, 0.0, g, sz, tm));
        }
    }
}

pub struct CameraShaker {
    pub remaining: f32,
    pub magnitude: f32,
    pub damping: f32
}

impl CameraShaker {
    pub fn update(&mut self, dt: f32) {
        self.magnitude *= (0.367f32).powf(dt * self.damping);
        self.remaining -= dt;
    }

    pub fn transform(&self) -> (f32, f32) {
        if self.remaining <= 0.0 {
            return (0.0, 0.0);
        }

        let theta = rand::random::<f32>() * 2.0 * 3.14159265;
        (theta.cos() * self.magnitude, theta.sin() * self.magnitude)
    }

    pub fn shake(&mut self, time: f32, magnitude: f32) {
        self.magnitude = magnitude;
        self.remaining = time;
    }
}