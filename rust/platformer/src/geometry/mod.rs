use pixelengine as px;
use serde::{Deserialize, Serialize};
use super::vfx::ParticleSystem;
use std::collections::HashMap;

mod generated;

pub use generated::{PadType, PadConsts, PadState};

#[derive(Debug, Copy, Clone, PartialEq, Deserialize, Serialize)]
pub struct Rect {
    pub top: f32,
    pub left: f32,
    pub width: f32,
    pub height: f32
}

#[derive(Debug, Copy, Clone, PartialEq, Deserialize, Serialize)]
pub struct Collision {
    pub restitution: f32,
    pub position: (f32, f32),
    pub normal: (f32, f32),
    pub tangent: (f32, f32),
    pub can_jump: bool,
    pub hard_surface: bool
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Hash)]
pub struct ObjectID(pub usize);

#[derive(Debug, Serialize, Deserialize)]
pub struct CollisionGeometry {
    pub platforms: HashMap<ObjectID, (Rect, (f32, f32, f32))>,
    pub pads: HashMap<ObjectID, (PadType, (f32, f32, f32, PadConsts, PadState))>,
    pub bounds: Rect,
    pub next_id: ObjectID
}

impl CollisionGeometry {
    pub fn new_id(&mut self) -> ObjectID {
        let id = self.next_id;
        self.next_id = ObjectID(self.next_id.0 + 1);
        id
    }

    pub fn draw_debug(&self, window: &mut px::Window, highlight: Option<ObjectID>) {
        window.no_fill();
        window.stroke(0.0, 0.0, 0.0, 1.0);
        window.rect(self.bounds.left, self.bounds.top, self.bounds.width, self.bounds.height);
        for (&id, (p, c)) in self.platforms.iter() {
            window.stroke(c.0, c.1, c.2, 1.0);
            window.rect(p.left, p.top, p.width, p.height);
            
            if Some(id) == highlight {
                window.stroke(0.0, 0.0, 0.0, 0.5);
                window.rect(p.left - 5.0, p.top - 5.0, p.width + 10.0, p.height + 10.0);
            }
        }

        for (&id, (_, (x, y, w, _, _))) in self.pads.iter() {
            window.stroke(0.0, 0.0, 0.0, 1.0);
            window.line(*x, *y, *x + *w, *y);

            if Some(id) == highlight {
                window.stroke(0.0, 0.0, 0.0, 0.5);
                window.rect(*x - 5.0, *y - 5.0, *w + 10.0, 10.0);
            }
        }
    }

    pub fn draw(&self, window: &mut px::Window) {
        window.no_fill();
        window.stroke(1.0, 0.0, 0.0, 1.0);
        window.stroke_weight(4.0);
        window.rect(self.bounds.left, self.bounds.top, self.bounds.width, self.bounds.height);


        for (p, c) in self.platforms.values() {
            window.stroke(c.0, c.1, c.2, 1.0);
            window.no_fill();
            window.stroke_weight(4.0);
            window.rect(p.left, p.top, p.width, p.height);
        }

        for (pad, (x, y, w, cs, st)) in self.pads.values() {
            pad.draw(window, *x, *y, *w, *cs, *st);
        }
    }

    pub fn update(&mut self, window: &px::Window, particles: &mut ParticleSystem, x: f32, y: f32, r: f32, dt: f32) {
        for (pad, (px, py, pw, cs, st)) in self.pads.values_mut() {
            pad.update(window, particles, *px, *py, *pw, x, y, r, dt, *cs, st);
        }
    }

    pub fn intersect(&mut self, x: f32, y: f32) -> Vec<ObjectID> {
        let mut res = Vec::new();

        for (&id, (r, _)) in self.platforms.iter() {
            if (r.left - 2.0 <= x && x <= r.left + r.width + 2.0) && ((y - r.top).abs() < 2.0 || (y - r.top - r.height).abs() < 2.0)   {
                res.push(id);
            } else if (r.top - 2.0 <= y && y <= r.top + r.height + 2.0) && ((x - r.left).abs() < 2.0 || (x - r.left - r.width).abs() < 2.0) {
                res.push(id);
            } 
        }

        for (&id, (_, (px, py, pw, _, _))) in self.pads.iter() {
            if (*px <= x && x <= *px + *pw) && (y.round() == (*py).round()) {
                res.push(id);
            }
        }
        
        res
    }

    pub fn collide_square(&mut self, x: f32, y: f32, r: f32, jump_time: f32, dt: f32) -> Option<Collision> {
        let mut res = Vec::new();
        let mut restitution = 0.0;
        let mut new_x = x;
        let mut new_y = y;
        let mut normal_x = 0.0;
        let mut normal_y = 0.0;

        if x + r >= self.bounds.left + self.bounds.width {
            new_x = self.bounds.left + self.bounds.width - r;
            normal_x = -1.0;
        }

        if x - r <= self.bounds.left {
            new_x = self.bounds.left + r;
            normal_x = 1.0;
        }

        if y + r >= self.bounds.top + self.bounds.height {
            new_y = self.bounds.top + self.bounds.height - r;
            normal_y = -1.0;
            normal_x = 0.0;
        }

        if y - r <= self.bounds.top {
            new_y = self.bounds.top + r;
            normal_y = 1.0;
            normal_x = 0.0;
            restitution = 1.0;
        }

        if normal_x != 0.0 || normal_y != 0.0 {
            res.push(Collision {
                restitution,
                position: (new_x, new_y),
                normal: (normal_x, normal_y),
                tangent: (normal_y, -normal_x),
                can_jump: true,
                hard_surface: true
            });
        }

        fn in_interval(p: f32, x: f32, r: f32) -> bool {
            p >= x - r && p <= x + r
        }

        for (p, _) in self.platforms.values() {
            let mut restitution = 0.0;
            let mut new_x = x;
            let mut new_y = y;
            let mut normal_x = 0.0;
            let mut normal_y = 0.0;

            let hw = p.width/2.0;
            let ox = in_interval(p.left, x, r) 
                  || in_interval(p.left + p.width, x, r) 
                  || in_interval(x - r, p.left + hw, hw) 
                  || in_interval(x + r, p.left + hw, hw);
            let hw = p.height/2.0;
            let oy = in_interval(p.top, y, r) 
                  || in_interval(p.top + p.height, y, r) 
                  || in_interval(y - r, p.top + hw, hw) 
                  || in_interval(y + r, p.top + hw, hw);

            if !ox || !oy {
                continue;
            }

            if (x >= p.left && x <= p.left + p.width) && (y >= p.top && y <= p.top + p.height) {
                continue;
            }

            if x < p.left {
                normal_x = -1.0;
                new_x = p.left - r;
            }

            if x > p.left + p.width {
                normal_x = 1.0;
                new_x = p.left + p.width + r;
            }

            if y < p.top {
                normal_y = -1.0;
                normal_x = 0.0;
                new_x = x;
                new_y = p.top - r;
            }

            if y > p.top + p.height {
                normal_y = 1.0;
                normal_x = 0.0;
                new_x = x;
                restitution = 1.0;
                new_y = p.top + p.height + r;
            }

            res.push(Collision {
                restitution,
                position: (new_x, new_y),
                normal: (normal_x, normal_y),
                tangent: (normal_y, -normal_x),
                can_jump: true,
                hard_surface: true
            });
        }

        let mut dx = 0.0;
        let mut dy = 0.0;
        let mut nx = 0.0;
        let mut ny = 0.0;
        let mut restitution = 0.0;

        for col in res.iter() {
            dx += col.position.0 - x;
            dy += col.position.1 - y;
            if col.normal.1 != 0.0 {
                if ny == 0.0 {
                    ny = col.normal.1;
                    nx = 0.0;

                    if col.normal.1 == -1.0 {
                        restitution = 0.0;
                    } else {
                        restitution = 1.0;
                    }
                } else if ny != col.normal.1 {
                    ny = -1.0;
                    nx = 0.0;
                    restitution = 0.0;
                }
            } else {
                if ny == 0.0 {
                    nx = col.normal.0;
                    restitution = 0.0;
                }
            }
        }

        let pcol = if nx != 0.0 || ny != 0.0 {
            Some(Collision {
                restitution,
                position: (x + dx, y + dy),
                normal: (nx, ny),
                tangent: (ny, -nx),
                can_jump: true,
                hard_surface: true
            })
        } else {
            None
        };

        for (pad, (px, py, pw, cs, st)) in self.pads.values_mut() {
            if let Some(c) = pad.collision(x, y, r, jump_time, dt, *px, *py, *pw, *cs, st) {
                return Some(c);
            }
        }

        pcol
    }
}