use super::{geometry::CollisionGeometry, vfx::{ParticleSystem, CameraShaker, ExpFilter}};

#[derive(Debug)]
pub struct Player {
    pub x: f32,
    pub y: f32,
    pub vx: ExpFilter<f32>,
    pub vy: f32,
    pub wx: Option<f32>,
    pub on_floor: bool,
    pub can_walljump: bool,
    pub debounce: bool,
    pub trying_to_jump: bool,
    pub jump_time: f32,
    pub fill_player: bool
}

impl Player {
    pub fn draw(&self, window: &mut px::Window) {
        if self.fill_player {
            window.fill(1.0, 1.0, 1.0, 0.5);
        } else {
            window.no_fill();
        }
        window.stroke_weight(2.0);
        window.stroke(1.0, 1.0, 1.0, 1.0);
        window.rect(self.x - 10.0, self.y - 10.0, 20.0, 20.0);
    }

    pub fn draw_debug(&self, window: &mut px::Window) {
        if self.on_floor || self.can_walljump {
            window.fill(1.0, 0.0, 0.0, 1.0);
        } else {
            window.fill(0.0, 1.0, 0.0, 1.0);
        }
        window.stroke(0.0, 0.0, 0.0, 1.0);
        window.rect(self.x - 10.0, self.y - 10.0, 20.0, 20.0);
    }

    pub fn update(&mut self, window: &px::Window, dt: f32, geometry: &mut CollisionGeometry, particles: &mut ParticleSystem, shaker: &mut CameraShaker) {
        geometry.update(window, particles, self.x, self.y, 10.0, dt);

        if self.on_floor && self.wx.is_some() {
            self.vx.update(self.wx.unwrap(), dt as f64);
        }

        self.jump_time += dt;
        let jump_t = if self.trying_to_jump {
            self.jump_time
        } else {
            -1.0
        };

        self.x += dt * self.vx.value();
        self.y += dt * self.vy;
        self.vy += 600.0 * dt;

        if let Some(collision) = geometry.collide_square(self.x, self.y, 10.0, jump_t, dt) {
            self.x = collision.position.0;
            self.y = collision.position.1;

            if collision.normal.1 != 0.0 && !self.debounce && self.vy.abs() > 400.0 && collision.hard_surface {
                let ptot = (self.vy.abs() / 40.0) as usize;
                particles.spawn(
                    ptot/2,
                    (self.x - 10.0, self.x + 10.0),
                    (self.y - collision.normal.1 * 10.0, self.y - collision.normal.1 * 10.0),
                    (0.2, 0.3),
                    collision.normal.0,
                    collision.normal.1,
                    self.vy.abs(),
                    150.0,
                    (1.0, 1.0, 1.0),
                    5.0,
                    0.7,
                    0.2
                );
                particles.spawn(
                    ptot/2,
                    (self.x - 10.0, self.x + 10.0),
                    (self.y - collision.normal.1 * 10.0, self.y - collision.normal.1 * 10.0),
                    (-0.2, 3.141592 - 0.3),
                    collision.normal.0,
                    collision.normal.1,
                    self.vy.abs(),
                    150.0,
                    (1.0, 1.0, 1.0),
                    5.0,
                    0.7,
                    0.2
                );

                shaker.shake(0.5 * self.vy.abs() / 1000.0, 10.0 * self.vy.abs() / 1000.0);
            }

            let dp = self.vx.value() * collision.normal.0 + self.vy * collision.normal.1;
            let c = 1.0 + collision.restitution;
            *self.vx.value_mut() -= c * dp * collision.normal.0;
            self.vy -= c * dp * collision.normal.1;

            if collision.normal.1 < 0.0 && !self.debounce && collision.restitution > 0.0 && !collision.can_jump {
                let nx = self.vx.value();
                let ny = self.vy + 200.0;
                let nd = (nx * nx + ny * ny).sqrt();
                let nx = nx/nd;
                let ny = ny/nd;
                particles.spawn(
                    10,
                    (self.x - 10.0, self.x + 10.0),
                    (self.y + 10.0, self.y + 10.0),
                    (0.0, 3.14159265/2.0),
                    nx,
                    ny,
                    nd,
                    600.0,
                    (1.0, 1.0, 1.0),
                    5.0,
                    0.2,
                    0.2
                );
            }

            if collision.normal.1 < 0.0 && collision.can_jump {
                self.on_floor = true;
            } else {
                self.on_floor = false;
            }

            if collision.normal.0 != 0.0 && self.vy.abs() < 150.0 {
                self.fill_player = true;
                if self.wx.is_some() {
                    let wx = self.wx.unwrap();
                    if wx != 0.0 && wx/wx.abs() == collision.normal.0 {
                        self.can_walljump = true;
                    } else {
                        self.can_walljump = false;
                    }
                } else {
                    self.can_walljump = false;
                }
            } else {
                self.fill_player = false;
                self.can_walljump = false;
            }

            self.debounce = true;
        } else {
            self.on_floor = false;
            self.can_walljump = false;
            self.fill_player = false;
            self.debounce = false;
        }
    }

    pub fn event(&mut self, event: px::Event) {
        match event {
            px::Event::KeyPressed(px::Key::D, _) => {
                self.wx = Some(300.0);
            },
            px::Event::KeyPressed(px::Key::A, _) => {
                self.wx = Some(-300.0);
            },
            px::Event::KeyReleased(px::Key::A, _) | px::Event::KeyReleased(px::Key::D, _) => {
                self.wx = Some(0.0);
            },
            px::Event::KeyPressed(px::Key::W, _) if self.on_floor => {
                self.vy = -300.0;
            },
            px::Event::KeyPressed(px::Key::W, _) if self.can_walljump => {
                self.vy = -300.0;
                *self.vx.value_mut() = self.wx.unwrap();
            },
            px::Event::KeyPressed(px::Key::W, _) => {
                self.trying_to_jump = true;
                self.jump_time = 0.0;
            },
            px::Event::KeyReleased(px::Key::W, _) => {
                self.trying_to_jump = false;
            },
            _ => ()
        }
    }

    pub fn gil_event(&mut self, event: gl::EventType) {
        match event {
            gl::EventType::ButtonPressed(gl::Button::South, _) if self.on_floor => {
                self.vy = -300.0;
            },
            gl::EventType::ButtonPressed(gl::Button::South, _) if self.can_walljump => {
                self.vy = -300.0;
                *self.vx.value_mut() = self.wx.unwrap();
            },
            gl::EventType::ButtonPressed(gl::Button::South, _) => {
                self.trying_to_jump = true;
                self.jump_time = 0.0;
            },
            gl::EventType::ButtonReleased(gl::Button::South, _) => {
                self.trying_to_jump = false;
            },
            gl::EventType::AxisChanged(gl::Axis::LeftStickX, value, _) => {
                if value.abs() > 0.1 {
                    self.wx = Some(300.0f32.copysign(value));
                } else {
                    self.wx = Some(0.0);
                }
            },
            _ => ()
        }
    }
}