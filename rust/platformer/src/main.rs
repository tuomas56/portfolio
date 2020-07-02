#![feature(vec_remove_item)]

extern crate pixelengine as px;
extern crate serde;
extern crate serde_json as json;
extern crate nfd;
extern crate rand;
extern crate gilrs as gl;

mod geometry;
mod vfx;
mod player;
mod editor;

use self::{geometry::*, vfx::*, player::*, editor::*};
use std::{rc, cell, fs, env, io::Read, collections::HashMap};

pub struct Platformer {
    player: Player,
    geometry: CollisionGeometry,
    start_rect: (f32, f32),
    editor_mode: bool,
    shaders: Vec<px::Shader>,
    buttons: rc::Rc<cell::RefCell<EditorButtons>>,
    ncol: (f32, f32, f32),
    ptype: Option<PadType>,
    start_point: (f32, f32),
    particles: rc::Rc<cell::RefCell<ParticleSystem>>,
    shaker: rc::Rc<cell::RefCell<CameraShaker>>,
    view_scale: ExpFilter<f32>,
    view_position: (ExpFilter<f32>, ExpFilter<f32>),
    gil: gl::Gilrs,
    edit_scale: f32,
    edit_position: (f32, f32),
    edit_drag: Option<(f32, f32)>,
    selected_object: Option<ObjectID>
}

impl px::App for Platformer {
    fn setup() -> (Self, px::Window) {
        let window = px::Window::new(
            640, 480, 
            "Platformer", 
            px::FPS::VSync, 
            px::Multisampling::Yes(8)
        );

        let geometry = CollisionGeometry {
                platforms: HashMap::new(),
                pads: HashMap::new(),
                bounds: Rect { top: 0.0, left: 0.0, width: 4.0*(window.width() as f32), height: 4.0*(window.height() as f32) },
                next_id: ObjectID(0)
        };


        let mut pstrs = PadType::names_vec();
        pstrs.insert(0, "Platform");
        let mut bvec = vec![
            "New", "Load", "Save",
            "Red", "Green", "Blue",
            "Cyan", "Magenta", "Yellow", 
            "Reset"
        ];
        bvec.extend(&pstrs);

        let buttons = rc::Rc::new(cell::RefCell::new(EditorButtons::new(&bvec[..], &[
            &[
                "Red", "Green", "Blue",
                "Cyan", "Magenta", "Yellow"
            ],
            &pstrs
        ], window.width() as f32)));
        buttons.borrow_mut().highlight("Platform");
        buttons.borrow_mut().highlight("Red");

        fn read_shader(path: &str) -> String {
            let mut s = String::new();
            fs::File::open(path)
                .expect("error opening shader")
                .read_to_string(&mut s)
                .expect("error reading shader");
            s
        }

        let main_vert = read_shader("shaders/main.vert");
        let pass1_frag = read_shader("shaders/pass1.frag");
        let pass2_frag = read_shader("shaders/pass2.frag");
        let pass3_frag = read_shader("shaders/pass3.frag");
        let pass4_frag = read_shader("shaders/pass4.frag");

        let start_point = (
            geometry.bounds.left + geometry.bounds.width/2.0, 
            geometry.bounds.top + geometry.bounds.height/2.0
        );

        let sx = (window.width() as f32) / geometry.bounds.width;
        let sy = (window.height() as f32) / geometry.bounds.height;
        let edit_position = (sx * window.width() as f32 / 2.0, sy * window.height() as f32 / 2.0);

        let platformer = Platformer {
            player: Player {
                x: start_point.0,
                y: start_point.1,
                vx: ExpFilter::new(0.0, 0.14),
                vy: 0.0,
                wx: None,
                on_floor: false,
                can_walljump: false,
                debounce: false,
                jump_time: 0.0,
                trying_to_jump: false,
                fill_player: false
            },
            geometry,
            start_rect: (0.0, 0.0),
            editor_mode: true,
            shaders: vec![
                window.compile_shader(main_vert.as_str(), pass1_frag.as_str()),
                window.compile_shader(main_vert.as_str(), pass2_frag.as_str()),
                window.compile_shader(main_vert.as_str(), pass3_frag.as_str()),
                window.compile_shader(main_vert.as_str(), pass4_frag.as_str())
            ],
            buttons,
            ncol: (1.0, 0.0, 0.0),
            start_point,
            particles: rc::Rc::new(cell::RefCell::new(ParticleSystem {
                particles: Vec::new()
            })),
            shaker: rc::Rc::new(cell::RefCell::new(CameraShaker {
                remaining: 0.0,
                magnitude: 0.0,
                damping: 2.0
            })),
            ptype: None,
            view_scale: ExpFilter::new(1.0, 0.06),
            view_position: (ExpFilter::new(start_point.0, 0.23), ExpFilter::new(start_point.1, 0.23)),
            gil: gl::Gilrs::new().unwrap(),
            edit_scale: 1.0,
            edit_position,
            edit_drag: None,
            selected_object: None
        };

        (platformer, window)
    }

    fn update(&mut self, dt: f64, window: &px::Window) {
        if !self.editor_mode {
            self.player.update(
                window,
                dt as f32, 
                &mut self.geometry, 
                &mut *self.particles.clone().borrow_mut(),
                &mut *self.shaker.clone().borrow_mut()
            );
            self.particles.borrow_mut().update(dt);
            self.shaker.borrow_mut().update(dt as f32);

            let pv = self.player.vx.value() * self.player.vx.value() + self.player.vy * self.player.vy;
            let s = (-2.84962746e-7 * pv).exp();
            self.view_scale.update(s, dt);
            self.view_position.0.update(self.player.x, dt);
            self.view_position.1.update(self.player.y, dt);
        }

        while let Some(gl::Event { event, .. }) = self.gil.next_event() {
            if let gl::EventType::ButtonReleased(gl::Button::Start, _) = event {
                self.editor_mode = !self.editor_mode;
                return;
            }
            
            if !self.editor_mode {
                self.player.gil_event(event);
            }
        }
    }

    fn draw(&self, window: &mut px::Window) {
        if !self.editor_mode {
            window.no_stroke();
            window.fill(0.0, 0.0, 0.0, 0.5 * self.view_scale.value().powf(4.0));
            window.rect(0.0, 0.0, window.width() as f32, window.height() as f32);
            window.translate(-self.player.x, -self.player.y);
            window.scale(self.view_scale.value(), self.view_scale.value());
            window.translate(self.player.x - self.view_position.0.value(), self.player.y - self.view_position.1.value());
            let (sx, sy) = self.shaker.borrow().transform();
            window.translate((window.width() as f32)/2.0 + sx, (window.height() as f32)/2.0 + sy);
            self.geometry.draw(window);
            self.player.draw(window);
            self.particles.borrow().draw(window);
            window.postprocess(self.shaders.clone());
        } else {
            window.background(1.0, 1.0, 1.0);
            let sx = self.edit_scale * (window.width() as f32) / self.geometry.bounds.width;
            let sy = self.edit_scale * (window.height() as f32) / self.geometry.bounds.height;
            window.stroke_weight(1.0/sx);
            window.translate(-self.edit_position.0, -self.edit_position.1);
            window.scale(sx, sy);
            window.translate(window.width() as f32 / 2.0, window.height() as f32 / 2.0);

            if sx.max(sy) > 6.0 {
                let (xzero, yzero) = window.transform(0.0, 0.0);
                window.push_identity();
                window.no_fill();
                window.stroke(0.5, 0.5, 0.5, 1.0);
                window.stroke_weight(1.0);
                let (xoff, yoff) = (
                    sx * ((xzero.abs() / sx) as isize as f32) - xzero.abs(),
                    sy * ((yzero.abs() / sy) as isize as f32) - yzero.abs()
                );
                window.translate(xoff, yoff);
                for i in (-2)..((window.width() as f32/sx) as isize + 5) {
                    window.line(i as f32 * sx, 0.0, i as f32 * sx, window.height() as f32);
                }
                for j in (-2)..((window.height() as f32/sy) as isize + 5) {
                    window.line(0.0, j as f32 * sy, window.width() as f32, j as f32 * sy);
                }
                window.stroke_weight(1.0/sx);
                window.pop_matrix();
            }

            window.push_matrix();
            self.geometry.draw_debug(window, self.selected_object);
            self.player.draw_debug(window);
            window.pop_matrix();

            window.stroke(0.0, 0.0, 0.0, 1.0);
            window.fill(0.0, 0.0, 0.0, 1.0);
            let (x, y) = self.get_mouse_pos(window);
            window.ellipse(x, y, 2.0/sx, 2.0/sx);

            if self.start_rect != (0.0, 0.0) {
                window.no_fill();
                window.stroke(0.0, 0.0, 0.0, 1.0);
                let (x2, y2) = self.get_mouse_pos(window);
                window.rect(
                    self.start_rect.0, 
                    self.start_rect.1, 
                    x2 - self.start_rect.0,
                    y2 - self.start_rect.1
                );
            }

            window.push_identity();
            self.buttons.borrow().draw(window);
            window.pop_matrix();
        }
        window.title(&format!("Platformer - {:02.0} fps", window.fps()));
    }

    fn event(&mut self, event: px::Event, window: &px::Window) {
        if let px::Event::KeyReleased(px::Key::E, _) = event {
            self.editor_mode = !self.editor_mode;
            return;
        }
        
        if self.editor_mode {
            let buttons_ref = self.buttons.clone();
            let mut buttons = buttons_ref.borrow_mut();
            let event = buttons.event(event, window, self);
            match event {
                Some(px::Event::MousePressed(px::Button::Left)) => {
                    self.start_rect = self.get_mouse_pos(window);
                },
                Some(px::Event::MouseReleased(px::Button::Left)) => {
                    let x1 = self.start_rect.0;
                    let y1 = self.start_rect.1;
                    let (x2, y2) = self.get_mouse_pos(window);

                    if x2 == x1 && y2 == y1 {
                        self.selected_object = self.geometry.intersect(x1, y1).iter().next().cloned();
                    }
                    
                    match self.ptype {
                        None => {
                            let id = self.geometry.new_id();
                            self.geometry.platforms.insert(id, (Rect { 
                                top: y1.min(y2),
                                left: x1.min(x2),
                                width: (x1 - x2).abs(),
                                height: (y1 - y2).abs()
                            }, self.ncol));
                        },
                        Some(ptype) => {
                            let id = self.geometry.new_id();
                            self.geometry.pads.insert(id,
                                (ptype, (x1.min(x2), y1, (x1 - x2).abs(), buttons.generate_params(ptype), PadState::default()))
                            );
                        },
                    }
                    
                    self.start_rect = (0.0, 0.0);
                },
                Some(px::Event::MouseScroll(px::Scroll::Up)) => {
                    self.edit_scale *= 1.05;
                },
                Some(px::Event::MouseScroll(px::Scroll::Down)) => {
                    self.edit_scale /= 1.05;
                },
                Some(px::Event::MousePressed(px::Button::Right)) => {
                    self.edit_drag = Some((window.mouse_x() as f32, window.mouse_y() as f32));
                },
                Some(px::Event::MouseReleased(px::Button::Right)) => {
                    self.edit_drag = None;
                },
                Some(px::Event::MouseMoved) if self.edit_drag.is_some() => {
                    let dr = self.edit_drag.unwrap();
                    let cur = (window.mouse_x() as f32, window.mouse_y() as f32);
                    let diff = (cur.0 - dr.0, cur.1 - dr.1);
                    self.edit_drag = Some(cur);
                    let sx = self.edit_scale * (window.width() as f32) / self.geometry.bounds.width;
                    let sy = self.edit_scale * (window.height() as f32) / self.geometry.bounds.height;
                    self.edit_position.0 -= diff.0 / sx;
                    self.edit_position.1 -= diff.1 / sy;
                },
                _ => ()
            }
        } else {
            self.player.event(event);
        }
    }
}

impl Platformer {
    fn get_mouse_pos(&self, window: &px::Window) -> (f32, f32) {
        let sx = self.edit_scale * (window.width() as f32) / self.geometry.bounds.width;
        let sy = self.edit_scale * (window.height() as f32) / self.geometry.bounds.height;
        let (x, y) = (window.mouse_x() as f32, window.mouse_y() as f32);
        let (x, y) = (
            (x - window.width() as f32 / 2.0)/sx + self.edit_position.0, 
            (y - window.height() as f32 / 2.0)/sy + self.edit_position.1
        );
        (x as isize as f32, y as isize as f32)
    }

    fn clicked(&mut self, name: String, buttons: &mut EditorButtons) {
        match name.as_ref() {
            "New" => {
                self.geometry.platforms.clear();
                self.geometry.pads.clear();
            },
            "Load" => {
                let curpath = env::current_dir().unwrap();
                let result = nfd::dialog().filter("json").default_path(curpath.to_str().unwrap()).open();

                if let Ok(nfd::Response::Okay(s)) = result {
                    let mut file = fs::File::open(s).unwrap();
                    let mut buf = String::new();
                    file.read_to_string(&mut buf).unwrap();
                    self.geometry = json::from_str(&buf).unwrap();
                }
            },
            "Save" => {
                let curpath = env::current_dir().unwrap();
                let result = nfd::dialog_save().filter("json").default_path(curpath.to_str().unwrap()).open();

                if let Ok(nfd::Response::Okay(s)) = result {
                    let file = fs::File::create(s).unwrap();
                    json::to_writer(file, &self.geometry).unwrap();
                }
            },
            "Red" => {
                self.ncol = (1.0, 0.0, 0.0);
                buttons.highlight("Red");
            },
            "Green" => {
                self.ncol = (0.0, 1.0, 0.0);
                buttons.highlight("Green");
            },
            "Blue" => {
                self.ncol = (0.0, 0.0, 1.0);
                buttons.highlight("Blue");
            },
            "Cyan" => {
                self.ncol = (0.0, 1.0, 1.0);
                buttons.highlight("Cyan");
            },
            "Magenta" => {
                self.ncol = (1.0, 0.0, 1.0);
                buttons.highlight("Magenta");
            },
            "Yellow" => {
                self.ncol = (1.0, 1.0, 0.0);
                buttons.highlight("Yellow");
            },
            "Reset" => {
                self.player.x = self.start_point.0;
                self.player.y = self.start_point.1;
                *self.player.vx.value_mut() = 0.0;
                self.player.vy = 0.0;
            },
            "Platform" => {
                self.ptype = None;
                buttons.highlight("Platform");
                buttons.set_pad_type(None);
            },
            p if PadType::from_str(p).is_some() => {
                self.ptype = PadType::from_str(p);
                buttons.highlight(p);
                buttons.set_pad_type(self.ptype);
            },
            _ => ()
        }
    }
}

fn main() {
    px::run::<Platformer>();
}
