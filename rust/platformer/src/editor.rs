use pixelengine as px;
use std::collections::HashMap;
use super::Platformer;
use super::geometry::{PadType, PadConsts};

pub struct EditorButtons {
    names: Vec<String>,
    width: f32,
    down: bool,
    groups: Vec<Vec<String>>,
    highlight: Vec<String>,
    params: HashMap<String, f32>
}

impl EditorButtons {
    pub fn new(names: &[&str], groups: &[&[&str]], width: f32) -> EditorButtons {
        EditorButtons {
            names: names.iter().cloned().map(str::to_string).collect(),
            groups: groups.iter().map(|s| s.iter().cloned().map(str::to_string).collect()).collect(),
            highlight: Vec::new(),
            params: HashMap::new(),
            width, down: false
        }
    }

    pub fn draw(&self, window: &mut px::Window) {
        let weight = window.untransform(1.0, 0.0).0;
        let dx = weight*self.width/(self.names.len().min(9) as f32);
        window.stroke(0.0, 0.0, 0.0, 1.0);
        window.stroke_weight(weight);
        let mut x = 0.0;
        let mut row = 0;
        for (i, name) in self.names.iter().enumerate() {
            if i > 0 && i % 9 == 0 {
                row += 1;
                x = 0.0;
            }

            if window.mouse_y() <= 20*(row + 1) && 
               window.mouse_y() > 20*row &&
               window.mouse_x() as f32 >= x/weight && 
               (window.mouse_x() as f32) < (x + dx)/weight {
                window.fill(0.5, 0.5, 0.5, 1.0);
            } else if self.highlight.contains(name) {
                window.fill(0.2, 0.2, 0.2, 1.0);
            } else {
                window.fill(1.0, 1.0, 1.0, 1.0);
            }
            window.rect(x, weight * 20.0 * (row as f32), dx, weight * 20.0);
            window.text_size(weight * 15.0);
            window.fill(0.0, 0.0, 0.0, 1.0);
            window.text(name, x + weight * 10.0, weight * 2.5 + weight * 20.0 * (row as f32));
            x += dx;
        }

        let mut y = 20.0 * ((row + 1) as f32) + 2.5;

        for (name, value) in self.params.iter() {
            window.text_size(weight * 15.0);
            window.fill(0.0, 0.0, 0.0, 1.0);
            window.text(&format!("{}: {}", name, value), weight * 10.0, weight * y);
            y += 20.0;
        }
    }

    pub fn event(&mut self, event: px::Event, window: &px::Window, p: &mut Platformer) -> Option<px::Event> {
        match event {
            px::Event::MousePressed(px::Button::Left) => {
                let dx = self.width/(self.names.len().min(9) as f32);
                let mut x = 0.0;
                let mut row = 0;

                for (i, name) in self.names.iter().enumerate() {
                    if i > 0 && i % 9 == 0 {
                        row += 1;
                        x = 0.0;
                    }

                    if window.mouse_y() <= 20*(row + 1) && 
                       window.mouse_y() > 20*row && 
                       window.mouse_x() as f32 >= x && 
                       (window.mouse_x() as f32) < x + dx {
                        self.down = true;
                        p.clicked(name.clone(), self);
                        return None;
                    }
                    x += dx;
                }
            },
            px::Event::MouseReleased(px::Button::Right) if self.down => {
                self.down = false;
                return None;
            },
            _ => ()
        }

        Some(event)
    }

    pub fn highlight(&mut self, name: &str) {
        let name = name.to_string();
        for group in &self.groups {
            if group.contains(&name) {
                for name in group {
                    self.highlight.remove_item(&name);
                }
                self.highlight.push(name);
                break;
            }
        }
    }

    pub fn generate_params(&self, ptype: PadType) -> PadConsts {
        let ps = self.params.values().cloned().collect::<Vec<_>>();
        PadConsts::from_params(ptype.name(), &ps[..])
    }

    pub fn set_pad_type(&mut self, ptype: Option<PadType>) {
        self.params.clear();
        if let Some(ptype) = ptype {
            for (k, v) in PadConsts::default_params(ptype.name()) {
                self.params.insert(k.to_string(), *v);
            }
        }
    }
}