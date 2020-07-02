use pixelengine as px;
use simple_physics::{*, glam::*};

struct Main {
    world: World,
    selected_entity: Option<usize>
}

impl px::App for Main {
    fn setup() -> (Main, px::Window) {
        let mut window = px::Window::basic(400, 400, "physics");
        window.viewport(-200.0, 200.0, 200.0, -200.0);

        let geometry = FixedGeometry::from_vec(vec![
            FixedBody {
                shape: Shape::AABB { width: 400.0, height: 0.0 },
                position: vec2(0.0, -200.0),
                friction: 0.2
            },
            FixedBody {
                shape: Shape::AABB { width: 400.0, height: 0.0 },
                position: vec2(0.0, 200.0),
                friction: 0.2
            },
            FixedBody {
                shape: Shape::AABB { width: 0.0, height: 400.0 },
                position: vec2(-200.0, 0.0),
                friction: 0.2
            },
            FixedBody {
                shape: Shape::AABB { width: 0.0, height: 400.0 },
                position: vec2(200.0, 0.0),
                friction: 0.2
            },
            FixedBody {
                shape: Shape::Rectangle { width: 100.0, height: 20.0, angle: (110.0).to_radians() },
                position: vec2(-100.0, 0.0),
                friction: 0.2
            },
            FixedBody {
                shape: Shape::Rectangle { width: 60.0, height: 20.0, angle: (-40.0).to_radians() },
                position: vec2(100.0, -100.0),
                friction: 0.2
            },
            FixedBody {
                shape: Shape::Rectangle { width: 150.0, height: 20.0, angle: (20.0).to_radians() },
                position: vec2(-10.0, 50.0),
                friction: 0.2
            }
        ]);

        let mut bodies = Vec::new();
        for _ in 0..10 {
            loop {
                let sphere = DynamicBody {
                    id: ID::new(),
                    radius: 20.0,
                    position: vec2(rand::random::<f32>() * 400.0 - 200.0, rand::random::<f32>() * 400.0 - 200.0),
                    velocity: vec2(rand::random::<f32>() * 100.0 - 50.0, rand::random::<f32>() * 100.0 - 50.0),
                    mass: 1.0,
                    restitution: 0.1,
                    gravity: 100.0
                };

                if geometry.collide(&sphere).next().is_some() {
                    continue
                }

                bodies.push(sphere);
                break;
            }
        }

        let main = Main {
            world: World { bodies, geometry },
            selected_entity: None
        };

        (main, window)
    }

    fn draw(&self, window: &mut px::Window) {
        self.world.draw_debug(window);
    }

    fn update(&mut self, dt: f64, window: &px::Window) {
        self.world.update(dt as f32);

        if let Some(entity) = self.selected_entity {
            let mouse: Vec2 = window.untransform(
                window.mouse_x() as f32,
                window.mouse_y() as f32
            ).into();

            self.world.bodies[entity].velocity = vec2(0.0, 0.0);

            if !self.world.collide(&DynamicBody {
                position: mouse, ..self.world.bodies[entity]
            }) {
                self.world.bodies[entity].position = mouse;
            }
        }
    }

    fn event(&mut self, event: px::Event, window: &px::Window) {
        match event {
            px::Event::MousePressed(px::Button::Left) => {
                self.selected_entity = self.world.pick_body(window.untransform(
                    window.mouse_x() as f32,
                    window.mouse_y() as f32
                ).into()).next();
            },
            px::Event::MouseReleased(px::Button::Left) => {
                self.selected_entity = None;
            },
            _ => ()
        }
    }
} 

fn main() {
    px::run::<Main>();
}