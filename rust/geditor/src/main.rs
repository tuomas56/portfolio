use pixelengine as px;
use simple_physics as ph;
use ph::glam::*;
use specs::prelude::*;
use specs::Component;
use macro_rules_attribute::macro_rules_attribute as derive_;

mod gui;

use gui::{Widget, WidgetExt, GUIBuilder, Update};

#[derive_(GUIBuilder!)]
#[derive(Debug)]
enum Shape {
    Rectangle {
        #[slider(0.0, 100.0, 10.0)]
        width: f32,
        #[slider(0.0, 100.0, 10.0)]
        height: f32
    },
    Circle {
        #[slider(0.0, 100.0, 10.0)]
        radius: f32
    }
}

impl Default for Shape {
    fn default() -> Self {
        Shape::Circle { radius: 0.0 }
    }
}

#[derive_(GUIBuilder!)]
#[derive(Debug)]
enum Color {
    Transparent,
    RGBA { 
        #[slider(0.0, 1.0, 0.0)]
        r: f32,
        #[slider(0.0, 1.0, 0.0)]
        g: f32,
        #[slider(0.0, 1.0, 0.0)]
        b: f32,
        #[slider(0.0, 1.0, 0.0)]
        a: f32
    }
}

impl Default for Color {
    fn default() -> Self {
        Color::Transparent
    }
}

#[derive_(GUIBuilder!)]
#[derive(Component, Debug, Default)]
struct Renderable {
    shape: Shape,
    fill: Color,
    stroke: Color
}

#[derive_(GUIBuilder!)]
#[derive(Debug, Default)]
struct V2 {
    #[slider(-100.0, 100.0, 0.0)]
    x: f32,
    #[slider(-100.0, 100.0, 0.0)]
    y: f32
}

#[derive_(GUIBuilder!)]
#[derive(Component, Debug, Default)]
struct Transform {
    position: V2,
    #[slider(0.0, 2.0 * 3.14159265, 0.0)]
    rotation: f32,
    scale: V2
}

struct App {
    world: World,
    ui: gui::StackItem<gui::StackItem<gui::Map<gui::Selector, &'static str, std::option::Option<Entity>>, Option<Entity>, gui::Stack<Option<Entity>>>, Option<Entity>, gui::StackItem<gui::Map<gui::StackItem<Box<dyn gui::UpdateWidget<(), Transform>>, (), gui::StackItem<std::boxed::Box<dyn gui::UpdateWidget<(), Renderable>>, (), gui::Stack<()>>>, (), Option<Entity>>, Option<Entity>, gui::Stack<Option<Entity>>>>,
    selected: Entity
}

impl px::App for App {
    fn setup() -> (App, px::Window) {
        let mut world = World::new();
        world.register::<Renderable>();
        world.register::<Transform>();

        let other = world.create_entity()
            .with(Renderable {
                shape: Shape::Circle {radius: 20.0 },
                fill: Color::Transparent,
                stroke: Color::RGBA { r: 0.0, g: 0.0, b: 0.0, a: 1.0}
            })
            .with(Transform {
                position: V2 { x: -50.0, y: 100.0 },
                rotation: 0.0,
                scale: V2 { x: 1.0, y: 2.0 }
            })
            .build();

        let selected = world.create_entity()
            .with(Renderable {
                shape: Shape::Rectangle { width: 100.0, height: 20.0 },
                fill: Color::RGBA { r: 1.0, g: 0.0, b: 0.0, a: 1.0 },
                stroke: Color::RGBA { r: 0.0, g: 0.0, b: 0.0, a: 1.0}
            })
            .with(Transform {
                position: V2 { x: 0.0, y: 0.0 },
                rotation: 0.0,
                scale: V2 { x: 1.0, y: 1.0 }
            })
            .build();

        let mut window = px::Window::new(800, 600, "test", px::FPS::VSync, px::Multisampling::Yes(8), false);
        window.viewport(-400.0, 400.0, -300.0, 300.0);

        let style = gui::Style {
            background: (0.0, 0.0, 0.0, 1.0),
            foreground: (1.0, 0.0, 0.0, 1.0),
            stroke: (1.0, 1.0, 1.0, 1.0),
            text_size: 12.0,
            padding: 4.0
        };


        let mut ui = gui::Stack::new()
            .item(gui::Stack::new()
                .item(Renderable::builder())
                .item(Transform::builder())
                .map(|()| Some(None), |_| None))
            .item(gui::Stack::new()
                .item(gui::Selector::new(&["Entity 1", "Entity 2"], "Entity 1")
                    .map(move |s| Some(match s {
                        "Entity 1" => Some(selected),
                        "Entity 2" => Some(other),
                        _ => unreachable!()
                    }), |_| None)))
            .style(&style);
        ui.next.contents.inner.contents.update_from(world.read_storage().get(selected).unwrap());
        ui.next.contents.inner.next.contents.update_from(world.read_storage().get(selected).unwrap());
        ui.layout(&window, 200.0, 600.0);

        (App {
            world, ui, selected
        }, window)
    }

    fn draw(&self, window: &mut px::Window) {
        window.background(1.0, 1.0, 1.0);

        let transforms = self.world.read_storage::<Transform>();
        let renderables = self.world.read_storage::<Renderable>();

        for (transform, renderable) in (&transforms, &renderables).join() {
            window.push_matrix();
            window.rotate(transform.rotation);
            window.scale(transform.scale.x, transform.scale.y);
            window.translate(transform.position.x, transform.position.y);

            if let Color::RGBA { r, g, b, a } = renderable.fill {
                window.fill(r, g, b, a);
            } else {
                window.no_fill();
            }
            if let Color::RGBA { r, g, b, a } = renderable.stroke {
                window.stroke(r, g, b, a);
            } else {
                window.no_stroke();
            }

            match renderable.shape {
                Shape::Rectangle { width, height } => window.rect(-width/2.0, -height/2.0, width, height),
                Shape::Circle { radius } => window.ellipse(0.0, 0.0, radius, radius)
            }
            window.pop_matrix()
        }

        self.ui.draw(window, -400.0, -300.0);
    }

    fn event(&mut self, event: px::Event, window: &px::Window) {
        let (ev, reflow) = match event {
            px::Event::MousePressed(px::Button::Left) => {
                let x = window.mouse_x() as f32;
                let y = window.mouse_y() as f32;
                let (x, y) = window.untransform(x, y);
                self.ui.click(false, x + 400.0, y + 300.0)
            },
            px::Event::MouseMoved if window.mouse_pressed() => {
                let x = window.mouse_x() as f32;
                let y = window.mouse_y() as f32;
                let (x, y) = window.untransform(x, y);
                self.ui.click(true, x + 400.0, y + 300.0)
            },
            _ => (None, false)
        };
        
        if reflow {
            self.ui.layout(window, 200.0, 600.0);
        }

        match ev {
            Some(Some(entity)) => if self.selected != entity {
                self.selected = entity;
                let reflow = self.ui.next.contents.inner.contents.update_from(self.world.read_storage().get(self.selected).unwrap());
                let reflow = reflow | self.ui.next.contents.inner.next.contents.update_from(self.world.read_storage().get(self.selected).unwrap());
                if reflow {
                    self.ui.layout(&window, 200.0, 600.0);
                }
            },
            Some(None) => {
                let mut storage = self.world.write_storage();
                self.ui.next.contents.inner.contents.update(storage.get_mut(self.selected).unwrap());
                let mut storage = self.world.write_storage();
                self.ui.next.contents.inner.next.contents.update(storage.get_mut(self.selected).unwrap());
            },
            None => ()
        }
    }
}

fn main() {
    px::run::<App>();
}