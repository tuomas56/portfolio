extern crate pixelengine as px;

struct App;

impl px::App for App {
    fn setup() -> (App, px::Window) {
        (App, px::Window::basic(400, 400, "Rectangle"))
    }

    fn draw(&self, window: &mut px::Window) {
        window.background(1.0, 1.0, 1.0);
        window.stroke(0.0, 0.0, 0.0, 1.0);
        window.fill(1.0, 0.0, 0.0, 1.0);
        window.rect(100.0, 100.0, 200.0, 200.0);
    }
}

fn main() {
    px::run::<App>();
}