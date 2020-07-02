extern crate pixelengine as px;

use px::glium::{self, glutin};

struct App;

impl px::App for App {
    fn setup() -> (App, px::Window) {
        let events_loop = glutin::EventsLoop::new();
        let wb = glutin::WindowBuilder::new();
        let cb = glutin::ContextBuilder::new()
                    .with_gl(glutin::GlRequest::GlThenGles{ opengl_version: (3,3), opengles_version: (3,2) });

        let display = glium::Display::new(wb, cb, &events_loop).unwrap();

        //let _ = unsafe { display.gl_window().make_current() };
        (App, px::Window::from_event_loop_display(events_loop, display, px::FPS::Unlimited))
    }

    fn draw(&self, window: &mut px::Window) {
        window.background(1.0, 0.0, 0.0);
    }
}

fn main() {
    px::run::<App>();
}