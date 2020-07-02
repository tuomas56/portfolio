extern crate pixelengine;

struct Game {
    px: f32,
    py: f32,
    vx: f32,
    vy: f32
}

impl Game {
    fn new() -> Game {
        Game {
            px: 200.0,
            py: 350.0,
            vx: 0.0,
            vy: 0.0
        }
    }
}

impl pixelengine::App for Game {
    fn setup() -> (Game, pixelengine::Window) {
        (Game::new(), pixelengine::Window::new(400, 400, "Pixel Engine Demo", pixelengine::FPS::Unlimited, pixelengine::Multisampling::Yes(16), false))
    }

    fn draw(&self, window: &mut pixelengine::Window) {
        window.background(1.0, 1.0, 1.0);
        window.no_fill();
        window.stroke(1.0, 0.0, 0.0, 1.0);
        window.stroke_weight(10.0);
        let mx = window.mouse_x() as f32;
        let my = window.mouse_y() as f32;
        window.triangle(mx, my, mx + 200.0, my, self.px + mx - 100.0, self.py + my - 200.0);

        window.title(&format!("Pixel Engine Demo - FPS: {:03.0}", window.fps()))
    }

    fn update(&mut self, dt: f64, _: &pixelengine::Window) {
        let ax1 = 100.0 - self.px;
        let ax2 = 300.0 - self.px;
        let ay1 = 200.0 - self.py;
        let ay2 = 200.0 - self.py;
        let d1: f32 = ax1*ax1 + ay1*ay1;
        let d1 = d1.sqrt();
        let d2: f32 = ax2*ax2 + ay2*ay2;
        let d2 = d2.sqrt();
        let a1 = 0.4 * d1;
        let a2 = 0.4 * d2;
        let ax1 = ax1/ax1.abs();
        let ax2 = ax2/ax2.abs();
        let ay1 = ay1/ay1.abs();
        let ay2 = ay2/ay2.abs();
        let ax = ax1 * a1 + ax2 * a2 - 0.3 * self.vx;
        let ay = ay1 * a1 + ay2 * a2 - 0.3 * self.vy;
        let dt = dt as f32;
        self.vx += ax * dt;
        self.vy += ay * dt;
        self.px += self.vx * dt;
        self.py += self.vy * dt;
    }
}


fn main() {
    pixelengine::run::<Game>();
}