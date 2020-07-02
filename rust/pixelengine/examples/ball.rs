extern crate pixelengine;
extern crate rand;

struct Ball {
    x: f32,
    y: f32,
    vx: f32,
    vy: f32,
    color: (f32, f32, f32)
}

impl Ball {
    fn new(x: f32, y: f32) -> Ball {
        let vx = 200.0 * (2.0 * rand::random::<f32>() - 1.0);
        let vy = 200.0 * (2.0 * rand::random::<f32>() - 1.0);
        let color = (rand::random::<f32>(), rand::random::<f32>(), rand::random::<f32>());
        Ball {
            x, y,
            vx, vy,
            color
        }
    }

    fn update(&mut self, dt: f64, window: &pixelengine::Window) {
        self.vy += 200.0 * (dt as f32);

        if self.x >= (window.width() - 20) as f32 {
            self.vx *= -1.0;
            self.x = (window.width() - 20) as f32;
        }

        if self.x <= (0 + 20) as f32 {
            self.vx *= -1.0;
            self.x = (0 + 20) as f32;
        }

        if self.y >= (window.height() - 20) as f32 {
            self.vy *= -1.0;
            self.y = (window.height() - 20) as f32;
        }

        if self.y <= (0 + 20) as f32 {
            self.vy *= -1.0;
            self.y = (0 + 20) as f32;
        }

        self.x += self.vx * (dt as f32);
        self.y += self.vy * (dt as f32);
    }
    
    fn draw(&self, window: &mut pixelengine::Window) {
        window.fill(self.color.0, self.color.1, self.color.2, 1.0);
        window.ellipse(self.x, self.y, 20.0, 20.0);
    }
}

struct Game {
    balls: Vec<Ball>
}

impl pixelengine::App for Game {
    fn setup() -> (Game, pixelengine::Window) {
        let window = pixelengine::Window::new(
            800, 800,
            "Pixel Engine Demo",
            pixelengine::FPS::VSync,
            pixelengine::Multisampling::Yes(8),
            false
        );
        (Game { balls: Vec::new() }, window)
    }

    fn draw(&self, window: &mut pixelengine::Window) {
        window.background(1.0, 1.0, 1.0);
        window.rotate(-0.1);
        window.stroke(0.0, 0.0, 0.0, 1.0);
        window.no_fill();
        window.rect(0.0, 0.0, window.width() as f32, window.height() as f32);
        window.stroke_weight(4.0);
        for ball in &self.balls {
            ball.draw(window);
        }
        window.title(&format!("Pixel Engine Demo - FPS: {:03.0}, Balls: {}", window.fps(), self.balls.len()));
    }

    fn update(&mut self, dt: f64, window: &pixelengine::Window) {
        if window.mouse_pressed() {
            let (x, y) = window.untransform(window.mouse_x() as f32, window.mouse_y() as f32);
            self.balls.push(Ball::new(x, y));
        }

        for ball in &mut self.balls {
            ball.update(dt, window);
        }
    }
}

fn main() {
    pixelengine::run::<Game>();
}