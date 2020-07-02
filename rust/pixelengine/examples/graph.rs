extern crate pixelengine;

#[derive(Copy, Clone, Debug)]
struct Line {
    color: (f32, f32, f32),
    thickness: f32
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum MarkerStyle {
    Round,
    Square
}

#[derive(Copy, Clone, Debug)]
struct Marker {
    fill: Option<(f32, f32, f32)>,
    stroke: Option<((f32, f32, f32), f32)>,
    size: f32,
    style: MarkerStyle
}

struct Data {
    x: Vec<f64>,
    y: Vec<f64>,
    marker: Option<Marker>,
    line: Option<Line>
}

impl Data {
    fn draw(&self, window: &mut pixelengine::Window, xlimits: ((f32, f64), (f32, f64)), ylimits: ((f32, f64), (f32, f64))) {
        let mut points = Vec::new();
        for (x, y) in self.x.iter().cloned().zip(self.y.iter().cloned()) {
            let tx = (x - (xlimits.0).1)/((xlimits.1).1 - (xlimits.0).1);
            let sx = ((xlimits.0).0 as f64) * (1.0 - tx) + ((xlimits.1).0 as f64) * tx;
            let ty = (y - (ylimits.0).1)/((ylimits.1).1 - (ylimits.0).1);
            let sy = ((ylimits.0).0 as f64) * ty + ((ylimits.1).0 as f64) * (1.0 - ty);
            points.push((sx as f32, sy as f32));
        }

        if let Some(line) = self.line {
            window.stroke(line.color.0, line.color.1, line.color.2, 1.0);
            window.stroke_weight(line.thickness);
            for i in 0..(points.len() - 1) {
                let (x1, y1) = points[i];
                let (x2, y2) = points[i + 1];
                window.line(x1, y1, x2, y2);
            }
        }

        if let Some(marker) = self.marker {
            if let Some(fill) = marker.fill {
                window.fill(fill.0, fill.1, fill.2, 1.0);
            }
            if let Some((stroke, stroke_weight)) = marker.stroke {
                window.stroke(stroke.0, stroke.1, stroke.2, 1.0);
                window.stroke_weight(stroke_weight);
            }
            if marker.style == MarkerStyle::Round {
                for &(x, y) in points.iter() {
                    window.ellipse(x, y, marker.size, marker.size);
                }
            } else {
                for &(x, y) in points.iter() {
                    window.rect(x - marker.size, y - marker.size, 2.0*marker.size, 2.0*marker.size);
                }
            }
        }
    }
}

struct Figure {
    data: Vec<Data>,
    margin: (f32, f32, f32, f32)
}

impl Figure {
    fn new() -> Figure {
        Figure { 
            data: Vec::new(),
            margin: (50.0, 50.0, 50.0, 50.0)
        }
    }

    fn plot(&mut self, data: Data) {
        self.data.push(data);
    }

    fn show(&self, window: &mut pixelengine::Window, x: f32, y: f32, width: f32, height: f32) {
        window.fill(1.0, 1.0, 1.0, 1.0);
        window.stroke_weight(1.0);
        window.stroke(0.0, 0.0, 0.0, 1.0);
        window.rect(x + 1.0, y + 1.0, width - 2.0, height - 2.0);

        let (margin_left, margin_right) = if width < self.margin.0 + self.margin.1 {
            (width / 10.0, width / 10.0)
        } else {
            (self.margin.0, self.margin.1)
        };
        
        let (margin_top, margin_bottom) = if height < self.margin.2 + self.margin.3 {
            (height / 10.0, height / 10.0)
        } else {
            (self.margin.2, self.margin.3)
        };

        let mut xmin = std::f64::INFINITY;
        let mut xmax = std::f64::NEG_INFINITY;
        let mut ymin = std::f64::INFINITY;
        let mut ymax = std::f64::NEG_INFINITY;
        for data in self.data.iter() {
            for &x in data.x.iter() {
                if x < xmin {
                    xmin = x;
                } else if x > xmax {
                    xmax = x;
                }
            }
            for &y in data.y.iter() {
                if y < ymin {
                    ymin = y;
                } else if y > ymax {
                    ymax = y;
                }
            }
        }

        

        let xlimits = ((x + margin_left, xmin), (x + width - margin_right, xmax));
        let ylimits = ((y + margin_top, ymin), (y + height - margin_bottom, ymax));

        let scale = |x: f64, y: f64| {
            let tx = (x - (xlimits.0).1)/((xlimits.1).1 - (xlimits.0).1);
            let sx = ((xlimits.0).0 as f64) * (1.0 - tx) + ((xlimits.1).0 as f64) * tx;
            let ty = (y - (ylimits.0).1)/((ylimits.1).1 - (ylimits.0).1);
            let sy = ((ylimits.0).0 as f64) * ty + ((ylimits.1).0 as f64) * (1.0 - ty);
            (sx, sy)
        };

        window.no_fill();
        window.stroke_weight(1.0);
        window.rect((xlimits.0).0, (ylimits.0).0, (xlimits.1).0 - (xlimits.0).0, (ylimits.1).0 - (ylimits.0).0);

        if (ymin <= 0.0) && (0.0 <= ymax) {
            let (x1, y1) = scale(xmin, 0.0);
            let (x2, y2) = scale(xmax, 0.0);
            window.stroke(0.0, 0.0, 0.0, 1.0);
            window.stroke_weight(2.0);
            window.line(x1 as f32, y1 as f32, x2 as f32, y2 as f32);
        }

        if (xmin <= 0.0) && (0.0 <= xmax) {
            let (x1, y1) = scale(0.0, ymin);
            let (x2, y2) = scale(0.0, ymax);
            window.stroke(0.0, 0.0, 0.0, 1.0);
            window.stroke_weight(2.0);
            window.line(x1 as f32, y1 as f32, x2 as f32, y2 as f32);
        }

        for data in self.data.iter() {
            data.draw(window, xlimits, ylimits);
        }
    }
}

fn range(start: f64, end: f64, step: f64, func: fn(f64) -> f64) -> Vec<f64> {
    let mut res = Vec::new();
    let mut f = start;
    while f < end + step {
        res.push(func(f));
        f += step;
    }
    res
}

struct App;

impl pixelengine::App for App {
    fn setup() -> (App, pixelengine::Window) {
        (App, pixelengine::Window::new(
            800, 800,
            "Pixel Engine Demo",
            pixelengine::FPS::VSync,
            pixelengine::Multisampling::Yes(16),
            false
        ))
    }

    fn draw(&self, window: &mut pixelengine::Window) {
        let mut fig = Figure::new();
        fig.plot(Data {
            x: range(-1.0, 1.0, 0.01, |x| x*3.141592),
            y: range(-1.0, 1.0, 0.01, |x| (x*3.141592).sin()),
            line: None,
            marker: Some(Marker {
                fill: Some((1.0, 0.0, 0.0)),
                stroke: Some(((0.0, 0.0, 0.0), 1.0)),
                size: 4.0,
                style: MarkerStyle::Round
            })
        });
        fig.plot(Data {
            x: range(0.0, 2.0*3.2, 0.05, |x| x),
            y: range(0.0, 2.0*3.2, 0.05, |x| x.cos()),
            line: Some(Line {
                color: (0.0, 0.0, 1.0),
                thickness: 4.0
            }),
            marker: None
        });
        fig.plot(Data {
            x: range(-3.141592/4.0, 3.141592/4.0, 0.05, |x| 3.0*x),
            y: range(-3.141592/4.0, 3.141592/4.0, 0.05, |x| x.tan()),
            line: Some(Line {
                color: (0.0, 1.0, 0.0),
                thickness: 2.0
            }),
            marker: Some(Marker {
                fill: Some((0.0, 1.0, 0.0)),
                stroke: Some(((0.0, 0.0, 0.0), 1.0)),
                size: 2.0,
                style: MarkerStyle::Square
            })
        });
        fig.show(window, 0.0, 0.0, window.width() as f32, window.height() as f32);
    }
}

fn main() {
    pixelengine::run::<App>();
}