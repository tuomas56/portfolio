pub extern crate glium;
extern crate rusttype;
extern crate image;

use std::{time, collections, thread, cell, borrow, rc};
use glium::{glutin, glutin::dpi, Surface};
use rusttype::gpu_cache;

pub use glium::glutin::VirtualKeyCode as Key;
pub use glium::glutin::ModifiersState as Modifiers;

trait AsFloatSecs {
    fn as_float_secs(&self) -> f64;
}

impl AsFloatSecs for time::Duration {
    fn as_float_secs(&self) -> f64 {
        self.as_nanos() as f64 * 1.0e-9
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Button {
    Left,
    Right,
    Middle
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Scroll {
    Left,
    Right,
    Up,
    Down
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Event {
    MousePressed(Button),
    MouseReleased(Button),
    MouseScroll(Scroll),
    MouseMoved,
    KeyPressed(Key, Modifiers),
    KeyReleased(Key, Modifiers),
    Resized
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum FPS {
    Limit(f64),
    Unlimited,
    VSync
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Multisampling {
    No,
    Yes(u16)
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Join {
    Bevel
}

#[derive(Debug, Clone)]
pub struct Shader(rc::Rc<glium::Program>);

#[derive(Debug, Copy, Clone, PartialEq)]
struct Vertex {
    pos: (f32, f32),
    color: (f32, f32, f32, f32),
    textured: u32,
    uv: (f32, f32)
}

glium::implement_vertex!(Vertex, pos, color, textured, uv);

#[derive(Debug, Copy, Clone, PartialEq)]
struct FrameVertex {
    pos: (f32, f32),
    uv: (f32, f32)
}

glium::implement_vertex!(FrameVertex, pos, uv);

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
struct GoodFloat(f32);

impl std::cmp::Eq for GoodFloat {}
impl std::cmp::Ord for GoodFloat {
    fn cmp(&self, other: &GoodFloat) -> std::cmp::Ordering {
        self.partial_cmp(other).expect("bad float!")
    }
}

impl std::convert::From<f32> for GoodFloat {
    fn from(a: f32) -> GoodFloat {
        GoodFloat(a)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum CacheShape {
    Ellipse(GoodFloat, GoodFloat)
}

static FONT_DATA: &[u8] = include_bytes!("../assets/Vera.ttf");

struct NoUVs;

impl Iterator for NoUVs {
    type Item = (f32, f32);

    fn next(&mut self) -> Option<(f32, f32)> {
        None
    }
}

#[derive(Clone)]
struct RcTex(rc::Rc<glium::texture::CompressedSrgbTexture2d>);


impl glium::uniforms::AsUniformValue for RcTex {
    fn as_uniform_value(&self) -> glium::uniforms::UniformValue {
        glium::uniforms::UniformValue::CompressedSrgbTexture2d(&*self.0, None)
    }
}

struct ImageLoader {
    cache: collections::HashMap<String, RcTex>,
    names: Vec<String>
}

impl ImageLoader {
    fn new() -> ImageLoader {
        ImageLoader {
            cache: collections::HashMap::new(),
            names: Vec::new()
        }
    }

    fn load(&mut self, display: &glium::Display, name: &str) -> u32 {
        let name = name.to_string();

        if !self.cache.contains_key(&name) {
            let image = image::open(&name).unwrap().to_rgba();
            let image_dimensions = image.dimensions();
            let image = glium::texture::RawImage2d::from_raw_rgba(image.into_raw(), image_dimensions);
            let texture = glium::texture::CompressedSrgbTexture2d::new(display, image).unwrap();
            self.cache.insert(name.clone(), RcTex(rc::Rc::new(texture)));
        }

        if self.names.contains(&name) {
            self.names.binary_search(&name).unwrap() as u32 + 2
        } else {
            self.names.push(name);
            self.names.len() as u32 + 1
        }
    }

    fn textures(&mut self) -> Vec<RcTex> {
        let res = self.names.iter().map(|n| self.cache[n].clone()).collect();
        self.names.clear();
        res
    }
}

struct WindowUniforms<'a> {
    fonttex: &'a glium::texture::Texture2d,
    textures: Vec<RcTex>
}

impl<'a> glium::uniforms::Uniforms for WindowUniforms<'a> {
    fn visit_values<'b, F: FnMut(&str, glium::uniforms::UniformValue<'b>)>(&'b self, mut f: F) {
        use glium::uniforms::AsUniformValue;

        f("fonttex", self.fonttex.as_uniform_value());
        for (i, tex) in self.textures.iter().enumerate() {
            f(&format!("textures[{}]", i), tex.as_uniform_value());
        }
    }
}

struct Viewport {
    left: f32,
    right: f32,
    top: f32,
    bottom: f32,
    swidth: f32,
    sheight: f32
}

impl Viewport {
    fn transform(&self, x: f32, y: f32) -> (f32, f32) {
        (
            self.swidth * (x - self.left) / (self.right - self.left),
            self.sheight * (y - self.top) / (self.bottom - self.top)
        )
    }

    fn untransform(&self, x: f32, y: f32) -> (f32, f32) {
        (
            (self.right - self.left) * x / self.swidth + self.left,
            (self.bottom - self.top) * y / self.sheight + self.top
        )
    }
}

pub struct Window {
    event_loop: glutin::EventsLoop,
    display: glium::Display,
    size: (u32, u32),
    program: glium::Program,
    stroke: Option<(f32, f32, f32, f32)>,
    fill: Option<(f32, f32, f32, f32)>,
    stroke_weight: f32,
    stroke_join: Join,
    mouse_x: u32,
    mouse_y: u32,
    mouse_pressed: bool,
    before: time::Instant,
    frametime_min: f64,
    frametimes: collections::VecDeque<f64>,
    last_frametime: f64,
    batch: cell::RefCell<Vec<Vertex>>,
    cache: collections::BTreeMap<CacheShape, (Vec<(f32, f32)>, Vec<(f32, f32)>)>,
    events: Vec<Event>,
    frame_tex: glium::texture::Texture2d,
    default_frame_program: Shader,
    frame_program: Option<Vec<Shader>>,
    frame_vertices: glium::VertexBuffer<FrameVertex>,
    fonttex: glium::texture::Texture2d,
    font: rusttype::Font<'static>,
    fontcache: gpu_cache::Cache<'static>,
    text_size: f32,
    dpi_factor: f32,
    image_loader: ImageLoader,
    matrix_stack: Vec<[[f32; 3]; 3]>,
    inverse_matrix_stack: Vec<[[f32; 3]; 3]>,
    time: f32,
    cursor_grab: bool,
    moffset: (f64, f64),
    viewport: Viewport
}

impl Window {
    pub fn basic(width: u32, height: u32, title: &str) -> Window {
        Window::new(width, height, title, FPS::VSync, Multisampling::Yes(16), false)
    }

    pub fn new(width: u32, height: u32, 
           title: &str, fps_policy: FPS,
           multisampling: Multisampling,
           resizable: bool
        ) -> Window {
        let event_loop = glutin::EventsLoop::new();
        let window = glutin::WindowBuilder::new()
            .with_resizable(resizable)
            .with_dimensions(
                dpi::LogicalSize::new(
                    width as f64, 
                    height as f64
                )
            ).with_title(title);
        let context = glutin::ContextBuilder::new()
                        .with_vsync(fps_policy == FPS::VSync)
                        .with_multisampling(if let Multisampling::Yes(samples) = multisampling { samples } else { 0 });
        let display = glium::Display::new(window, context, &event_loop).unwrap();
        Window::from_event_loop_display(event_loop, display, fps_policy)
    }

    pub fn from_event_loop_display(event_loop: glutin::EventsLoop, display: glium::Display, fps_policy: FPS) -> Window {
        let (width, height) = display.gl_window().window().get_inner_size().unwrap().into();
        let program = glium::Program::from_source(&display, r#"
            #version 410

            in vec2 pos;
            in vec4 color;
            in uint textured;
            in vec2 uv;

            out vec4 col;
            flat out uint v_textured;
            out vec2 v_uv;

            void main() {
                gl_Position = vec4(pos, 0.0, 1.0);
                col = color;
                v_textured = textured;
                v_uv = uv;
            }
        "#, r#"
            #version 410
            #define MAX_TEXTURES 10

            in vec4 col;
            flat in uint v_textured;
            in vec2 v_uv;

            uniform sampler2D fonttex;
            uniform sampler2D textures[MAX_TEXTURES];

            out vec4 color;

            void main() {
                if (v_textured == 1) {
                    color = vec4(col.rgb, texture(fonttex, v_uv).r * col.a);
                } else if (v_textured > 1) {
                    color = texture(textures[v_textured - 2], v_uv);
                } else {
                    color = col;
                }
            }
        "#, None).unwrap();
        let frame_program = Shader(rc::Rc::new(glium::Program::from_source(&display, r#"
            #version 410

            in vec2 pos;
            in vec2 uv;

            out vec2 v_uv;

            void main() {
                gl_Position = vec4(pos, 0.0, 1.0);
                v_uv = uv;
            }
        "#, r#"
            #version 410

            in vec2 v_uv;

            out vec4 col;

            uniform sampler2D screen;

            void main() {
                col = texture(screen, v_uv);
            }
        "#, None).unwrap()));
        let frame_vertices = glium::VertexBuffer::new(&display, &[
            FrameVertex { pos: (-1.0, -1.0), uv: (0.0, 0.0) },
            FrameVertex { pos: (-1.0, 1.0), uv: (0.0, 1.0) },
            FrameVertex { pos: (1.0, 1.0), uv: (1.0, 1.0) },
            FrameVertex { pos: (-1.0, -1.0), uv: (0.0, 0.0) },
            FrameVertex { pos: (1.0, 1.0), uv: (1.0, 1.0) },
            FrameVertex { pos: (1.0, -1.0), uv: (1.0, 0.0) }
        ]).unwrap();
        let dpi_factor = display.gl_window().window().get_hidpi_factor() as f32;
        let cache_width = (1024.0 * dpi_factor) as u32;
        let cache_height = (1024.0 * dpi_factor) as u32;
        let frame_tex = glium::texture::Texture2d::empty(
            &display, 
            (dpi_factor * (width as f32)) as u32, 
            (dpi_factor * (height as f32)) as u32
        ).unwrap();
        let fonttex = glium::texture::Texture2d::with_format(
            &display,
            glium::texture::RawImage2d {
                data: borrow::Cow::Owned(vec![0u8; cache_width as usize * cache_height as usize]),
                width: cache_width as u32,
                height: cache_height as u32,
                format: glium::texture::ClientFormat::U8,
            },
            glium::texture::UncompressedFloatFormat::U8,
            glium::texture::MipmapsOption::NoMipmap,
        ).unwrap();
        fonttex.as_surface().clear_color(1.0, 0.0, 0.0, 1.0);
        let font = rusttype::FontCollection::from_bytes(FONT_DATA).unwrap().into_font().unwrap();
        let fontcache = gpu_cache::CacheBuilder::default().dimensions(cache_width, cache_height).build();
        let image_loader = ImageLoader::new();

        Window {
            event_loop, display, program,
            size: (width, height),
            stroke: Some((0.0, 0.0, 0.0, 1.0)),
            fill: Some((1.0, 1.0, 1.0, 1.0)),
            stroke_weight: 1.0,
            stroke_join: Join::Bevel,
            mouse_x: 0,
            mouse_y: 0,
            mouse_pressed: false,
            frametime_min: match fps_policy {
                FPS::Limit(f) => 1.0/f,
                _ => 0.0
            },
            before: time::Instant::now(),
            frametimes: [0.0; 20].iter().map(|&f| f).collect(),
            last_frametime: 0.0,
            batch: cell::RefCell::new(Vec::new()),
            cache: collections::BTreeMap::new(),
            events: Vec::new(),
            frame_tex,
            default_frame_program: frame_program,
            frame_program: None,
            frame_vertices,
            fonttex,
            font,
            fontcache,
            text_size: 20.0,
            dpi_factor,
            image_loader,
            matrix_stack: vec![[[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]]],
            inverse_matrix_stack: vec![[[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]]],
            time: 0.0,
            cursor_grab: false,
            moffset: (0.0, 0.0),
            viewport: Viewport {
                top: 0.0,
                bottom: height as f32,
                left: 0.0,
                right: width as f32,
                sheight: height as f32,
                swidth: width as f32
            }
        }
    }

    pub fn grab_cursor(&mut self) {
        self.cursor_grab = true;
    }

    pub fn release_cursor(&mut self) {
        self.cursor_grab = false;
    }

    pub fn background(&mut self, r: f32, g: f32, b: f32) {
        self.frame_tex.as_surface().clear_color(r, g, b, 1.0);
    }

    pub fn title(&mut self, title: &str) {
        self.display.gl_window().window().set_title(title);
    }

    pub fn stroke(&mut self, r: f32, g: f32, b: f32, a: f32) {
        self.stroke = Some((r, g, b, a));
    }

    pub fn fill(&mut self, r: f32, g: f32, b: f32, a: f32) {
        self.fill = Some((r, g, b, a));
    }

    pub fn no_stroke(&mut self) {
        self.stroke = None;
    }

    pub fn no_fill(&mut self) {
        self.fill = None;
    }

    pub fn stroke_weight(&mut self, weight: f32) {
        self.stroke_weight = weight;
    }

    pub fn stroke_join(&mut self, join: Join) {
        self.stroke_join = join;
    }

    pub fn text_size(&mut self, size: f32) {
        self.text_size = size;
    }

    pub fn push_matrix(&mut self) {
        self.matrix_stack.push(self.matrix_stack.last().unwrap().clone());
        self.inverse_matrix_stack.push(self.inverse_matrix_stack.last().unwrap().clone());
    }

    pub fn viewport(&mut self, left: f32, right: f32, top: f32, bottom: f32) {
        self.viewport.left = left;
        self.viewport.right = right;
        self.viewport.top = top;
        self.viewport.bottom = bottom;
    }

    pub fn pop_matrix(&mut self) {
        self.matrix_stack.pop().unwrap();
        self.inverse_matrix_stack.pop().unwrap();
    }

    pub fn postprocess(&mut self, shaders: Vec<Shader>) {
        self.frame_program = Some(shaders);
    }

    fn mult_mat_top(&mut self, mat: [[f32; 3]; 3], inv: [[f32; 3]; 3]) {
        let top = self.matrix_stack.last_mut().unwrap();
        let mut aux = [[0.0; 3]; 3];

        aux[0][0] = mat[0][0] * top[0][0] + mat[0][1] * top[1][0] + mat[0][2] * top[2][0];
        aux[0][1] = mat[0][0] * top[0][1] + mat[0][1] * top[1][1] + mat[0][2] * top[2][1];
        aux[0][2] = mat[0][0] * top[0][2] + mat[0][1] * top[1][2] + mat[0][2] * top[2][2];
        aux[1][0] = mat[1][0] * top[0][0] + mat[1][1] * top[1][0] + mat[1][2] * top[2][0];
        aux[1][1] = mat[1][0] * top[0][1] + mat[1][1] * top[1][1] + mat[1][2] * top[2][1];
        aux[1][2] = mat[1][0] * top[0][2] + mat[1][1] * top[1][2] + mat[1][2] * top[2][2];
        aux[2][0] = mat[2][0] * top[0][0] + mat[2][1] * top[1][0] + mat[2][2] * top[2][0];
        aux[2][1] = mat[2][0] * top[0][1] + mat[2][1] * top[1][1] + mat[2][2] * top[2][1];
        aux[2][2] = mat[2][0] * top[0][2] + mat[2][1] * top[1][2] + mat[2][2] * top[2][2];

        *top = aux;

        let top = self.inverse_matrix_stack.last_mut().unwrap();

        aux[0][0] = inv[0][0] * top[0][0] + inv[0][1] * top[1][0] + inv[0][2] * top[2][0];
        aux[0][1] = inv[0][0] * top[0][1] + inv[0][1] * top[1][1] + inv[0][2] * top[2][1];
        aux[0][2] = inv[0][0] * top[0][2] + inv[0][1] * top[1][2] + inv[0][2] * top[2][2];
        aux[1][0] = inv[1][0] * top[0][0] + inv[1][1] * top[1][0] + inv[1][2] * top[2][0];
        aux[1][1] = inv[1][0] * top[0][1] + inv[1][1] * top[1][1] + inv[1][2] * top[2][1];
        aux[1][2] = inv[1][0] * top[0][2] + inv[1][1] * top[1][2] + inv[1][2] * top[2][2];
        aux[2][0] = inv[2][0] * top[0][0] + inv[2][1] * top[1][0] + inv[2][2] * top[2][0];
        aux[2][1] = inv[2][0] * top[0][1] + inv[2][1] * top[1][1] + inv[2][2] * top[2][1];
        aux[2][2] = inv[2][0] * top[0][2] + inv[2][1] * top[1][2] + inv[2][2] * top[2][2];

        *top = aux;
    }

    pub fn translate(&mut self, x: f32, y: f32) {
        self.mult_mat_top([[1.0, 0.0, x], [0.0, 1.0, y], [0.0, 0.0, 1.0]], [[1.0, 0.0, -x], [0.0, 1.0, -y], [0.0, 0.0, 1.0]]);
    }

    pub fn scale(&mut self, x: f32, y: f32) {
        self.mult_mat_top([[x, 0.0, 0.0], [0.0, y, 0.0], [0.0, 0.0, 1.0]], [[1.0/x, 0.0, 0.0], [0.0, 1.0/y, 0.0], [0.0, 0.0, 1.0]]);
    }

    pub fn rotate(&mut self, alpha: f32) {
        let (s, c) = (alpha.sin(), alpha.cos());
        self.mult_mat_top([[c, -s, 0.0], [s, c, 0.0], [0.0, 0.0, 1.0]], [[c, s, 0.0], [-s, c, 0.0], [0.0, 0.0, 1.0]]);
    }

    pub fn push_identity(&mut self) {
        self.matrix_stack.push([[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]]);
        self.inverse_matrix_stack.push([[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]]);
    }

    pub fn transform(&self, x: f32, y: f32) -> (f32, f32) {
        let mat = self.matrix_stack.last().unwrap();
        self.viewport.transform(mat[0][0] * x + mat[0][1] * y + mat[0][2], mat[1][0] * x + mat[1][1] * y + mat[1][2])
    }

    pub fn untransform(&self, x: f32, y: f32) -> (f32, f32) {
        let mat = self.inverse_matrix_stack.last().unwrap();
        self.viewport.untransform(mat[0][0] * x + mat[0][1] * y + mat[0][2], mat[1][0] * x + mat[1][1] * y + mat[1][2])
    }

    pub fn compile_shader(&self, vertex_shader: &str, fragment_shader: &str) -> Shader {
        Shader(rc::Rc::new(glium::Program::from_source(&self.display, vertex_shader, fragment_shader, None).unwrap()))
    }

    fn start_batch(&mut self) {
        self.frame_program = None;
        self.batch.borrow_mut().clear();
        self.matrix_stack.clear();
        self.matrix_stack.push([[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]]);
        self.inverse_matrix_stack.clear();
        self.inverse_matrix_stack.push([[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]]);
    }

    fn draw_tris<I: Iterator<Item=(f32, f32)>, J: Iterator<Item=(f32, f32)>>(&self, data: I, color: (f32, f32, f32, f32), uv: Option<J>, textured: u32) {
        let width = self.size.0 as f32;
        let height = self.size.1 as f32;
        if let Some(uv) = uv {
            self.batch.borrow_mut().extend(data.zip(uv).map(|((x, y), (u, v))| {
                let (x, y) = self.transform(x, y);
                Vertex { pos: (2.0 * x / (width as f32) - 1.0, 1.0 - 2.0 * y / (height as f32)), color, textured, uv: (u, v) } 
            }));
        } else {
            self.batch.borrow_mut().extend(data.map(|(x, y)| {
                let (x, y) = self.transform(x, y);
                Vertex { pos: (2.0 * x / (width as f32) - 1.0, 1.0 - 2.0 * y / (height as f32)), color, textured, uv: (0.0, 0.0) } 
            }));
        }
    }

    fn end_batch(&mut self) {
        let vertices = glium::VertexBuffer::new(&self.display, &self.batch.borrow()).unwrap();
        self.frame_tex.as_surface().draw(
            &vertices,
            glium::index::NoIndices(glium::index::PrimitiveType::TrianglesList),
            &self.program,
            &WindowUniforms {
                fonttex: &self.fonttex,
                textures: self.image_loader.textures()
            },
            &glium::DrawParameters {
                blend: glium::Blend::alpha_blending(),
                ..Default::default()
            }
        ).unwrap();
    }

    fn line_geom(weight: f32, x1: f32, y1: f32, x2: f32, y2: f32) -> (Vec<(f32, f32)>, (f32, f32)) {
        let (dx, dy) = if (x1 != x2) && (y1 != y2) {
            let m = (y2 - y1)/(x2 - x1);
            let m_p = -1.0/m;
            let tan = m_p;
            let cos = 1.0/(tan * tan + 1.0).sqrt();
            let sin = tan * cos;
            (weight * cos / 2.0, weight * sin / 2.0)
        } else if y1 == y2 {
            (0.0, weight / 2.0)
        } else {
            (weight / 2.0, 0.0)
        };

        (vec![(x1 + dx, y1 + dy), (x2 - dx, y2 - dy), (x1 - dx, y1 - dy),
              (x1 + dx, y1 + dy), (x2 + dx, y2 + dy), (x2 - dx, y2 - dy)],
         (dx, dy))
    }

    fn raw_line(&mut self, x1: f32, y1: f32, x2: f32, y2: f32) -> (f32, f32) {
        let (geom, (dx, dy)) = Self::line_geom(self.stroke_weight as f32, x1, y1, x2, y2);
        if let Some(color) = self.stroke {
            self.draw_tris::<_, NoUVs>(geom.iter().cloned(), color, None, 0);
        }
        (dx, dy)
    }

    fn do_stroke_join(&mut self, x: f32, y: f32, dx1: f32, dy1: f32, dx2: f32, dy2: f32) {
        if let Some(color) = self.stroke {
            match self.stroke_join {
                Join::Bevel => {
                    self.draw_tris::<_, NoUVs>(Self::stroke_join_geom(x, y, dx1, dy1, dx2, dy2).iter().cloned(), color, None, 0);
                }
            }
        }
    }

    fn stroke_join_geom(x: f32, y: f32, dx1: f32, dy1: f32, dx2: f32, dy2: f32) -> Vec<(f32, f32)> {
        vec![(x, y), (x + dx1, y + dy1), (x + dx2, y + dy2),
             (x, y), (x - dx1, y - dy1), (x - dx2, y - dy2),
             (x, y), (x + dx1, y + dy1), (x - dx2, y - dy2),
             (x, y), (x - dx1, y - dy1), (x + dx2, y + dy2)]
    }

    pub fn line(&mut self, x1: f32, y1: f32, x2: f32, y2: f32) {
        self.raw_line(x1, y1, x2, y2);
    }

    fn upload_fontcache(&mut self) {
        let mut res = Vec::new();
        self.fontcache.cache_queued(|rect, data| {
            res.push((rect, data.iter().cloned().collect::<Vec<_>>()));
        }).unwrap();
        for (rect, data) in res.drain(..) {
            self.fonttex.write(glium::Rect {
                    left: rect.min.x,
                    bottom: rect.min.y,
                    width: rect.width(),
                    height: rect.height(),
            }, glium::texture::RawImage2d {
                    data: borrow::Cow::Owned(data),
                    width: rect.width(),
                    height: rect.height(),
                    format: glium::texture::ClientFormat::U8,
            });
        }
    }

    fn layout_text(&self, text: &str, scale: rusttype::Scale) -> Vec<rusttype::PositionedGlyph<'static>> {
        let mut result = Vec::new();
        let v_metrics = self.font.v_metrics(scale);
        let mut caret = rusttype::Point { x: 0.0, y: v_metrics.ascent };
        let mut last_glyph_id = None;
        for c in text.chars() {
            let base_glyph = self.font.glyph(c);
            if let Some(id) = last_glyph_id.take() {
                caret.x += self.font.pair_kerning(scale, id, base_glyph.id());
            }
            last_glyph_id = Some(base_glyph.id());
            let glyph = base_glyph.scaled(scale).positioned(caret);
            caret.x += glyph.unpositioned().h_metrics().advance_width;
            result.push(glyph);
        }
        result
    }

    pub fn text(&mut self, text: &str, x: f32, y: f32) {
        if let Some(color) = self.fill {
            let glyphs = self.layout_text(text, rusttype::Scale::uniform(self.text_size * self.dpi_factor));
            for glyph in &glyphs {
                self.fontcache.queue_glyph(0, glyph.clone());
            }
            self.upload_fontcache();
            for glyph in &glyphs {
                if let Some((uv_rect, xy_rect)) = self.fontcache.rect_for(0, &glyph).unwrap() {
                    let tris = [(x + (xy_rect.min.x as f32) / self.dpi_factor, y + (xy_rect.min.y as f32) / self.dpi_factor), 
                                (x + (xy_rect.max.x as f32) / self.dpi_factor, y + (xy_rect.min.y as f32) / self.dpi_factor), 
                                (x + (xy_rect.max.x as f32) / self.dpi_factor, y + (xy_rect.max.y as f32) / self.dpi_factor),
                                (x + (xy_rect.min.x as f32) / self.dpi_factor, y + (xy_rect.min.y as f32) / self.dpi_factor), 
                                (x + (xy_rect.max.x as f32) / self.dpi_factor, y + (xy_rect.max.y as f32) / self.dpi_factor), 
                                (x + (xy_rect.min.x as f32) / self.dpi_factor, y + (xy_rect.max.y as f32) / self.dpi_factor)];
                    let uvcs = [(uv_rect.min.x, uv_rect.min.y), (uv_rect.max.x, uv_rect.min.y), (uv_rect.max.x, uv_rect.max.y),
                                (uv_rect.min.x, uv_rect.min.y), (uv_rect.max.x, uv_rect.max.y), (uv_rect.min.x, uv_rect.max.y)];
                    self.draw_tris(tris.iter().cloned(), color, Some(uvcs.iter().cloned()), 1);
                }
            }
        }
    }

    pub fn text_width(&self, text: &str, text_size: Option<f32>) -> f32 {
        let glyphs = self.layout_text(text, rusttype::Scale::uniform(text_size.unwrap_or(self.text_size) * self.dpi_factor));
        let gl = glyphs.len() - 1;
        if let Some(bb) = glyphs[0].pixel_bounding_box() {
            let start = bb.min.x as f32;
            if let Some(bb) = glyphs[gl].pixel_bounding_box() {
                let end = bb.max.x as f32;
                (end - start) / self.dpi_factor
            } else {
                0.0
            }
        } else {
            0.0
        }
    }

    pub fn image(&mut self, name: &str, x: f32, y: f32, w: f32, h: f32) {
        let tex = self.image_loader.load(&self.display, name);
        self.draw_tris(
            [(x, y), (x + w, y), (x + w, y + h), (x, y), (x + w, y + h), (x, y + h)].iter().cloned(), 
            (0.0, 0.0, 0.0, 1.0), 
            Some([(0.0, 0.0), (1.0, 0.0), (1.0, 1.0), (0.0, 0.0), (1.0, 1.0), (0.0, 1.0)].iter().cloned()), 
            tex
        );
    }

    pub fn triangle(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x3: f32, y3: f32) {
        if let Some(color) = self.fill {
            self.draw_tris::<_, NoUVs>([(x1 , y1 ), (x2 , y2 ), (x3 , y3 )].iter().cloned(), color, None, 0);
        }

        if self.stroke.is_some() {
            let (dx1, dy1) = self.raw_line(x1, y1, x2, y2);
            let (dx2, dy2) = self.raw_line(x2, y2, x3, y3);
            self.do_stroke_join(x2 , y2 , dx1, dy1, dx2, dy2);
            let (dx3, dy3) = self.raw_line(x3, y3, x1, y1);
            self.do_stroke_join(x3, y3, dx2, dy2, dx3, dy3);
            self.do_stroke_join(x1, y1, dx3, dy3, dx1, dy1);
        }
    }

    pub fn quad(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x3: f32, y3: f32, x4: f32, y4: f32) {
        if let Some(color) = self.fill {
            self.draw_tris::<_, NoUVs>([(x1, y1), (x2, y2), (x3, y3), 
                            (x1, y1), (x3, y3), (x4, y4)].iter().cloned(), color, None, 0);
        }

        if self.stroke.is_some() {
            let (dx1, dy1) = self.raw_line(x1 , y1 , x2 , y2 );
            let (dx2, dy2) = self.raw_line(x2 , y2 , x3 , y3 );
            self.do_stroke_join(x2 , y2 , dx1, dy1, dx2, dy2);
            let (dx3, dy3) = self.raw_line(x3 , y3 , x4 , y4 );
            self.do_stroke_join(x3 , y3 , dx2, dy2, dx3, dy3);
            let (dx4, dy4) = self.raw_line(x4 , y4 , x1 , y1 );
            self.do_stroke_join(x4 , y4 , dx3 , dy3 , dx4 , dy4 );
            self.do_stroke_join(x1 , y1 , dx4 , dy4 , dx1 , dy1 );
        }
    }

    pub fn ellipse(&mut self, x: f32, y: f32, rx: f32, ry: f32) {
        let key = CacheShape::Ellipse(rx.into(), ry.into());
        if self.cache.contains_key(&key) {
            let tris = &self.cache[&key].0;
            let stroke_tris = &self.cache[&key].1;
            if let Some(color) = self.fill {
                self.draw_tris::<_, NoUVs>(tris.iter().map(|(ax, ay)| (ax + x, ay + y)), color, None, 0);
            }
            if let Some(color) = self.stroke {
                self.draw_tris::<_, NoUVs>(stroke_tris.iter().map(|(ax, ay)| (ax + x, ay + y)), color, None, 0);
            }

            return
        }

        let mut points = Vec::new();
        let n = 10 + (rx.max(ry).ceil() as u32);
        for i in 0..n {
            let theta = 6.28318530 * (i as f32) / (n as f32);
            points.push((rx * theta.cos(), ry * theta.sin()));
        }

        let mut tris = Vec::new();
        let (x0, y0) = points[0];
        for pair in points.windows(2).skip(1) {
            let (x1, y1) = pair[0];
            let (x2, y2) = pair[1];
            tris.push((x0, y0));
            tris.push((x1, y1));
            tris.push((x2, y2));
        }

        if let Some(color) = self.fill {
            self.draw_tris::<_, NoUVs>(tris.iter().map(|(ax, ay)| (ax + x, ay + y)), color, None, 0);
        }

        let mut stroke_tris = Vec::new();
        let mut corners = Vec::new();
        for pair in points.windows(2) {
            let (x0, y0) = pair[0];
            let (x1, y1) = pair[1];
            let (mut geom, (dx0, dy0)) = Self::line_geom(self.stroke_weight as f32, x0, y0, x1, y1);
            corners.push((dx0, dy0, x0, y0));
            stroke_tris.append(&mut geom);
        }
        let (x0, y0) = points[points.len() - 1];
        let (x1, y1) = points[0];
        let (mut geom, (dx0, dy0)) = Self::line_geom(self.stroke_weight as f32, x0, y0, x1, y1);
        corners.push((dx0, dy0, x0, y0));
        stroke_tris.append(&mut geom);
        for pair in corners.windows(2) {
            let (dx0, dy0, _, _) = pair[0];
            let (dx1, dy1, x1, y1) = pair[1];

            stroke_tris.append(&mut Self::stroke_join_geom(x1, y1, dx0, dy0, dx1, dy1));
        }
        let (dx0, dy0, _, _) = corners[corners.len() - 1];
        let (dx1, dy1, x1, y1) = corners[0];
        stroke_tris.append(&mut Self::stroke_join_geom(x1, y1, dx0, dy0, dx1, dy1));

        if let Some(color) = self.stroke {
            self.draw_tris::<_, NoUVs>(stroke_tris.iter().map(|(ax, ay)| (ax + x, ay + y)), color, None, 0);
        }

        self.cache.insert(CacheShape::Ellipse(rx.into(), ry.into()), (tris, stroke_tris));
    }

    pub fn rect(&mut self, x: f32, y: f32, width: f32, height: f32) {
        self.quad(x, y, x + width, y, x + width, y + height, x, y + height);
    }

    pub fn mouse_x(&self) -> u32 {
        //println!("{:?}", self.moffset);
        (self.mouse_x as f64 + self.moffset.0) as u32
    }

    pub fn mouse_y(&self) -> u32 {
        (self.mouse_y as f64 + self.moffset.1) as u32
    }

    pub fn mouse_pressed(&self) -> bool {
        self.mouse_pressed
    }

    pub fn width(&self) -> u32 {
        self.size.0
    }

    pub fn height(&self) -> u32 {
        self.size.1
    }

    fn update(&mut self) -> bool {
        let mut events = Vec::with_capacity(16);
        self.event_loop.poll_events(|e| events.push(e));

        for event in events.drain(..) {
            match event {
                glutin::Event::WindowEvent { event: glutin::WindowEvent::CloseRequested, .. } => return true,
                glutin::Event::WindowEvent { event: glutin::WindowEvent::CursorMoved { position, .. }, .. } => {
                    self.mouse_x = position.x as u32;
                    self.mouse_y = position.y as u32;
                    self.events.push(Event::MouseMoved);
                },
                glutin::Event::WindowEvent { event: glutin::WindowEvent::MouseInput { state, button, .. }, ..} => {
                    match state {
                        glutin::ElementState::Pressed => {
                            self.mouse_pressed = true;
                            self.events.push(Event::MousePressed(match button {
                                glutin::MouseButton::Left => Button::Left,
                                glutin::MouseButton::Right => Button::Right,
                                glutin::MouseButton::Middle => Button::Middle,
                                glutin::MouseButton::Other(_) => continue
                            }));
                        },
                        glutin::ElementState::Released => {
                            self.mouse_pressed = false;
                            self.events.push(Event::MouseReleased(match button {
                                glutin::MouseButton::Left => Button::Left,
                                glutin::MouseButton::Right => Button::Right,
                                glutin::MouseButton::Middle => Button::Middle,
                                glutin::MouseButton::Other(_) => continue
                            }));
                        }
                    }
                },
                glutin::Event::WindowEvent { event: glutin::WindowEvent::MouseWheel { delta, .. }, .. } => {
                    let (x, y) = match delta {
                        glutin::MouseScrollDelta::LineDelta(x, y) => (x, y),
                        glutin::MouseScrollDelta::PixelDelta(glutin::dpi::LogicalPosition { x, y }) => (x as f32, y as f32)
                    };

                    if x > 0.0 {
                        self.events.push(Event::MouseScroll(Scroll::Right));
                    } else if x < 0.0 {
                        self.events.push(Event::MouseScroll(Scroll::Left));
                    }

                    if y > 0.0 {
                        self.events.push(Event::MouseScroll(Scroll::Up));
                    } else if y < 0.0 {
                        self.events.push(Event::MouseScroll(Scroll::Down));
                    }
                },
                glutin::Event::WindowEvent { event: glutin::WindowEvent::KeyboardInput { input, .. }, .. } => {
                    match input.state {
                        glutin::ElementState::Pressed => self.events.push(Event::KeyPressed(input.virtual_keycode.unwrap(), input.modifiers)),
                        glutin::ElementState::Released => self.events.push(Event::KeyReleased(input.virtual_keycode.unwrap(), input.modifiers))
                    }
                },
                glutin::Event::WindowEvent { event: glutin::WindowEvent::Resized(_), .. } => {
                    self.size = self.display.gl_window().window().get_inner_size().unwrap().into();
                    self.viewport.swidth = self.size.0 as f32;
                    self.viewport.sheight = self.size.1 as f32;
                    self.events.push(Event::Resized);
                },
                _ => ()
            }
        }

        if self.cursor_grab {
            let gl_window = self.display.gl_window();
            let window = gl_window.window();
            let glium::glutin::dpi::LogicalSize { width, height } = window.get_inner_size().unwrap();
            if self.mouse_x as f64 > width * 0.9 {
                window.set_cursor_position(glium::glutin::dpi::LogicalPosition::new(0.1*width, self.mouse_y as f64)).unwrap();
                self.moffset.0 += width*0.8;
            }
            if (self.mouse_x as f64) < 0.1 * width {
                window.set_cursor_position(glium::glutin::dpi::LogicalPosition::new(0.9*width, self.mouse_y as f64)).unwrap();
                self.moffset.0 -= width*0.8;
            }
            if self.mouse_y as f64 > height * 0.9 {
                window.set_cursor_position(glium::glutin::dpi::LogicalPosition::new(self.mouse_x as f64, 0.1*height)).unwrap();
                self.moffset.1 += height*0.8;
            }
            if (self.mouse_y as f64) < 0.1 * height {
                window.set_cursor_position(glium::glutin::dpi::LogicalPosition::new(self.mouse_y as f64, 0.9*height)).unwrap();
                self.moffset.1 -= height*0.8;
            }
            //window.hide_cursor(self.cursor_grab);
        }
        false
    }

    fn poll_events(&mut self) -> Vec<Event> {
        self.events.drain(..).collect()
    }

    fn render(&mut self) {
        let mut frame = self.display.draw();
        if self.frame_program.is_some() && self.frame_program.as_ref().unwrap().len() > 0 {
            let shaders = self.frame_program.take().unwrap();
            let (last, init) = shaders.split_last().unwrap();
            for shader in init {
                self.frame_tex.as_surface().draw(
                    &self.frame_vertices, 
                    glium::index::NoIndices(glium::index::PrimitiveType::TrianglesList),
                    shader.0.as_ref(),
                    &glium::uniform! {
                        screen: self.frame_tex.sampled(),
                        time: self.time
                    },
                    &Default::default()
                ).unwrap();
            }
            frame.draw(
                &self.frame_vertices, 
                glium::index::NoIndices(glium::index::PrimitiveType::TrianglesList),
                last.0.as_ref(),
                &glium::uniform! {
                    screen: self.frame_tex.sampled(),
                    time: self.time
                },
                &Default::default()
            ).unwrap();
        } else {
            frame.draw(
                &self.frame_vertices, 
                glium::index::NoIndices(glium::index::PrimitiveType::TrianglesList),
                self.default_frame_program.0.as_ref(),
                &glium::uniform! {
                    screen: self.frame_tex.sampled(),
                    time: self.time
                },
                &Default::default()
            ).unwrap();
        }
        frame.finish().unwrap();
    }

    fn tic(&mut self) {
        self.before = time::Instant::now();
    }

    fn toc(&mut self) {
        while self.before.elapsed().as_float_secs() < self.frametime_min { thread::yield_now() }
        self.frametimes.pop_front();
        self.last_frametime = self.before.elapsed().as_float_secs();
        self.frametimes.push_back(self.before.elapsed().as_float_secs());
        self.time += self.last_frametime as f32;
    }

    pub fn fps(&self) -> f64 {
        let f: f64 = self.frametimes.iter().map(|&f| f).sum();
        (self.frametimes.len() as f64)/f
    }

    pub fn frametime(&self) -> f64 {
        self.last_frametime
    }
}

pub trait App: Sized {
    fn setup() -> (Self, Window);

    fn draw(&self, _: &mut Window) {}

    fn update(&mut self, _: f64, _: &Window) {}

    fn event(&mut self, _: Event, _: &Window) {}
}

pub fn run<A: App>() {
    let (mut game, mut window) = A::setup();

    loop {
        window.tic();

        window.start_batch();
        game.draw(&mut window);
        window.end_batch();
        window.render();

        if window.update() {
            break;
        } else {
            for event in window.poll_events() {
                game.event(event, &window);
            }

            game.update(window.frametime(), &window);
        }

        window.toc();
    }
}
