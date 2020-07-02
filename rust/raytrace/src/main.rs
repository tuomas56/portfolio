use glam::*;
use image::GenericImage;
use rayon::prelude::*;
use std::sync::Arc;

const PI: f32 = 3.14159265;

#[derive(Debug)]
struct Ray {
    ori: Vec3,
    dir: Vec3
}

impl Ray {
    fn new(ori: Vec3, dir: Vec3) -> Ray {
        Ray {
            ori, dir: dir.normalize()
        }
    }

    fn at(&self, t: f32) -> Vec3 {
        self.ori + t * self.dir
    }
}

#[derive(Debug)]
struct Camera {
    pos: Vec3,
    top_left: Vec3,
    horizontal: Vec3,
    vertical: Vec3
}

impl Camera {
    fn new(pos: Vec3, dir: Vec3, up: Vec3, fov: f32, aspect_ratio: f32) -> Camera {
        let a = (fov / 2.0).tan();
        let hhor = a * up.cross(dir);
        let hver = a / aspect_ratio * up;
        let top_left = pos + dir + hhor + hver;
        let horizontal = -2.0 * hhor;
        let vertical = -2.0 * hver;

        Camera {
            pos, top_left, horizontal, vertical
        }
    }

    fn pixel_ray(&self, u: f32, v: f32) -> Ray {
        let ori = self.pos;
        let dir = self.top_left + u * self.horizontal + v * self.vertical;
        Ray::new(ori, dir)
    }
}

struct Token;

trait Material: Send + Sync {
    fn exact_radiance(&self, scene: &Scene, tokens: &mut Vec<Token>, intersection: &Intersection, ray: &Ray) -> Option<Vec3>;
    fn approximate_radiance(&self, _scene: &Scene, _intersection: &Intersection, _ray: &Ray) -> Vec3 {
        Vec3::zero()
    }

    fn is_always_exact(&self) -> bool {
        false
    }
}

struct Diffuse {
    albedo: Vec3,
    fanout: usize
}

impl Material for Diffuse {
    fn exact_radiance(&self, scene: &Scene, tokens: &mut Vec<Token>, intersection: &Intersection, _ray: &Ray) -> Option<Vec3> {
        if tokens.len() == 0 {
            return None;
        }

        let ori = intersection.point + 0.0001 * intersection.normal;
        let mut col = Vec3::zero();
        let mut n = 0.0;
        for _ in 0..self.fanout {
            let u = 2.0 * PI * rand::random::<f32>();
            let v = 2.0 * rand::random::<f32>() - 1.0;
            let r = (1.0 - v * v).sqrt();
            let ru = Vec3::new(r * u.cos(), r * u.sin(), v);
            let target = intersection.normal + ru;
            let bounce_ray = Ray::new(ori, target);
            
            if let Some(token) = tokens.pop() {
                col += self.albedo * scene.radiance(bounce_ray, token, tokens);
                n += 1.0;
            } else {
                break;
            }
        }
        Some(col / n)
    }

    fn approximate_radiance(&self, scene: &Scene, intersection: &Intersection, _ray: &Ray) -> Vec3 {
        let mut col = Vec3::zero();
        let mut tokens = Vec::new();
        for sphere in &scene.spheres {
            if sphere.material.is_always_exact() {
                let ori = intersection.point + 0.0001 * intersection.normal;
                let dir = sphere.pos - ori;
                let ray = Ray::new(ori, dir);
                if let Some(nintersection) = scene.cast(&ray) {
                    if nintersection.id == sphere.id {
                        if let Some(radiance) = sphere.material.exact_radiance(scene, &mut tokens, &nintersection, &ray) {
                            let area_factor = sphere.radius * sphere.radius / (4.0 * dir.dot(dir));
                            col += ray.dir.dot(intersection.normal) * area_factor * self.albedo * radiance;
                        }
                    }
                }
            }
        }
        col + self.albedo * scene.ambient
    }
}

struct Metal {
    albedo: Vec3,
    roughness: f32
}

impl Material for Metal {
    fn exact_radiance(&self, scene: &Scene, tokens: &mut Vec<Token>, intersection: &Intersection, ray: &Ray) -> Option<Vec3> {
        if let Some(token) = tokens.pop() {
            let ori = intersection.point + 0.0001 * intersection.normal;
            if self.roughness > 0.0 {
                let dir = ray.dir - 2.0 * ray.dir.dot(intersection.normal) * intersection.normal;
                let u = 2.0 * PI * rand::random::<f32>();
                let v = 2.0 * rand::random::<f32>() - 1.0;
                let r = (1.0 - v * v).sqrt();
                let ru = Vec3::new(r * u.cos(), r * u.sin(), v);
                let bounce_ray = Ray::new(ori, dir + self.roughness * ru);
                Some(self.albedo * scene.radiance(bounce_ray, token, tokens))
            } else {
                let dir = ray.dir - 2.0 * ray.dir.dot(intersection.normal) * intersection.normal;
                let bounce_ray = Ray::new(ori, dir);
                Some(self.albedo * scene.radiance(bounce_ray, token, tokens))
            }
        } else {
            None
        }
    }

    fn approximate_radiance(&self, scene: &Scene, intersection: &Intersection, ray: &Ray) -> Vec3 {
        let dir = ray.dir - 2.0 * ray.dir.dot(intersection.normal) * intersection.normal;
        self.albedo * scene.envmap.radiance(dir)
    }
}
   
struct Emissive {
    radiance: Vec3
}

impl Material for Emissive {
    fn exact_radiance(&self, _scene: &Scene, _tokens: &mut Vec<Token>, _intersection: &Intersection, _ray: &Ray) -> Option<Vec3> {
        Some(self.radiance)
    }

    fn approximate_radiance(&self, _scene: &Scene, _intersection: &Intersection, _ray: &Ray) -> Vec3 {
        self.radiance
    }

    fn is_always_exact(&self) -> bool {
        true
    }
}

struct Glossy {
    refractive_index: f32,
    roughness: f32,
    diffuse: Diffuse
}

impl Material for Glossy {
    fn exact_radiance(&self, scene: &Scene, tokens: &mut Vec<Token>, intersection: &Intersection, ray: &Ray) -> Option<Vec3> {
        let r0 = (self.refractive_index - 1.0) / (self.refractive_index + 1.0);
        let r0 = r0 * r0;
        let normal = if self.roughness > 0.0 {
            let u = 2.0 * PI * rand::random::<f32>();
            let v = 2.0 * rand::random::<f32>() - 1.0;
            let r = (1.0 - v * v).sqrt();
            let ru = Vec3::new(r * u.cos(), r * u.sin(), v);
            (1.001 * intersection.normal + self.roughness * ru).normalize()
        } else {
            intersection.normal
        };
        let r = r0 + (1.0 - r0) * (1.0 - normal.dot(-ray.dir)).powf(5.0);

        let ori = intersection.point + 0.0001 * intersection.normal;
        let bounce_ray = {
            let dir = ray.dir - 2.0 * ray.dir.dot(normal) * normal;
            Ray::new(ori, dir)
        };
        let specular = r * scene.radiance(bounce_ray, tokens.pop()?, tokens);
        let diffuse = (1.0 - r) * self.diffuse.exact_radiance(scene, tokens, intersection, ray)?;

        Some(specular + diffuse)
    }

    fn approximate_radiance(&self, scene: &Scene, intersection: &Intersection, ray: &Ray) -> Vec3 {
        self.diffuse.approximate_radiance(scene, intersection, ray)
    }
}

#[allow(dead_code)]
struct Intersection<'a> {
    t: f32,
    id: usize,
    point: Vec3,
    normal: Vec3,
    front_face: bool,
    material: &'a dyn Material
}

struct Sphere {
    id: usize,
    pos: Vec3,
    radius: f32,
    material: Box<dyn Material>
}

impl Sphere {
    fn intersect(&self, r: &Ray) -> Option<Intersection> {
        let oc = r.ori - self.pos;

        let a = r.dir.dot(r.dir);
        let b = oc.dot(r.dir);
        let c = oc.dot(oc) - self.radius * self.radius;

        let disc = b * b - a * c;
        if disc > 0.0 {
            let sdisc = disc.sqrt();
            let t_min = -b - sdisc;
            let t_max = -b + sdisc;

            let t = if t_min > 0.0 {
                t_min / a
            } else if t_max > 0.0 {
                t_max / a
            } else {
                return None
            };

            let point = r.at(t);
            let normal = (point - self.pos).normalize();
            let front_face = normal.dot(r.dir) > 0.0;
            let normal = if c > 0.0 { normal } else { -normal };

            Some(Intersection { id: self.id, t, point, normal, front_face, material: &*self.material })
        } else {
            None
        }
    }
}

#[allow(dead_code)]
enum EnvMap {
    Image(image::DynamicImage),
    Constant(Vec3)
}

impl EnvMap {
    fn radiance(&self, dir: Vec3) -> Vec3 {
        match self {
            EnvMap::Image(image) => {
                let lat = dir.y().asin() + PI/2.0;
                let lon = dir.x().atan2(dir.z()) + PI;
                let x = ((lon / (2.0 * PI)) * (image.width() - 1) as f32) as u32;
                let y = ((1.0 - (lat / PI)) * (image.height() - 1) as f32) as u32;
                let p = image.get_pixel(x, y);
                Vec3::new(p[0] as f32 / 255.0, p[1] as f32 / 255.0, p[2] as f32 / 255.0)
            },
            EnvMap::Constant(c) => *c
        }
    }
}

struct Scene {
    envmap: EnvMap,
    ambient: Vec3,
    spheres: Vec<Sphere>
}

impl Scene {
    fn cast(&self, ray: &Ray) -> Option<Intersection> {
        let mut min_t = f32::INFINITY;
        let mut min_intersection = None;

        for sphere in &self.spheres {
            if let Some(intersection) = sphere.intersect(ray) {
                if intersection.t <= min_t {
                    min_t = intersection.t;
                    min_intersection = Some(intersection);
                }
            }
        }

        min_intersection
    }

    fn radiance(&self, ray: Ray, _token: Token, tokens: &mut Vec<Token>) -> Vec3 {
        if let Some(intersection) = self.cast(&ray) {
            intersection.material.exact_radiance(self, tokens, &intersection, &ray)
                .unwrap_or(intersection.material.approximate_radiance(self, &intersection, &ray))
        } else {
            self.envmap.radiance(ray.dir)
        }
    }
}

fn main() {
    let width = 600;
    let height = 600;
    let max_cast = 5;
    let k = 256;

    let mut event_loop = glium::glutin::EventsLoop::new();
    let window_builder = glium::glutin::WindowBuilder::new()
        .with_title("raytrace")
        .with_dimensions(glium::glutin::dpi::LogicalSize { width: width as f64 / 2.0, height: height as f64 / 2.0 });
    let context_builder = glium::glutin::ContextBuilder::new();
    let display = glium::Display::new(window_builder, context_builder, &event_loop).unwrap();

    let camera = Camera::new(vec3(0.0, 0.0, 0.1), Vec3::unit_z(), Vec3::unit_y(), PI / 2.0, width as f32 / height as f32);

    let envmap = EnvMap::Image(image::open("env.jpg").unwrap());

    let mut ambient = Vec3::zero();
    for _ in 0..10 {
        let u = 2.0 * PI * rand::random::<f32>();
        let v = 2.0 * rand::random::<f32>() - 1.0;
        let r = (1.0 - v * v).sqrt();
        let ru = Vec3::new(r * u.cos(), r * u.sin(), v);
        ambient += envmap.radiance(ru);
    }
    ambient /= 10.0;

    let scene  = Scene {
        envmap, ambient,
        spheres: vec![
            Sphere {
                id: 7,
                pos: vec3(0.0, 0.0, 5.0),
                radius: 1.0,
                material: Box::new(Glossy {
                    diffuse: Diffuse { albedo: vec3(0.5, 0.5, 0.5), fanout: 1 },
                    roughness: 0.05, refractive_index: 1.57
                })
            },
            Sphere {
                id: 5,
                pos: vec3(0.0, -11.0, 5.0),
                radius: 10.0,
                material: Box::new(Diffuse { albedo: vec3(0.5, 0.5, 0.5), fanout: 1 })
            },
            Sphere {
                id: 6,
                pos: vec3(0.0, 4.5, 5.0),
                radius: 1.0,
                material: Box::new(Emissive {
                    radiance: vec3(20.0, 20.0, 10.0)
                })
            }
        ]
    };

    let data = Arc::new(vec![0.0f32; width * height * 4]);

    #[derive(Copy, Clone)]
    struct Vertex { pos: [f32; 2] }

    glium::implement_vertex!(Vertex, pos);

    let vertices = glium::VertexBuffer::new(&display, &[
        Vertex { pos: [-1.0, -1.0] },
        Vertex { pos: [-1.0, 1.0] },
        Vertex { pos: [1.0, -1.0] },
        Vertex { pos: [1.0, -1.0] },
        Vertex { pos: [1.0, 1.0] },
        Vertex { pos: [-1.0, 1.0] }
    ]).unwrap();

    const VERT_SRC: &'static str = "
        #version 150

        in vec2 pos;
        out vec2 uv;

        void main() {
            gl_Position = vec4(pos, 0.0, 1.0);
            uv = (pos + vec2(1.0, -1.0)) / vec2(2.0, -2.0);
        }
    ";

    const FRAG_SRC: &'static str = "
        #version 150

        in vec2 uv;
        out vec4 col;

        uniform sampler2D tex;
        uniform float sample_num;

        void main() {
            col = texture(tex, uv);
            col *= col;
        }
    ";

    let program = glium::Program::from_source(&display, VERT_SRC, FRAG_SRC, None).unwrap();

    fn radicalinverse_vdc(mut bits: usize) -> f32 {
        bits = (bits << 16) | (bits >> 16);
        bits = ((bits & 0x55555555) << 1) | ((bits & 0xAAAAAAAA) >> 1);
        bits = ((bits & 0x33333333) << 2) | ((bits & 0xCCCCCCCC) >> 2);
        bits = ((bits & 0x0F0F0F0F) << 4) | ((bits & 0xF0F0F0F0) >> 4);
        bits = ((bits & 0x00FF00FF) << 8) | ((bits & 0xFF00FF00) >> 8);
       (bits as f32) * 2.3283064365386963e-10 // / 0x100000000
    }

    fn hammersley2d(i: usize, n: usize) -> (f32, f32) {
        ((i as f32)/(n as f32), radicalinverse_vdc(i))
    }

    let new_data = data.clone();
    std::thread::spawn(move || {
        let mut sample = 0.0;
        loop {
            unsafe { &mut *((&*new_data) as *const Vec<f32> as usize as *mut Vec<f32>) }.par_chunks_mut(4).enumerate().for_each(|(i, p)| {
                let x = i % width;
                let y = i / width;
                let mut color = Vec3::zero();
                let mut tokens = Vec::new();
                for i in 0..k {
                    tokens.clear();
                    for _ in 0..max_cast {
                        tokens.push(Token);
                    }
                    let (a, b) = if x > (width / 2) {
                        hammersley2d(i, k)
                    } else {
                        (rand::random(), rand::random())
                    };
                    let u = (x as f32 + a) / width as f32;
                    let v = (y as f32 + b) / height as f32;
                    let ray = camera.pixel_ray(u, v);
                    color += scene.radiance(ray, Token, &mut tokens);
                };
                color /= k as f32;
                color *= 1.0;
                color /= color + vec3(1.0, 1.0, 1.0);

                p[0] *= sample;
                p[1] *= sample;
                p[2] *= sample;
                p[0] += color.x().sqrt();
                p[1] += color.y().sqrt();
                p[2] += color.z().sqrt();
                p[0] /= sample + 1.0;
                p[1] /= sample + 1.0;
                p[2] /= sample + 1.0;
            });
            sample += 1.0;
        }
    });

    let mut events = Vec::new();
    loop {
        event_loop.poll_events(|e| events.push(e));
        for event in events.drain(..) {
            match event {
                glium::glutin::Event::WindowEvent {
                    event: glium::glutin::WindowEvent::CloseRequested, ..
                } => {
                    let img = image::ImageBuffer::from_fn(width as u32, height as u32, |x, y| {
                        let r = (data[4 * (y as usize * width + x as usize) + 0] * 255.0) as u8;
                        let g = (data[4 * (y as usize * width + x as usize) + 1] * 255.0) as u8;
                        let b = (data[4 * (y as usize * width + x as usize) + 2] * 255.0) as u8;
                        image::Rgb([r, g, b])
                    });
                    img.save("test.png").unwrap();
                    return;
                },
                _ => ()
            }
        }


        let im = glium::texture::RawImage2d {
            data: std::borrow::Cow::Borrowed(&*data),
            width: width as u32,
            height: height as u32,
            format: glium::texture::ClientFormat::F32F32F32F32
        };
        let texture = glium::texture::Texture2d::new(&display, im).unwrap();

        let mut frame = display.draw();
        use glium::Surface;
        frame.draw(
            &vertices,
            glium::index::NoIndices(glium::index::PrimitiveType::TrianglesList),
            &program,
            &glium::uniform! {
                tex: texture.sampled().magnify_filter(glium::uniforms::MagnifySamplerFilter::Nearest)
            },
            &Default::default()
        ).unwrap();
        frame.finish().unwrap();
    }
}