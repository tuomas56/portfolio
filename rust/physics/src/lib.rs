//! A crate for simple 2D physics simulation. Computes and resolves
//! collisions between a set of moving circular bodies and a set of fixed
//! rectangles and circles. Enable the feature `pixelengine` for debug drawing.
//! See the `simple` example for basic usage.
//! 
//! Minimal example:
//! ```rust
//! use simple_physics::{
//!     World, FixedGeometry, FixedBody,
//!     DynamicBody, Shape, DegreeRadian,
//!     ID, glam
//! };
//! 
//! fn main() {   
//!     let mut world = World {
//!         geometry: FixedGeometry::from_vec(vec![
//!             FixedBody {
//!                 shape: Shape::Rectangle {
//!                     width: 100.0, height: 50.0, angle: (20.0).to_radians()
//!                 },
//!                 position: glam::vec2(0.0, 0.0),
//!                 friction: 0.3
//!             }
//!         ]),
//!         bodies: vec![
//!             DynamicBody {
//!                 id: ID::new(),
//!                 radius: 10.0,
//!                 position: glam::vec2(0.0, 100.0),
//!                 velocity: glam::vec2(0.0, 0.0),
//!                 gravity: 100.0,
//!                 mass: 1.0,
//!                 restitution: 0.5
//!             }
//!         ]
//!     };
//! 
//!     for _ in 0..10 {
//!         world.update(1.0 / 10.0);
//!         println!("{:?}", world);
//!     }
//! }
//! ```

#[cfg(feature = "pixelengine")]
use pixelengine as px;
use glam::*;
use aabb_quadtree as qt;
pub use snowflake::ProcessUniqueId as ID;
pub use glam;


/// Convert between degrees and radians representation.
pub trait DegreeRadian {
    /// Convert degrees to radians, you may not assume that the result
    /// will be in `[0, 2pi]` or any other range.
    fn to_radians(self) -> Self;

    /// Convert radians to degrees, you may not assume that the result
    /// will be in `[0, 360]` or any other range.
    fn to_degrees(self) -> Self;
}

impl DegreeRadian for f32 {
    fn to_radians(self) -> Self {
        3.14159265 * self / 180.0
    }

    fn to_degrees(self) -> Self {
        180.0 * self / 3.14159265
    }
}

/// The available shapes for `FixedBody`s.
#[derive(Debug, Copy, Clone)]
pub enum Shape {
    /// A rectangle that is of size `width` in the x-axis by
    /// `height` in the y-axis rotated by `angle` radians
    /// about the center, anti-clockwise from the x-axis.
    Rectangle {
        width: f32,
        height: f32,
        angle: f32
    },
    /// An axis-aligned rectangle. Should be preferred to
    /// `Rectangle` where possible.
    AABB {
        width: f32,
        height: f32
    },
    Circle {
        radius: f32
    }
}

impl Shape {
    /// Draw this shape in black wireframe, to scale at location (x, y).
    #[cfg(feature = "pixelengine")]
    pub fn draw_debug(&self, x: f32, y: f32, window: &mut px::Window) {
        window.no_fill();
        window.stroke(0.0, 0.0, 0.0, 1.0);
        match self {
            Shape::Circle { radius } => window.ellipse(x, y, *radius, *radius),
            Shape::Rectangle { width, height, angle } => {
                window.push_matrix();
                window.rotate(*angle);
                window.translate(x, y);
                window.rect(- width / 2.0, - height / 2.0, *width, *height);
                window.pop_matrix();
            },
            Shape::AABB { width, height } => window.rect(x - width / 2.0, y - height / 2.0, *width, *height)
        }
    }
}

/// An axis aligned bounding box.
#[derive(Debug, Copy, Clone, Default)]
pub struct BoundingBox {
    pub top_left: Vec2,
    pub width_height: Vec2
}

impl BoundingBox {
    /// Set `self` to the smallest AABB enclosing both `self` and `other`.
    pub fn extend(&mut self, other: BoundingBox) {
        self.top_left = self.top_left.min(other.top_left);
        self.width_height = (self.top_left + self.width_height).max(other.top_left + other.width_height) - self.top_left;
    }

    /// Extend `self` by the specified size in each direction.
    pub fn grow(&mut self, extent: Vec2) {
        self.top_left -= extent;
        self.width_height += 2.0 * extent;
    }

    /// Draw this AABB to scale in red wireframe.
    #[cfg(feature = "pixelengine")]
    pub fn draw_debug(&self, window: &mut px::Window) {
        window.stroke(1.0, 0.0, 0.0, 1.0);
        window.no_fill();
        window.rect(self.top_left.x(), self.top_left.y(), self.width_height.x(), self.width_height.y());
    }
}

impl<T> From<BoundingBox> for euclid::TypedRect<f32, T> {
    fn from(bbox: BoundingBox) -> Self {
        euclid::TypedRect::new(
            <(f32, f32)>::from(bbox.top_left).into(),
            <(f32, f32)>::from(bbox.width_height).into()
        )
    }
}

impl<T> From<euclid::TypedRect<f32, T>> for BoundingBox {
    fn from(bbox: euclid::TypedRect<f32, T>) -> Self {
        BoundingBox {
            top_left: bbox.origin.to_tuple().into(),
            width_height: bbox.size.to_tuple().into()
        }
    }
}

/// A body that does not move or collide with other `FixedBody`s.
#[derive(Debug, Clone)]
pub struct FixedBody {
    /// The shape of the body.
    pub shape: Shape,
    /// The location of the center of the shape.
    pub position: Vec2,
    /// The coefficient of static friction for objects
    /// colliding with this body.
    pub friction: f32
}

impl FixedBody {
    /// Draw the shape of the body to scale.
    #[cfg(feature = "pixelengine")]
    pub fn draw_debug(&self, window: &mut px::Window) {
        self.shape.draw_debug(self.position.x(), self.position.y(), window);
    }

    /// Check if `self` collides with a `DynamicBody`, returning a
    /// tuple of normal direction (towards `self`) and a penetration
    /// distance, if it does.
    pub fn collide(&self, other: &DynamicBody) -> Option<(Vec2, f32)> {
        match self.shape {
            Shape::Circle { radius } => {
                let prel = self.position - other.position;
                if prel.dot(prel) < (radius + other.radius) * (radius + other.radius) {
                    let normal = prel.normalize();
                    let dist = prel.dot(prel).sqrt() - radius - other.radius;
                    Some((normal, dist))
                } else {
                    None
                }
            },
            Shape::Rectangle { width, height, angle } => {
                let to_world = Mat2::from_angle(angle);
                let to_aabb = to_world.inverse();
                let pos = to_aabb * (other.position - self.position) + self.position;
                let hextent = vec2(width/2.0, height/2.0);
                let closest = pos.min(self.position + hextent).max(self.position - hextent);
                let disp = closest - pos;
                if disp.dot(disp) < other.radius * other.radius {
                    let normal = to_world * disp.normalize();
                    let dist = other.radius - disp.dot(disp).sqrt();
                    Some((normal, dist))
                } else {
                    None
                }
            },
            Shape::AABB { width, height } => {
                let hextent = vec2(width/2.0, height/2.0);
                let closest = other.position.min(self.position + hextent).max(self.position - hextent);
                let disp = closest - other.position;
                if disp.dot(disp) < other.radius * other.radius {
                    let normal = disp.normalize();
                    let dist = other.radius - disp.dot(disp).sqrt();
                    Some((normal, dist))
                } else {
                    None
                }
            }
        }
    }

    /// The minimal bounding box containing this body.
    pub fn bounding_box(&self) -> BoundingBox {
        match self.shape {
            Shape::Circle { radius } => BoundingBox {
                top_left: self.position - vec2(radius, radius),
                width_height: vec2(2.0 * radius, 2.0 * radius)
            },
            Shape::AABB { width, height } => {
                let hextent = vec2(width / 2.0, height / 2.0);
                BoundingBox {
                    top_left: self.position - hextent,
                    width_height: 2.0 * hextent
                }
            },
            Shape::Rectangle { width, height, angle } => {
                let hextentx = vec2(width / 2.0, 0.0);
                let hextenty = vec2(0.0, height / 2.0);
                let to_world = Mat2::from_angle(angle);
                let a = self.position + to_world * -(hextentx + hextenty);
                let b = self.position + to_world * -(hextentx - hextenty);
                let c = self.position + to_world * (hextentx + hextenty);
                let d = self.position + to_world * (hextentx - hextenty);
                let top_left = a.min(b).min(c).min(d);
                let bottom_right = a.max(b).max(c).max(d);
                BoundingBox {
                    top_left, width_height: bottom_right - top_left
                }
            }
        }
    }
}

/// A set of `FixedBody`s that can be efficiently collided against,
/// implemented with a quad-tree.
#[derive(Debug, Clone)]
pub struct FixedGeometry {
    tree: qt::QuadTree<FixedBody, (), [(qt::ItemId, euclid::TypedRect<f32, ()>); 0]>
}

impl FixedGeometry {
    /// Create a `FixedGeometry` from a vector of bodies. The limits
    /// of the quad-tree are set as the minimum bounding box enclosing
    /// all of them, plus 1 unit on each side.
    pub fn from_vec(bodies: Vec<FixedBody>) -> FixedGeometry {
        let mut bbox = bodies.get(0).map(|b| b.bounding_box()).unwrap_or_default();
        for body in &bodies {
            bbox.extend(body.bounding_box());
        }
        bbox.grow(vec2(1.0, 1.0));

        let mut tree = qt::QuadTree::default(bbox.into(), bodies.len());
        
        for body in bodies {
            let bbox = body.bounding_box().into();
            tree.insert_with_box(body, bbox);
        }

        FixedGeometry { tree }
    }

    /// Create an empty `FixedGeometry` with the given limits.
    pub fn empty(limits: BoundingBox) -> FixedGeometry {
        let tree = qt::QuadTree::default(limits.into(), 0);
        FixedGeometry { tree }
    }

    /// Add a body to the quad-tree. Will panic if its
    /// bounding box does not lie within the limits of the tree.
    pub fn push(&mut self, body: FixedBody) {
        let bbox = body.bounding_box().into();
        self.tree.insert_with_box(body, bbox);
    }

    /// Draw each body and its bounding box to scale.
    #[cfg(feature = "pixelengine")]
    pub fn draw_debug(&self, window: &mut px::Window) {
        for (_, (body, bbox)) in self.tree.iter() {
            body.draw_debug(window);
            BoundingBox::from(*bbox).draw_debug(window);
        }
    }

    /// Compute all collisions of a `DynamicBody` with the geometry,
    /// calling the given closure once for each collision, providing:
    /// 
    /// * A reference to the fixed body it collided with,
    /// * the normal direction of the collision,
    /// * the penetration distance of the body into the geometry,
    /// * and a mutable reference to the dynamic body.
    /// 
    /// The reference is mutable so that the body can be updated
    /// between each collision. However, the bounding-box is not
    /// re-computed between calls, so you should be careful not
    /// to move the body too much, or collisions will be missed.
    /// It returns the number of collisions that occurred.
    pub fn collide_mut<F: FnMut(&FixedBody, Vec2, f32, &mut DynamicBody)>(
        &self, other: &mut DynamicBody, mut handler: F
    ) -> usize {
        let mut count = 0;
        for (body, _, _) in self.tree.query(other.bounding_box().into()) {
            if let Some((loc, dist)) = body.collide(other) {
                handler(body, loc, dist, other);
                count += 1;
            }
        }
        count
    }

    /// Compute all collisions of a `DynamicBody` with the geometry,
    /// returning an iterator which yields for each collision:
    /// 
    /// * A reference to the fixed body it collided with,
    /// * the normal direction of the collision,
    /// * the penetration distance of the body into the geometry.
    /// 
    /// If you want to modify the dynamic body for each collision without
    /// any extra allocation or overhead, try `collide_mut`.
    pub fn collide<'c, 'a: 'c, 'b: 'c>(
        &'a self, other: &'b DynamicBody
    ) -> CollisionIterator<'c> {
        CollisionIterator {
            query: self.tree.query(other.bounding_box().into()),
            other, idx: 0
        }
    }
}


/// An iterator over collisions between a `DynamicBody` and a `FixedGeometry`.
/// Created with the `FixedGeometry::collide` method.
pub struct CollisionIterator<'c> {
    query: smallvec::SmallVec<[(
        &'c FixedBody, euclid::TypedRect<f32, ()>, qt::ItemId
    ); 3]>,
    other: &'c DynamicBody,
    idx: usize
}

impl<'c> Iterator for CollisionIterator<'c> {
    type Item = (&'c FixedBody, Vec2, f32);

    fn next(&mut self) -> Option<Self::Item> {
        while self.idx < self.query.len() {
            let body = self.query[self.idx].0;
            if let Some((normal, dist)) = body.collide(self.other) {
                return Some((body, normal, dist));
            }

            self.idx += 1;
        }

        None
    }
}

/// A moving body. All `DynamicBody`s are circular in shape.
#[derive(Debug, Clone, Default)]
pub struct DynamicBody {
    /// A unique ID for the body, used to check for equality.
    pub id: ID,
    /// The radius of the circle for the body.
    pub radius: f32,
    /// Position of the center of the body.
    pub position: Vec2,
    /// Velocity of the body.
    pub velocity: Vec2,
    /// Gravitational acceleration for the body.
    pub gravity: f32,
    /// Mass of the body.
    pub mass: f32,
    /// The coefficient of restitution for the body.
    /// This is the value used for collisions between this
    /// and a `FixedBody`. When colliding with another 
    /// `DynamicBody` the lowest of the two values is taken. 
    pub restitution: f32
}

impl DynamicBody {
    /// Draw the shape of the body to scale in green wireframe,
    /// and its bounding box in red wireframe
    #[cfg(feature = "pixelengine")]
    pub fn draw_debug(&self, window: &mut px::Window) {
        window.no_fill();
        window.stroke(0.0, 1.0, 0.0, 1.0);
        window.ellipse(self.position.x(), self.position.y(), self.radius, self.radius);
        self.bounding_box().draw_debug(window);
    }

    /// Collide this with another `DynamicBody`, and if it occurs, return
    /// the normal direction (towards `self`) and the penetration distance.
    pub fn collide(&self, other: &DynamicBody) -> Option<(Vec2, f32)> {
        let prel = self.position - other.position;
        if prel.dot(prel) < (self.radius + other.radius) * (self.radius + other.radius) {
            let normal = prel.normalize();
            let dist = prel.dot(prel).sqrt() - self.radius - other.radius;
            Some((normal, dist))
        } else {
            None
        }
    }

    /// The minimal bounding box of this body.
    pub fn bounding_box(&self) -> BoundingBox {
        let hextent = vec2(self.radius, self.radius);
        BoundingBox {
            top_left: self.position - hextent,
            width_height: 2.0 * hextent
        }
    }
}

/// A structure containing all fixed and dynamic bodies.
#[derive(Debug, Clone)]
pub struct World {
    pub geometry: FixedGeometry,
    pub bodies: Vec<DynamicBody>
}

impl World {
    /// Draw all bodies and their bounding boxes to scale.
    #[cfg(feature = "pixelengine")]
    pub fn draw_debug(&self, window: &mut px::Window) {
        window.background(1.0, 1.0, 1.0);
        window.no_fill();
        window.stroke(0.0, 0.0, 0.0, 1.0);
        window.stroke_weight(2.0);
        self.geometry.draw_debug(window);
        for body in &self.bodies {
            body.draw_debug(window);
        }
    }

    /// Return the indices of dynamic bodies that overlap the given position.
    /// These are raw indices into the bodies list. If you update the list,
    /// you are responsible for modifying any previously obtained indices.
    pub fn pick_body<'a>(&'a self, position: Vec2) -> impl Iterator<Item=usize> + 'a {
        let cursor = DynamicBody {
            id: ID::new(), radius: 0.0, position, ..Default::default()
        };

        self.bodies.iter().enumerate().filter(move |(_, body)| body.collide(&cursor).is_some()).map(|(i, _)| i)
    }

    /// Check if the given dynamic body collides with anything in the world.
    pub fn collide(&self, body: &DynamicBody) -> bool {
        self.geometry.collide(&mut body.clone()).next().is_some()
        || self.bodies.iter().any(|b| b.id != body.id && b.collide(body).is_some())
    }

    /// Simulate `dt` seconds of time in the world, updating all bodies accordingly.
    /// This is `O(c * d^2 * log f)` where `c` is the number of collisions per timestep,
    /// `d` is the number of dynamic bodies and `f` is the number of fixed bodies.
    pub fn update(&mut self, dt: f32) {
        for body in &mut self.bodies {
            body.velocity += dt * body.gravity * vec2(0.0, -1.0);
            body.position += dt * body.velocity;
        }

        for body in &mut self.bodies {
            self.geometry.collide_mut(body, |wall, loc, dist, body| {
                let par_j_pre = body.velocity.dot(loc);
                let perp_j_pre = body.velocity - par_j_pre * loc;
                let par_j_post = -body.restitution * par_j_pre;
                let nimpulse = (par_j_post - par_j_pre).abs();
                let perp_mag = perp_j_pre.dot(perp_j_pre).sqrt();
                let perp_j_post = if perp_mag > 0.0 {
                    let fimpulse = (wall.friction * nimpulse).min(perp_mag - 0.1).max(0.0) * perp_j_pre.normalize();
                    perp_j_pre - fimpulse 
                } else {
                    perp_j_pre
                };
                body.velocity = perp_j_post + par_j_post * loc;
                body.position -= dist * loc;
            });
        }

        for i in 0..self.bodies.len() {
            for j in 0..self.bodies.len() {
                if i != j {
                    if let Some((loc, dist)) = self.bodies[i].collide(&self.bodies[j]) {
                        let par_i_pre = self.bodies[i].velocity.dot(loc);
                        let par_j_pre = self.bodies[j].velocity.dot(loc);
                        let perp_i = self.bodies[i].velocity - par_i_pre * loc;
                        let perp_j = self.bodies[j].velocity - par_j_pre * loc;
                        let coefr = self.bodies[i].restitution.min(self.bodies[j].restitution);
                        let evrel = coefr * (par_i_pre - par_j_pre);
                        let msum = self.bodies[i].mass + self.bodies[j].mass;
                        let momentum_pre = self.bodies[i].mass * par_i_pre + self.bodies[j].mass * par_j_pre;
                        let par_j_post = (momentum_pre + self.bodies[i].mass * evrel) / msum;
                        let par_i_post = par_j_post - evrel;
                        self.bodies[i].velocity = perp_i + par_i_post * loc;
                        self.bodies[j].velocity = perp_j + par_j_post * loc;
                        self.bodies[i].position -= dist/2.0 * loc;
                        self.bodies[j].position += dist/2.0 * loc;
                    }
                }
            }
        }
    }
}
