from __future__ import division

class Circle:
    def __init__(self, x, y, radius):
        self.x = x
        self.y = y
        self.vx = 0
        self.vy = 0
        self.a = 0
        self.va = 0
        self.ax = 0
        self.ay = 0
        self.aa = 0
        self.da = 0.99
        self.dx = 0.99
        self.dy = 0.99
        self.radius = radius
        
    def draw(self):
        fill(0)
        noStroke()
        ellipse(self.x, self.y, self.radius*2, self.radius*2)
        stroke(255)
        line(self.x, self.y, 
             self.x + self.radius*cos(self.a),
             self.y + self.radius*sin(self.a))
        
    def apply_force(self, fx, fy, d):
        self.ax = fx
        self.ay = fy
        self.aa = sqrt(fx**2 + fy**2) * d / (0.5 * self.radius**2)
        
    def update(self):
        self.vx += self.ax
        self.vy += self.ay
        self.va += self.aa
        self.x += self.vx
        self.y += self.vy
        self.a += self.va
        self.va *= self.da
        self.vx *= self.dx
        self.vy *= self.dy
        self.ax = 0
        self.ay = 0
        self.aa = 0
        
c = Circle(400, 400, 20)

def setup():
    size(800, 800)

stage = 0

def draw():
    background(255)
    c.draw()
    stroke(255, 0, 0)
    global px, py, stage, fx, fy
    if stage == 0:
        dd = dist(c.x, c.y, mouseX, mouseY)
        dx = (mouseX - c.x)/dd
        dy = (mouseY - c.y)/dd
        px = c.radius*dx
        py = c.radius*dy
    elif stage == 1:
        dd = dist(px, py, mouseX, mouseY)
        fx = -(mouseX - c.x - px)/dd
        fy = -(mouseY - c.y - py)/dd
    elif stage == 2:
        ca = (fx*px + fy*py)/c.radius
        sa = sqrt(1 - ca**2)
        c.apply_force(10*fx, 10*fy, c.radius*sa)
        stage = 0
    line(c.x + px, c.y + py, mouseX, mouseY)
    c.update()
    
def mousePressed():
    global stage
    stage += 1