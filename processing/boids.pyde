import random

class Vec:
    def __init__(self, x, y):
        self.x, self.y = x, y
    
    def __add__(self, other):
        return Vec(self.x + other.x, self.y + other.y)
    
    def __sub__(self, other):
        return self + (-1 * other)
    
    def __mul__(self, other):
        return Vec(self.x * other, self.y * other)
    
    def __rmul__(self, other):
        return self * other
    
    def __div__(self, other):
        return self * (1 / other)
    
    def __rdiv__(self, other):
        return self / other
    
    def __eq__(self, other):
        return self.x == other.x and self.y == other.y
    
    def __abs__(self):
        return self.mag2() ** 0.5
    
    def mag2(self):
        return self & self
    
    def __and__(self, other):
        return self.x * other.x + self.y * other.y
    
    def __invert__(self):
        return self / abs(self) if abs(self) > 0 else self
    
    def limit(self, mag):
        if abs(self) > mag:
            return mag * ~self
        else:
            return self

class Body:
    def __init__(self, pos):
        self.pos = pos
        self.vel = Vec(0, 0)
        self.acc = Vec(0, 0)
        self.apos = 0
        self.avel = 0
        self.aacc = 0
        
    def update(self):
        self.vel += self.acc
        self.pos += self.vel
        self.avel += self.aacc
        self.apos += self.avel
        
    def predraw(self):
        pushMatrix()
        translate(self.pos.x, self.pos.y)
        rotate(self.apos)
        
    def postdraw(self):
        popMatrix()
        
class SpringMass(Body):
    def __init__(self, pos, r, sa):
        Body.__init__(self, pos)
        self.r = r
        self.rv = 0
        self.ra = 0
        self.apos = sa
        
    def update(self):
        self.aacc = 0.5 * cos(self.apos) / self.r
        self.ra = -0.001 * self.r
        self.ra += 0.15 * sin(self.apos)
        self.rv += self.ra
        self.r += self.rv
        self.rv *= 0.995
        Body.update(self)
        self.avel *= 0.995
        
    def draw(self):
        line(0, 0, self.r, 0)
        ellipse(self.r, 0, 10, 10)
        
class Seeker(Body):
    def __init__(self, pos):
        self.maxspeed = 6
        self.maxforce = 0.1
        Body.__init__(self, pos)
        self.vel = Vec(random.randint(-2, 2), random.randint(-2, 2))
        self.c = color(0, 0, 0)
    
    def draw(self):
        stroke(self.c)
        rect(-10, -5, 20, 10)
        stroke(0)
    
    def predraw(self):
        Body.predraw(self)
    
    def update(self):
        self.apos = atan2(self.vel.y, self.vel.x)
        if self.pos.x < 25:
            self.maxspeed = 2
            desired = Vec(self.maxspeed, self.vel.y)
            self.c = color(255, 0, 0)
            self.maxforce = 0.2
        elif self.pos.x > 375:
            self.maxspeed = 2
            desired = Vec(-self.maxspeed, self.vel.y)
            self.c = color(255, 0, 0)
            self.maxforce = 0.2
        elif self.pos.y < 25:
            self.maxspeed = 2
            desired = Vec(self.vel.x, self.maxspeed)
            self.c = color(255, 0, 0)
            self.maxforce = 0.2
        elif self.pos.y > 375:
            self.maxspeed = 2
            desired = Vec(self.vel.x, -self.maxspeed)
            self.c = color(255, 0, 0)
            self.maxforce = 0.2
        else:
            self.maxspeed = 6
            d = (target - self.pos)
            desired = min(abs(d) / 25, self.maxspeed) * ~d
            self.c = 0
            self.maxforce = 0.1
            if abs(d) < 25:
                global target
                target = Vec(random.randint(25, 375), random.randint(25, 375))
            flee = Vec(0, 0)
            for object in objects:
                ds = abs(self.pos - object.pos)
                if 0 < ds < 20:
                    flee += 20 * ~(self.pos - object.pos) / ds
            desired += ~flee
        self.acc = (desired - self.vel).limit(self.maxforce)
        Body.update(self)
        
def setup():
    size(400, 400)
        
target = Vec(100, 100)
objects = [Seeker(Vec(random.randint(25, 375), random.randint(25, 375))) for _ in range(20)]

def draw():
    background(255)
    
    noFill()
    rect(25, 25, 350, 350)
    ellipse(target.x, target.y, 20, 20)
    fill(255)
    
    for object in objects:
        object.update()
        
    for object in objects:
        object.predraw()
        object.draw()
        object.postdraw()
        