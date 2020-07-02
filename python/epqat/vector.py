import math

class Vec2:
    def __init__(self, x, y):
        self.x, self.y = x, y

    def __abs__(self):
        return (self & self) ** 0.5

    def __and__(self, other):
        return self.x * other.x + self.y * other.y

    def __add__(self, other):
        return Vec2(self.x + other.x, self.y + other.y)
    
    def __sub__(self, other):
        return self + -other
    
    def __mul__(self, other):
        return Vec2(self.x * other, self.y * other)

    def __rmul__(self, other):
        return self * other

    def __truediv__(self, other):
        return self * (1/other)

    def __neg__(self):
        return -1 * self

    def __iter__(self):
        yield self.x
        yield self.y

    def mag2(self):
        return self & self

    @staticmethod
    def from_polar(theta, r):
        return Vec2(r * math.cos(theta), r * math.sin(theta))

    @property
    def r(self):
        return abs(self)

    @r.setter
    def r(self, val):
        self.x = val * math.cos(self.theta)
        self.y = val * math.sin(self.theta)

    @property
    def theta(self):
        return math.atan2(self.y, self.x)

    @theta.setter
    def theta(self, val):
        self.x = self.r * math.cos(val)
        self.y = self.r * math.sin(val)

ORIGIN = Vec2(0, 0)
