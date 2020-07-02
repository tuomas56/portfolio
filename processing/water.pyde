def setup():
    size(400, 400)
    
GRAVITY = 0.1
DRAGCONST = 0.8
VWATER = 1
VAIR = 0.82
WATERLINE = 300

class Ball:
    def __init__(self, x, y):
        self.x, self.y = x, y
        self.vx, self.vy = 0, 0
        self.ax, self.ay = 0, 0
        self.fixed = True
        self.restitution = 0.1
        self.subst = VAIR
        
    def draw(self):
        fill(0)
        ellipse(self.x, self.y, 10, 10)
    
    def update(self):
        if self.fixed: return
        if self.x < 0:
            self.vx *= -(1 - self.restitution)
            self.x = 0
        elif self.x > width:
            self.vx *= -(1 - self.restitution)
            self.x = width
        if self.y < 0:
            self.vy *= -(1 - self.restitution)
            self.y = 0
        elif self.y > height:
            self.vy *= -(1 - self.restitution)
            self.y = height
          
        self.ay = GRAVITY
        self.ax = 0
                
        if self.y >= WATERLINE:
            self.subst = VWATER
        else:
            self.subst = VAIR
            
        self.vx *= DRAGCONST / self.subst
        self.vy *= DRAGCONST / self.subst
            
        self.vx += self.ax
        self.vy += self.ay
        self.x += self.vx
        self.y += self.vy
        
    
ball = Ball(100, 100)
mouseDown = False
    
def draw():
    noStroke()
    fill(255, 255, 255, 100)
    rect(0, 0, width, height)
    ball.update()
    ball.draw()
    if mouseDown:
        stroke(0)
        line(ball.x, ball.y, mouseX, mouseY)
    fill(100, 100, 100, 100)
    noStroke()
    rect(0, WATERLINE, width, height - WATERLINE)
    
    
def mousePressed():
    global mouseDown
    mouseDown = True
    ball.fixed = True
    
def mouseReleased():
    ball.vx = (mouseX - ball.x) / 5
    ball.vy = (mouseY - ball.y) / 5
    ball.fixed = False
    global mouseDown
    mouseDown = False