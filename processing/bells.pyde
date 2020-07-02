h = 600
w = 400

px = w // 2
py = 0

vy = 0

class Board:
    def __init__(self, x, y):
        self.x = x
        self.y = y
        
    def draw(self):
        fill(255, 0, 0)
        noStroke()
        rect(self.x - 10, self.y + py - 5, 20, 10)
        
    def update(self):
        global vy
        if self.x - 20 < px < self.x + 20 and \
           self.y + py - 10 < h - 205 < self.y + py + 10:
            self.x = random(w)
            self.y = -py
            vy = 6
        if self.y + py - 5 > h:
            self.x = random(w)
            self.y = -py

boards = [Board(random(w), random(h)) for _ in range(10)]

def setup():
    size(w, h)
    smooth(16)
    font = loadFont("SansSerif-16.vlw")
    textFont(font)
    pixelDensity(displayDensity())
    
lost = False
    
def draw():
    global lost
    if lost:
        return
    
    global px, py, vy
    
    background(255)
    noStroke()
    fill(0)
    
    text(py, 20, 20)
    
    rect(px - 5, h - 205, 10, 10)
    
    px += 0.1 * (mouseX - px)
    py += vy
    
    if py != 0:
        vy -= 0.1
    
    allo = True
    for board in boards:
        if board.y + py + 100 - 5 > 0:
            allo = False
        board.draw()
        board.update()
            
    if allo:
        lost = True
        
        
def mousePressed():
    global vy, py
    if py == 0 and focused:
        vy = 4