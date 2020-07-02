from __future__ import division, print_function

import random

def setup():
    size(800, 400, P2D)
    smooth(16)
    pixelDensity(displayDensity())
    
    global balls
    n = 20
    balls = [Ball() for _ in range(n)]
    
    global graph
    graph = OcclusionGraph()
    
    global layer_colors
    layer_colors = []
    colorMode(HSB)
    for i in range(2*n):
        layer_colors.append(color(i*255/(n - 1), 255, 255)) 
    colorMode(RGB)
    
    global pause, lv
    pause = False
    lv = None
    
    textSize(15)
    background(0)
    
class OcclusionGraph:
    def __init__(self):
        self.nodes = []
        self.edges = {0}
        self.layers = []
        
    def clear(self):
        self.nodes = []
        self.edges = set()
        self.layers = []
        
    def add(self, x, y, w, h):
        for i, (bx, by, bw, bh) in enumerate(self.nodes):
            if ((bx < x < bx + bw) or (bx < x + w < bx + w)) and ((by < y < by + bh) or (by < y + h < by + h)):
                self.edges.add((len(self.nodes), i))
        self.nodes.append((x, y, w, h))
        
    def solve(self):
        self.layers = [-1 for _ in range(len(self.nodes))]
        edges = self.edges.copy()
        nodes = list(range(len(self.nodes)))
        
        while len(nodes):
            ledges = edges.copy()
            lnodes = list(iter(nodes))
            for i in lnodes:
                self.layers[i] += 1
                if not any(a == i for a, b in ledges):
                    for (a, b) in edges:
                        if b == i:
                            edges.remove((a, b))
                    nodes.remove(i)
        
    def draw(self):
        nspace = height/(len(self.nodes) + 1)    
        for (i, j) in self.edges:
            iy = (i + 1)*nspace
            jy = (j + 1)*nspace
            my = (iy + jy)/2
            dy = abs(iy - jy)/2
            stroke(0)
            noFill()
            if abs(i - j) % 2 == 1:
                arc(3*width/4, my, dy, 2*dy, -HALF_PI, HALF_PI)
                tx = 3*width/4 + dy/2
            else:
                arc(3*width/4, my, dy, 2*dy, HALF_PI, 3*HALF_PI)
                tx = 3*width/4 - dy/2
            ty = my
            if j > i:
                line(tx, ty, tx - 5, ty - 5)
                line(tx, ty, tx + 5, ty - 5)
            else:
                line(tx, ty, tx - 5, ty + 5)
                line(tx, ty, tx + 5, ty + 5)
                
        for i in range(len(self.nodes)):
            stroke(0)
            fill(layer_colors[self.layers[i]])
            ellipse(3*width/4, (i + 1)*nspace, 20, 20)
            fill(0)
            s = str(self.layers[i])
            text(s, 3*width/4 - textWidth(s)/2, (i + 1)*nspace - 10, textWidth(s)*2, 20)
            
    
class Ball:
    def __init__(self):
        self.x = 200
        self.y = 200
        self.vx = 2*(1 - 2*random.random())
        self.vy = 2*(1 - 2*random.random())
        self.r = 20
        
    def draw(self, l=None):
        stroke(0)
        if l is not None:
            fill(layer_colors[l])
        else:
            fill(255, 0, 0)
        ellipse(self.x, self.y, 2*self.r, 2*self.r)
        
    def update(self):
        if self.x < self.r:
            self.x = self.r
            self.vx *= -1
            
        if self.x > width/2 - self.r:
            self.x = width/2 - self.r
            self.vx *= -1
        
        if self.y < self.r:
            self.y = self.r
            self.vy *= -1
            
        if self.y > height - self.r:
            self.y = height - self.r
            self.vy *= -1
            
        self.y += self.vy
        self.x += self.vx
        self.vy += 0.1
    
def draw():
    background(255)
    stroke(0)
    line(width/2, 0, width/2, height)
    
    if not pause:
        graph.clear()
        for ball in balls:
            ball.update()
            graph.add(ball.x - ball.r, ball.y - ball.r, 2*ball.r, 2*ball.r)
        graph.solve()
        graph.draw()
        for ball in balls:
            ball.draw()
    else:
        graph.draw()
        if lv is None:
            for i, ball in enumerate(balls):
                ball.draw(graph.layers[i])
        else:
            for i, ball in enumerate(balls):
                if graph.layers[i] <= lv:
                    ball.draw(graph.layers[i])
        
            
def mousePressed():
    balls.append(Ball())
    
def keyPressed():
    global lv
    if key == 'e':
        if lv is None:
            lv = 0
        else:
            lv = None
    elif key == 'w':
        lv += 1
        if lv > max(graph.layers):
            lv = 0
    elif key == 's':
        lv -= 1
        if lv < 0:
            lv = max(graph.layers)
    elif key == 'p':
        global pause
        pause = ~pause