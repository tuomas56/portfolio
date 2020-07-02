from __future__ import division, print_function

WIDTH = 60
HEIGHT = 60

class Cell:
    def __init__(self, loc, inert, flow, pres):
        self.loc, self.pres = loc, pres
        self.inert, self.flow = inert, flow
        self.new_pres = pres
        
    def nbs(self):
        if 0 < self.loc[0]:
            yield (self.loc[0] - 1, self.loc[1])
        if self.loc[0] < WIDTH - 1:
            yield (self.loc[0] + 1, self.loc[1])
        if 0 < self.loc[1]:
            yield (self.loc[0], self.loc[1] - 1)
        if self.loc[1] < HEIGHT - 1:
            yield (self.loc[0], self.loc[1] + 1)
    
    def update(self, cells):
        if self.inert:
            return
        for nb in self.nbs():
            nb = cells[nb[1] * WIDTH + nb[0]]
            if nb.inert:
                continue
            dpress = self.pres - nb.pres
            flow = self.flow * dpress;
            self.new_pres -= flow
            nb.new_pres += flow
    
    def confirm(self):
        self.pres = self.new_pres

cells = [Cell((x, y), False, 0.1, 0.0) for y in range(HEIGHT) for x in range(WIDTH)]
i = 0

def setup():
    size(400, 400, P2D)
    smooth(16)
    pixelDensity(displayDensity())
    
def draw():
    background(255)
    global cells
    global i
    
    its = 10
    for cell in cells[i%its::its]:
        cell.update(cells)
    
    if i % its == 0:
        for cell in cells:
            cell.confirm()
        
    i += 1
    cw = 400/WIDTH
    ch = 400/HEIGHT
    for cell in cells:
        if cell.inert:
            fill(0, 0, 255)
            noStroke()
            rect(cell.loc[0] * cw, cell.loc[1] * ch, cw, ch)
        else:
            noStroke()
            fill(0, (atan(cell.pres) + 1.0) * 127.5, 0)
            rect(cell.loc[0] * cw, cell.loc[1] * ch, cw, ch)
    
    if mousePressed:
        loc = (int(WIDTH * mouseX / 400), int(HEIGHT * mouseY / 400))
        if mouseButton == LEFT:
            cells[loc[1] * WIDTH + loc[0]].pres = 100.0
        elif mouseButton == RIGHT:
            cells[loc[1] * WIDTH + loc[0]].inert = True
    