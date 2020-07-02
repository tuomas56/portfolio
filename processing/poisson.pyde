from __future__ import division

import random
import collections

R = 20
active = []
inactive = []

def setup():
    size(400, 400, P2D)
    smooth(16)
    pixelDensity(displayDensity())
    global active
    r = random.random() * 200 * 200
    theta = random.random() * 2 * PI
    x = 200 + sqrt(r) * sin(theta)
    y = 200 + sqrt(r) * cos(theta)
    active.append((x, y))
    
def draw():
    global active, inactive, R
    background(255)
    
    noFill()
    stroke(0, 0, 0)
    ellipse(200, 200, 400, 400)
    
    for (x, y) in active:
        fill(255, 0, 0)
        noStroke()
        ellipse(x, y, 5, 5)
    
    for (x, y) in inactive:
        fill(0, 0, 0)
        noStroke()
        ellipse(x, y, 5, 5)
        
    if len(active):
        x, y = active[-1]
        
        for _ in range(20):
            r = random.random() * 4 * R * R + R * R
            theta = random.random() * 2 * PI
            px = x + sqrt(r) * sin(theta)
            py = y + sqrt(r) * cos(theta)
            
            if (px - 200)**2 + (py - 200)**2 < 200*200:
                for (ox, oy) in active + inactive:
                    if (ox - px)**2 + (oy - py)**2 < R*R:
                        break
                else:
                    active.insert(-2, (px, py))
                    break
        else:
            inactive.append(active.pop())