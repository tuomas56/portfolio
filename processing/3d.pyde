from __future__ import division

def setup():
    size(800, 600, P2D)
    pixelDensity(displayDensity())
    smooth(16)
    frameRate(60)
    global minimap, glw
    minimap = createGraphics(100, 100)
    glw = this.surface.getNative()
    glw.confinePointer(True)
    glw.setPointerVisible(False)
    
player_x = 0.0
player_y = 0.0
player_a = 0.0
player_vf = 0.0
player_vs = 0.0
player_va = 0.0
walls = [(color(255, 0, 0), -50, 100, 150, 150), 
         (color(0, 255, 0), 150, 150, 150, -50), 
         (color(0, 0, 255), 150, -50, 200, -200),
         (color(255, 0, 255), 200, -200, -100, -200),
         (color(255, 255, 0), -100, -200, -150, 0),
         (color(0, 255, 255), -150, 0, -50, 100),
         (color(255, 100, 0), 20, 20, 20, -20),
         (color(255, 0, 100), 20, -20, -20, -20),
         (color(100, 255, 0), -20, -20, -20, 20),
         (color(100, 0, 255), -20, 20, 20, 20)]
depth = [float('inf')] * 400
ticks = 0
    
def do_clip(rx1, ry1, rx2, ry2):
    p1 = None
    p2 = None
    k1 = rx1 * (rx2 - ry2) + rx2 * (ry1 - rx1)
    d1 = (ry1 - ry2) - (rx1 - rx2)
    if d1 == 0: d1 = 0.01
    k1 /= d1
    k2 = rx2 * (ry1 + rx1) - rx1 * (rx2 + ry2)
    d2 = (ry1 - ry2) + (rx1 - rx2)
    if d2 == 0: d2 = 0.01
    k2 /= d2
    if ry1 >= rx1 >= -ry1 and ry2 >= rx2 >= -ry2:
        p1 = (rx1, ry1)
        p2 = (rx2, ry2)
    elif ry1 >= rx1 >= -ry1:
        p1 = (rx1, ry1)
        if rx2 > 0:
            if k1 > 0:
                p2 = (k1, k1)
            else:
                p2 = (k2, -k2)
        else:
            if k2 < 0:
                p2 = (k2, -k2)
            else:
                p2 = (k1, k1)
    elif ry2 >= rx2 >= -ry2:
        p2 = (rx2, ry2)
        if rx1 > 0:
            if k1 > 0:
                p1 = (k1, k1)
            else:
                p1 = (k2, -k2)
        else:
            if k2 < 0:
                p1 = (k2, -k2)
            else:
                p1 = (k1, k1)
    else:
        l1 = (rx1 > ry1 and ry2 >= rx2) or (ry1 >= rx1 and rx2 > ry2)
        l2 = (rx1 >= -ry1 and -ry2 > rx2) or (-ry1 > rx1 and rx2 >= -ry2)
        if l1 and l2:
            if rx1 > 0:
                p1 = (k1, k1)
            else:
                p1 = (k2, -k2)
            if rx2 > 0:
                p2 = (k1, k1)
            else:
                p2 = (k2, -k2)
        else:
            return None
    if p1[1] < 0 or p2[1] < 0:
        return None    
    return (p1, p2)
    
def grad_quad(x1, x2, y1, y2, c, s, e, d1, d2):
    global depth
    if x1 == x2: return
    r, g, b = red(c), green(c), blue(c)
    if x2 < x1:
        x2, x1 = x1, x2
        y2, y1 = y1, y2
        s, e = e, s
        d1, d2 = d2, d1
    px = x1
    pw = y1
    for x in range(int(x1), int(x2) + 1):
        f = (x - x1) / (x2 - x1)
        w = lerp(y1, y2, f)
        cf = lerp(s, e, f)
        d = lerp(d1, d2, f)
        if d <= depth[x]:
            stroke(cf * r, cf * g, cf * b)
            line(x, w / 2, x, -w / 2)
            line(px, pw / 2, x, w / 2)
            line(px, -pw / 2, x, -w / 2)
            depth[x] = d
        px = x
        pw = w
    
def clamp(f, mi, ma):
    return max(min(f, ma), mi)
    
def draw():
    global player_x, player_y, player_a, depth, minimap, ticks
    pushMatrix()
    translate(400, 300 + 3 * sin(ticks / 5))
    rotate(cos(ticks/5) / 300)
    scale(2.0, -2.0)
    minimap.beginDraw()
    minimap.translate(50, 50)
    minimap.scale(0.25, -0.25)
    minimap.strokeWeight(4)

    background(50)
    minimap.background(100)
    depth = [float('inf')] * 800
    
    for i in range(0, 200):
        noFill()
        stroke(i / 2, i / 2, i / 2)
        line(-200, i, 200, i)
        line(-200, -i, 200, -i)
    
    minimap.stroke(0)
    minimap.line(0, 0, 0, 10)
    minimap.fill(0)
    minimap.ellipse(0, 0, 5, 5)
    minimap.fill(color(100, 100, 100, 50))
    minimap.triangle(0, 0, -200, 200, 200, 200)
    for (c, x1, y1, x2, y2) in walls:
        a = player_a
        tx1, ty1 = x1 - player_x, y1 - player_y
        tx2, ty2 = x2 - player_x, y2 - player_y
        rx1 = cos(a) * tx1 - sin(a) * ty1
        ry1 = sin(a) * tx1 + cos(a) * ty1
        rx2 = cos(a) * tx2 - sin(a) * ty2
        ry2 = sin(a) * tx2 + cos(a) * ty2
        if ry1 == 0.0: ry1 = 0.1
        if ry2 == 0.0: ry2 = 0.1
        minimap.stroke(c)
        minimap.fill(c)
        minimap.line(rx1, ry1, rx2, ry2)
        ps = do_clip(rx1, ry1, rx2, ry2)
        if ps is not None:
            ((p1x, p1y), (p2x, p2y)) = ps
            d1 = sqrt(p1x**2 + p1y**2)
            d2 = sqrt(p2x**2 + p2y**2)
            d1 /= 400
            d2 /= 400
            d1 = clamp(d1, 0, 0.8)
            d2 = clamp(d2, 0, 0.8)
            d1 = 1 - d1
            d2 = 1 - d2
            minimap.fill(lerpColor(color(0, 0, 0), c, d1))
            minimap.ellipse(p1x, p1y, 10, 10)
            minimap.fill(lerpColor(color(0, 0, 0), c, d2))
            minimap.ellipse(p2x, p2y, 10, 10)
            if p1y == 0: p1y = 0.01
            if p2y == 0: p2y = 0.01
            p1x /= p1y / 200
            p2x /= p2y / 200
            grad_quad(p1x, p2x, 100 * 200 / p1y, 100 * 200 / p2y, c, d1, d2, p1y, p2y)
    
    player_x += player_vf * sin(player_a)
    player_y += player_vf * cos(player_a)
    player_x += player_vs * sin(PI / 2 + player_a)
    player_y += player_vs * cos(PI / 2 + player_a)
    ticks += 1 if (player_vf != 0) or (player_vs != 0) else 0
    
    minimap.endDraw()
    popMatrix()
    image(minimap, 700, 0)
    
    this.surface.setTitle("FPS: %d" % frameRate)
    
def keyPressed():
    global player_vf, player_vs
    if key == 'a':
        player_vs = -2.0
    elif key == 'd':
        player_vs = 2.0
    elif key == 'w':
        player_vf = 2.0
    elif key == 's':
        player_vf = -2.0
        
def keyReleased():
    global player_vs, player_vf
    print(key)
    if str(key) in 'ad':
        player_vs = 0.0
    elif str(key) in 'ws':
        player_vf = 0.0
        
def mouseMoved():
    global player_a, glw
    player_a += (mouseX - 200) / 800
    print(mouseX)
    glw.warpPointer(400, 300)