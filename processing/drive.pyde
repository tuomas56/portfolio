from __future__ import division
import random

def setup():
    size(600, 600, P3D)
    smooth(16)
    pixelDensity(displayDensity())
    generate_grid()
    
tile_size = 50
camera_x = 0
camera_y = 0
car_x = 0
car_y = 0
car_a_rho = 0
car_phi = 0
car_v_phi = 0
car_vx = 0
car_vy = 0
car_width = 0.5*tile_size
car_height = 0.6*car_width
zoom = 1.0
selset = set()

def generate_grid():
    global grid, hmap
    gwidth = 64
    gheight = 64
    grid = [[0]*gwidth for _ in range(gheight)]
    hmap = [[0]*gwidth for _ in range(gheight)]
    rhmap = [[0]*(gwidth >> 3) for _ in range(gheight >> 3)]
    for x in range(gwidth >> 3):
        for y in range(gheight >> 3):
            r = random.randint(0, 4)
            for i in range(1 << 3):
                for j in range(1 << 3):
                    hmap[(x << 3) + i][(y << 3) + j] = r
    def get_hmap(x, y):
        x = max(0, min(x, gwidth - 1))
        y = max(0, min(y, gheight - 1))
        return hmap[x][y]
    kernel = [[0.015026, 0.028569, 0.035391, 0.028569, 0.015026], 
              [0.028569, 0.054318, 0.067288, 0.054318, 0.028569], 
              [0.035391, 0.067288, 0.083355, 0.067288, 0.035391], 
              [0.028569, 0.054318, 0.067288, 0.054318, 0.028569], 
              [0.015026, 0.028569, 0.035391, 0.028569, 0.015026]]
    nhmap = [[0]*gwidth for _ in range(gheight)]
    for x in range(gwidth):
        for y in range(gheight):
            if hmap[x][y] <= 0: continue
            for i in [-2, -1, 0, 1, 2]:
                for j in [-2, -1, 0, 1, 2]:
                    nhmap[x][y] += kernel[i + 2][j + 2] * get_hmap(x + i, y + j)
            nhmap[x][y] = int(nhmap[x][y])
    hmap = nhmap
    for x in range(gwidth):
        for y in range(gheight):
            if hmap[x][y] <= 0:
                grid[x][y] = -2
                hmap[x][y] = 0
    centers = []
    for _ in range(10):
        x = random.randint(0, gwidth - 1)
        y = random.randint(0, gheight - 1)
        if grid[x][y] != -2:
            grid[x][y] = random.randint(2, 3)
            print(x, y)
            centers.append((x, y))
    print(centers)
    for _ in range(4):
        for x in range(gwidth):
            for y in range(gheight):
                def next_to(t0, t1):
                    for (i, j) in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
                        xt = max(0, min(x + i, gwidth - 1))
                        yt = max(0, min(y + j, gheight - 1))
                        if t0 <= grid[xt][yt] <= t1: return True
                    return False
                if grid[x][y] != 0:
                    continue
                if next_to(-2, -2):
                    grid[x][y] = 0
                elif next_to(1, 3):
                    if random.random() < 0.7:
                        grid[x][y] = random.randint(1, 3)
    def draw_a_road(x1, x2, y1, y2):
        global grid
        for x in range(x1, x2 + 1):
            grid[x][y1] = -1
            grid[x][y1 + 1] = -1
            grid[x][y1 - 1] = -1
        for y in range(y1, y2 + 1):
            grid[x2][y] = -1
            grid[x2 + 1][y] = -1
            grid[x2 - 1][y] = -1
    for ca in centers:
        for cb in centers:
            if ca != cb:
                draw_a_road(ca[0], cb[0], ca[1], cb[1])
    
def draw():
    global car_x, car_y, car_phi, car_vx, car_vy, mgx, mgy, selset
    car_phi += car_v_phi
    car_vx += car_a_rho * cos(car_phi) - 0.03*car_vx
    car_vy += car_a_rho * sin(car_phi) - 0.03*car_vy
    car_x += car_vx
    car_y += car_vy
    cfx = cos(car_phi)
    cfy = sin(car_phi)
    dtv = cfx * car_vx + cfy * car_vy
    fcmpx = dtv * cos(car_phi)
    fcmpy = dtv * sin(car_phi)
    xcmpx = car_vx - fcmpx
    xcmpy = car_vy - fcmpy
    car_vx = fcmpx + 0.2 * xcmpx
    car_vy = fcmpy + 0.2 * xcmpy
    camera_x = car_x
    camera_y = car_y
    mgx = None
    mgy = None
    background(0)
    directionalLight(255, 255, 255, 0, 0, -1)
    ambientLight(150, 150, 150)
    camera(camera_x, camera_y, zoom * (height/2.0) / tan(PI*45.0 / 180.0), camera_x, camera_y, 0, 0, 1, 0)
    for x in range(len(grid[0])):
        for y in range(len(grid)):
            if grid[x][y] == 0:
                fill(0, 255, 0)
                noStroke()
                pushMatrix()
                translate((x + 0.5)*tile_size, (y + 0.5)*tile_size, 0.5 * hmap[x][y] * tile_size);
                box(tile_size, tile_size, hmap[x][y] * tile_size)
                popMatrix()
            elif grid[x][y] == -1:
                fill(0, 255, 0)
                noStroke()
                pushMatrix()
                translate((x + 0.5)*tile_size, (y + 0.5)*tile_size, 0.5 * hmap[x][y] * tile_size);
                box(tile_size, tile_size, hmap[x][y] * tile_size)
                popMatrix()
                fill(50, 50, 50)
                noStroke()
                def get_grid(x, y):
                    x = max(0, min(x, len(grid[0]) - 1))
                    y = max(0, min(y, len(grid) - 1))
                    return (grid[x][y], hmap[x][y])
                def drawg(lu, ld, ru, rd):
                    beginShape(QUADS)
                    fill(50, 50, 50)
                    vertex(x*tile_size, y*tile_size, lu * tile_size)
                    vertex((x + 1)*tile_size, y*tile_size, ru * tile_size)
                    vertex((x + 1)*tile_size, (y + 1)*tile_size, rd * tile_size)
                    vertex(x*tile_size, (y + 1)*tile_size, ld * tile_size)
                    endShape()
                    beginShape(QUADS)
                    fill(0, 255, 0)
                    vertex(x*tile_size, y*tile_size, g[1]*tile_size)
                    vertex(x*tile_size, (y + 1)*tile_size, g[1]*tile_size)
                    vertex(x*tile_size, y*tile_size, lu*tile_size)
                    vertex(x*tile_size, (y + 1)*tile_size, ld*tile_size)
                    vertex((x + 1)*tile_size, y*tile_size, g[1]*tile_size)
                    vertex((x + 1)*tile_size, (y + 1)*tile_size, g[1]*tile_size)
                    vertex((x + 1)*tile_size, y*tile_size, ru*tile_size)
                    vertex((x + 1)*tile_size, (y + 1)*tile_size, rd*tile_size)
                    vertex(x*tile_size, y*tile_size, g[1]*tile_size)
                    vertex((x + 1)*tile_size, y*tile_size, g[1]*tile_size)
                    vertex(x*tile_size, y*tile_size, lu*tile_size)
                    vertex((x + 1)*tile_size, y*tile_size, ru*tile_size)
                    vertex(x*tile_size, (y + 1)*tile_size, g[1]*tile_size)
                    vertex((x + 1)*tile_size, (y + 1)*tile_size, g[1]*tile_size)
                    vertex(x*tile_size, (y + 1)*tile_size, ld*tile_size)
                    vertex((x + 1)*tile_size, (y + 1)*tile_size, rd*tile_size)
                    endShape()
                g = get_grid(x, y)
                gl = get_grid(x - 1, y)
                gr = get_grid(x + 1, y)
                gu = get_grid(x, y - 1)
                gd = get_grid(x, y + 1)
                noStroke()
                if gl[1] > g[1] and gl[0] == -1:
                    drawg(gl[1], gl[1], g[1], g[1])
                elif gr[1] > g[1] and gr[0] == -1:
                    drawg(g[1], g[1], gr[1], gr[1])
                elif gu[1] > g[1] and gu[0] == -1:
                    drawg(gu[1], g[1], gu[1], g[1])
                elif gd[1] > g[1] and gd[0] == -1:
                    drawg(g[1], gd[1], g[1], gd[1])
                else:
                    drawg(g[1], g[1], g[1], g[1])
            elif grid[x][y] == -2:
                fill(0, 0, 255)
                noStroke()
                pushMatrix()
                translate((x + 0.5)*tile_size, (y + 0.5)*tile_size, 0.5 * hmap[x][y] * tile_size);
                box(tile_size, tile_size, hmap[x][y] * tile_size)
                popMatrix()
            else:
                fill(0, 255, 0)
                noStroke()
                pushMatrix()
                translate((x + 0.5)*tile_size, (y + 0.5)*tile_size, 0.5 * hmap[x][y] * tile_size);
                box(tile_size, tile_size, hmap[x][y] * tile_size)
                popMatrix()
                fill(160, 82, 45)
                stroke(139, 69, 19)
                pushMatrix()
                translate((x + 0.5)*tile_size, (y + 0.5)*tile_size, (hmap[x][y] + 0.5*grid[x][y])*tile_size)
                box(tile_size, tile_size, tile_size * grid[x][y])
                popMatrix()
                pushMatrix()
                fill(100, 100, 100)
                stroke(0, 0, 0)
                translate(x*tile_size, y*tile_size, (hmap[x][y] + grid[x][y]) * tile_size)
                rect(0, 0, tile_size, tile_size)
                popMatrix()
                fill(0, 255, 0)
                noStroke()
            mx = screenX(x*tile_size, y*tile_size, (hmap[x][y] + max(grid[x][y], 0)) * tile_size)
            my = screenY(x*tile_size, y*tile_size, (hmap[x][y] + max(grid[x][y], 0)) * tile_size)
            if (mx < mouseX < mx + tile_size) and (my < mouseY < my + tile_size):
                mgx = x
                mgy = y
    fill(255, 0, 0)
    stroke(0, 0, 0)
    pushMatrix()
    translate(car_x, car_y, hmap[int(car_x / len(grid[0]))][int(car_y / len(grid))] * tile_size)
    rotate(car_phi)
    box(car_width, car_height, 10.0)
    popMatrix()
    if mgx is not None:
        noFill()
        stroke(255, 0, 255)
        for (x, y) in selset:
            pushMatrix()
            translate(0, 0, (hmap[x][y] + max(grid[x][y], 0)) * tile_size)
            rect(tile_size*x, tile_size*y, tile_size, tile_size)
            popMatrix()
        noFill()
        stroke(0, 0, 255)
        pushMatrix()
        translate(0, 0, (hmap[mgx][mgy] + max(grid[mgx][mgy], 0)) * tile_size)
        rect(tile_size*mgx, tile_size*mgy, tile_size, tile_size)
        popMatrix()
        if mousePressed and mouseButton == LEFT:
            selset.add((mgx, mgy))
            
def keyPressed():
    global car_v_phi, car_a_rho, zoom
    if key == 'w':
        car_a_rho = 0.15
    elif key == 'a':
        car_v_phi = -0.04
    elif key == 'd':
        car_v_phi = 0.04
    elif key == 'u':
        zoom *= 1.5
    elif key == 'o':
        zoom /= 1.5
    elif str(key).lower() == 'j':
        for (x, y) in selset:
            if key == 'j':
                grid[x][y] += 1
            else:
                hmap[x][y] += 1
    elif str(key).lower() == 'l':
        for (x, y) in selset:
            if key == 'l':
                grid[x][y] -= 1
            else:
                hmap[x][y] -= 1
        
def keyReleased():
    global car_v_phi, car_a_rho
    if key == 'w':
        car_a_rho = 0.0
    elif key == 'a':
        car_v_phi = 0.0
    elif key == 'd':
        car_v_phi = 0.0
        
def mousePressed():
    global selset
    if mouseButton == RIGHT:
        selset = set()