add_library('minim')
from time import time
import random

prevVal3 = 0
prevVal2 = 0
prevVal1 = 0
prevTime = time()
thresh = 40
minVal = 50
particles = []
fireworks = []
gravity = 0.08
tick = 0

def setup():
    background(0, 0, 0)
    size(500, 500)
    noCursor()
    noStroke()
    minim = Minim(this)
    global song
    song = minim.loadFile("...")
    global fft
    fft = FFT(song.bufferSize(), song.sampleRate())
    fft.logAverages(50, 1)
    song.play()
   
def spawnExplosion(x, y, c1):
    c2 = color(random.randrange(100, 255), random.randrange(100, 255), random.randrange(100, 255))
    for i in range(int(random.randrange(10, 50))):
        particles.append({
            'x': x,
            'y': y,
            'vx': random.randrange(-5, 5),
            'vy': random.randrange(-5, 5),
            'color': lerpColor(c1, c2, random.random()),
            'size': random.randrange(5, 10)
        })

def spawnFirework(x, vx, c):
    fireworks.append({
       'x': x,
       'y': height,
       'vy': -random.randrange(2, 4),
       'vx': vx,
       'color': c
    })
                  
def draw():
    explode = False
    noStroke()
    fill(color(0, 0, 0, 20))
    rect(0, 0, width, height)
    
    global fft, song, prevVal3, prevVal2, prevVal1, prevTime
    fft.forward(song.mix)
    
    b3 = fft.getBand(3)
    b2 = fft.getBand(2)
    b1 = fft.getBand(1)
    t = time()
    if (prevVal3 - b3 >= thresh \
        and t - prevTime >= 0.25 \
        and prevVal2 - b3 >= thresh \
        and prevVal3 > minVal \
        and prevVal2 > minVal) \
        or (prevVal1 > minVal \
        and prevVal1 - b1 >= thresh):
        explode = True
        prevTime = t
    prevVal3 = b3
    prevVal2 = b2
    
    global tick
    tick += 1
    if tick % 50 == 0:
        c = color(random.randrange(100, 255), random.randrange(100, 255), random.randrange(100, 255))
        d = randomGaussian() / 4
        count = random.randrange(1, 5)
        for _ in range(count):
            spawnFirework(width / 2, d, c)
    
    def doVel(firework):
        firework['y'] += firework['vy']
        firework['x'] += firework['vx']
        return firework
        
    def checkTy(firework):
        if(explode and firework['y'] <= height / 2):
            spawnExplosion(firework['x'], firework['y'], firework['color'])
            return False;
        elif firework['y'] <= 0:
            return False
        return True;
        
    def drawFirework(firework):
        noStroke()
        fill(firework['color'])
        ellipse(firework['x'], firework['y'], 5, 10)
        return firework
        
    def doPVel(particle):
        particle['x'] += particle['vx'];
        particle['y'] += particle['vy'];
        particle['vy'] += gravity;
        if tick % 8 == 0:
            particle['size'] -= 1
        return particle

    def checkBounds(particle):
        inX = particle['x'] >= 0 and particle['x'] <= width
        inY = particle['y'] >= 0 and particle['y'] <= height
        return inX and inY and particle['size'] > 0

    def drawParticle(particle):
        c = particle['color']
        strokeWeight(15)
        stroke(red(c), green(c), blue(c), 10)
        fill(c)
        ellipse(particle['x'], particle['y'], particle['size'], particle['size'])
        return particle

    global fireworks
    global particles

    fireworks = [drawFirework(doVel(firework)) for firework in fireworks if checkTy(firework)]
    
    particles = [drawParticle(doPVel(particle)) for particle in particles if checkBounds(particle)]

def stop():
    song.close()