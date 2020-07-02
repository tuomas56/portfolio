from __future__ import division
add_library('minim')
t = 0
c1 = color(255, 0, 0)
c2 = color(220, 100, 0)
c3 = color(255, 180, 70)
f = lambda x: x**0.7
s = 300
n = 372
running = False
pbx = 19
pby = 19
pbr = 6
import time
import os

def setup():
    noStroke()
    fill(0)
    textAlign(LEFT, TOP)
    size(400, 400, P2D)
    smooth(16)
    pixelDensity(displayDensity())
    minim = Minim(this)
    global song
    song = minim.getLineIn(Minim.STEREO, 2048)
    global fft
    fft = FFT(song.bufferSize(), song.sampleRate())
    global running
    running = True
    background(0)
    
def fc(l):
    v = 2 * log(l) / log(100)
    if v <= 2.4:
        return lerpColor(c1, c2, v / 2)
    else:
        return lerpColor(c2, c3, v / 2)
    
def draw():
    fill(color(0, 0, 0, 10))
    rect(0, 0, width, height, 0)
    if not running: return
    global t, fft, song
    t += 0.03
    fft.forward(song.mix)
    translate(width/2, height/2)
    rotate(-PI/2)
    for i in range(n):
        rotate(TWO_PI/n)
        l = fft.getBand(i) * 2
        fill(fc(l))
        c = 10 * f(l) + s
        arc(0, 0, c, c, 0, TWO_PI/n)
    for i in range(0, n):
        rotate(TWO_PI/n)
        l = fft.getBand(i) * 2
        fill(fc(l))
        arc(0, 0, s - 5, s - 5, 0, TWO_PI/n)
        c = 10 * f(l)
        fill(color(0, 0, 0))
        arc(0, 0, s - c - 10, s - c - 10, 0, TWO_PI / n)
    rotate(PI / 2)
    translate(-width / 2, -height / 2)