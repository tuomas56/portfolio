import pygame
from vector import Vec2
import subprocess
import threading
import random
import pyaudio
import wave

RIGHT, LEFT = 0, 1
JOYMAPS = { 'Wireless Controller': { 'jump': 1, 'slide': 2 , 'melee': 11, 'shoot': 7, 'mov': 0, 'nmov': 1}}

class Timer:
    def __init__(self, dt):
        self.dt = dt
        self.time = pygame.time.get_ticks()

    def tick(self):
        ntime = pygame.time.get_ticks()
        if ntime - self.time < self.dt:
            return False
        else:
            self.time = ntime
            return True

class WavePlayer:
    def __init__(self, fname):
        self.fname = fname

    def play(self):
        subprocess.Popen(['sox', self.fname, '-d'])

class Anim:
    def __init__(self, f, minf, maxf, scl, loop, fps, oncomplete=lambda:0, ids=[0]):
        ids[0] += 1
        self.id = ids[0]
        self.f = f
        self.minf = minf
        self.maxf = maxf
        self.df = self.maxf - self.minf
        self.scl = scl
        self.loop = loop
        self.dt = 1000 / fps
        self.images = []
        self.frame = 0
        self.time = pygame.time.get_ticks()
        self.dir = RIGHT
        self.oncomplete = oncomplete
        self.loaded = False

    def load(self):
        if self.loaded: return self
        self.loaded = True
        for i in range(self.minf, self.maxf + 1):
            self.images.append(pygame.image.load(self.f(i)).convert_alpha())
            self.images[-1] = pygame.transform.smoothscale(self.images[-1], self.scl)
        return self

    def face(self, dir):
        if dir != self.dir:
            for i in range(len(self.images)):
                self.images[i] = pygame.transform.flip(self.images[i], True, False)
            self.dir = dir

    def reset(self):
        self.frame = 0

    def next(self):
        ntime = pygame.time.get_ticks()
        if ntime - self.time < self.dt:
            return self.images[self.frame]
        self.time = ntime
        self.frame += 1
        if self.loop and self.frame > self.df:
            self.frame = 0
        elif self.frame > self.df:
            self.frame = self.df
            self.oncomplete()
        return self.images[self.frame]

    def copy(self):
        a = Anim(self.f, self.minf, self.maxf, self.scl, self.loop, 1000 / self.dt, self.oncomplete)
        a.images = self.images.copy()
        return a

    def __eq__(self, other):
        return self.id == other.id

class Entity:
    maxhealth = None

    def __init__(self, world, pos, vel=Vec2(0, 0)):
        self.pos = pos
        self.vel = vel
        self.acc = Vec2(0, 0)
        self.gravity = Vec2(0, 0.01)
        self.image = None
        self.paused = False
        self.world = world
        self.health = 0

    def load(self):
        pass

    def drawhealthbar(self, surface):
        ap = self.pos + self.world.bgx + Vec2(10, 0)
        surface.fill((0, 0, 0), (ap.x - 1, ap.y - 10, 66, 5))
        hp = self.health / self.maxhealth
        if hp > 0.66:
            hc = (0, 255, 0)
        elif hp > 0.33:
            hc = (255, 165, 0)
        else:
            hc = (255, 0, 0)
        surface.fill(hc, (ap.x, ap.y - 9, hp * 64, 3))

    def update(self, dt):
        if not self.paused:
            self.acc += self.gravity
            self.vel += self.acc
            self.pos += dt * self.vel
            self.acc = Vec2(0, 0)

    def draw(self, surface):
        surface.blit(self.image, tuple(self.pos + self.world.bgx))

    def handle_event(self, ev):
        pass

class Bullet(Entity):
    def __init__(self, world, pos, d, index):
        Entity.__init__(self, world, pos, Vec2(d * 0.6, 0))
        self.gravity = Vec2(0, 0)
        self.anim = Anim("png/Objects/Bullet_00%s.png".__mod__, 0, 4, (15, 15), True, 10)
        self.d = d
        self.index = index
        self.damage = 5
        self.shown = True
    
    def load(self):
        self.anim.load()
        if self.d < 0:
            self.anim.face(LEFT)
        else:
            self.anim.face(RIGHT)

    def die(self):
        self.shown = False

    def update(self, dt):
        if not self.shown: return
        ap = self.pos + self.world.bgx 
        if not (-10 < ap.x < self.world.width + 10):
            self.die()
        self.image = self.anim.next()
        Entity.update(self, dt)
        for zombie in self.world.zombies:
            if zombie.paused: continue
            if (zombie.pos.x < 0 and zombie.pos.x < self.pos.x < zombie.pos.x + 100 \
               or zombie.pos.x > 0 and zombie.pos.x + 100 > self.pos.x > zombie.pos.x) \
               and zombie.pos.y + 100 > self.pos.y > zombie.pos.y:
                zombie.hit(self.damage)
                self.die()

    def draw(self, surface):
        if not self.shown: return
        Entity.draw(self, surface)

class HealthPickup(Entity):
    def __init__(self, world, pos):
        Entity.__init__(self, world, pos)
        self.anim = Anim("png/Objects/Health (%s).png".__mod__, 1, 8, (23, 23), True, 10)
        self.shown = True
        self.pickupsound = WavePlayer("sounds/pickup.wav")

    def load(self):
        self.anim.load()

    def draw(self, surface):
        if not self.shown: return
        Entity.draw(self, surface)

    def update(self, dt):
        if not self.shown: return
        self.image = self.anim.next()
        self.vel.x = self.vel.y = 0
        if not self.world.player.paused:
            if (self.world.player.pos.x < 0 and self.world.player.pos.x < self.pos.x < self.world.player.pos.x + 100 \
               or self.world.player.pos.x > 0 and self.world.player.pos.x + 100 > self.pos.x > self.world.player.pos.x) \
               and self.world.player.pos.y + 100 > self.pos.y > self.world.player.pos.y:
                self.world.player.heal(10)
                self.shown = False
                if self.world.sfx: self.pickupsound.play()

class Zombie(Entity):
    size = (100, 100)
    walkmanim_ = Anim("png/male/Walk (%s).png".__mod__, 1, 10, size, True, 15)
    attackmanim_ = Anim("png/male/Attack (%s).png".__mod__, 1, 8, size, True, 10)
    diemanim_ = Anim("png/male/Dead (%s).png".__mod__, 1, 12, size, False, 15)
    walkfanim_ = Anim("png/female/Walk (%s).png".__mod__, 1, 10, size, True, 15)
    attackfanim_ = Anim("png/female/Attack (%s).png".__mod__, 1, 8, size, True, 10)
    diefanim_ = Anim("png/female/Dead (%s).png".__mod__, 1, 12, size, False, 15)
    runspeed = 0.2
    damage = 5
    maxhealth = 10

    @classmethod
    def loadanims(cls):
        cls.walkmanim_.load()
        cls.attackmanim_.load()
        cls.diemanim_.load()
        cls.walkfanim_.load()
        cls.attackfanim_.load()
        cls.diefanim_.load()

    def __init__(self, world, pos, cls):
        Entity.__init__(self, world, pos)
        if random.random() > 0.5:
            self.walkanim = cls.walkmanim_.copy()
            self.attackanim = cls.attackmanim_.copy()
            self.dieanim = cls.diemanim_.copy()
        else:
            self.walkanim = cls.walkfanim_.copy()
            self.attackanim = cls.attackfanim_.copy()
            self.dieanim = cls.diefanim_.copy()
        self.walkanim.reset()
        self.attackanim.reset()
        self.dieanim.reset()
        self.ground = self.pos.y
        self.anim = self.walkanim
        self.facing = RIGHT
        self.health = self.maxhealth

    def load(self):
        pass

    def die(self):
        self.anim = self.dieanim
        self.anim.face(self.facing)
        self.vel.x = 0
        self.paused = True
        self.world.kills += 1

    def hit(self, damage):
        self.health -= damage

    def draw(self, surface):
        if not self.paused:
            self.drawhealthbar(surface)
        Entity.draw(self, surface)

    def update(self, dt):
        self.image = self.anim.next()
        if self.health <= 0 and not self.paused:
            self.die()
        if self.paused: return
        dp = self.pos.x - self.world.player.pos.x
        self.anim = self.walkanim
        if dp < -20:
            self.vel.x = self.runspeed
            self.facing = RIGHT
            self.anim.face(RIGHT)
        elif dp > 20:
            self.vel.x = -self.runspeed
            self.facing = LEFT
            self.anim.face(LEFT)
        else:
            self.anim = self.attackanim
            self.anim.face(self.facing)
            self.vel.x = 0
            if self.anim.frame == 0 and self.world.player.pos.y >= self.ground:
                self.world.player.hit(self.damage)
        if self.pos.y > self.ground and self.vel.y > 0:
            self.pos.y = self.ground
            self.vel.y = 0
        Entity.update(self, dt)

class BigZombie(Zombie):
    size = (150, 150)
    runspeed = 0.1
    maxhealth = 40
    damage = 10
    attackmanim_ = Anim("png/male/Attack (%s).png".__mod__, 1, 8, size, True, 5)
    walkmanim_ = Anim("png/male/Walk (%s).png".__mod__, 1, 10, size, True, 7)
    walkfanim_ = Anim("png/female/Walk (%s).png".__mod__, 1, 10, size, True, 7)
    attackfanim_ = Anim("png/female/Attack (%s).png".__mod__, 1, 8, size, True, 5)
    diefanim_ = Anim("png/female/Dead (%s).png".__mod__, 1, 12, size, False, 5)
    diemanim_ = Anim("png/male/Dead (%s).png".__mod__, 1, 12, size, False, 5)

    def __init__(self, world, pos, cls):
        ap = pos - Vec2(50, 50)
        Zombie.__init__(self, world, ap, cls)

    def die(self):
        self.world.kills += 4
        Zombie.die(self)


class Player(Entity):
    def __init__(self, world, pos):
        Entity.__init__(self, world, pos, Vec2(0, 0))
        self.size = (100, 100)
        self.runspeed = 0.3
        self.jumpspeed = 0.3
        self.runanim = Anim("png/Run (%s).png".__mod__, 1, 8, self.size, True, 15)
        self.idleanim = Anim("png/Idle (%s).png".__mod__, 1, 10, self.size, True, 15)
        self.jumpanim = Anim("png/Jump (%s).png".__mod__, 1, 10, self.size, False, 10)
        self.shootanim = Anim("png/Shoot (%s).png".__mod__, 1, 4, self.size, False, 15, self.resetanim)
        self.runshootanim = Anim("png/RunShoot (%s).png".__mod__, 1, 9, self.size, False, 15, self.resetanim)
        self.jumpshootanim = Anim("png/JumpShoot (%s).png".__mod__, 1, 5, self.size, False, 15, self.resetanim)
        self.slideanim = Anim("png/Slide (%s).png".__mod__, 1, 10, self.size, False, 10, self.resetanim)
        self.dieanim = Anim("png/Dead (%s).png".__mod__, 1, 10, self.size, False, 15)
        self.jumpmeleeanim = Anim("png/JumpMelee (%s).png".__mod__, 1, 8, self.size, False, 15, self.resetanim)
        self.meleeanim = Anim("png/Melee (%s).png".__mod__, 1, 8, self.size, False, 15, self.resetanim)
        self.anim = self.idleanim
        self.facing = RIGHT
        self.ground = self.pos.y
        self.maxhealth = 20
        self.health = self.maxhealth
        self.shootsound = WavePlayer('sounds/shoot.wav')
        self.jumpsound = WavePlayer('sounds/jump.wav')
        self.meleedamage = 10


    def resetanim(self):
        if self.pos.y < self.ground:
            self.anim = self.jumpanim
            self.anim.face(self.facing) 
        elif abs(self.world.bgv.x) > 0:
            self.anim = self.runanim
            self.anim.face(self.facing)
        else:
            self.anim = self.idleanim
            self.anim.face(self.facing)

    def die(self):
        self.anim = self.dieanim
        self.anim.face(self.facing)
        self.world.bgv.x = 0
        self.vel.x = 0
        self.paused = True
        self.world.stoptimer = 2000

    def hit(self, damage):
        if self.anim != self.slideanim:
            self.health -= damage

    def heal(self, by):
        self.health += by
        self.health = min(self.health, self.maxhealth)

    def load(self):
        self.jumpanim.load()
        self.runanim.load()
        self.idleanim.load()
        self.shootanim.load()
        self.runshootanim.load()
        self.jumpshootanim.load()
        self.dieanim.load()
        self.slideanim.load()
        self.jumpmeleeanim.load()
        self.meleeanim.load()

    def draw(self, surface):
        if not self.paused:
            self.drawhealthbar(surface)
        Entity.draw(self, surface)

    def mov_right(self):
        if self.pos.y >= self.ground and self.anim != self.meleeanim:
            self.anim = self.runanim
            self.anim.face(RIGHT)
            self.facing = RIGHT
            self.world.bgv.x = -self.runspeed
            self.vel.x = self.runspeed

    def mov_left(self):
        if self.pos.y >= self.ground and self.anim != self.meleeanim:
            self.anim = self.runanim
            self.anim.face(LEFT)
            self.facing = LEFT
            self.world.bgv.x = self.runspeed
            self.vel.x = -self.runspeed

    def jump(self):
        if self.pos.y >= self.ground and self.anim != self.slideanim:
            self.anim = self.jumpanim
            self.anim.reset()
            self.anim.face(self.facing)
            self.vel.y = -self.jumpspeed
            if self.world.sfx: self.jumpsound.play()

    def shoot(self):
        if self.pos.y < self.ground:
            self.anim = self.jumpshootanim
        elif abs(self.world.bgv.x) > 0:
            self.anim = self.runshootanim
        else:
            self.anim = self.shootanim
        self.anim.face(self.facing)
        self.anim.reset()
        if self.facing == RIGHT:
            b = Bullet(self.world, self.pos + Vec2(70, 45), 1, len(self.world.objects))
        elif self.facing == LEFT:
            b = Bullet(self.world, self.pos + Vec2(0, 45), -1, len(self.world.objects))
        b.load()
        self.world.objects.append(b)
        if self.world.sfx: self.shootsound.play()

    def slide(self):
        if self.pos.y >= self.ground and self.anim != self.slideanim and abs(self.world.bgv.x) > 0:
            self.anim = self.slideanim
            self.anim.reset()
            self.anim.face(self.facing)

    def melee(self):
        if self.anim not in (self.meleeanim, self.jumpmeleeanim):
            if self.pos.y < self.ground:
                self.anim = self.jumpmeleeanim
                self.anim.face(self.facing)
                self.anim.reset()
            else:
                self.world.bgv.x = 0
                self.vel.x = 0
                self.anim = self.meleeanim
                self.anim.face(self.facing)
                self.anim.reset()

            for zombie in self.world.zombies:
                if zombie.paused: continue
                if zombie.pos.x - 50 < self.pos.x < zombie.pos.x + 150:
                    zombie.hit(self.meleedamage)

    def stop_mov(self):
        if self.pos.y >= self.ground:
            self.anim = self.idleanim
            self.world.bgv.x = 0
            self.vel.x = 0
            self.anim.face(self.facing)
        else:
            self.world.bgv.x = 0
            self.vel.x = 0

    def handle_event(self, ev):
        if self.paused: return
        if ev.type == pygame.KEYDOWN:
            if ev.key == pygame.K_RIGHT:
                self.mov_right()
            elif ev.key == pygame.K_LEFT:
                self.mov_left()
            elif ev.key == pygame.K_UP:
                self.jump()
            elif ev.key == pygame.K_SPACE:
                self.shoot()
            elif ev.key == pygame.K_DOWN:
                self.slide()
            elif ev.key == pygame.K_e:
                self.melee()
        elif ev.type == pygame.KEYUP:
            if ev.key == pygame.K_LEFT or ev.key == pygame.K_RIGHT:
                self.stop_mov()
        elif ev.type == pygame.JOYBUTTONDOWN:
            if ev.button == self.world.joymap['shoot']:
                self.shoot()
            elif ev.button == self.world.joymap['slide']:
                self.slide()
            elif ev.button == self.world.joymap['melee']:
                self.melee()
            elif ev.button == self.world.joymap['jump']:
                self.jump()
        elif ev.type == pygame.JOYAXISMOTION:
            if ev.axis == self.world.joymap['mov'] and ev.value > 0.2 \
               and -0.1 < self.world.joystick.get_axis(self.world.joymap['nmov']) < 0.1:
                self.mov_right()
            elif ev.axis == self.world.joymap['mov'] and ev.value < -0.2 \
               and -0.1 < self.world.joystick.get_axis(self.world.joymap['nmov']) < 0.1:
                self.mov_left()
            elif ev.axis == self.world.joymap['mov'] and abs(ev.value) <= 0.2 \
               and -0.1 < self.world.joystick.get_axis(self.world.joymap['nmov']) < 0.1 \
               and abs(self.vel.x) != 0:
                self.stop_mov()

    
    def update(self, dt):
        self.image = self.anim.next()
        if self.health <= 0 and not self.paused:
            self.die()
        if self.pos.y > self.ground and self.vel.y > 0:
            self.pos.y = self.ground
            self.vel.y = 0
        Entity.update(self, dt)
        if self.pos.y > self.ground:
            if self.anim == self.jumpanim:
                self.resetanim()

class Game:
    def __init__(self, surface, fps=60, music=True, sfx=True):
        self.objects = []
        self.fps = fps
        self.surface = surface
        self.width, self.height = self.surface.get_size()
        self.clock = pygame.time.Clock()
        self.stopped = False
        self.bgv = Vec2(0, 0)
        self.bgx = Vec2(0, 0)
        self.bga = Vec2(0, 0)
        self.bg = None
        self.player = None
        self.zombies = []
        self.kills = 0
        self.font = None
        self.zombietimer = Timer(1000)
        self.healthtimer = Timer(5000)
        self.stoptimer = None
        self.joystick = None
        self.joymap = None
        self.music = music
        self.sfx = sfx

    def setup(self):
        Zombie.loadanims()
        BigZombie.loadanims()
        self.player = Player(self, Vec2(self.width // 2 - 50, self.height - 125))
        self.objects.append(self.player)
    
    def load(self):
        self.bg = pygame.image.load("png/bg.png")
        self.bg = pygame.transform.smoothscale(self.bg, (self.width, self.height))
        self.font = pygame.font.Font("font.ttf", 20)

        for obj in self.objects:
            obj.load()

    def spawnzombie(self):
        if self.player.paused: return
        if random.random() < 0.1:
            c = BigZombie
        else:
            c = Zombie
        z = c(self, Vec2(self.player.pos.x + random.choice([400, -400]), self.player.ground), c)
        z.load()
        self.zombies.append(z)
        self.objects.append(z)

    def spawnpickup(self):
        if self.player.paused: return
        z = HealthPickup(self, Vec2(self.player.pos.x + random.choice([300, -300]), self.player.pos.y))
        z.load()
        self.objects.append(z)

    def update(self, dt):
        self.bgx += dt * self.bgv
        self.bga += dt * self.bgv
        if self.bga.x > self.width or self.bga.x < -self.width:
            self.bga.x = 0

        if self.zombietimer.tick():
            self.spawnzombie()

        if self.healthtimer.tick():
            self.spawnpickup()

        if self.stoptimer is not None:
            self.stoptimer -= dt

        if self.stoptimer and self.stoptimer <= 0:
            self.stopped = True

        for obj in self.objects:
            obj.update(dt)

    def draw(self):
        self.surface.blit(self.bg, tuple(self.bga))
        self.surface.blit(self.bg, tuple(self.bga - Vec2(self.width, 0)))
        self.surface.blit(self.bg, tuple(self.bga + Vec2(self.width, 0)))
        self.surface.blit(self.font.render("Score: %s" % self.kills, True, (0, 0, 0)), (10, 10))

        for obj in self.objects:
            obj.draw(self.surface)

        self.player.draw(self.surface)

    def handle_event(self, ev):
        for obj in self.objects:
            obj.handle_event(ev)

    def playmusic(self):
        while True:
            s = subprocess.Popen(['sox', 'sounds/newbattle.wav', '-d'], stdout=subprocess.DEVNULL)
            while s.poll() is None:
                if self.stopped == True:
                    s.kill()
                    return

    def setupjoy(self):
        self.joystick = pygame.joystick.Joystick(0)
        self.joystick.init()
        self.joymap = JOYMAPS[self.joystick.get_name()]

    def mainloop(self):
        if self.music:
            t = threading.Thread(target=self.playmusic)
            t.start()
        if pygame.joystick.get_count() > 0:
            self.setupjoy()
        pygame.key.set_repeat(100, 200)
        self.setup()
        self.load()
        while not self.stopped:
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    self.stopped = True
                self.handle_event(event)
            self.update(self.clock.tick(self.fps))
            self.surface.fill((0, 0, 0))
            self.draw()
            pygame.display.flip()
        self.surface.blit(self.bg, (0, 0))
        self.surface.blit(self.font.render("You died!", True, (0, 0, 0)), (self.width // 2 - 100, self.height // 2 - 20))
        self.surface.blit(self.font.render("Your score was: %s" % self.kills, True, (0, 0, 0)), 
            (self.width // 2 - 100, self.height // 2))
        pygame.display.flip()
        pygame.time.wait(2000)
