import pygame
import game
import threading

from pgu import gui

class TestDialog(gui.Dialog):
    def __init__(self, menu):
        self.menu = menu
        title = gui.Label("Options")
        table = gui.Table(height = 200, width = 400)
        table.tr()
        musicswitch = gui.Switch(menu.music)
        def msc():
            menu.music = not menu.music
        musicswitch.connect(gui.CLICK, msc)
        table.td(gui.Label("Music"))
        table.td(musicswitch)
        sfxswitch = gui.Switch(menu.sfx)
        def ssc():
            menu.sfx = not menu.sfx
        sfxswitch.connect(gui.CLICK, ssc)
        table.tr()
        table.td(gui.Label("SFX"))
        table.td(sfxswitch)
        gui.Dialog.__init__(self, title, table)

class Menu:
    def __init__(self, surface):
        self.music = True
        self.sfx = True

        self.surface = surface

        self.bg = pygame.image.load("png/bg.png")

        self.surface.blit(self.bg, (0, 0))

        self.app = gui.App(theme=gui.Theme("data/themes/clean"))
        self.app.connect(gui.QUIT, self.app.quit, None)

        self.table = gui.Table(width=self.surface.get_width(),height=self.surface.get_height())

        self.table.tr()


        self.font = pygame.font.Font("font.ttf", 50)
        self.sfont = pygame.font.Font("font.ttf", 16)

        self.table.td(gui.Label("Zombie Hordes", font=self.font))

        self.table.tr()

        self.button = gui.Button("Play",  width=200, font=self.sfont)
        self.button.connect(gui.CLICK, self.playgame)

        self.table.td(self.button)

        self.quit = gui.Button("Quit", width=200, font=self.sfont)
        self.quit.connect(gui.CLICK, self.app.quit, None)

        self.dlg = None

        def dlo():
            self.dlg = TestDialog(self)
            self.dlg.open()
            threading.Thread(target=dlc).start()
        
        def dlc():
            while self.dlg.is_open(): pass
            self.surface.blit(self.bg, (0, 0))
            self.app.repaint()

        self.options = gui.Button("Options", width=200, font=self.sfont)
        self.options.connect(gui.CLICK, dlo)

        self.table.tr()
        self.table.td(self.options)

        self.table.tr()
        self.table.td(self.quit)

        self.game = None

    def playgame(self):
        self.surface.fill((255, 255, 255))
        self.game = game.Game(self.surface, music=self.music, sfx=self.sfx)
        self.game.mainloop()
        self.surface.blit(self.bg, (0, 0))
        self.app.run(self.table)

    def mainloop(self):
        self.app.run(self.table)
