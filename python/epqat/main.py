import menu
import pygame

if __name__ == "__main__":
    pygame.init()
    g = menu.Menu(pygame.display.set_mode((640, 269)))
    g.mainloop()
    
