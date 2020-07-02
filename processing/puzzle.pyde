class SimpleContinuousLinePuzzle:
    def __init__(self, allowed, start, goals):
        self.board = [[[0, 0] if allowed[y*10 + x] else [1, 0] for x in range(10)] for y in range(10)]
        self.board[start[1]][start[0]][1] = 1
        self.allowed = []
        for y in range(10):
            for x in range(10):
                if allowed[y*10 + x]:
                    self.allowed.append((x, y))
        self.goals = goals
        for (x, y) in self.goals:
            self.board[y][x][0] = 2
        self.start = start
        self.cx, self.cy = start
        
    def add(self, x, y):
        self.board[y][x][1] = 1

    def remove(self, x, y):
        self.board[y][x][1] = 0

    def type(self, x, y):
        return self.board[y][x][0]

    def occupied(self, x, y):
        return self.board[y][x][1]

    def accept(self):
        marked = [(x, y) for x in range(10) for y in range(10) if self.occupied(x, y)]
        alm = all((x, y) in self.allowed for (x, y) in marked)
        glm = all(goal in marked for goal in self.goals)
        clm = all(any(mhdistance(x1, y1, x2, y2) == 1 for (x2, y2) in marked) for (x1, y1) in marked)
        return alm and glm and clm
                
def mhdistance(x1, y1, x2, y2):
    return abs(x2 - x1) + abs(y2 - y1)

puzzle1 = SimpleContinuousLinePuzzle([1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                      0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                                      0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                                      0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                                      0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                                      0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                                      0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                                      0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                                      0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                                      0, 0, 0, 0, 0, 0, 0, 0, 0, 1], (0, 0), [(9, 9)]) 

puzzle2 = SimpleContinuousLinePuzzle([1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                      1, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                                      1, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                                      1, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                                      1, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                                      1, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                                      1, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                                      1, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                                      1, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                                      1, 1, 1, 1, 1, 1, 1, 1, 1, 1], (0, 0), [(9, 9), (0, 9), (9, 0)])


def getpuzzles():
    yield puzzle1
    yield puzzle2
    
puzzles = getpuzzles()

puzzle = next(puzzles)
slidea = 0
sliding = False
stg = 0

def setup():
    size(610, 610, P2D)
    smooth(16)
    pixelDensity(displayDensity())

def drawCell(ix, iy, x, y):
    c = puzzle.type(ix, iy)
    if c == 0:
        fill(127, 140, 141)
        rect(x, y, 50, 50, 10)
    elif c == 1:
        fill(149, 165, 166)
        rect(x, y, 50, 50, 10)
    elif c == 2:
        fill(52, 73, 94)
        rect(x, y, 50, 50, 10)
    if puzzle.occupied(ix, iy):
        fill(color(189, 195, 199) if ix == puzzle.cx and iy == puzzle.cy else color(236, 240, 241))
        rect(x + 10, y + 10, 30, 30, 10)

def draw():
    global slidea, stg, puzzle, sliding
    noStroke()
    background(149, 165, 166)
    translate(slidea, 0)
    if sliding:
        slidea += 10
    if slidea >= stg:
        sliding = False
    if slidea >= 600:
        puzzle = next(puzzles)
        slidea = -600
        sliding = True
        stg = 0
    for y in range(10):
        for x in range(10):
            drawCell(x, y, 10 + x * 60, 10 + y * 60)
            
def keyPressed():
    global sliding, stg
    if key == 'd' and puzzle.cx < 9:
        if puzzle.occupied(puzzle.cx + 1, puzzle.cy):
            puzzle.remove(puzzle.cx, puzzle.cy)
            puzzle.cx += 1
        else:
            puzzle.cx += 1
            puzzle.add(puzzle.cx, puzzle.cy)
    elif key == 'a' and puzzle.cx > 0:
        if puzzle.occupied(puzzle.cx - 1, puzzle.cy):
            puzzle.remove(puzzle.cx, puzzle.cy)
            puzzle.cx -= 1
        else:
            puzzle.cx -= 1
            puzzle.add(puzzle.cx, puzzle.cy)
    elif key == 's' and puzzle.cy < 9:
        if puzzle.occupied(puzzle.cx, puzzle.cy + 1):
            puzzle.remove(puzzle.cx, puzzle.cy)
            puzzle.cy += 1
        else:
            puzzle.cy += 1
            puzzle.add(puzzle.cx, puzzle.cy)
    elif key == 'w' and puzzle.cy > 0:
        if puzzle.occupied(puzzle.cx, puzzle.cy - 1):
            puzzle.remove(puzzle.cx, puzzle.cy)
            puzzle.cy -= 1
        else:
            puzzle.cy -= 1
            puzzle.add(puzzle.cx, puzzle.cy)
    elif key == 'e':
        if puzzle.accept():
            sliding = True
            stg = 600