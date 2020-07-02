from __future__ import division, print_function

import random
import time

def bubble_sort():
    global arr
    for j in range(len(arr)):
        sorted = True
        for i in range(len(arr) - j - 1):
            yield (i, i + 1)
            if arr[i + 1] < arr[i]:
                sorted = False
                arr[i + 1], arr[i] = arr[i], arr[i + 1]
        if sorted:
            break

def median3(a, b, c):
    global arr

    def swap(f, g):
        if arr[g] > arr[f]:
            return (g, f)
        else:
            return (f, g)
    
    a, c = swap(a, c)
    a, b = swap(a, b)
    b, c = swap(b, c)
    
    return b

def quick_sort(s, e, h):
    global arr
    if s >= e:
        return
    elif h > 0 and e - s <= h:
        for n in insertion_sort(s, e + 1):
            yield n
    elif e - s == 1:
        yield (s, e)
        if arr[s] > arr[e]:
            arr[s], arr[e] = arr[e], arr[s]
    else:
        pivot = median3(
            median3(*random.sample(range(s, e + 1), 3)),
            median3(*random.sample(range(s, e + 1), 3)),
            median3(*random.sample(range(s, e + 1), 3)),
        ) 
        l = 0
        
        for i in range(s, e + 1):
            yield (i, pivot)
            if arr[i] <= arr[pivot]:
                l += 1
        
        arr[s + l - 1], arr[pivot] = arr[pivot], arr[s + l - 1]
        pivot = s + l - 1
        
        i = s
        j = pivot + 1
        while (i < pivot) and (j <= e):
            yield (i, pivot)
            if arr[i] <= arr[pivot]:
                while arr[i] <= arr[pivot]:
                    i += 1
                    yield (i, pivot)
                continue
                
            yield (j, pivot)
            if arr[j] > arr[pivot]:
                while arr[j] > arr[pivot]:
                    j += 1
                    yield (j, pivot)
                continue
                
            if (arr[i] > arr[pivot]) and (arr[j] <= arr[pivot]):
                yield (i, j)
                arr[i], arr[j] = arr[j], arr[i]
                
        for n in quick_sort(s, pivot - 1, h):
            yield n
        for n in quick_sort(pivot, e, h):
            yield n
        
def selection_sort(s, e):
    for i in range(s, e - 1):
        m = i
        for j in range(i + 1, e):
            yield (m, j)
            if arr[j] < arr[m]:
                m = j
        arr[i], arr[m] = arr[m], arr[i]
        
def insertion_sort(s, e):
    for i in range(s, e - 1):
        j = i
        yield (j, j + 1)
        while j >= s and arr[j] >= arr[j + 1]:
            yield (j, j + 1)
            arr[j + 1], arr[j] = arr[j], arr[j + 1]
            j -= 1
       
def shift_down(s, e, k):
    global arr
    for i in range(s, e):
        yield (i - k, i)
        arr[i - k] = arr[i]
                                                             
def block_sort(k):
    global arr
    
    for i, s in enumerate(range(0, len(arr) - k, k)):
        for n in insertion_sort(s, s + k):
            yield n
            
    aux = []
    for j in range(k):
        block = []
        for i in range(j, len(arr), k):
            yield (i, i)
            block.append(arr[i])
        aux.extend(block)
    
    for i in range(len(arr)):
        yield (i, i)
        arr[i] = aux[i]

    for n in insertion_sort(0, len(arr)):
        yield n

def merge_sort(s, e, k):
    if e - s <= k:
        for n in insertion_sort(s, e):
            yield n
    else:
        middle = int((s + e) / 2) + 1
        for n in merge_sort(s, middle, k):
            yield n
        for n in merge_sort(middle, e, k):
            yield n
            
        aux = []
        p1 = s
        p2 = middle
        while (p1 < middle) and (p2 < e):
            yield (p1, p2)
            if arr[p1] < arr[p2]:
                aux.append(arr[p1])
                p1 += 1
            else:
                aux.append(arr[p2])
                p2 += 1
                
        while p1 < middle:
            yield (p1, p1)
            aux.append(arr[p1])
            p1 += 1
            
        while p2 < e:
            yield (p2, p2)
            aux.append(arr[p2])
            p2 += 1
                
        for i in range(len(aux)):
            yield (s + i, s + i)
            arr[s + i] = aux[i]
            
def reverses(s, e):
    for i in range(0, int((e - s)/2)):
        yield (s + i, e - i - 1)
        arr[s + i], arr[e - i - 1] = arr[e - i - 1], arr[s + i]
            
def block_swap(a, b, l):
    global arr
    for i in range(l):
        yield (a + i, b + i)
        arr[a + i], arr[b + i] = arr[b + i], arr[a + i]
            
def merge(s, middle, e):
    aux = []
    p1 = s
    p2 = middle
    while (p1 < middle) and (p2 < e):
        yield (p1, p2)
        if arr[p1] < arr[p2]:
            aux.append(arr[p1])
            p1 += 1
        else:
            aux.append(arr[p2])
            p2 += 1
            
    while p1 < middle:
        yield (p1, p1)
        aux.append(arr[p1])
        p1 += 1
        
    while p2 < e:
        yield (p2, p2)
        aux.append(arr[p2])
        p2 += 1
            
    for i in range(len(aux)):
        yield (s + i, s + i)
        arr[s + i] = aux[i]
            
def in_place_merge(a, b, e):
    global arr
    n = e - a
    blk = int(n ** 0.5)
    ta = blk + (b - a - 1) % blk
    tb = blk + (e - b + 1) % blk
    

    for p in reverses(b, e):
        yield p
    for p in reverses(b - ta - 1, e):
        yield p
    for p in insertion_sort(e - ta - tb, e):
        yield p
    for p in reverses(a, e):
        yield p
    for p in reverses(a + blk, e):
        yield p
        
    for i in range(1, blk - 1):
        k = i
        for j in range(i + 1, blk):
            yield (a + (j + 1) * blk - 1, a + (k + 1) * blk - 1)
            if arr[a + (j + 1) * blk - 1] < arr[a + (k + 1) * blk - 1]:
                k = j
        for p in block_swap(a + i * blk, a + k * blk, blk):
            yield p
    
    i = 0
    p2 = a + blk
    lbuf = a
    
    while True:
        pi = i
        for i in range(i + 1, blk - 1):
            yield (a + (i + 1) * blk - 1, a + (i + 1) * blk)
            if arr[a + (i + 1) * blk - 1] > arr[a + (i + 1) * blk]:
                break
        if i == pi:
            break
            
        p1 = p2
        p2 = a + (i + 1) * blk
        
        while p1 < a + (i + 1) * blk:
            yield (p1, p2)
            if arr[p1] < arr[p2]:
                yield (p1, lbuf)
                arr[p1], arr[lbuf] = arr[lbuf], arr[p1]
                p1 += 1
                lbuf += 1
            else:
                yield (p2, lbuf)
                arr[p2], arr[lbuf] = arr[lbuf], arr[p2]
                p2 += 1
                lbuf += 1
    
    for p in reverses(lbuf, e):
        yield p
    for p in reverses(lbuf, lbuf + e - p2):
        yield p
    for p in insertion_sort(a, e):
        yield p

def in_place_merge_sort(s, e, k):
    if e - s <= k:
        for n in insertion_sort(s, e):
            yield n
        return
    
    global arr
    mq = int((s + e) / 2) + 1
    for n in in_place_merge_sort(s, mq, k):
        yield n
    for n in in_place_merge_sort(mq, e, k):
        yield n

    for n in in_place_merge(s, mq, e):
        yield n

def shuffle():
    global arr
    for i in range(len(arr) - 1, 0, -1):
        j = random.randint(0, i)
        yield (i, j)
        arr[j], arr[i] = arr[i], arr[j]
        
def reverse():
    global arr
    for i in range(int(len(arr)/2)):
        yield (i, len(arr) - i - 1)
        arr[i], arr[len(arr) - i - 1] = arr[len(arr) - i - 1], arr[i]

arr = range(2000)
i = 0
hn, hk = None, None
b = 10
tasks = [
    (in_place_merge_sort, (0, len(arr), 64), 20),
]
proc = shuffle()
sk = 20
done = False
ttime = time.time()

def setup():
    global k, kh
    size(800, 600, P2D)
    smooth(16)
    pixelDensity(displayDensity())
    k = width / len(arr)
    kh = height / len(arr)
    
def draw():
    background(0)
    fill(255)
    noStroke()
    
    global i, hn, hk, arr, done, proc, ttime, sk
    for _ in range(sk):
        if not done:
            try:
                hn, hk = next(proc)
            except StopIteration:
                if len(tasks) == 0:
                    done = True
                    print(time.time() - ttime)
                else:
                    f, rest, sk = tasks.pop(0)
                    proc = f(*rest)
                    print(time.time() - ttime)
                    ttime = time.time()
    
    for i, ar in enumerate(arr):
        if i == hn:
            fill(255, 0, 0)
        elif i == hk:
            fill(0, 0, 255)
        else:
            fill(255)
        rect(k * i, height - kh * ar, k, kh * ar) 