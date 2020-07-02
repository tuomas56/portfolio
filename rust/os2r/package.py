import collections
import os
import json

def walk(dir, paths, data):  
    for entry in os.scandir(dir):
        if entry.is_dir():
            paths[entry.name] = (entry.name, {})
            walk(entry.path, paths[entry.name][1], data)
        elif entry.is_file():
            with open(entry.path, 'rb') as file:
                fdata = file.read()
                paths[entry.name] = (entry.name, len(data), len(fdata))
                data.extend(fdata)

paths = {}
data = []
walk('files', paths, data)

with open("iso/boot/files.bin", "wb") as file:
    j = json.dumps(("", paths), separators=(',', ':'))
    file.write(len(j).to_bytes(4, 'little'))
    file.write(j.encode('utf8'))
    file.write(bytearray(data))
