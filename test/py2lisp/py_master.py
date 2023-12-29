# -*- coding: utf-8 -*-
import time

from lib.py2lisp import Py2Lisp
from lib.dkmp import SharedMemory


handle, filepath = SharedMemory.namedAlloc(65536 * 1)

lisp = Py2Lisp("./mmap-test.core")
lisp.mode = Py2Lisp.COMPILE_MODE

lisp.eval(f'(open-mmap "{filepath}")')

stime = time.time()

for i in range(10000):
    SharedMemory.write({"a": i, "b": [i,2,3], "c": 1.0, "d": "asdasdasdasda", "e": "中国"}, handle)
    lisp.eval('(read-data)', parse=True)

print(time.time() - stime)

lisp.eval('(close-mmap)')

lisp.close()

SharedMemory.dealloc(handle)
