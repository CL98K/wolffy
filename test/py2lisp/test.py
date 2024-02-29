# -*- coding: utf-8 -*-
import time
from py2lisp import Py2Lisp

lisp = Py2Lisp("./lisp.core")
lisp.mode = Py2Lisp.COMPILE_MODE

lisp.initialize()

stime = time.time()

for i in range(10000):
    x = lisp.test_interface({"a": 1, "b": [i,2,3], "c": 1.0, "d": "asdas\ndasdasda", "e": "中国"}, block=False)

print(time.time() - stime)

lisp.finalize()
