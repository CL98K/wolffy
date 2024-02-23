# -*- coding: utf-8 -*-
import time
from py2lisp import Py2Lisp

lisp = Py2Lisp("./lisp.core")
lisp.mode = Py2Lisp.COMPILE_MODE

lisp.initialize()

print(lisp.print(1))

# stime = time.time()

# for i in range(20000):
#     lisp.print({"a": i, "b": [i,2,3], "c": 1.0, "d": "asdasdasdasda", "e": "中国"})

# print(time.time() - stime)

lisp.finalize()
