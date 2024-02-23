# -*- coding: utf-8 -*-
import time
from py2lisp import Py2Lisp

lisp = Py2Lisp("./lisp.core")
lisp.mode = Py2Lisp.COMPILE_MODE

lisp.initialize()

lisp.print(1)
lisp.print(1)
lisp.print(1)

# stime = time.time()

# for i in range(3):
#     lisp.print(1)

# print(time.time() - stime)

lisp.finalize()
