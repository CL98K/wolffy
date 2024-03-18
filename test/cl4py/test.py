# -*- coding: utf-8 -*-
import time
from cl4py import Lisp

def call_n_times(n):
    start = time.time()
    for i in range(n):
        x = str(i)
    return time.time() - start


print(call_n_times(1000000))


lisp = Lisp("./lisp.core")
lisp.mode = Lisp.COMPILE_MODE

lisp.initialize()

stime = time.time()

for i in range(100000):
    # x = lisp.test_interface({"a": 1, "b": [i,2,3], "c": 1.0, "d": "asdas\ndasdasda", "e": "中国"}, block=False)
    lisp.write_to_string(i, block=False)

print(time.time() - stime)

lisp.finalize()
