# -*- coding: utf-8 -*-
import time
from cl4py import Lisp


with open("./_smp/1231.sm", "rb") as fp:
    print([x for x in fp.read(20)])
    
with open("./_smp/1231_X.sm", "rb") as fp:
    print([x for x in fp.read(20)])
    

# lisp = Lisp("./lisp.core", parallel=1)
# lisp.mode = Lisp.COMPILE_MODE

# lisp.initialize()

# lisp.benchmark()

# lisp.finalize()

