#! /bin/sh
sbcl --load //mnt/d/Worker/Lisp/other/py2lisp/lisp_master.lisp \
     --eval '(save-lisp-and-die "mmap-test.core" :executable t)'
     