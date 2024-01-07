#! /bin/sh
sbcl --load ./lisp_master.lisp \
     --eval '(save-lisp-and-die "mmap-test.core" :executable t)'
     