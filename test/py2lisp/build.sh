#! /bin/sh
sbcl --load ./lisper.lisp \
     --eval '(save-lisp-and-die "lisp.core" :executable t)'
     