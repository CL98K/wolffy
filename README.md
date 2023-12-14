# Wollfy
Wolffy (abbreviation: wo or wol) mainly encapsulates some convenient packages for interacting with Python.  
wo-pickle provides serialization and deserialization with Python and Lisp data structures.  
wo-io provides some operations on binary streams.  


## wo-pickle
**highest-protocol**  
This is the highest protocol number we know how to read.

**default-protocal**  
The protocol we write by default. May be less than HIGHEST_PROTOCOL. Only bump this if the oldest still supported version of Python already includes it.

**py-load** *(file &key (fix-imports t) (element-type "ascii")*  
Deserialize Python data from a file.
```
CL-USER> (wo-pickle:py-load "D:/worker/Lisp/test/xx.plk")
12312312112312312
CL-USER> 
```

**py-loads** *(stream &key (fix-imports t) (element-type "ascii")*  
Deserialize Python data from binary streams.
```
CL-USER> (setf s (wo-io:make-binary-stream :initial-data #(128 4 149 71 0 0 0 0 0 0 0 125 148 40 140 1 97 148 75 1 140 1 98 148 93 148 40 75 1 75 2 75 3 101 140 1 99 148 71 63 240 0 0 0 0 0 0 140 1 100 148 140 13 97 115 100 97 115 100 97 115 100 97 115 100 97 148 140 1 101 148 140 6 228 184 173 229 155 189 148 117 46)))
#<IO:BINARY-STREAM {1006EAA673}>
CL-USER> (wo-pickle:py-loads s)
#<HASH-TABLE :TEST EQUAL :COUNT 5 {1006EC30F3}>
CL-USER> (kit:printf (wo-pickle:py-loads s))
 {
  "a": 1,
  "b": (1 2 3 ), 
  "c": 1.0d0,
  "d": "asdasdasdasda",
  "e": "中国",}
2
```

**lisp-dump** *(obj file &key protocol (fix-imports t)*  
Interfaces to be implemented.  

**lisp-dumps** *(obj &key (stream (wo-io:make-binary-stream)) protocol (fix-imports t)*  
Interfaces to be implemented.


## wo-io
**defclass binary-stream**


**make-binary-stream** *(&key (initial-data (make-array 0)) (initial-size 128) (restream-size 1.5) (upgrade-p t))*


**binary-stream-close** *(instance)*


**binary-stream-info** *(instance)*


**binary-stream-memery-view** *(instance)*


**binary-stream-file-position** *(instance &optional position-spec)*


**binary-stream-upgrade-space** *(instance &optional (size 0))*


**binary-stream-write** *(instance integer)*


**binary-stream-writes** *(instance integers &key (upgrade-p t))*


**binary-stream-read** *(instance)*


**binary-stream-reads** *(instance &optional (n 1))*


**binary-stream-read-line** *(instance)*


**binary-stream-read-into** *(instance buffer)*


**binary-stream-read-sequence** *(instance buffer &optional start end)*


**file-stream-to-binary-stream** *(stream)*