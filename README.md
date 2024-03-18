# Wollfy
Wolffy (abbreviation: wo or wol) mainly encapsulates some convenient packages for interacting with Python.  


# wo-pickle(or wo-pkl)
wo-pickle provides serialization and deserialization with Python and Lisp data structures.  

Performance metrics compared to python Pickler.  
Test environment:
CentOS Linux release 7.9.2009 (Core), 4 core, 8 threads, 32 GB memory, Python 3.11, SBCL 2.1.6 

| Test 100 times   | python-cPickle   | python-pickle   | wo-pickle(optimizing ....) |
|:----------------:|:----------------:|:---------------:|:--------------------------:|
|    load          |         0.35     |    6.79         |   2.1                     |
|    dump          |         0.26     |    -            |   0.53                    |


***highest-protocol***  
This is the highest protocol number we know how to read.

***default-protocol***  
The protocol we write by default. May be less than HIGHEST_PROTOCOL. Only bump this if the oldest still supported version of Python already includes it.  
Currently, most data types of Python are supported (excluding Python class objects, etc.)

**load** *(file &key (fix-imports t) (element-type "ascii") (:fast nil)*  
Deserialize Python data from a file.
```
CL-USER> (wo-pkl:load "D:/worker/Lisp/test/xx.plk")
12312312112312312
CL-USER> 
```

**loads** *(stream &key (fix-imports t) (element-type "ascii") (:fast nil)*  
Deserialize Python data from binary streams.
```
CL-USER> (setf s (wo-io:make-binary-stream :initial-data #(128 4 149 71 0 0 0 0 0 0 0 125 148 40 140 1 97 148 75 1 140 1 98 148 93 148 40 75 1 75 2 75 3 101 140 1 99 148 71 63 240 0 0 0 0 0 0 140 1 100 148 140 13 97 115 100 97 115 100 97 115 100 97 115 100 97 148 140 1 101 148 140 6 228 184 173 229 155 189 148 117 46)))
#<IO:BINARY-STREAM {1006EAA673}>
CL-USER> (wo-pkl:loads s)
#<HASH-TABLE :TEST EQUAL :COUNT 5 {1006EC30F3}>
CL-USER> (kit:printf (wo-pkl:loads s))
 {
  "a": 1,
  "b": (1 2 3 ), 
  "c": 1.0d0,
  "d": "asdasdasdasda",
  "e": "中国",}
2
```

**dump** *(obj file &key (protocol 0) (fix-imports t) (:fast nil)*  
Serialize the LISP data structure and output the results to a file.

**dumps** *(obj &key (protocol 0) (fix-imports t) (:fast nil)*  
Serialize the LISP data structure.
```
CL-USER> (wo-pkl:dumps 1231111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111)
#(128 4 149 51 0 0 0 0 0 0 0 138 48 199 113 28 199 113 28 199 113 28 199 113 28
  199 113 208 26 55 93 75 174 117 201 44 252 41 14 85 10 222 251 116 27 55 160
  188 136 91 34 145 184 252 242 137 190 42 170 255 7 46)
62
```


# wo-io
wo-io provides some stream operations, existing Binary-stream( == Python io.BytesIO).

**defclass binary-stream**


**make-binary-stream** *(&key (initial-data (make-array 0)) (initial-size 128) (restream-size 1.5) (upgrade-p t))*  
Create a binary stream object.
```
CL-USER> (setf s (wo-io:make-binary-stream :initial-data #(128 4 149 71 0 0 0 0 0 0 0 125 148 40 140 1 97 148 75 1 140 1 98 148 93 148 40 75 1 75 2 75 3 101 140 1 99 148 71 63 240 0 0 0 0 0 0 140 1 100 148 140 13 97 115 100 97 115 100 97 115 100 97 115 100 97 148 140 1 101 148 140 6 228 184 173 229 155 189 148 117 46)))
#<IO:BINARY-STREAM {1006EAA673}>
```

**binary-stream-close** *(instance)*


**binary-stream-info** *(instance)*


**binary-stream-memery-view** *(instance)*


**binary-stream-file-position** *(instance &optional position-spec)*


**binary-stream-upgrade-space** *(instance &optional (size 0))*  
This method is internal and cannot be called externally.  


**binary-stream-write** *(instance integer)*


**binary-stream-writes** *(instance integers &key (upgrade-p t))*


**binary-stream-read** *(instance)*


**binary-stream-reads** *(instance &optional (n 1))*


**binary-stream-read-line** *(instance)*


**binary-stream-read-into** *(instance buffer)*


**binary-stream-read-sequence** *(instance buffer &optional start end)*


**file-stream-to-binary-stream** *(stream)*  
This method provides a way for files stream to binary streams.  