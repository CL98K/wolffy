#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import os
import mmap
import json
import time
import atexit
import pickle
import tempfile
import subprocess

from functools import partial

class Error(Exception): pass
class CoreNotExist(Exception): pass
class EvalModeError(Exception): pass
class SetEvalModeError(Exception): pass
class InitLISPENVError(Exception): pass
class PackageNotCall(Exception): pass
class ModeNotExist(Exception): pass

class SharedMemory():
    BLOCK_IDX = None
    BLOCK_NUM = None
    BLOCK_SET = None
    BLOCK_RATE = {}
    MMAP_MAX_SIZE = None
    MMAP_FPATH_MAP = {}
    
    @classmethod
    def alloc(cls, length, bnum=None):
        """申请共享内存"""
        fp = tempfile.TemporaryFile(mode="a+b")
        
        fp.write(b"\x00" * length)
        mm = mmap.mmap(fp.fileno(), length, flags=mmap.MAP_SHARED, prot=mmap.PROT_WRITE|mmap.PROT_READ)
        
        cls.MMAP_MAX_SIZE = length
        cls.block(length, bnum)
        
        return mm, None

    @classmethod
    def namedAlloc(cls, length, bnum=None):
        """建议只作为调试使用,否则需要手动清理内存映射文件"""
        path = "./_smp"
        filepath = "{0}/{1}.sm".format(path, os.getpid())
        
        if not os.path.exists(path):
            os.makedirs(path)
        
        fp = open(filepath, "a+b")
        fp.write(b"\x00" * length)
        mm = mmap.mmap(fp.fileno(), length, flags=mmap.MAP_SHARED, prot=mmap.PROT_WRITE|mmap.PROT_READ)
        
        cls.MMAP_MAX_SIZE = length
        cls.MMAP_FPATH_MAP[mm] = (fp, filepath)
        cls.block(length, bnum)

        return mm, filepath
    
    @classmethod
    def dealloc(cls, handle):
        """释放共享内存"""
        handle.close()
        
        if handle in cls.MMAP_FPATH_MAP:
            handle, filepath = cls.MMAP_FPATH_MAP[handle]
            try:
                handle.close()
                if os.path.exists(filepath): os.remove(filepath)
            except Exception as e:
                pass
        
        return True
    
    @classmethod
    def block(cls, size, number):
        """创建 block"""
        cls.BLOCK_SET = []
        
        temp = [i for i in range(0, size+number, int(size / number))]
        for i in range(1, len(temp)):
            cls.BLOCK_SET.append((temp[i-1], temp[i]-1))

        cls.BLOCK_NUM = len(cls.BLOCK_SET)
        
        return True
    
    @classmethod
    def getBlock(cls, handle, callback):
        """获取可用 block"""
        circle = 0
        
        while True:
            if (not cls.BLOCK_NUM):
                if cls.done(handle, 0) or (cls.BLOCK_IDX not in cls.BLOCK_RATE):
                    cls.BLOCK_RATE.setdefault(cls.BLOCK_IDX, 0)
                    cls.BLOCK_RATE[cls.BLOCK_IDX] += 1
                    callback(cls.BLOCK_IDX, cls.read(handle, 0))
                    return cls.BLOCK_IDX, (0, cls.MMAP_MAX_SIZE)
            else:
                cls.BLOCK_IDX = 0 if cls.BLOCK_IDX is None else cls.BLOCK_IDX if cls.BLOCK_IDX < (cls.BLOCK_NUM - 1) else 0
                
                for i in range(cls.BLOCK_IDX, cls.BLOCK_NUM):
                    cls.BLOCK_IDX = i
                    if cls.done(handle, cls.BLOCK_SET[cls.BLOCK_IDX][0]) or (cls.BLOCK_IDX not in cls.BLOCK_RATE):
                        cls.BLOCK_RATE.setdefault(cls.BLOCK_IDX, 0)
                        cls.BLOCK_RATE[cls.BLOCK_IDX] += 1
                        callback(cls.BLOCK_IDX, cls.read(handle, cls.BLOCK_SET[cls.BLOCK_IDX][0]))
                        return cls.BLOCK_IDX, cls.BLOCK_SET[cls.BLOCK_IDX]
            
            circle += 1
            if circle % 5000 == 0: time.sleep(0.1)
            
    @classmethod
    def done(cls, handle, offset):
        """写完毕"""
        if handle[offset] == 48: return True
        return False
            
    @classmethod
    def write(cls, data, handle, offset=0):
        """写共享内存文件"""
        data = pickle.dumps(data)
        dlen = len(data)
        length = bytes(f"{dlen}\x00", encoding="utf-8")
        
        handle.seek(offset+1)
        handle.write(length + data)
        handle.seek(offset)
        handle.write(b"1")
        
        return len(length), dlen, handle.tell()
    
    @classmethod
    def read(cls, handle, offset=0):
        """读共享内存文件"""
        index = handle.find(b"\x00", offset)
        if (index == -1) or (index == offset): return None
        
        length = int(str(handle[offset+1:index], encoding="utf-8"))
        index += 1
        
        return pickle.loads(handle[index: index + length])
    
    @classmethod
    def readX(cls, handle):
        """解析共享内存"""
        start, index = 0, 0
        
        while True:
            index = handle.find(b"\x00", start)
            if (index == -1) or (index == start): break
        
            length = int(str(handle[start:index], encoding="utf-8"))
            start = index + length + 1
            
            yield pickle.loads(handle[index+1:start])
            

class LSPackage():
    """LISP Package 对象"""
    def __init__(self, env, pname):
        self.env = env
        self.pname = pname

    def __getattr__(self, name):
        def __call(syntax, *args, **kwargs):
            if "block" in kwargs:
                tag = kwargs["block"]
                del kwargs["block"]
            else:
                tag = True
            
            iblock, (ipoint, limit) = SharedMemory.getBlock(self.env._Py2Lisp__sm, self.env._Py2Lisp__setFuture)
            future = None if tag else Future(self.env._Py2Lisp__sm, ipoint)
            if future: self.env._Py2Lisp__future[iblock] = future
            
            offset, size, _ = SharedMemory.write([args, kwargs], self.env._Py2Lisp__sm, ipoint)
            self.env.eval(f"(pycall #'{self.pname}:{syntax} :offset {offset} :size {size} :ipoint {ipoint} :limit {limit})", Py2Lisp.E_MODE_BACKGROUND|Py2Lisp.E_MODE_BLOCK if tag else Py2Lisp.E_MODE_BACKGROUND|Py2Lisp.E_MODE_NONBLOCK)
            return SharedMemory.read(self.env._Py2Lisp__sm, ipoint) if tag else future
        
        symbols = self.env.eval(f"(let ((col nil)) (do-external-symbols (s '{self.pname}) (push (symbol-name s) col)) col)", Py2Lisp.E_MODE_FOREGROUND)

        for symbol in eval(symbols):
            nsymbol = symbol.replace("-", "_").replace("*", "").replace("+", "").lower()
            describe = self.env.eval(f"(describe '{self.pname}:{symbol})", self.E_MODE_FOREGROUND)

            if "names a compiled function" in describe:
                self.__dict__[nsymbol] = partial(__call, symbol)
                self.__dict__[nsymbol].__doc__ = describe
            elif "names a macro" in describe:
                self.__dict__[nsymbol] = partial(__call, symbol)
                self.__dict__[nsymbol].__doc__ = describe
            elif "names a special variable" in describe:
                tag = "Value: "
                idx = describe.find(tag) + len(tag)
                self.__dict__[nsymbol] = describe[idx:describe.find("\n", idx+1)]
            elif "names a constant variable" in describe:
                tag = "Value: "
                idx = describe.find(tag) + len(tag)
                self.__dict__[nsymbol] = describe[idx:describe.find("\n", idx+1)]

        return self.__dict__[name]


class Future():
    def __init__(self, space, offset):
        self.__space = space
        self.__offset = offset
        self.__success = False
        self.__result = None
        self.__dgauge = False
        
    def done(self):
        return self.__success
    
    def get_result(self):
        if self.__dgauge: return self.__result
        
        if SharedMemory.done(self.__space, self.__offset):
            self.__result = SharedMemory.read(self.__space, self.__offset)
            self.__success = True
            self.__dgauge = True
        
        return self.__result
    
    def set_result(self, data):
        self.__result = data
        self.__dgauge = True
        return True
    
    def set_status(self, flag):
        self.__success = flag
        return True
    

class Py2Lisp():
    E_MODE_FOREGROUND = 1
    E_MODE_BACKGROUND = 2
    E_MODE_BLOCK = 10
    E_MODE_NONBLOCK = 20
    
    COMPILE_MODE = ":compile"
    INTERPRET_MODE = ":interpret"

    SYMBOL = b"* "
    FEEDBACK = r"#\Nul"
    END_SYMBOL = bytes(FEEDBACK, encoding="utf-8")
    NULL_SYMBOL = bytes("NIL", encoding="utf-8")
    EXCEPTION_FLAG = "!"
    EXCEPTION = {"!INDEX-TOO-LARGE-ERROR": IndexError,
                 "!TYPE-ERROR": TypeError,
                 "!READER-IMPOSSIBLE-NUMBER-ERROR": ZeroDivisionError,
                 "!UNBOUND-VARIABLE":  NameError,}
    
    def __init__(self, core=None):
        """core: image 文件路径"""
        if not os.path.exists(core): raise CoreNotExist(f"{repr(core)} not exist!")
        
        self.__sm, self.__sm_file = SharedMemory.namedAlloc(65536 * 1, 8) #创建交互空间
        self.__lisp = subprocess.Popen(core, shell=False, bufsize=-1, stdin=subprocess.PIPE, stdout=subprocess.PIPE) #Lisp Core 对象
        self.__mode = None #执行模式
        self.__io_mode = True #阻塞模式
        self.__future = {} #Future 对象集
        
        def exitHook(): self.finalize()
        atexit.register(exitHook)

    def __getattr__(self, name):
        def __call(syntax, *args, **kwargs):
            if "block" in kwargs:
                tag = kwargs["block"]
                del kwargs["block"]
            else:
                tag = True
            
            iblock, (ipoint, limit) = SharedMemory.getBlock(self.__sm, self.__setFuture)
            future = None if tag else Future(self.__sm, ipoint)
            if future: self.__future[iblock] = future
            
            offset, size, _ = SharedMemory.write([args, kwargs], self.__sm, ipoint)
            self.eval(f"(pycall #'{syntax} :offset {offset} :size {size} :ipoint {ipoint} :limit {limit})", self.E_MODE_BACKGROUND|self.E_MODE_BLOCK if tag else self.E_MODE_BACKGROUND|self.E_MODE_NONBLOCK)
            return SharedMemory.read(self.__sm, ipoint) if tag else future
        
        symbol = f"*{name[2:]}*" if name.startswith("V_") else name
        symbol = f"+{symbol[2:]}+" if symbol.startswith("C_") else symbol
        symbol = symbol.replace("_", "-")
        
        describe = self.eval(f"(describe '{symbol})", self.E_MODE_FOREGROUND)
        package = self.eval(f"(find-package '{symbol})", self.E_MODE_FOREGROUND)
        
        if "names a compiled function" in describe:
            self.__dict__[name] = partial(__call, symbol)
            self.__dict__[name].__doc__ = describe
        elif "names a macro" in describe:
            self.__dict__[name] = partial(__call, symbol)
            self.__dict__[name].__doc__ = describe
        elif "names a special variable" in describe:
            tag = "Value: "
            idx = describe.find(tag) + len(tag)
            self.__dict__[name] = describe[idx:describe.find("\n", idx+1)]
        elif "names a constant variable" in describe:
            tag = "Value: "
            idx = describe.find(tag) + len(tag)
            self.__dict__[name] = describe[idx:describe.find("\n", idx+1)]
        elif package and package != "NIL":
            self.__dict__[name] = LSPackage(self, symbol)
        else:
            raise AttributeError(f"'Py2Lisp' object has no attribute '{name}'")

        return self.__dict__[name]
        
    def __setEvalMode(self):
        """设置求值模式"""
        return self.eval(f"(setf *evaluator-mode* {self.__mode})", self.E_MODE_BACKGROUND)

    def __setBlockMode(self, flag):
        """设置 block 模式"""
        if flag: #非阻塞模式
            if self.__io_mode: #由阻塞模式设置为非阻塞模式
                self.__io_mode = False
                os.set_blocking(self.__lisp.stdout.fileno(), self.__io_mode)
        else: #阻塞模式
            if not self.__io_mode: #由非阻塞模式设置为阻塞模式
                self.__io_mode = True
                self.__lisp.stdout.read()
                os.set_blocking(self.__lisp.stdout.fileno(), self.__io_mode)
        
        return True
    
    def __setFuture(self, inode, data):
        """设置 Future"""
        if inode in self.__future:
            self.__future[inode].set_result(data)
            self.__future[inode].set_status(True)
            del self.__future[inode]
        
        return True

    def __syntaxCheck(self, string):
        """语法检查"""
        if string.count("(") != string.count(")"):
            return SyntaxError(f"unexpected EOF while parsing: {string}")
    
    def __readPIPE(self, tag):
        """读取数据"""
        if tag == 1:
            infos = []
            
            for line in iter(self.__lisp.stdout.readline, b""):
                if self.END_SYMBOL in line: break
                index = 2 if line.startswith(self.SYMBOL) else 0
                line = line.rstrip()[index:].decode("utf-8")
                infos.append(line)
            
            return infos
        elif tag == 2:
            for line in iter(self.__lisp.stdout.readline, b""):
                if self.END_SYMBOL in line: break
                if line.startswith(self.SYMBOL): continue
                if self.NULL_SYMBOL in line: continue
                if line == b"\n": continue
                print("Lisp >>", line.rstrip().decode("utf-8"))
        elif tag == 3:
            self.__lisp.stdout.read() # 防止管道阻塞
        
        return True
    
    @property
    def mode(self):
        return self.__mode
    
    @mode.setter
    def mode(self, value):
        if not value: return True
        
        if value != self.COMPILE_MODE and value != self.INTERPRET_MODE:
            raise SetEvalModeError(f"{repr(value)} not exist!")
        
        self.__mode = value
        self.__setEvalMode()
        
        return True

    def initialize(self):
        """初始化"""
        try:
            if not self.__sm_file: raise Error()
            self.eval(f'(open-space "{self.__sm_file}")', self.E_MODE_BACKGROUND)
        except Exception as e:
            raise InitLISPENVError("LISP environment initialization error!!!")
    
    def finalize(self):
        """结束清理"""
        try:
            self.eval("(close-space)", self.E_MODE_BACKGROUND)
            self.eval("(exit)", self.E_MODE_BACKGROUND)
        except Exception as e:
            pass
        
        self.__lisp.terminate()
        self.__lisp.wait()
        
        SharedMemory.dealloc(self.__sm)
    
        return True

    def eval(self, syntax, mode):
        """求值计算"""
        if mode == self.E_MODE_FOREGROUND:
            syntax = f"(shasht:write-json {syntax} nil)\n{self.FEEDBACK}\n".encode("utf-8")
        else:
            syntax = f"{syntax}\n{self.FEEDBACK}\n".encode("utf-8")
        
        self.__lisp.stdin.write(syntax)
        self.__lisp.stdin.flush()
        
        if (mode == self.E_MODE_FOREGROUND) or (mode == self.E_MODE_FOREGROUND|self.E_MODE_BLOCK):
            self.__setBlockMode(False)
            infos = self.__readPIPE(1)
            if not infos: return ""
            
            target = infos[0]
            if target[:1] != self.EXCEPTION_FLAG:
                if len(infos) > 1:
                    return "\n".join(infos)
                try:
                    return json.loads(target)
                except Exception as e:
                    return target
            
            index = target.find(":")
            return self.EXCEPTION.get(target[:index], Error)("\n".join([target[index+2:]] + infos[1:]))
        elif (mode == self.E_MODE_BACKGROUND) or (mode == self.E_MODE_BLOCK) or (mode == self.E_MODE_BACKGROUND|self.E_MODE_BLOCK):
            self.__setBlockMode(False)
            self.__readPIPE(2)
        elif (mode == self.E_MODE_NONBLOCK) or (mode == self.E_MODE_BACKGROUND|self.E_MODE_NONBLOCK):
            self.__setBlockMode(True)
            self.__readPIPE(3)
        else:
            raise ModeNotExist(mode)
        
    def repl(self):
        if not self.mode: raise EvalModeError("Set the evaluation mode first")
        
        while True:
            code = input("py2lisp> ")
            if not code: continue
            if code.lower() == "quit": break
            if code.lower() == "exit": break
        
            checkRt = self.__syntaxCheck(code)
            if checkRt:
                print(checkRt)
            else:
                result = self.eval(code, self.E_MODE_FOREGROUND|self.E_MODE_BLOCK)
                print(result)
        
        return True


if __name__== "__main__":
    lisp = Py2Lisp("./lisp.core")
    lisp.mode = Py2Lisp.COMPILE_MODE
    
    lisp.initialize()
    lisp.repl()
    lisp.finalize()
