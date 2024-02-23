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


class Error(Exception): pass
class CoreNotExist(Exception): pass
class EvalModeError(Exception): pass
class SetEvalModeError(Exception): pass
class InitLISPENVError(Exception): pass

class SharedMemory():
    MMAP_FPATH_MAP = {}
    
    @classmethod
    def alloc(cls, length):
        """申请共享内存"""
        fp = tempfile.TemporaryFile(mode="a+b")
        
        fp.write(b"\x00" * length)
        mm = mmap.mmap(fp.fileno(), length, flags=mmap.MAP_SHARED, prot=mmap.PROT_WRITE|mmap.PROT_READ)
        
        return mm, None

    @classmethod
    def namedAlloc(cls, length):
        """建议只作为调试使用,否则需要手动清理内存映射文件"""
        path = "./_smp"
        filepath = "{0}/{1}.sm".format(path, os.getpid())
        
        if not os.path.exists(path):
            os.makedirs(path)
        
        fp = open(filepath, "a+b")
        fp.write(b"\x00" * length)
        mm = mmap.mmap(fp.fileno(), length, flags=mmap.MAP_SHARED, prot=mmap.PROT_WRITE|mmap.PROT_READ)
        
        cls.MMAP_FPATH_MAP[mm] = (fp, filepath)
    
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
    def write(cls, data, handle, offset=0):
        """写 MMAP 文件"""
        data = pickle.dumps(data)
        length = bytes(f"{len(data)}\x00", encoding="utf-8")
        
        handle.seek(offset)
        handle.write(length + data)
        
        return handle.tell()
    
    @classmethod
    def read(cls, handle, offset=0):
        """读 MMAP 文件"""
        index = handle.find(b"\x00", offset)
        if index == -1: return None
    
        length = int(str(handle[0:index], encoding="utf-8"))
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


class Benchmark():
    def __test(self):
        cnt = 10
        number = 10000
        times = []
        code = "1111111111"
        
        for _ in range(cnt):
            stime = time.time()
            
            for _ in range(number):
                self.eval(code)
                
            times.append(time.time() - stime)
            
        return int(number * cnt / sum(times))
    
    def benchmark(self):
        print("------------------ Py2Lisp Bemchmark ------------------")
        
        self.mode = self.COMPILE_MODE
        print("Compile Mode: ", self.__test())
        
        self.mode = self.INTERPRET_MODE
        print("Interpret Mode: ", self.__test())
        
        return True


class Py2Lisp(Benchmark):
    COMPILE_MODE = ":compile"
    INTERPRET_MODE = ":interpret"
    
    SYMBOL = b"* "
    FEEDBACK = r"#\Nul"
    EXCEPTION_FLAG = "!"
    EXCEPTION = {"!INDEX-TOO-LARGE-ERROR": IndexError,
                 "!TYPE-ERROR": TypeError,
                 "!READER-IMPOSSIBLE-NUMBER-ERROR": ZeroDivisionError,
                 "!UNBOUND-VARIABLE":  NameError,}
    
    def __init__(self, core=None):
        """core: image 文件路径"""
        if not os.path.exists(core): raise CoreNotExist(f"{repr(core)} not exist!")
        
        self.__sm, self.__sm_file = SharedMemory.namedAlloc(65536 * 1) #创建交互空间
        self.__lisp = subprocess.Popen(core, shell=False, bufsize=-1, stdin=subprocess.PIPE, stdout=subprocess.PIPE)
        self.__mode = None
        
        def exitHook(): self.finalize()
        atexit.register(exitHook)
        
    def __getattr__(self, name):
        def __call(syntax, args, kwargs):
            SharedMemory.write([args, kwargs], self.__sm)
            self.eval(f"(pycall #'{lname})", parse=False)
            return SharedMemory.read(self.__sm)
        
        lname = f"*{name[2:]}*" if name.startswith("V_") else name
        lname = f"+{lname[2:]}+" if lname.startswith("C_") else lname
        lname = lname.replace("_", "-")
        
        describe = self.eval(f"(describe '{lname})", parse=True)
        package = self.eval(f"(find-package '{lname})", parse=True)
        
        if "names a compiled function" in describe:
            self.__dict__[name] = lambda *args, **kwargs: __call(lname, args, kwargs)
            self.__dict__[name].__doc__ = describe
        elif "names a macro" in describe:
            self.__dict__[name] = lambda *args, **kwargs: __call(lname, args, kwargs)
            self.__dict__[name].__doc__ = describe
        elif "names a special variable" in describe:
            tag = "Value: "
            self.__dict__[name] = describe[describe.find(tag) + len(tag):]
        elif "names a constant variable" in describe:
            tag = "Value: "
            self.__dict__[name] = describe[describe.find(tag) + len(tag):]
        elif package and package != "NIL":
            pass
        else:
            raise AttributeError(f"'Py2Lisp' object has no attribute '{name}'")

        return self.__dict__[name]
        
    def __setEvalMode(self):
        """设置求值模式"""
        return self.eval(f"(setf *evaluator-mode* {self.__mode})")

    def __syntaxCheck(self, string):
        """语法检查"""
        if string.count("(") != string.count(")"):
            return SyntaxError(f"unexpected EOF while parsing: {string}")
    
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
            self.eval(f'(open-space "{self.__sm_file}")')
        except Exception as e:
            raise InitLISPENVError("LISP environment initialization error!!!")
    
    def finalize(self):
        """清理"""
        try:
            self.eval('(close-space)')
            self.eval("(exit)")
        except Exception as e:
            pass
        
        self.__lisp.terminate()
        self.__lisp.wait()
        
        SharedMemory.dealloc(self.__sm)
        
    def eval(self, string, repl=False, parse=False):
        """求值计算"""
        self.__lisp.stdin.write(f"(shasht:write-json {string} nil)\n{self.FEEDBACK}\n".encode("utf-8") if repl else f"{string}\n{self.FEEDBACK}\n".encode("utf-8"))
        self.__lisp.stdin.flush()
        
        if parse:
            result = []
            for line in iter(self.__lisp.stdout.readline, b""):
                index = 2 if line.startswith(self.SYMBOL) else 0
                line = line.rstrip()[index:].decode("utf-8")
                if "Nul" in line: break
                result.append(line)
        else:
            for line in iter(self.__lisp.stdout.readline, b""):
                if b"Nul" in line: break
                if line.startswith(b"* "): continue
                print(line.rstrip().decode("utf-8"))
            return True

        if not result: return ""
        
        target = result[0]
        if target[:1] != self.EXCEPTION_FLAG:
            if len(result) > 1:
                return "\n".join(result)
            try:
                return json.loads(target)
            except Exception as e:
                return target
        
        index = target.find(":")
        exception = self.EXCEPTION.get(target[:index], Error)("\n".join([target[index+2:]] + result[1:]))
        
        if repl: return exception
        raise exception
        
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
                result = self.eval(code, True, True)
                print(result)
        
        return True


if __name__== "__main__":
    lisp = Py2Lisp("./sbcl.core")
    lisp.mode = Py2Lisp.COMPILE_MODE
    
    lisp.benchmark()
    lisp.repl()
    
    lisp.close()
