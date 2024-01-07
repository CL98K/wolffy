#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import os
import json
import time
import atexit
import subprocess


class Error(Exception): pass
class CoreNotExist(Exception): pass
class EvalModeError(Exception): pass
class SetEvalModeError(Exception): pass

class Benchmark():
    def __test(self):
        cnt = 10
        number = 10000
        times = []
        code = "1"
        
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
    
    FEEDBACK = r"#\Nul"
    EXCEPTION_FLAG = "!"
    EXCEPTION = {"!INDEX-TOO-LARGE-ERROR": IndexError,
                 "!TYPE-ERROR": TypeError,
                 "!READER-IMPOSSIBLE-NUMBER-ERROR": ZeroDivisionError,
                 "!UNBOUND-VARIABLE":  NameError,}
    
    def __init__(self, core=None):
        """core: image 文件路径"""
        if not os.path.exists(core): raise CoreNotExist(f"{repr(core)} not exist!")
        self.__lisp = subprocess.Popen(core, shell=False, bufsize=-1, stdin=subprocess.PIPE, stdout=subprocess.PIPE)
        self.__mode = None
        
        def exitHook(): self.close()
        atexit.register(exitHook)

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

    def close(self):
        try:
            self.eval(f'(exit)')
        except Exception as e:
            pass
        
        self.__lisp.terminate()
        self.__lisp.wait()
        
        return True
        
    def eval(self, string, repl=False, parse=False):
        """求值计算"""
        self.__lisp.stdin.write(f"(shasht:write-json {string} nil)\n{self.FEEDBACK}\n".encode("utf-8") if parse else f"{string}\n{self.FEEDBACK}\n".encode("utf-8"))
        self.__lisp.stdin.flush()
        
        result = []
        for line in iter(self.__lisp.stdout.readline, b""):
            line = line.rstrip()[2:].decode("utf-8")
            if "Nul" in line: break
            result.append(line)
        
        if not parse: return True
        if not result: return ""
        
        target = result[0]
        if target[:1] != self.EXCEPTION_FLAG:
            if len(result) > 1:
                return "\n".join(result)
            return json.loads(target)
        
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
