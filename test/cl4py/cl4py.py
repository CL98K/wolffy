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


class Future():
    """Future"""
    def __init__(self, space, offset):
        self.__space = space
        self.__offset = offset
        self.__success = False
        self.__result = None
        self.__dgauge = False
        
    def done(self):
        return self.__success
    
    def get_result(self, timeout=None):
        if self.__dgauge: return self.__result
        
        timeout = timeout if isinstance(timeout, (int, float)) else 2 ** 32
        stime = time.time()
        
        while True:
            if SharedMemory.done(self.__space, self.__offset):
                self.__result = SharedMemory.read(self.__space, self.__offset)
                self.__success = True
                self.__dgauge = True
                break
            if (time.time() - stime) >= timeout: break
            time.sleep(0.1)
        
        return self.__result
    
    def set_result(self, data):
        self.__result = data
        self.__dgauge = True
        return True
    
    def set_status(self, flag):
        self.__success = flag
        return True
            

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
            
            iblock, (ipoint, limit) = SharedMemory.getBlock(self.env._Lisp__sm, self.env._Lisp__setFuture)
            future = None if tag else Future(self.env._Lisp__sm, ipoint)
            if future: self.env._Lisp__future[iblock] = future
            
            offset, size, _ = SharedMemory.write([args, kwargs], self.env._Lisp__sm, ipoint)
            self.env.eval(f"(pycall #'{self.pname}:{syntax} :offset {offset} :size {size} :ipoint {ipoint} :limit {limit})", Lisp.E_MODE_BACKGROUND|Lisp.E_MODE_BLOCK if tag else Lisp.E_MODE_BACKGROUND|Lisp.E_MODE_NONBLOCK)
            return SharedMemory.read(self.env._Lisp__sm, ipoint) if tag else future
        
        symbols = self.env.eval(f"(let ((col nil)) (do-external-symbols (s '{self.pname}) (push (symbol-name s) col)) col)", Lisp.E_MODE_FOREGROUND)

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


class Benchmark():
    """Benchmark Test"""
    def __inter_benchmark(self):
        """Interactive benchmark"""
        data = {'_root': {'premise': '1'}, \
                'FLIGHT_DYNAMICS': {'PLAN_ARR_TIME_CUR': '', 'ALT_ARR_TIME_CUR': '', 'AIPORTIATA_PREV': '', \
                                    'REAL_DEP_TIME_PREF': '', 'PLAN_ARR_TIME_FL': '', 'ALT_ARR_TIME_FL': '', \
                                    'ARR_ROUTEID': '', 'DELAY_TIME': '', 'FLIGHT_STATE_CODE': 'PUBLISH', 'IMPACTED_ROUTEID': '', \
                                    'REAL_ARR_TIME_CUR': '', 'STATUS_PREV_FLT': '', 'premise': '1', 'IMPACTED_AIRPORT': '', \
                                    'INNER_ABN_RSN': '', 'ETA_CUR': '', 'DEP_ROUTEID': '', 'PLAN_DEP_TIME_CUR': '', \
                                    'ALT_DEP_TIME_CUR': '', 'DTL_STATUS': 'XK', 'OUT_ABN_STATUS': '', 'AIRPORTIATA_FL': '', \
                                    'ETD_CUR': '', 'OUTER_ABN_RSN': '', 'REAL_ARR_TIME_FL': '', 'IN_ABN_STATUS': '', \
                                    'REAL_DEP_TIME_CUR': '', 'PLAN_DEP_TIME_PREV': '', 'ALT_DEP_TIME_PREV': ''}, \
                    'ROUTE_INFO': {'ROUTE_ID1': '616824585', 'premise': '1', 'ROUTE_ID2': '616824586', 'ROUTE_ID3': '616824587', \
                                   'ROUTE_ID4': '', 'ROUTE_ID5': '', 'ID': '616842338', 'BIZKEY': '20240318-3U3457-O-W/Z-202403180710'}, \
                    'FLIGHT_SCHEDULE': {'premise': '1', 'FLIGHT_DATE': '2024-03-18', 'REAL_DATE': ''}, \
                    'ROUTE_AIRLINE': {'ALT_LANDING': '', 'PLAN_TAKEOFF_2': '20240318 11:30:00', 'PLAN_TAKEOFF_3': '', \
                                      'REAL_TAKEOFF': '', 'REAL_LANDING_2': '', 'REAL_LANDING_3': '', 'premise': '1', \
                                      'PLAN_TAKEOFF': '20240318 07:10:00', 'ISALTERAIRPORT_2': '0', 'ISALTERAIRPORT_3': '0', \
                                      'PLAN_LANDING_2': '20240318 10:45:00', 'PLAN_LANDING_3': '20240318 13:05:00', 'REAL_LANDING': '', \
                                      'ROUTEORDER_2': '2', 'ROUTEORDER_3': '3', 'ISALTERAIRPORT': '0', 'STAY_TYPE': '', 'PLAN_LANDING': '', \
                                      'AIRPORTIATA_2': 'XXX', 'AIRPORTIATA_3': 'NXX', 'ROUTEORDER': '1', 'ALT_TAKEOFF_2': '', \
                                      'ALT_TAKEOFF_3': '', 'REGION_2': 'XXXX', 'REGION_3': 'XXXX', 'REGION_4': 'XXXX', \
                                      'AIRPORTIATA': 'XXX', 'ALT_TAKEOFF': '', 'REGION': 'XXXX', 'ALT_LANDING_2': '', 'ALT_LANDING_3': '', \
                                      'REAL_TAKEOFF_2': '', 'REAL_TAKEOFF_3': ''}, \
                    'SEAT_INFO': {'CRAFT': '', 'SEATNAME_2': '', 'SEATNAME_3': '', 'SEATNAME_4': '', 'SEATNAME_5': '', 'DRAGED_2': '', \
                                  'DRAGED_3': '', 'DRAGED_4': '', 'REAL_ARV_TIME': '', 'SEATORDER': '', 'OCCUPYTYPE': '', 'DRAGED_5': '', \
                                  'SEATNAME': '', 'PLAN_ARV_TIME_2': '', 'premise': '1', 'PLAN_ARV_TIME_3': '', 'REAL_DEP_TIME_2': '', \
                                  'PLAN_ARV_TIME_4': '', 'REAL_DEP_TIME_3': '', 'DRAGED': '', 'PLAN_ARV_TIME_5': '', 'REAL_DEP_TIME_4': '', \
                                  'REAL_DEP_TIME_5': '', 'PLAN_ARV_TIME': '', 'REAL_DEP_TIME': '', 'PLAN_DEP_TIME_2': '', 'PLAN_DEP_TIME_3': '', \
                                  'PLAN_DEP_TIME_4': '', 'PLAN_DEP_TIME_5': '', 'PLAN_DEP_TIME': '', 'CRAFT_2': '', 'CRAFT_3': '', 'CRAFT_4': '', \
                                  'CRAFT_5': '', 'REAL_ARV_TIME_2': '', 'SEATORDER_2': '', 'OCCUPYTYPE_2': '', 'REAL_ARV_TIME_3': '', \
                                  'SEATORDER_3': '', 'OCCUPYTYPE_3': '', 'REAL_ARV_TIME_4': '', 'SEATORDER_4': '', 'OCCUPYTYPE_4': '', \
                                  'REAL_ARV_TIME_5': '', 'SEATORDER_5': '', 'OCCUPYTYPE_5': ''}, \
                    'SHARED_FLIGHT': {'Share_Flight_NO': '9138,5139', 'premise': '1', 'BIZKEY': '', 'AIRLINE_NAME': 'CZ,MF', 'AIRLINE_ID': '57,73'}, \
                    'LOADINGINFO': {'premise': '1', 'Passenger_Num': '', 'Mail_Ttl_Weight': '', 'Luggage_Ttl_Weight': '', \
                                    'Package_Ttl_Weight': '', 'Package_Method': '', 'Luggage_Num': '', 'Package_Num': '', 'Mail_Num': ''}, \
                    'SPECIAL_PSNGR_TAG': {'premise': '1', 'DISBL_AMT': '', 'HASVIP': '0', 'HAS_DISBL': '', 'VIP_AMT': ''}, \
                    'AIRLINE_INFO': {'AIRLINE_NAME': 'XX', 'AIRLINE_ID': 'XX', 'premise': '1'}, \
                    'DISPATCH_INFO': {'PLAN_START_TIME': '', 'ALT_START_TIME': '', 'premise': '1', 'ACRSL_ID': '', 'ACRSL_NAME': '', \
                                      'REAL_END_TIME': '', 'DISPATCH_UNIQ_ID': '', 'PROC_STAGE': '', 'REGION_ID': 'DMST', \
                                      'PLAN_END_TIME': '', 'ALT_END_TIME': '', 'BUILDING_INFO': 'XX', 'REAL_START_TIME': '', \
                                      'DISPATCH_TYPE': ''}, 'FLIGHT_TASK': {'TASK_TYPE': 'W/Z', 'premise': '1'}, \
                    'BUILDING_INFO': {'premise': '1', 'DMST_BUILDING': 'XXX', 'GATE_BYAUTO': '', 'INTER_BUILDING': '', 'GATE_NAME': '', 'GATE_ID': ''}, \
                    'ACDM_MSG_BASIC': {'premise': '1', 'SNDR': 'XXXX', 'SUBTYPE': 'MODIFY', 'TYPE': 'DYNFLIGHT', 'RCVR': '', 'SENDTIME': '20240318 07:09:28', 'SEQN': '1710716968612'}, \
                    'FLIGHT_INFO': {'Flight_NO': 'XXXX', 'premise': '1', 'DEP_FLIGHT_ID': '15151687', 'ID': '15151687', \
                                    'IS_ARR_FLIGHT': '0', 'BIZKEY': 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX', 'ARR_FLIGHT_ID': ''}, \
                    'PEOPLE_INFO': {'premise': '1', 'P_ID': '', 'P_NAME': '', 'P_JOBNUMBER': '', 'P_TEAMID': '', 'P_DEPTID': ''}, \
                    'AIRLINE_CRAFT': {'CRAFT_NO': 'XXXX', 'premise': '1', 'CRAFTMODEL': 'XXXX', 'AIRLINE_NAME': 'XX', 'ENGINE_NO': '', 'ENGINE_TYPE': ''}}

        stime = time.time()

        for i in range(1):
            self.test_interface(data, block=False)
        
        print(time.time() - stime)
        
        return True
    
    def benchmark(self):
        self.__inter_benchmark()
        
        return True


class Lisp(Benchmark):
    """基于共享内存的 Python 调用 Lisp"""
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
    
    def __init__(self, core=None, parallel=8):
        """core: image 文件路径"""
        if not os.path.exists(core): raise CoreNotExist(f"{repr(core)} not exist!")
        
        self.__max_space = 65536 * 1
        self.__block = 1024 * 8
        if (parallel * self.__block) > self.__max_space:
            parallel = int(self.__max_space / self.__block)
        
        self.__sm, self.__sm_file = SharedMemory.namedAlloc(self.__max_space, parallel) #创建交互空间
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
            if future: self.__future[iblock] = (int(time.time()), future)
            
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
            raise AttributeError(f"'Lisp' object has no attribute '{name}'")

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
                for inode, (_, future) in sorted(self.__future.items(), key=lambda x: x[-1][0]): #等待所有 future 对象完成
                    self.__lisp.stdout.read()
                    future.get_result()
                    del self.__future[inode]
                
                self.__io_mode = True
                self.__lisp.stdout.read()
                os.set_blocking(self.__lisp.stdout.fileno(), self.__io_mode)
        
        return True
    
    def __setFuture(self, inode, data):
        """设置 Future"""
        if inode in self.__future:
            self.__future[inode][-1].set_result(data)
            self.__future[inode][-1].set_status(True)
            del self.__future[inode]
        
        return True

    def __syntaxCheck(self, string):
        """语法检查"""
        if string.count("(") != string.count(")"):
            return SyntaxError(f"unexpected EOF while parsing: {string}")
        
    def __writePIPE(self, data):
        """写数据"""
        self.__lisp.stdin.write(data)
        self.__lisp.stdin.flush()
        
        return True
    
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

        if (mode == self.E_MODE_FOREGROUND) or (mode == self.E_MODE_FOREGROUND|self.E_MODE_BLOCK):
            self.__setBlockMode(False)
            self.__writePIPE(syntax)
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
            self.__writePIPE(syntax)
            self.__readPIPE(2)
        elif (mode == self.E_MODE_NONBLOCK) or (mode == self.E_MODE_BACKGROUND|self.E_MODE_NONBLOCK):
            self.__setBlockMode(True)
            self.__writePIPE(syntax)
            self.__readPIPE(3)
        else:
            raise ModeNotExist(mode)
        
    def repl(self):
        if not self.mode: raise EvalModeError("Set the evaluation mode first")
        
        while True:
            code = input("Lisp> ")
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
    lisp = Lisp("./lisp.core")
    lisp.mode = Lisp.COMPILE_MODE
    
    lisp.initialize()
    lisp.repl()
    lisp.finalize()
