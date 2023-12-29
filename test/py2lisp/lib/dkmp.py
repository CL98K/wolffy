#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import os
import mmap
import time
import pickle
import tempfile
import multiprocessing as mp
from multiprocessing import Process, Queue, Lock


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
            
        
class DKMP():
    def __init__(self, workers=None, qsize=100, indicator=False):
        self.workers = workers if workers else mp.cpu_count()
        self.indicator = indicator
        self._queue = Queue(qsize)
        self._lock = Lock()
        
    def __enter__(self):
        if self.indicator:
            self._time = time.time()
        return self
    
    def __exit__(self, exc_type, exc_value, traceback):
        if self.indicator:
            print(time.time() - self._time)
        return True
    
    def __exec(self, flag, datas, funcs, handle, queue, lock):
        if flag == 1:
            rets = funcs(datas)
        elif flag == 2:
            rets = [func(datas) for func in funcs]
        elif flag == 3:
            rets = [func(data) for func, data in zip(funcs, datas)]
        
        if not handle: return True
        
        rets = pickle.dumps(rets)
        length = bytes(f"{len(rets)}\x00", encoding="utf-8")
        
        lock.acquire()
        
        handle.seek(0 if queue.qsize() == 0 else queue.get())
        
        handle.write(length)
        handle.write(rets)
        
        queue.put(handle.tell())
        
        lock.release()
        
        return True
    
    def _wait(self, futures):
        for future in futures:
            future.start()
            
        for future in futures:
            future.join()

        return True
    
    def map(self, func, data, handle=None):
        """func 调用方法 data 可迭代数据 handle 共享内存句柄用于搜集结果""" 
        length = len(data)
        if length == 0: return True
        
        point, futures = int(length / self.workers), []
        
        for index in range(0, length, point):
            futures.append(Process(target=self.__exec, args=(1, data[index: index+point], func, handle, self._queue, self._lock)))
        
        self._wait(futures)
        
        return True
    
    def parallel(self, funcs, data, handle=None):
        """funcs 调用方法列表 data 可迭代数据 handle 共享内存句柄用于搜集结果""" 
        length = len(funcs)
        if length == 0: return True

        futures = []
        funcGroup = [[] for _ in range(self.workers)]
        
        for index, func in enumerate(funcs):
            funcGroup[index % self.workers].append(func)
            
        for funcs in funcGroup:
            futures.append(Process(target=self.__exec, args=(2, data, funcs, handle, self._queue, self._lock)))
        
        self._wait(futures)
        
        return True
    
    def parallelX(self, funcs, datas, handle=None):
        """funcs 调用方法列表 datas 可迭代数据列表 handle 共享内存句柄用于搜集结果"""
        length = len(funcs)
        if length == 0: return True

        futures = []
        funcGroup = [[] for _ in range(self.workers)]
        dataGroup = [[] for _ in range(self.workers)]
        
        for index, (func, data) in enumerate(zip(funcs, datas)):
            funcGroup[index % self.workers].append(func)
            dataGroup[index % self.workers].append(data)
            
        for funcs, data in zip(funcGroup, dataGroup):
            futures.append(Process(target=self.__exec, args=(3, data, funcs, handle, self._queue, self._lock)))
        
        self._wait(futures)
    
        return True
        

if __name__ == "__main__":
    def test(data):
        return data
    
    handle = SharedMemory.alloc(65536 * 2100)
    
    with DKMP(4, indicator=True) as fp:
        fp.map(test, [1,2,3,4], handle)

    for x in SharedMemory.readX(handle):
        print(x, type(x))
    
    SharedMemory.dealloc(handle)
