import pandas as pd
import numpy as np
import random

import queue
from threading import Thread, Event

def chunks(l, n):
    for i in range(0, len(l), n):
        yield l[i:i + n]
        
def enqueue(queue, stop, gen_func):
    gen = gen_func()
    while True:
        if stop.is_set():
            return

        queue.put(next(gen))

class data_generators():
    def __init__(self, bz, dataframes, preprocess):
        '''
        bz:
            batch size
        dataframes:
            dataframes for generators
            
        preprocess:
            preprocess function
            will receive a dataframe
            and should out put in the format
            [x1, x2], y, file/id 
        
        '''
        
        self.bz = bz
        
        self.train_dfs = dataframes[0]
        self.val_dfs = dataframes[1]
        
        self.preprocess = preprocess
        
    def get_train_idx(self):
        data_len = len(self.train_dfs)
        
        batch_num = data_len//self.bz

        batch_nth = 0

        select = np.arange(data_len)

        random.shuffle(select)

        while True:
            if batch_nth >= batch_num:
                batch_nth = 0
                random.shuffle(select)
            idxs = select[batch_nth*self.bz:(batch_nth+1)*self.bz]
            batch_nth += 1

            yield idxs
    
    def get_train_data(self):
        while True:
            idxs = self.train_idx_queue.get()
            select_list = []
            for idx in idxs:
                select_list.append(self.train_dfs.iloc[idx])

            train_list = pd.DataFrame(select_list).sample(frac=1)
            x, y, _ = self.preprocess(train_list, aug=True)

            yield x,  y
    
    def start_train_threads(self):
        '''
        jobs:number of threads
        '''
        
        self.train_queue = queue.Queue(maxsize = 3)
        self.train_idx_queue =queue.Queue(maxsize = 10)
        self.jobs = 1

        ### for stop threads after training ###
        self.events=[]

        ### enqueue train index ###
        event = Event()
        thread = Thread(target = enqueue, args = (self.train_idx_queue, event, self.get_train_idx))
        thread.start()
        self.events.append(event)

        ### enqueue train batch ###
        for i in range(self.jobs):
            event = Event()
            thread = Thread(target = enqueue,args = (self.train_queue, event, self.get_train_data))
            thread.start()
            self.events.append(event)

        
    #### val ####
    def load_val(self):
        val_list = self.val_dfs.sample(frac=1)
        x, y, _ = self.preprocess(val_list)

        self.val_x = list(chunks(x, self.bz))
        self.val_y = list(chunks(y, self.bz))

        self.val_len = len(self.val_y)
    
    def iter_val(self):
        for i in range(self.val_len):
            yield self.val_x[i], self.val_y[i], len(self.val_y[i])
    
    #### test ####
    def get_test_data(self):
        test_list = pd.concat(self.test_dfs)
        test_list = chunks(test_list, self.bz)

        for data in test_list:
            x, y, files = self.preprocess(data, self.num_class)

            yield x, y, files