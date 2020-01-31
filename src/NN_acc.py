# -*- coding: utf-8 -*-
"""
Created on Fri Jan 17 21:55:01 2020

@author: rasmu
"""

import numpy as np

est_acc = []

for i in range(1,30):
    temp = np.load("test_acc_final{}.npy".format(i))
    est_acc.append(temp[:,649])
    

acc = np.load("test_acc_final.npy")

est_acc.append(acc[:,649])

for i in range(30):
    est_acc[i] = np.sum(est_acc[i])
    

mu = np.mean(est_acc)
sigma = np.std(est_acc)


CI = [mu+1.96*sigma/np.sqrt(30),mu-1.96*sigma/np.sqrt(30)]

