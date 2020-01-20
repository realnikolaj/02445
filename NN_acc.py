# -*- coding: utf-8 -*-
"""
Created on Fri Jan 17 21:55:01 2020

@author: rasmu
"""

import numpy as np

est_acc = []

for i in range(1,25):
    temp = np.load("test_acc_final{}.npy".format(i))
    est_acc.append(temp[:,649])
    

acc = np.load("test_acc_final.npy")

est_acc.append(acc[:,649])

for i in range(25):
    est_acc[i] = np.sum(est_acc[i])
    

print(np.mean(est_acc))
print(np.std(est_acc))