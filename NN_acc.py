# -*- coding: utf-8 -*-
"""
Created on Fri Jan 17 21:55:01 2020

@author: rasmu
"""

import numpy as np

acc = np.load("test_acc_final.npy")
acc1 = np.load("test_acc_final1.npy")
acc2 = np.load("test_acc_final2.npy")
acc3 = np.load("test_acc_final3.npy")
acc4 = np.load("test_acc_final4.npy")
acc5 = np.load("test_acc_final5.npy")

#print(np.sum(acc[:,649]))
#print(np.sum(acc1[:,649]))
#print(np.sum(acc2[:,649]))
#print(np.sum(acc3[:,649]))
#print(np.sum(acc4[:,649]))
#print(np.sum(acc5[:,649]))

est_acc = np.array([66,72,73,72,69,71])

print(np.mean(est_acc))
print(np.std(est_acc))
