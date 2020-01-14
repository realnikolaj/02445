# -*- coding: utf-8 -*-
"""
Created on Tue Jan 14 12:23:22 2020

@author: Bruger
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

data = np.zeros((10,10,100,3))

for i in range(1,11):
    for j in range(1,11):
        temp = pd.read_csv("data/data{}_{}".format(i,j))
        temp = pd.DataFrame.to_numpy(temp)
        temp = temp[:,1:]
        data[i-1,j-1,:,:] = temp

                

#plt.plot(temp[:,0],temp[:,2])