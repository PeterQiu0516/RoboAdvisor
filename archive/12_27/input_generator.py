import numpy as np
import pandas as pd
import random
from openpyxl import Workbook
from openpyxl import load_workbook


def sort_index(raw,target):
    tmp = raw.copy();
    for i in range(0, 790):
        raw[i] = tmp[int(target[i])]

wb = Workbook()
ws = wb.active
#specify risk level, can be obtained
risk_level = random.sample(range(1,10),1)

#ws_ISIN = pd.read_csv('ISIN.csv',header = None, low_memory=False)
ws_mean = pd.read_csv('mean.csv',header = None, low_memory=False)
ws_history = pd.read_csv('input.csv', header = None, low_memory=False)

rate_mean = np.zeros(500)
mean_index = np.zeros(500)
for i in range(50*(risk_level-1), 50*(risk_level)):
    rate_mean[i] = int(ws_mean.iloc[i,1])
    
#ISIN = [];
for i in range(0,500):
    mean_index[i] = ws_mean.iloc[i,0]
    #ISIN.append(ws_ISIN.iloc[i,0])

#sort mean and the corresponding index and ISIN
mean_sorted = np.zeros((50,2))
mean_sorted[:,0] = np.argsort(rate_mean)
mean_sorted[:,1] = np.sort(rate_mean)
sort_index(mean_index, mean_sorted[:,0])
#sort_index(ISIN, mean_sorted[:,0])

#get the historical data, num can be specified
num = 5
ISIN = []
for i in range(0,num):
    fund_id = mean_index[i]
    ISIN.append(ws.iloc[0,fund_id*4+1])
    
    

#.to_csv('C:/Users/qiuch/Desktop/portfolio_proj/input/input_generated.csv',index=False,header=False)