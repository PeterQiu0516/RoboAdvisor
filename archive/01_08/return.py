import pandas as pd
import numpy as np

def weighted_avg(in_put,weight):
    if np.ndim(in_put) == 1:
        return np.sum(in_put*weight)
    else:
        size = in_put.shape[0]
        tmp = np.zeros(size)
        for i in range(0,size):
            tmp[i] = np.sum(in_put[i,:]*weight)
        return tmp

ws = pd.read_csv("weight.csv",header = 0)

###initialization
data = np.array(ws)
nav = data[:-1,1:] #here -1: space for weights
#print(nav)
weight = data[-1,1:]
date = nav.shape[0]
fund_num = nav.shape[1]
data_all = np.zeros((10,date)) #here minus 1: space for weights
data_all[0,0:5] = weight

###---daily returns
daily_returns = np.zeros((date - 1,fund_num))
for j in range(0,fund_num):
    daily_returns[:,j] = np.diff(nav[:,j]) / nav[:-1,j]
    #print(daily_returns[:,j])
data_all[9,:-1] = weighted_avg(daily_returns,weight)
#print(data_all[9,:-1])

###---Step 1:accumulative daily returns
accu_daily_returns = np.zeros((date - 1,fund_num))
for j in range(0,fund_num):
    for i in range(0,date - 1):
        accu_daily_returns[i,j] = float(nav[i+1,j] / nav[0,j] - 1)
data_all[1,:-1] = weighted_avg(accu_daily_returns,weight)

###---Step 2:annualized return & std
annual_return = ((1+(data_all[1,-2]))**(1/(date/252)) - 1) #data_all[1,-2] is the second last number of the data_all row, which is the accumulative rate
data_all[2,0] = annual_return
#print(annual_return)
annual_std = np.std(data_all[9,:-1]) * np.sqrt(252)
data_all[2,2] = annual_std
#print(annual_std)

###---Step 3: Chosen date return
chosen_return = np.zeros((48,fund_num))
dis = int(np.floor(date/48))
#print(dis)
for j in range(0,5):
    for i in range(0,48):
        if i == 47: 
            upper_bound = date - 1
        else:
            upper_bound = (i+1) * dis
        chosen_return[i,j] = float(nav[upper_bound,j]/nav[i*dis,j] - 1)
data_all[3,:48] = weighted_avg(chosen_return,weight)
#print(data_all[3,:48])

###---Step 4: Calculate date for Step 3

###---Step 5: Recent value
within_0 = np.zeros(fund_num)
for i in range(0,fund_num):
    within_0[i] = float(nav[date-1,j] / nav[date-1-5,j] - 1) #5 weekdays
data_all[5,0] = weighted_avg(within_0,weight)#within a week

within_1 = np.zeros(fund_num)
for i in range(0,fund_num):
    within_1[i] = float(nav[date-1,j] / nav[date-1-21,j] - 1) #21 month days
data_all[5,1] = weighted_avg(within_1,weight)#within a month

within_2 = np.zeros(fund_num)
for i in range(0,fund_num):
    within_2[i] = float(nav[date-1,j] / nav[date-1-63,j] - 1) #63 3months
data_all[5,2] = weighted_avg(within_2,weight)#within 3 months

within_3 = np.zeros(fund_num)
for i in range(0,fund_num):
    within_3[i] = float(nav[date-1,j] / nav[date-1-126,j] - 1) #126 half a year
data_all[5,3] = weighted_avg(within_3,weight)#within half a year

within_4 = np.zeros(fund_num)
for i in range(0,fund_num):
    within_4[i] = float(nav[date-1,j] / nav[date-1-252,j] - 1) #252 days a year
data_all[5,4] = weighted_avg(within_4,weight)#within a year

within_5 = np.zeros(fund_num)
for i in range(0,fund_num):
    within_5[i] = float(nav[date-1,j] / nav[date-1-(date%252),j] - 1) #this year
data_all[5,5] = weighted_avg(within_5,weight)#this year

#print(data_all[5,0:5])

###---Step 6: Sharpe Ratio

###---Step 7: Moving Volatility
moving_std = np.zeros(date-1)
for i in range(0,date-1):
    moving_std[i] = np.std(data_all[9,:(i+2)]) * np.sqrt(252)
data_all[7,:-1] = moving_std
print(data_all[7,:-1])

###---Step 8: Correlation Matrix
