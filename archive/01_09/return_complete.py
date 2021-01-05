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
data_all[2, 0] = float("%.2f" % (annual_return*100))
#print(annual_return)
annual_std = np.std(data_all[9,:-1]) * np.sqrt(252)
data_all[2, 2] = float("%.2f" %(annual_std*100))
#print(annual_std)

import math
    ## Sharpe Ratio
    SR = data_all[9].mean() / data_all[9].std()*math.sqrt(252)
    data_all[2, 1]=float("%.2f" %SR)
    data_all[6, 0] = SR

    ## 最大回撤
    # index_j = np.argmax(np.maximum.accumulate(data_all[9]) - data_all[9])  # 结束位置
    # index_i = np.argmax(data_all[9][:index_j])  # 开始位置
    # d = data_all[9][index_j] - data_all[9][index_i]  # 最大回撤
    # data_all[2, 3] = float("%.2f" % (abs(d)*100))

    def prices(returns, base):
        # Converts returns into prices
        s = [base]
        for i in range(len(returns)):
            s.append(base * (1 + returns[i]))
        return np.array(s)

    def dd(returns, tau):
        # Returns the draw-down given time period tau
        values = prices(returns, 100)
        pos = len(values) - 1
        pre = pos - tau
        drawdown = float('+inf')
        # Find the maximum drawdown given tau
        while pre >= 0:
            dd_i = (values[pos] / values[pre]) - 1
            if dd_i < drawdown:
                drawdown = dd_i
            pos, pre = pos - 1, pre - 1
        # Drawdown should be positive
        return abs(drawdown)

    def max_dd(returns):
        # Returns the maximum draw-down for any tau in (0, T) where T is the length of the return series
        max_drawdown = float('-inf')
        for i in range(0, len(returns)):
            drawdown_i = dd(returns, i)
            if drawdown_i > max_drawdown:
                max_drawdown = drawdown_i
        # Max draw-down should be positive
        return abs(max_drawdown)
    maxdd_tmp=max_dd(data_all[9])
    data_all[2, 3] = float("%.2f" % (maxdd_tmp * 100))


## 95% VAR
    d_sort=data_all[9,:-1].copy()
    d_sort.sort()
    va=d_sort[int((date-1)*0.05)]
    data_all[2, 4] = float("%.2f" % (va*100))

    t2=[]
    t2.append(data_all[2, 0])
    t2.append(data_all[2, 1])
    t2.append(data_all[2, 2])
    t2.append(data_all[2, 3])
    t2.append(data_all[2, 4])


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
date_g = pd.read_csv("complete.csv", header=None).astype(str)
    ##48个日期
    date_tmp = []
    for i in range(0, 47):
        date_tmp.append(date_g[0][i * int(date / 48) + 1])
    date_tmp.append(date_g[0][date])
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
##Sortino Ratio
    def lpm(returns, threshold, order):
        # This method returns a lower partial moment of the returns
        # Create an array he same length as returns containing the minimum return threshold
        threshold_array = np.empty(len(returns))
        threshold_array.fill(threshold)
        # Calculate the difference between the threshold and the returns
        diff = threshold_array - returns
        # Set the minimum of each to 0
        diff = diff.clip(min=0)
        # Return the sum of the different to the power of order
        return np.sum(diff ** order) / len(returns)
    def sortino_ratio(er, returns, rf, target=0):
        return (er - rf) / math.sqrt(lpm(returns, target, 2))
    data_all[6, 1]=sortino_ratio(data_all[9].mean(), data_all[9], 0, 0)

    ##Sterling Ratio
    def average_dd(returns, periods):
        # Returns the average maximum drawdown over n periods
        drawdowns = []
        for i in range(0, len(returns)):
            drawdown_i = dd(returns, i)
            drawdowns.append(drawdown_i)
        drawdowns = sorted(drawdowns)
        total_dd = abs(drawdowns[0])
        for i in range(1, periods):
            total_dd += abs(drawdowns[i])
        return total_dd / periods
    def sterling_ration(er, returns, rf, periods):
        return (er - rf) / average_dd(returns, periods)
    data_all[6, 2]=sterling_ration(data_all[9].mean(),data_all[9],0,10)

    ##Calmar Ratio
    def calmar_ratio(er, returns, rf):
        return (er - rf) / max_dd(returns)
    data_all[6, 3]=calmar_ratio(data_all[9].mean(), data_all[9], 0)

###---Step 7: Moving Volatility
    moving_std = np.zeros(date - 1)
    for i in range(21, date - 1):
        moving_std[i-21] = np.std(data_all[9, (i-21):(i)])* np.sqrt(252)
    data_all[7, :-1] = moving_std
    data_all[12, :-1] = data_all[7, :-1]*100

###---Step 8: Correlation Matrix

a0 = data[1:-2, 0]
    a1 = data[1:-2, 1]
    a2 = data[1:-2, 2]
    a3 = data[1:-2, 3]
    a4 = data[1:-2, 4]
    t5=[]
    t5.append(np.corrcoef(a0,a1)[0,1])
    t5.append(np.corrcoef(a0,a2)[0,1])
    t5.append(np.corrcoef(a0, a3)[0, 1])
    t5.append(np.corrcoef(a0, a4)[0, 1])
    t5.append(np.corrcoef(a1, a2)[0, 1])
    t5.append(np.corrcoef(a1, a3)[0, 1])
    t5.append(np.corrcoef(a1, a4)[0, 1])
    t5.append(np.corrcoef(a2, a3)[0, 1])
    t5.append(np.corrcoef(a2, a4)[0, 1])
    t5.append(np.corrcoef(a3, a4)[0, 1])