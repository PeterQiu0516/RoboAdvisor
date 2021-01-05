import pandas as pd
import numpy as np
import datetime
import time
from pypfopt.efficient_frontier import EfficientFrontier
from pypfopt import risk_models
from pypfopt import expected_returns


def seven(data):
    #data = pd.read_csv("filter1.csv",header = 0,index_col=[0])
    data_fund = pd.read_csv("newfund.csv", header=0, encoding="UTF-8")
    fund_1 = data_fund.iloc[:, 7:9]
    fund_2 = pd.concat([data_fund.iloc[:, 0], data_fund.iloc[:, 4]], axis=1)

    max_date = data.shape[0]
    fund_tmp = np.array([range(0, max_date+1000), range(0, max_date+1000)])
    fund_tmp = fund_tmp.transpose()
    fund_tmp = fund_tmp * -1.0
    date_tmp = np.array([range(0, max_date+1000), range(0, max_date+1000)])
    date_tmp = date_tmp.transpose()
    date_tmp = date_tmp * -1.0

    '''
    fund_list_tmp = [[]for i in range(2)]
    for i in range(0,-max_date,-1):
        fund_list_tmp[0].append(i)
    for i in range(0,-max_date,-1):
        fund_list_tmp[1].append(i)
    date_list_tmp = [[]for i in range(2)]
    for i in range(0,-max_date,-1):
        date_list_tmp[0].append(i)
    for i in range(0,-max_date,-1):
        date_list_tmp[1].append(i)
    '''

    fund_1_date = 1
    fund_2_date = 1
    while (type(fund_1.iloc[fund_1_date, 0]) == type("aha") or (not np.isnan(fund_1.iloc[fund_1_date, 0]))):
        fund_1_date = fund_1_date+1
    while (type(fund_2.iloc[fund_2_date, 0]) == type("aha") or (not np.isnan(fund_2.iloc[fund_2_date, 0]))):
        fund_2_date = fund_2_date+1

    # print(fund_1_date,fund_2_date)

    data_date_tmp = list(data.index).copy()
    for i in range(0, len(data_date_tmp)):
        tmp = datetime.datetime.strptime(data_date_tmp[i], "%Y/%m/%d")
        y = tmp.year
        m = tmp.month
        d = tmp.day
        data_date_tmp[i] = y*365+m*30+d

    '''
    #print(fund_1.iloc[1564,0])
    #print(type(fund_1.iloc[1564,0]))
    #print(np.isnan(int(fund_1.iloc[1564,0])))
    '''

    fund_tmp[0:fund_1_date, 0] = fund_1.iloc[0:fund_1_date, 1]
    fund_tmp[0:fund_2_date, 1] = fund_2.iloc[0:fund_2_date, 1]
    for i in range(0, fund_1_date):
        tmp = datetime.datetime.strptime(fund_1.iloc[i, 0], "%Y/%m/%d")
        y = tmp.year
        m = tmp.month
        d = tmp.day
        date_tmp[i, 0] = y*365+m*30+d
    for i in range(0, fund_2_date):
        tmp = datetime.datetime.strptime(fund_2.iloc[i, 0], "%Y/%m/%d")
        y = tmp.year
        m = tmp.month
        d = tmp.day
        date_tmp[i, 1] = y*365+m*30+d

    # print(fund_tmp[-100:,:])
    # print(date_tmp[-100:,:])

    # print(max_date)
    i = 0
    while i <= max_date - 1:
        if date_tmp[i, 0] < 0:
            date_tmp[i, 0] = data_date_tmp[i]
            fund_tmp[i, 0] = -1000000  # NaN
            fund_1_date = fund_1_date + 1
        elif date_tmp[i, 0] > data_date_tmp[i]:
            if i < max_date - 1:
                # print(date_tmp[i,0],data_date_tmp[i])
                # print(i,fund_1_date)
                # print(fund_1_date)
                fund_tmp[(i+1):(fund_1_date+1),0] = fund_tmp[(i):(fund_1_date), 0]
                date_tmp[(i+1):(fund_1_date+1),0] = date_tmp[(i):(fund_1_date), 0]
                date_tmp[i, 0] = data_date_tmp[i]
                fund_tmp[i, 0] = -1000000  # NaN
            elif i == max_date - 1:
                date_tmp[i, 0] = data_date_tmp[i]
                fund_tmp[i, 0] = -1000000  # NaN
            fund_1_date = fund_1_date + 1
        elif date_tmp[i, 0] < data_date_tmp[i]:
            # print(date_tmp[i,0],data_date_tmp[i])
            # print(i,fund_1_date)
            # print(fund_1_date)
            fund_tmp[(i):(fund_1_date), 0] = fund_tmp[(i+1):(fund_1_date+1), 0]
            date_tmp[(i):(fund_1_date), 0] = date_tmp[(i+1):(fund_1_date+1), 0]
            fund_tmp[fund_1_date-1, 0] = -1000000  # NaN
            date_tmp[fund_1_date-1, 0] = -1000000  # NaN
            fund_1_date = fund_1_date - 1
            i = i - 1
        i = i + 1

    i = 0
    while i <= max_date - 1:
        if date_tmp[i, 1] < 0:
            date_tmp[i, 1] = data_date_tmp[i]
            fund_tmp[i, 1] = -1000000  # NaN
            fund_2_date = fund_2_date + 1
        elif date_tmp[i, 1] > data_date_tmp[i]:
            if i < max_date - 1:
                # print(date_tmp[i,1],data_date_tmp[i])
                # print(i,fund_2_date)
                # print(fund_1_date)
                fund_tmp[(i+1):(fund_2_date+1),1] = fund_tmp[(i):(fund_2_date), 1]
                date_tmp[(i+1):(fund_2_date+1),1] = date_tmp[(i):(fund_2_date), 1]
                date_tmp[i, 1] = data.iloc[i, 1]
                fund_tmp[i, 1] = -1000000  # NaN
            elif i == max_date - 1:
                date_tmp[i, 1] = data.iloc[i, 1]
                fund_tmp[i, 1] = -1000000  # NaN
            fund_2_date = fund_2_date + 1
        elif date_tmp[i, 1] < data_date_tmp[i]:
            fund_tmp[(i):(fund_2_date), 1] = fund_tmp[(i+1):(fund_2_date+1), 1]
            date_tmp[(i):(fund_2_date), 1] = date_tmp[(i+1):(fund_2_date+1), 1]
            fund_tmp[fund_2_date-1, 1] = -1000000  # NaN
            date_tmp[fund_2_date-1, 1] = -1000000  # NaN
            fund_2_date = fund_2_date - 1
            i = i - 1
        i = i + 1

    fund_tmp = fund_tmp[:max_date, :]
    # print(fund_tmp[-60:,:])
    fund_new = pd.DataFrame(
        fund_tmp, columns=["沪深300", "标普500"], index=data.index)
    data = pd.concat([data, fund_new], axis=1)
    # print(fund_new.iloc[-60:,:])
    # print(data.iloc[-60:,:])
    return data


def fund_completion(data):
    data = seven(data)

    date = data.shape[0]
    fund_num = data.shape[1]
    # print(rows,fund_num)

    for j in range(0, fund_num):
        for i in range(0, date):
            if data.iloc[i, j] == -1000000:
                if i != date - 1:
                    i_tmp = i + 1
                    while (data.iloc[i_tmp, j] == -1000000 and i_tmp <= date - 1):
                        i_tmp = i_tmp + 1
                    if i == 0:
                        data.iloc[i, j] = data.iloc[i_tmp, j]
                    elif i_tmp == date - 1:
                        data.iloc[i, j] = data[i-1, j]
                    else:
                        data.iloc[i, j] = (data.iloc[i-1, j] + data.iloc[i_tmp, j]) / 2
                else:
                    data.iloc[i, j] = data.iloc[i-1, j]
    return data


def weights(data):
    #df = pd.read_csv("complete_new.csv",parse_dates=[0], index_col=0,infer_datetime_format=True)
    df = data
    fund = df.iloc[0:,0:5]

    mu = expected_returns.mean_historical_return(fund)
    S = risk_models.sample_cov(fund)
    
    ### Method 1: Markowitz Mean-Variance Model
    ef = EfficientFrontier(mu, S,weight_bounds=(0.05,0.4))
    raw_weights = ef.max_sharpe()
    cleaned_weights = ef.clean_weights()
    #print(cleaned_weights)
    #ef.save_weights_to_file("weights.csv")  # saves to file
    #ef.portfolio_performance(verbose=True)
    
    weights = pd.DataFrame(cleaned_weights.values(),index =cleaned_weights.keys(),columns=["weights"])
    weights_T = pd.DataFrame(weights.values.T,index= weights.columns, columns = weights.index)
    #print(weights_T)
    
    ### Method 2: Black-litterman Model
    
    
    
    ### Output
    df = df.append(weights_T,sort=False)
    #print(df[-10:])
    
    
    data_index = pd.DataFrame(df.index,index = df.index)
    return_data = pd.concat([data_index,df],axis = 1)
    #print(return_data)
    return return_data

data = pd.read_csv("filter1.csv", header=0, index_col=[0])
print(weights(fund_completion(data)))

