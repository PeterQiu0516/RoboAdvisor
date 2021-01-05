import pandas as pd
import numpy as np
import datetime
import time
import math
from pypfopt import risk_models
from pypfopt import expected_returns
from pypfopt import black_litterman
from pypfopt.efficient_frontier import EfficientFrontier
from pypfopt.black_litterman import BlackLittermanModel
from statsmodels.tsa.arima_model import ARIMA


def filter(init, source, asset_arr=[1, 2, 3, 4], geo_arr=[7, 2, 3, 5, 4, 6, 1], score=3):
    # Filter according to user's rank
    asset_class = ["Equity", "Fixed Income",
                   "Mixed Allocation", "Money Market"]
    geo_class = ["Africa& Middle West Region", "Asian Pacific Region", "European Region", "Greater China",
                 "International", "Latin American Region", "U.S."]
    fund_num = init.shape[0]
    filter_re = []
    for i in range(0, fund_num):
        asset_tmp = init['Asset Class'][i]
        geo_tmp = init['Geographical Focus'][i]
        if ((asset_tmp == asset_class[asset_arr[0] - 1] or asset_tmp == asset_class[asset_arr[1] - 1] or asset_tmp == asset_class[asset_arr[2] - 1]) and (geo_tmp == geo_class[geo_arr[0] - 1] or geo_tmp == geo_class[geo_arr[1] - 1] or geo_tmp == geo_class[geo_arr[2] - 1] or geo_tmp == geo_class[geo_arr[3] - 1])):
            filter_re.append(init['ISIN'][i])

    # If number of the funds filted is smaller than 100(can be specified), choose again
    fund_filted_min = 100
    for i in range(4, 7):
        if (len(filter_re) < fund_filted_min):
            for j in range(0, fund_num):
                asset_tmp = init['Asset Class'][j]
                if ((asset_tmp == asset_class[asset_arr[0] - 1] or asset_tmp == asset_class[asset_arr[1] - 1] or asset_tmp == asset_class[asset_arr[2] - 1]) and geo_class[geo_arr[i] - 1] == init['Geographical Focus'][j]):
                    filter_re.append(init['ISIN'][j])
        else:
            break

    # data: names after filter + their risks
    data = pd.DataFrame()
    data.insert(loc=0, column='name', value=[])
    data.insert(loc=1, column='risk', value=[])

    for i in range(0, len(filter_re)):
        col_index = source.columns.get_loc(filter_re[i])
        price = source.iloc[:, col_index + 1]
        price = price.dropna().reset_index(drop=True)
        returns = np.diff(price) / price[:-1]
        ann_risk = np.std(returns) * math.sqrt(252)
        len_data = len(data)
        data.loc[len_data, 'name'] = filter_re[i]
        data.loc[len_data, 'risk'] = ann_risk

    # Sort according to their risks
    data_sort = data.sort_values(
        axis=0, ascending=True, by='risk').reset_index(drop=True)

    '''
    print("\n---risk---")
    print(data_sort)
    print()
    '''

    # get corresponding funds according to scores
    len_index = int(np.floor(len(data_sort['name']) / 5))
    fil_name = []
    if (score == 5):
        for i in range(len_index * 4, len(data_sort['name'])):
            fil_name.append(data_sort.loc[i, 'name'])
    else:
        for i in range(len_index * (score - 1), len_index * score):
            fil_name.append(data_sort.loc[i, 'name'])

    ### result: name + returns
    result = pd.DataFrame()
    result.insert(loc=0, column='name', value=[])
    result.insert(loc=1, column='returns', value=[])

    for i in range(0, len(fil_name)):
        col_index = source.columns.get_loc(fil_name[i])
        price = source.iloc[:, col_index + 1]
        price = price.dropna().reset_index(drop=True)
        returns = np.diff(price) / price[:-1]
        rets_add_one = returns + 1
        cum_rets = rets_add_one.cumprod() - 1
        len_data = len(result)
        result.loc[len_data, 'name'] = fil_name[i]
        result.loc[len_data, 'returns'] = cum_rets[len(cum_rets) - 1]

    # Sort according to their returns
    result_sort = result.sort_values(
        axis=0, ascending=False, by='returns').reset_index(drop=True)

    '''
    print("\n---return---")
    print(result_sort)
    print()
    '''

    # name_final: 5 names
    name_final = []
    for i in range(0, 5):
        name_final.append(result_sort.loc[i, 'name'])

    # price_five: 5 names + their prices
    price_five = pd.DataFrame()
    for i in range(0, len(name_final)):
        price_five.insert(loc=i * 2, column=i, value=[])
        price_five.insert(loc=i * 2 + 1, column=name_final[i], value=[])
    for i in range(0, len(name_final)):
        col_index = source.columns.get_loc(name_final[i])
        date = source.iloc[:, col_index]
        price = source.iloc[:, col_index + 1]
        price_five.iloc[:, i * 2 + 1] = price
        price_five.iloc[:, i * 2] = date

    # combine
    tmp = pd.DataFrame()
    tmp.insert(loc=0, column='date', value=[])
    tmp.insert(loc=1, column=name_final[0], value=[])
    tmp['date'] = price_five.iloc[:, 0]
    tmp[name_final[0]] = price_five.iloc[:, 1]

    for i in range(1, 5):
        price_five.rename(columns={i: 'date'}, inplace=True)
        tmp = pd.merge(
            tmp, price_five.iloc[:, 2 * i:2 * i + 2], on='date', how='outer')
        tmp = tmp.sort_values(axis=0, ascending=True,
                              by='date').reset_index(drop=True)
        tmp = tmp.iloc[:len(source), :]
    tmp = tmp.dropna(how='all')

    data_date_tmp = list(tmp['date']).copy()
    for i in range(0, len(data_date_tmp)):
        if(type(data_date_tmp[i]) != type("aha")):
            break
        tempt = datetime.datetime.strptime(data_date_tmp[i], "%Y/%m/%d")
        y = tempt.year
        m = tempt.month
        d = tempt.day
        data_date_tmp[i] = y * 365 + m * 30 + d
    tmp['trans'] = data_date_tmp

    tmp = tmp.sort_values(axis=0, ascending=True,
                          by='trans').reset_index(drop=True)
    tmp = tmp.iloc[:len(source), :6]

    filter1 = tmp.set_index('date')
    return filter1

    # filter1.to_csv("filter1.csv")
    # print(filter1)

    # df1 = pd.DataFrame({'d': ['2018/1/1', np.nan,'2019/8/3'], 'd1': [1,2,np.nan]})
    # df2 = pd.DataFrame({'d': ['2018/1/1', '2019/1/3'], 'd2': [1,3]})
    # df=pd.merge(df1,df2, on='d', how='outer')
    # df=df.sort_values(axis=0, ascending=True, by='d').reset_index(drop=True)
    # print(df)


def seven(data):
    #data = pd.read_csv("filter1.csv",header = 0,index_col=[0])
    #print(data)
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
        if (fund_2_date == fund_2.shape[0]):
            break;

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
                fund_tmp[(i+1):(fund_1_date+1),
                         0] = fund_tmp[(i):(fund_1_date), 0]
                date_tmp[(i+1):(fund_1_date+1),
                         0] = date_tmp[(i):(fund_1_date), 0]
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
                fund_tmp[(i+1):(fund_2_date+1),
                        1] = fund_tmp[(i):(fund_2_date), 1]
                date_tmp[(i+1):(fund_2_date+1),
                        1] = date_tmp[(i):(fund_2_date), 1]
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
            if (data.iloc[i, j] == -1000000 or np.isnan(data.iloc[i, j])):
                if i != date - 1:
                    i_tmp = i + 1
                    while ((data.iloc[i_tmp, j] == -1000000 or np.isnan(data.iloc[i_tmp, j])) and i_tmp <= date - 1):
                        i_tmp = i_tmp + 1
                    if i == 0:
                        data.iloc[i, j] = data.iloc[i_tmp, j]
                    elif i_tmp == date - 1:
                        data.iloc[i, j] = data[i-1, j]
                    else:
                        data.iloc[i, j] = (
                            data.iloc[i-1, j] + data.iloc[i_tmp, j]) / 2
                else:
                    data.iloc[i, j] = data.iloc[i-1, j]
    return data


def weights(data):
    #df = pd.read_csv("complete_new.csv",parse_dates=[0], index_col=0,infer_datetime_format=True)
    df = data
    fund = df.iloc[0:, 0:5]
    #print(fund.columns)
    mu = expected_returns.mean_historical_return(fund)
    # print(mu)
    cov_matrix = risk_models.sample_cov(fund)
    # print(S)

    '''
    # Method 1: Markowitz Mean-Variance Model
    ef = EfficientFrontier(mu, cov_matrix, weight_bounds=(0.05, 0.4))
    raw_weights = ef.max_sharpe()
    cleaned_weights = ef.clean_weights()
    # print(cleaned_weights)
    # ef.save_weights_to_file("weights.csv")  # saves to file
    ef.portfolio_performance(verbose=True)

    weights = pd.DataFrame(cleaned_weights.values(),
                            index=cleaned_weights.keys(), columns=["weights"])
    weights_T = pd.DataFrame(
        weights.values.T, index=weights.columns, columns=weights.index)
    # print(weights_T)
    '''
    
    # Method 2: Black-litterman Model
    
    # Calculate Prior Return
    spy_prices = df.iloc[0:, 6]
    risk_pre = black_litterman.market_implied_risk_aversion(spy_prices)
    mcaps = dict(zip(fund.columns,[1.0,1.0,1.0,1.0,1.0]))
    prior = black_litterman.market_implied_prior_returns(mcaps, risk_pre, cov_matrix)
    print(mu)
    print(prior)
    
    # Generate Absolute View by ARIMA
    view_generated = [0.0, 0.0, 0.0, 0.0,0.0]   
    for fund_index in range (0,5):
        model = ARIMA(df.iloc[:, fund_index],order=(5,1,0))
        model_fit = model.fit(disp=-1, maxiter = 1000)
        view_generated[fund_index] = (model_fit.forecast()[0] / df.iloc[-1,fund_index])- 1
        #print("Predicted = {}" .format(model_fit.forecast()))
        #print("Last = {}".format(df.iloc[-1, fund_index]))
    viewdict = dict(zip(fund.columns,view_generated))
    print(viewdict)
    #print(mu)
    
    bl_model = BlackLittermanModel(cov_matrix, absolute_views = viewdict, pi = prior)
    rets = bl_model.bl_returns()
    #print(rets)
    
    # Generate Efficient Frontier and Optimize the model
    ef = EfficientFrontier(rets, cov_matrix)
    raw_weights = ef.max_sharpe()
    cleaned_weights = ef.clean_weights()
    ef.portfolio_performance(verbose=True)
    
    # Output
    df = df.append(bl_model.weights, sort=False)
    # print(df[-10:])

    data_index = pd.DataFrame(df.index, index=df.index)
    return_data = pd.concat([data_index, df], axis=1)
    # print(return_data)
    return return_data


def RoboAdvisor(init, source, asset_arr=[1, 2, 3, 4], geo_arr=[5, 2, 3, 7, 4, 6, 1], score=5):
    #init = pd.read_csv("filter.csv")
    #source = pd.read_csv("bloomberg.csv", low_memory=False)

    level_1 = filter(init, source, asset_arr, geo_arr, score)

    '''
    print("\n---Level 1---")
    print(level_1)
    print()
    '''

    level_2 = fund_completion(level_1)

    '''
    print("\n---Level 2---")
    print(level_2)
    print()
    '''

    level_3 = weights(level_2)

    '''
    print("\n---Level 3---")
    print(level_3)
    print()
    '''

    return(level_3)


init = pd.read_csv("filter.csv")
source = pd.read_csv("bloomberg.csv", low_memory=False)

RoboAdvisor(init, source)
