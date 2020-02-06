# -*- coding: utf-8 -*- 
from flask import Flask, render_template, request, redirect, url_for
import json
import pandas as pd
import numpy as np
import datetime
import time
import math
from pypfopt.efficient_frontier import EfficientFrontier
from pypfopt import risk_models
from pypfopt import expected_returns

app = Flask(__name__)

NEWFUND = pd.read_csv("newfund.csv", header=0, encoding="UTF-8")
FILTER = pd.read_csv("filter.csv")
BLOOMBERG = pd.read_csv("bloomberg.csv", low_memory=False)

@app.route('/sub', methods=['POST'])
def getLoginRequest():
    data = request.json
    data1 = data.get('data1', {})
    data2 = data.get('data2', [])

    sum = 0
    rank = []
    total = data1['data']['total']
    high = 0
    low = 0
    score = 0
    for i in range(0, total):
        if data1['data']['question'][i]['type'] == "单选":
            length = len(data1['data']['question'][i]['o'])
            sum = sum + 5 - (int(data2[i]) - 1) * 5 / length
            high = high + 5
            low = low + 5 / length
        elif data1['data']['question'][i]['type'] == "多选":
            length = len(data1['data']['question'][i]['o'])
            sum = sum + int(data2[i][len(data2[i]) - 1])
            high = high + 5
            low = low + 5 / length
        else:
            rank.append(data2[i])
    interval = (high - low + 1) / 5
    for i in range(0, 5):
        if sum >= low + interval * i and sum < low + interval * (i + 1):
            score = i + 1
            break

    ans_list = [[0] * 50 for i in range(len(rank))]
    for i in range(0, len(rank)):
        tmp_list = []
        for j in range(0, len(rank[i]), 2):
            tmp_list.append(int(rank[i][j]))
        ans_list[i] = tmp_list

    def filter(init, source, asset_arr=[1, 2, 3, 4], geo_arr=[7, 2, 3, 5, 4, 6, 1], score=3):
        ### Filter according to user's rank
        asset_class = ["Equity", "Fixed Income", "Mixed Allocation", "Money Market"]
        geo_class = ["Africa& Middle West Region", "Asian Pacific Region", "European Region", "Greater China",
                     "International", "Latin American Region", "U.S."]
        fund_num = init.shape[0]
        filter_re = []
        for i in range(0, fund_num):
            asset_tmp = init['Asset Class'][i]
            geo_tmp = init['Geographical Focus'][i]
            if ((asset_tmp == asset_class[asset_arr[0] - 1] or asset_tmp == asset_class[
                asset_arr[1] - 1] or asset_tmp ==
                 asset_class[asset_arr[2] - 1]) and (
                    geo_tmp == geo_class[geo_arr[0] - 1] or geo_tmp == geo_class[geo_arr[1] - 1] or geo_tmp ==
                    geo_class[
                        geo_arr[2] - 1] or geo_tmp == geo_class[geo_arr[3] - 1])):
                filter_re.append(init['ISIN'][i])

        ### If number of the funds filted is smaller than 100(can be specified), choose again
        fund_filted_min = 100
        for i in range(4, 7):
            if (len(filter_re) < fund_filted_min):
                for j in range(0, fund_num):
                    asset_tmp = init['Asset Class'][j]
                    if ((asset_tmp == asset_class[asset_arr[0] - 1] or asset_tmp == asset_class[
                        asset_arr[1] - 1] or asset_tmp == asset_class[asset_arr[2] - 1]) and geo_class[
                        geo_arr[i] - 1] ==
                            init['Geographical Focus'][j]):
                        filter_re.append(init['ISIN'][j])
            else:
                break

        ### data: names after filter + their risks
        data = pd.DataFrame()
        data.insert(loc=0, column='name', value=[])
        data.insert(loc=1, column='risk', value=[])

        name_tmp = []
        ann_risk_tmp = []
        for i in range(0, len(filter_re)):
            col_index = source.columns.get_loc(filter_re[i])
            price = source.iloc[:, col_index + 1]
            price = price.dropna().reset_index(drop=True)
            returns = np.diff(price) / price[:-1]
            ann_risk = np.std(returns) * math.sqrt(252)
            name_tmp.append(filter_re[i])
            ann_risk_tmp.append(ann_risk)
        data['risk'] = ann_risk_tmp
        data['name'] = name_tmp

        ### Sort according to their risks
        data_sort = data.sort_values(axis=0, ascending=True, by='risk').reset_index(drop=True)

        ### get corresponding funds according to scores
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

        fil_name_tmp = []
        for i in range(0, len(fil_name)):
            col_index = source.columns.get_loc(fil_name[i])
            price = source.iloc[:, col_index + 1]
            price = price.dropna().reset_index(drop=True)
            returns = np.diff(price) / price[:-1]
            rets_add_one = returns + 1
            cum_rets = rets_add_one.cumprod() - 1
            len_data = len(result)
            fil_name_tmp.append(fil_name[i])
            result.loc[len_data, 'returns'] = cum_rets[len(cum_rets) - 1]
        result['name'] = fil_name_tmp

        ### Sort according to their returns
        result_sort = result.sort_values(axis=0, ascending=False, by='returns').reset_index(drop=True)

        ### name_final: 5 names
        name_final = []
        for i in range(0, 5):
            name_final.append(result_sort.loc[i, 'name'])

        ### price_five: 5 names + their prices
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

        ### combine
        tmp = pd.DataFrame()
        tmp.insert(loc=0, column='date', value=[])
        tmp.insert(loc=1, column=name_final[0], value=[])
        tmp['date'] = price_five.iloc[:, 0]
        tmp[name_final[0]] = price_five.iloc[:, 1]

        for i in range(1, 5):
            price_five.rename(columns={i: 'date'}, inplace=True)
            tmp = pd.merge(tmp, price_five.iloc[:, 2 * i:2 * i + 2], on='date', how='outer')
            tmp = tmp.sort_values(axis=0, ascending=True, by='date').reset_index(drop=True)
            tmp = tmp.iloc[:len(source), :]
        tmp = tmp.dropna(how='all')

        data_date_tmp = list(tmp['date']).copy()
        for i in range(0, len(data_date_tmp)):
            if (type(data_date_tmp[i]) != type("aha")):
                break
            tempt = datetime.datetime.strptime(data_date_tmp[i], "%Y/%m/%d")
            y = tempt.year
            m = tempt.month
            d = tempt.day
            data_date_tmp[i] = y * 365 + m * 30 + d
        tmp['trans'] = data_date_tmp

        tmp = tmp.sort_values(axis=0, ascending=True, by='trans').reset_index(drop=True)
        tmp = tmp.iloc[:len(source), :6]

        filter1 = tmp.set_index('date')
        return filter1

    def seven(data):
        data_fund = NEWFUND
        fund_1 = data_fund.iloc[:, 7:9]
        fund_2 = pd.concat([data_fund.iloc[:, 0], data_fund.iloc[:, 4]], axis=1)

        max_date = data.shape[0]
        fund_tmp = np.array([range(0, max_date + 1000), range(0, max_date + 1000)])
        fund_tmp = fund_tmp.transpose()
        fund_tmp = fund_tmp * -1.0
        date_tmp = np.array([range(0, max_date + 1000), range(0, max_date + 1000)])
        date_tmp = date_tmp.transpose()
        date_tmp = date_tmp * -1.0

        fund_1_date = 1
        fund_2_date = 1
        while (type(fund_1.iloc[fund_1_date, 0]) == type("aha") or (not np.isnan(fund_1.iloc[fund_1_date, 0]))):
            fund_1_date = fund_1_date + 1
        while (type(fund_2.iloc[fund_2_date, 0]) == type("aha") or (not np.isnan(fund_2.iloc[fund_2_date, 0]))):
            fund_2_date = fund_2_date + 1
            if (fund_2_date == fund_2.shape[0]):
                break

        data_date_tmp = list(data.index).copy()
        for i in range(0, len(data_date_tmp)):
            tmp = datetime.datetime.strptime(data_date_tmp[i], "%Y/%m/%d")
            y = tmp.year
            m = tmp.month
            d = tmp.day
            data_date_tmp[i] = y * 365 + m * 30 + d

        fund_tmp[0:fund_1_date, 0] = fund_1.iloc[0:fund_1_date, 1]
        fund_tmp[0:fund_2_date, 1] = fund_2.iloc[0:fund_2_date, 1]
        for i in range(0, fund_1_date):
            tmp = datetime.datetime.strptime(fund_1.iloc[i, 0], "%Y/%m/%d")
            y = tmp.year
            m = tmp.month
            d = tmp.day
            date_tmp[i, 0] = y * 365 + m * 30 + d
        for i in range(0, fund_2_date):
            tmp = datetime.datetime.strptime(fund_2.iloc[i, 0], "%Y/%m/%d")
            y = tmp.year
            m = tmp.month
            d = tmp.day
            date_tmp[i, 1] = y * 365 + m * 30 + d

        i = 0
        while i <= max_date - 1:
            if date_tmp[i, 0] < 0:
                date_tmp[i, 0] = data_date_tmp[i]
                fund_tmp[i, 0] = -1000000  # NaN
                fund_1_date = fund_1_date + 1
            elif date_tmp[i, 0] > data_date_tmp[i]:
                if i < max_date - 1:
                    fund_tmp[(i + 1):(fund_1_date + 1), 0] = fund_tmp[(i):(fund_1_date), 0]
                    date_tmp[(i + 1):(fund_1_date + 1), 0] = date_tmp[(i):(fund_1_date), 0]
                    date_tmp[i, 0] = data_date_tmp[i]
                    fund_tmp[i, 0] = -1000000  # NaN
                elif i == max_date - 1:
                    date_tmp[i, 0] = data_date_tmp[i]
                    fund_tmp[i, 0] = -1000000  # NaN
                fund_1_date = fund_1_date + 1
            elif date_tmp[i, 0] < data_date_tmp[i]:
                fund_tmp[(i):(fund_1_date), 0] = fund_tmp[(i + 1):(fund_1_date + 1), 0]
                date_tmp[(i):(fund_1_date), 0] = date_tmp[(i + 1):(fund_1_date + 1), 0]
                fund_tmp[fund_1_date - 1, 0] = -1000000  # NaN
                date_tmp[fund_1_date - 1, 0] = -1000000  # NaN
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
                    fund_tmp[(i + 1):(fund_2_date + 1), 1] = fund_tmp[(i):(fund_2_date), 1]
                    date_tmp[(i + 1):(fund_2_date + 1), 1] = date_tmp[(i):(fund_2_date), 1]
                    date_tmp[i, 1] = data.iloc[i, 1]
                    fund_tmp[i, 1] = -1000000  # NaN
                elif i == max_date - 1:
                    date_tmp[i, 1] = data.iloc[i, 1]
                    fund_tmp[i, 1] = -1000000  # NaN
                fund_2_date = fund_2_date + 1
            elif date_tmp[i, 1] < data_date_tmp[i]:
                fund_tmp[(i):(fund_2_date), 1] = fund_tmp[(i + 1):(fund_2_date + 1), 1]
                date_tmp[(i):(fund_2_date), 1] = date_tmp[(i + 1):(fund_2_date + 1), 1]
                fund_tmp[fund_2_date - 1, 1] = -1000000  # NaN
                date_tmp[fund_2_date - 1, 1] = -1000000  # NaN
                fund_2_date = fund_2_date - 1
                i = i - 1
            i = i + 1

        fund_tmp = fund_tmp[:max_date, :]
        fund_new = pd.DataFrame(
            fund_tmp, columns=["沪深300", "标普500"], index=data.index)
        data = pd.concat([data, fund_new], axis=1)
        return data

    def fund_completion(data):
        data = seven(data)

        date = data.shape[0]
        fund_num = data.shape[1]

        for j in range(0, fund_num):
            for i in range(0, date):
                if (data.iloc[i, j] == -1000000 or np.isnan(data.iloc[i, j])):
                    if i != date - 1:
                        i_tmp = i + 1
                        while ((data.iloc[i_tmp, j] == -1000000 or np.isnan(
                                data.iloc[i_tmp, j])) and i_tmp <= date - 1):
                            i_tmp = i_tmp + 1
                        if i == 0:
                            data.iloc[i, j] = data.iloc[i_tmp, j]
                        elif i_tmp == date - 1:
                            data.iloc[i, j] = data[i - 1, j]
                        else:
                            data.iloc[i, j] = (data.iloc[i - 1, j] + data.iloc[i_tmp, j]) / 2
                    else:
                        data.iloc[i, j] = data.iloc[i - 1, j]
        return data

    def weights(data):
        # df = pd.read_csv("complete_new.csv",parse_dates=[0], index_col=0,infer_datetime_format=True)
        df = data
        fund = df.iloc[0:, 0:5]

        mu = expected_returns.mean_historical_return(fund)
        S = risk_models.sample_cov(fund)

        ### Method 1: Markowitz Mean-Variance Model
        ef = EfficientFrontier(mu, S, weight_bounds=(0.05, 0.4))
        raw_weights = ef.max_sharpe()
        cleaned_weights = ef.clean_weights()

        weights = pd.DataFrame(cleaned_weights.values(), index=cleaned_weights.keys(), columns=["weights"])
        weights_T = pd.DataFrame(weights.values.T, index=weights.columns, columns=weights.index)

        ### Method 2: Black-litterman Model

        ### Output
        df = df.append(weights_T, sort=False)

        data_index = pd.DataFrame(df.index, index=df.index)
        return_data = pd.concat([data_index, df], axis=1)
        return return_data

    def RoboAdvisor(init, source, asset_arr=[4, 3, 2, 1], geo_arr=[5, 2, 3, 7, 4, 6, 1], score=1):

        level_1 = filter(init, source, asset_arr, geo_arr, score)

        level_2 = fund_completion(level_1)

        level_3 = weights(level_2)

        return (level_3)

    init = FILTER 
    source = BLOOMBERG

    return_data = RoboAdvisor(init, source, ans_list[0], ans_list[1], score)
    length = return_data.shape[0]

    ## Change data type
    data = np.array(return_data)
    date_g = data[:-1, 0]
    data = data[:, 1:length].astype(float)

    ## A function to calculate the average
    def weighted_avg(in_put, weight):
        if np.ndim(in_put) == 1:
            return np.sum(in_put * weight)
        else:
            size = in_put.shape[0]
            tmp = np.zeros(size)
            for i in range(0, size):
                tmp[i] = np.sum(in_put[i, :] * weight)
            return tmp

    ###initialization_fund
    nav = data[:-1, :5]  # here -1: space for weights
    weight = data[-1, :5]
    date = nav.shape[0]

    fund_num = nav.shape[1]
    data_all = np.zeros((15, date))  # here minus 1: space for weights

    ###initialization_base1
    nav_b1 = data[:-1, 5]  # here -1: space for weights
    data_all_b1 = np.zeros((15, date))  # here minus 1: space for weights

    ###initialization_base2
    nav_b2 = data[:-1, 6]  # here -1: space for weights
    data_all_b2 = np.zeros((15, date))  # here minus 1: space for weights

    ###daily returns_fund
    daily_returns = np.zeros((date - 1, fund_num))
    for j in range(0, fund_num):
        daily_returns[:, j] = np.diff(nav[:, j]) / nav[:-1, j]
    data_all[9, :-1] = weighted_avg(daily_returns, weight)

    ###daily returns_base1
    data_all_b1[9, :-1] = np.diff(nav_b1[:]) / nav_b1[:-1]

    ###daily returns_base2
    data_all_b2[9, :-1] = np.diff(nav_b2[:]) / nav_b2[:-1]

    ###---Step 1
    ###accumulative daily returns_fund
    accu_daily_returns = np.zeros((date - 1, fund_num))
    for j in range(0, fund_num):
        for i in range(0, date - 1):
            accu_daily_returns[i, j] = float(nav[i + 1, j] / nav[0, j] - 1)
    data_all[1, :-1] = weighted_avg(accu_daily_returns, weight)
    data_all[10, :-1] = data_all[1, :-1] * 100
    data_all[14, :-1] = np.around(data_all[10, :-1], decimals=2)
    t1 = []
    t1.append(float("%.2f" % (data_all[1, -2] * 1000 + 1000)))

    ###accumulative daily returns_b1
    for i in range(0, date - 1):
        data_all_b1[1, i] = float(nav_b1[i + 1] / nav_b1[0] - 1)
    data_all_b1[10, :-1] = data_all_b1[1, :-1] * 100
    data_all_b1[14, :-1] = np.around(data_all_b1[10, :-1], decimals=2)
    t1_b1 = []
    t1_b1.append(float("%.2f" % (data_all_b1[1, -2] * 1000 + 1000)))

    ###accumulative daily returns_b2
    for i in range(0, date - 1):
        data_all_b2[1, i] = float(nav_b2[i + 1] / nav_b2[0] - 1)
    data_all_b2[10, :-1] = data_all_b2[1, :-1] * 100
    data_all_b2[14, :-1] = np.around(data_all_b2[10, :-1], decimals=2)
    t1_b2 = []
    t1_b2.append(float("%.2f" % (data_all_b2[1, -2] * 1000 + 1000)))

    ###get Chinese name
    name_out = []
    for j in range(0, 5):
        for i in range(0, init.shape[0]):
            if return_data.columns[j + 1] == init.loc[i, 'ISIN']:
                name_out.append(init.loc[i, 'Name'])

    t7 = []
    for i in range(0, 5):
        t7.append(name_out[i])
    for i in range(0, 5):
        t7.append("%.2f" % (weight[i] * 100))

    ###---Step 2
    ###annualized return & std_fund
    annual_return = ((1 + (data_all[1, -2])) ** (1 / (date / 252)) - 1)
    # data_all[1,-2] is the second last number of the data_all row, which is the acumulative rate
    data_all[2, 0] = annual_return * 100
    annual_std = np.std(data_all[9, :-1]) * np.sqrt(252)
    data_all[2, 2] = annual_std * 100

    ###annualized return & std_b1
    annual_return_b1 = ((1 + (data_all_b1[1, -2])) ** (1 / (date / 252)) - 1)
    # data_all[1,-2] is the second last number of the data_all row, which is the acumulative rate
    data_all_b1[2, 0] = annual_return_b1 * 100
    annual_std_b1 = np.std(data_all_b1[9, :-1]) * np.sqrt(252)
    data_all_b1[2, 2] = annual_std_b1 * 100

    ###annualized return & std_b2
    annual_return_b2 = ((1 + (data_all_b2[1, -2])) ** (1 / (date / 252)) - 1)
    # data_all[1,-2] is the second last number of the data_all row, which is the acumulative rate
    data_all_b2[2, 0] = annual_return_b2 * 100
    annual_std_b2 = np.std(data_all_b2[9, :-1]) * np.sqrt(252)
    data_all_b2[2, 2] = annual_std_b2 * 100

    ## Sharpe Ratio_fund
    # annual_return_mean = pow((pow(data_all[1][date - 2] + 1, 1.0 / (date - 1))), 252) - 1
    SR = np.mean(data_all[9]) / np.std(data_all[9]) * math.sqrt(252)
    data_all[2, 1] = SR
    data_all[6, 0] = "%.2f" % SR

    ## Sharpe Ratio_fund_b1
    # annual_return_mean_b1 = pow((pow(data_all_b1[1][date - 2] + 1, 1.0 / (date - 1))), 252) - 1
    SR_b1 = np.mean(data_all_b1[9]) / np.std(data_all_b1[9]) * math.sqrt(252)
    data_all_b1[2, 1] = SR_b1
    data_all_b1[6, 0] = "%.2f" % SR_b1

    ## Sharpe Ratio_fund_b2
    # annual_return_mean_b2 = pow((pow(data_all_b2[1][date - 2] + 1, 1.0 / (date - 1))), 252) - 1
    SR_b2 = np.mean(data_all_b2[9]) / np.std(data_all_b2[9]) * math.sqrt(252)
    data_all_b2[2, 1] = SR_b2
    data_all_b2[6, 0] = "%.2f" % SR_b2

    ## max drawdown_fund
    price_ave = weighted_avg(nav, weight)
    max_value = price_ave.copy()
    max_tmp = 0
    for i in range(0, len(price_ave)):
        if price_ave[i] > max_tmp:
            max_tmp = price_ave[i]
        else:
            max_value[i] = max_tmp
    max_ratio = np.zeros((date, fund_num))
    for i in range(0, date):
        max_ratio[i] = abs(price_ave[i] / max_value[i] - 1)
    maxdd_tmp = np.max(max_ratio)
    avedd = np.mean(max_ratio)
    data_all[2, 3] = maxdd_tmp * 100

    ## max drawdown_b1
    max_value_b1 = nav_b1.copy()
    max_tmp_b1 = 0
    for i in range(0, len(nav_b1)):
        if nav_b1[i] > max_tmp_b1:
            max_tmp_b1 = nav_b1[i]
        else:
            max_value_b1[i] = max_tmp_b1
    max_ratio_b1 = np.zeros(date)
    for i in range(0, date):
        max_ratio_b1[i] = abs(nav_b1[i] / max_value_b1[i] - 1)
    maxdd_tmp_b1 = np.max(max_ratio_b1)
    avedd_b1 = np.mean(max_ratio)
    data_all_b1[2, 3] = maxdd_tmp_b1 * 100

    ## max drawdown_b2
    max_value_b2 = nav_b2.copy()
    max_tmp_b2 = 0
    for i in range(0, len(nav_b2)):
        if nav_b2[i] > max_tmp_b2:
            max_tmp_b2 = nav_b2[i]
        else:
            max_value_b2[i] = max_tmp_b2
    max_ratio_b2 = np.zeros(date)
    for i in range(0, date):
        max_ratio_b2[i] = abs(nav_b2[i] / max_value_b2[i] - 1)
    maxdd_tmp_b2 = np.max(max_ratio_b2)
    avedd_b2 = np.mean(max_ratio)
    data_all_b2[2, 3] = maxdd_tmp_b2 * 100

    ## 95% VAR_fund
    d_sort = data_all[9, :-1].copy()
    d_sort.sort()
    va = d_sort[int((date - 1) * 0.05)]
    data_all[2, 4] = va * 100

    ## 95% VAR_b1
    d_sort_b1 = data_all_b1[9, :-1].copy()
    d_sort_b1.sort()
    va_b1 = d_sort_b1[int((date - 1) * 0.05)]
    data_all_b1[2, 4] = va_b1 * 100

    ## 95% VAR_b2
    d_sort_b2 = data_all_b2[9, :-1].copy()
    d_sort_b2.sort()
    va_b2 = d_sort_b2[int((date - 1) * 0.05)]
    data_all_b2[2, 4] = va_b2 * 100

    t2 = []
    for i in range(0, 5):
        t2.append("%.2f" % (data_all[2, i]))
    for i in range(0, 5):
        t2.append("%.2f" % (data_all_b1[2, i]))
    for i in range(0, 5):
        t2.append("%.2f" % (data_all_b2[2, i]))

    ###---Step 3: Chosen date return
    ###fund
    chosen_return = np.zeros((48, fund_num))
    dis = int(np.floor(date / 48))
    for j in range(0, 5):
        for i in range(0, 48):
            if i == 47:
                upper_bound = date - 1
            else:
                upper_bound = (i + 1) * dis
            chosen_return[i, j] = float(nav[upper_bound, j] / nav[i * dis, j] - 1)
    data_all[3, :48] = weighted_avg(chosen_return, weight)
    data_all[11, :48] = np.around(data_all[3, :48] * 100, decimals=2)

    ###base1
    dis_b1 = int(np.floor(date / 48))
    for i in range(0, 48):
        if i == 47:
            upper_bound_b1 = date - 1
        else:
            upper_bound_b1 = (i + 1) * dis_b1
        data_all_b1[3, i] = float(nav_b1[upper_bound_b1] / nav_b1[i * dis_b1] - 1)
    data_all_b1[11, :48] = np.around(data_all_b1[3, :48] * 100, decimals=2)

    ###base2
    dis_b2 = int(np.floor(date / 48))
    for i in range(0, 48):
        if i == 47:
            upper_bound_b2 = date - 1
        else:
            upper_bound_b2 = (i + 1) * dis_b2
        data_all_b2[3, i] = float(nav_b2[upper_bound_b2] / nav_b2[i * dis_b2] - 1)
    data_all_b2[11, :48] = np.around(data_all_b2[3, :48] * 100, decimals=2)

    ###---Step 4: Calculate date for Step 3
    ##48个日期
    date_tmp = []
    for i in range(0, 47):
        date_tmp.append(date_g[i * int(date / 48)])
    date_tmp.append(date_g[date - 1])

    ###---Step 5: Recent value
    ###fund
    within_0 = np.zeros(fund_num)
    for j in range(0, fund_num):
        within_0[j] = float(nav[date - 1, j] / nav[date - 1 - 5, j] - 1)  # 5 weekdays
    data_all[5, 0] = weighted_avg(within_0, weight)  # within a week

    within_1 = np.zeros(fund_num)
    for j in range(0, fund_num):
        within_1[j] = float(nav[date - 1, j] / nav[date - 1 - 21, j] - 1)  # 21 month days
    data_all[5, 1] = weighted_avg(within_1, weight)  # within a month
    within_2 = np.zeros(fund_num)
    for j in range(0, fund_num):
        within_2[j] = float(nav[date - 1, j] / nav[date - 1 - 63, j] - 1)  # 63 3months
    data_all[5, 2] = weighted_avg(within_2, weight)  # within 3 months

    within_3 = np.zeros(fund_num)
    for j in range(0, fund_num):
        within_3[j] = float(nav[date - 1, j] / nav[date - 1 - 126, j] - 1)  # 126 half a year
    data_all[5, 3] = weighted_avg(within_3, weight)  # within half a year

    within_4 = np.zeros(fund_num)
    for j in range(0, fund_num):
        within_4[j] = float(nav[date - 1, j] / nav[date - 1 - 252, j] - 1)  # 252 days a year
    data_all[5, 4] = weighted_avg(within_4, weight)  # within a year

    # within_5 = np.zeros(fund_num)
    # for j in range(0, fund_num):
    #     within_5[j] = float(nav[date - 1, j] / nav[date - 1 - (date % 252), j] - 1)  # this year
    # data_all[5, 5] = weighted_avg(within_5, weight)  # this year

    ###b1
    data_all_b1[5, 0] = float(nav_b1[date - 1] / nav_b1[date - 1 - 5] - 1)  # 5 weekdays
    data_all_b1[5, 1] = float(nav_b1[date - 1] / nav_b1[date - 1 - 21] - 1)  # 21 month days
    data_all_b1[5, 2] = float(nav_b1[date - 1] / nav_b1[date - 1 - 63] - 1)  # 63 3months
    data_all_b1[5, 3] = float(nav_b1[date - 1] / nav_b1[date - 1 - 126] - 1)  # 126 half a year
    data_all_b1[5, 4] = float(nav_b1[date - 1] / nav_b1[date - 1 - 252] - 1)  # 252 days a year

    ###b2
    data_all_b2[5, 0] = float(nav_b2[date - 1] / nav_b2[date - 1 - 5] - 1)  # 5 weekdays
    data_all_b2[5, 1] = float(nav_b2[date - 1] / nav_b2[date - 1 - 21] - 1)  # 21 month days
    data_all_b2[5, 2] = float(nav_b2[date - 1] / nav_b2[date - 1 - 63] - 1)  # 63 3months
    data_all_b2[5, 3] = float(nav_b2[date - 1] / nav_b2[date - 1 - 126] - 1)  # 126 half a year
    data_all_b2[5, 4] = float(nav_b2[date - 1] / nav_b2[date - 1 - 252] - 1)  # 252 days a year

    t3 = []
    for i in range(0, 5):
        t3.append("%.2f" % (data_all[5, i] * 100))
    for i in range(0, 5):
        t3.append("%.2f" % (data_all_b1[5, i] * 100))
    for i in range(0, 5):
        t3.append("%.2f" % (data_all_b2[5, i] * 100))

    ### Yearly Return_fund
    this_year_index = date - 1
    for i in range(0, date):
        if date_g[i][0:3] == date_g[date - 1][0:3]:
            this_year_index = i
            break

    year_num = int(np.floor(this_year_index / 252))
    year_re = np.zeros(fund_num)
    for i in range(0, year_num - 1):
        for j in range(0, fund_num):
            year_re[j] = float(
                (nav[this_year_index - 1 - 252 * i, j] / nav[this_year_index - 1 - 252 * (i + 1), j] - 1))
        data_all[13, i] = float("%.2f" % (weighted_avg(year_re, weight) * 100))
    for j in range(0, fund_num):
        year_re[j] = float((nav[this_year_index - 252 * (year_num - 1), j] / nav[0, j] - 1))  # first year
    data_all[13, (year_num - 1)] = float("%.2f" % (weighted_avg(year_re, weight) * 100))

    ### Yearly Return_b1
    for i in range(0, year_num - 1):
        data_all_b1[13, i] = float("%.2f" % float(
            (nav_b1[this_year_index - 1 - 252 * i] / nav_b1[this_year_index - 1 - 252 * (i + 1)] - 1) * 100))
    data_all_b1[13, (year_num - 1)] = float("%.2f" % float(
        (nav_b1[this_year_index - 1 - 252 * (year_num - 1)] / nav_b1[0] - 1) * 100))  # first year

    ### Yearly Return_b2
    for i in range(0, 5):
        data_all_b2[13, i] = float("%.2f" % float(
            (nav_b2[this_year_index - 1 - 252 * i] / nav_b2[this_year_index - 1 - 252 * (i + 1)] - 1) * 100))
    data_all_b2[13, (year_num - 1)] = float("%.2f" % float(
        (nav_b2[date - 1 - 252 * (year_num - 1)] / nav_b2[0] - 1) * 100))  # first year

    t6 = []
    for i in range(0, year_num):
        t6.append("%.2f" % (data_all[13, i]))
    for i in range(0, year_num):
        t6.append("%.2f" % (data_all_b1[13, i]))
    for i in range(0, year_num):
        t6.append("%.2f" % (data_all_b2[13, i]))

    ###---Step 6: Ratio
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

    ###fund
    data_all[6, 1] = float("%.2f" % (sortino_ratio(np.mean(data_all[9]), data_all[9], 0, 0) * math.sqrt(252)))
    ###base1
    data_all_b1[6, 1] = float("%.2f" % (sortino_ratio(np.mean(data_all_b1[9]), data_all_b1[9], 0, 0) * math.sqrt(252)))
    ###base2
    data_all_b2[6, 1] = float("%.2f" % (sortino_ratio(np.mean(data_all_b2[9]), data_all_b2[9], 0, 0) * math.sqrt(252)))

    ###Sterling Ratio_fund
    data_all[6, 2] = float("%.2f" % (np.mean(data_all[9]) * 252 / avedd))
    ###Sterling Ratio_b1
    data_all_b1[6, 2] = float("%.2f" % (np.mean(data_all_b1[9]) * 252 / avedd_b1))
    ###Sterling Ratio_b2
    data_all_b2[6, 2] = float("%.2f" % (np.mean(data_all_b2[9]) * 252 / avedd_b2))

    ###Calmar Ratio_fund
    data_all[6, 3] = float("%.2f" % (np.mean(data_all[9]) * 252 / (data_all[2, 3] / 100)))
    ###Calmar Ratio_base1
    data_all_b1[6, 3] = float("%.2f" % (np.mean(data_all_b1[9]) * 252 / (data_all_b1[2, 3] / 100)))
    ###Calmar Ratio_base2
    data_all_b2[6, 3] = float("%.2f" % (np.mean(data_all_b2[9]) * 252 / (data_all_b2[2, 3] / 100)))

    t4 = []
    for i in range(0, 4):
        t4.append("%.2f" % (data_all[6, i]))
    for i in range(0, 4):
        t4.append("%.2f" % (data_all_b1[6, i]))
    for i in range(0, 4):
        t4.append("%.2f" % (data_all_b2[6, i]))

    ###---Step 7: Moving Volatility
    ###fund
    moving_std = np.zeros(date - 1)
    for i in range(21, date - 1):
        moving_std[i - 21] = np.std(data_all[9, (i - 21):(i)]) * np.sqrt(252)
    data_all[7, :-1] = moving_std
    data_all[12, :-1] = np.around(data_all[7, :-1] * 100, decimals=2)

    ###base1
    moving_std_b1 = np.zeros(date - 1)
    for i in range(21, date - 1):
        moving_std_b1[i - 21] = np.std(data_all_b1[9, (i - 21):(i)]) * np.sqrt(252)
    data_all_b1[7, :-1] = moving_std_b1
    data_all_b1[12, :-1] = np.around(data_all_b1[7, :-1] * 100, decimals=2)

    ###base2
    moving_std_b2 = np.zeros(date - 1)
    for i in range(21, date - 1):
        moving_std_b2[i - 21] = np.std(data_all_b2[9, (i - 21):(i)]) * np.sqrt(252)
    data_all_b2[7, :-1] = moving_std_b2
    data_all_b2[12, :-1] = np.around(data_all_b2[7, :-1] * 100, decimals=2)

    ###---Step 8: Correlation Matrix
    a0 = data[1:-2, 0]
    a1 = data[1:-2, 1]
    a2 = data[1:-2, 2]
    a3 = data[1:-2, 3]
    a4 = data[1:-2, 4]

    t5 = []
    t5.append("%.2f" % (np.corrcoef(a0, a1)[0, 1]))
    t5.append("%.2f" % (np.corrcoef(a0, a2)[0, 1]))
    t5.append("%.2f" % (np.corrcoef(a0, a3)[0, 1]))
    t5.append("%.2f" % (np.corrcoef(a0, a4)[0, 1]))
    t5.append("%.2f" % (np.corrcoef(a1, a2)[0, 1]))
    t5.append("%.2f" % (np.corrcoef(a1, a3)[0, 1]))
    t5.append("%.2f" % (np.corrcoef(a1, a4)[0, 1]))
    t5.append("%.2f" % (np.corrcoef(a2, a3)[0, 1]))
    t5.append("%.2f" % (np.corrcoef(a2, a4)[0, 1]))
    t5.append("%.2f" % (np.corrcoef(a3, a4)[0, 1]))
    t5.append("%.2f" % 1)
    t5_tmp = []
    for i in range(0, len(t5)):
        t5_tmp.append(float(t5[i]))
    t5.append(1 - 2 * (1 - min(t5_tmp)))

    date_now = date_g[-1]
    date_begin = date_g[0]

    g1 = {
        "title": {
            "text": "我的组合",
            "left": "center",
            "y": "top"
        },
        "tooltip": {
            "trigger": "item",
            "formatter": "{b} :{d}%",
            "position": ['50%', '50%']
        },
        "legend": {
            "orient": "vertical",
            "left": "left",
            "top": 40,
            "data": [name_out[0], name_out[1], name_out[2], name_out[3], name_out[4]]
        },
        "series": [
            {
                "name": "基金比重",
                "type": "pie",
                "radius": "55%",
                "label": {
                    "show": ""
                },
                "center": [
                    "50%",
                    "70%"
                ],
                "data": [
                    {'value': weight[0], 'name': name_out[0]},
                    {'value': weight[1], 'name': name_out[1]},
                    {'value': weight[2], 'name': name_out[2]},
                    {'value': weight[3], 'name': name_out[3]},
                    {'value': weight[4], 'name': name_out[4]}
                ],
                "emphasis": {
                    "itemStyle": {
                        "shadowBlur": 10,
                        "shadowOffsetX": 0,
                        "shadowColor": "rgba(0, 0, 0, 0.5)"
                    }
                }
            }
        ]
    };

    g2 = {
        "title": {
            "text": "累计收益走势",
            "left": "center",
            "y": "top"
        },
        "tooltip": {
            "formatter": "{b}<br/>{a0} : {c0}%<br/>{a1} : {c1}%<br/>{a2} : {c2}%",
            "trigger": "axis"
        },
        "legend": {
            "top": 35,
            "data": [
                "我的组合",
                "基准1(沪深300)",
                "基准2(标普500)"
            ]
        },
        "grid": {
            "left": "4%",
            "right": "6%",
            "bottom": "5%",
            "top": "25%",
            "containLabel": "true"
        },
        "xAxis": {
            "type": "category",
            "boundaryGap": "false",
            "data": list(date_g[2:- 1])
        },
        "yAxis": {
            "type": "value",
            "axisLabel": {
                "show": "true",
                "interval": "auto",
                "formatter": "{value} %"
            }
        },
        "series": [
            {
                "name": "我的组合",
                "type": "line",
                "data": list(data_all[14, :-1])
            },
            {
                "name": "基准1(沪深300)",
                "type": "line",
                "data": list(data_all_b1[14, :-1])
            },
            {
                "name": "基准2(标普500)",
                "type": "line",
                "data": list(data_all_b2[14, :-1])
            }
        ]
    };

    g3 = {
        "title": {
            "text": "月度收益分布",
            "left": "center",
            "y": "top"
        },
        "tooltip": {
            "formatter": "{b}<br/>{a0} : {c0}%<br/>{a1} : {c1}%<br/>{a2} : {c2}%",
            "trigger": "axis",
            "axisPointer": {
                "type": "cross",
                "crossStyle": {
                    "color": "#999"
                }
            }
        },
        "grid": {
            "left": "4%",
            "right": "6%",
            "bottom": "5%",
            "top": "25%",
            "containLabel": "true"
        },
        "legend": {
            "top": 35,
            "data": [
                "我的组合",
                "基准1(沪深300)",
                "基准2(标普500)"
            ]
        },
        "xAxis": [
            {
                'type': 'category',
                'data': date_tmp,
                'axisPointer': {
                    'type': 'shadow'
                }
            }
        ],
        "yAxis": [
            {
                "type": "value",
                "axisLabel": {
                    "show": "true",
                    "interval": "auto",
                    "formatter": "{value} %"
                }
            }
        ],
        "series": [
            {
                "name": "我的组合",
                "type": "bar",
                "data": list(data_all[11])
            },
            {
                "name": "基准1(沪深300)",
                "type": "bar",
                "data": list(data_all_b1[11])
            },
            {
                "name": "基准2(标普500)",
                "type": "bar",
                "data": list(data_all_b2[11])
            }
        ]
    };

    g4 = """
    {
        "title": {
            "text": "业绩评价",
            "left": "center",
            "y": "top"
        },
        "tooltip": {
            "formatter": "{b}<br/>{a0} : {c0}%%<br/>{a1} : {c1}%%<br/>{a2} : {c2}%%",
            "trigger": "axis",
            "axisPointer": {
                "type": "cross",
                "crossStyle": {
                    "color": "#999"
                }
            }
        },
        "grid": {
            "left": "4%%",
            "right": "6%%",
            "bottom": "5%%",
            "top": "25%%",
            "containLabel": "true"
        },
        "legend": {
            "top": 35,
            "data": [
                "我的组合",
                "基准1(沪深300)",
                "基准2(标普500)"
            ]
        },
        "xAxis": [
            {
                "type": "category",
                "data": [
                    "夏普比率 (衡量风险调整后的收益表现)",
                    "索提诺比率(衡量风险调整后的收益表现并强调了下行风险)",
                    "斯图泽比率(衡量风险调整后的收益表现并强调了回撤阶段的风险)",
                    "卡玛比率 (衡量风险调整后的收益表现并强调了最大回撤阶段的风险)"
                ],
                "axisPointer": {
                    "type": "shadow"
                },
                "axisLabel":{
                    "formatter":
                        function(res){
                            var tempStr = "";
                            tempStr = res.substring(0, 5);
                            return tempStr;
                        }
                }
            }
        ],
        "yAxis": [
            {
                "type": "value"
            }
        ],
        "series": [
            {
                "name": "我的组合",
                "type": "bar",
                "data": %s
            },
            {
                "name": "基准1(沪深300)",
                "type": "bar",
                "data": %s
            },
            {
                "name": "基准2(标普500)",
                "type": "bar",
                "data": %s
            }
        ]
    }""" % (list(data_all[6]), list(data_all_b1[6]), list(data_all_b2[6]))

    g5 = {
        "title": {
            "text": "滚动年化波动率走势图",
            "left": "center",
            "y": "top"
        },
        "tooltip": {
            "formatter": "{b}<br/>{a0} : {c0}%<br/>{a1} : {c1}%<br/>{a2} : {c2}%",
            "trigger": "axis"
        },
        "legend": {
            "top": 35,
            "data": [
                "我的组合",
                "基准1(沪深300)",
                "基准2(标普500)"
            ]
        },
        "grid": {
            "left": "4%",
            "right": "6%",
            "bottom": "5%",
            "top": "25%",
            "containLabel": "true"
        },
        "xAxis": {
            "type": "category",
            "boundaryGap": "false",
            "data": list(date_g[22:])
        },
        "yAxis": {
            "type": "value",
            "axisLabel": {
                "show": "true",
                "interval": "auto",
                "formatter": "{value} %"
            }
        },
        "series": [
            {
                "name": "我的组合",
                "type": "line",
                "data": list(data_all[12][0:(date - 22)])
            },
            {
                "name": "基准1(沪深300)",
                "type": "line",
                "data": list(data_all_b1[12][0:(date - 22)])
            },
            {
                "name": "基准2(标普500)",
                "type": "line",
                "data": list(data_all_b2[12][0:(date - 22)])
            }
        ]
    };

    data_g6_1 = [name_out[0], name_out[1], name_out[2], name_out[3], name_out[4]]
    data_g6_2 = [name_out[4], name_out[3], name_out[2], name_out[1], name_out[0]]
    data_g6_3 = [[4, 0, t5[10]], [4, 1, t5[0]], [4, 2, t5[1]], [4, 3, t5[2]], [4, 4, t5[3]],
                 [3, 0, t5[0]], [3, 1, t5[10]], [3, 2, t5[4]], [3, 3, t5[5]], [3, 4, t5[6]],
                 [2, 0, t5[1]], [2, 1, t5[4]], [2, 2, t5[10]], [2, 3, t5[7]], [2, 4, t5[8]],
                 [1, 0, t5[2]], [1, 1, t5[5]], [1, 2, t5[7]], [1, 3, t5[10]], [1, 4, t5[9]],
                 [0, 0, t5[3]], [0, 1, t5[6]], [0, 2, t5[8]], [0, 3, t5[9]], [0, 4, t5[10]]]
    data_g6_len = t5[len(t5) - 1]

    g6 = """
    {
        "title": {
            "text": "相关系数矩阵",
            "left": "center",
            "y": "top"
        },
        "tooltip": {
            "position": "top",
            "formatter":
                function(res){
                    return %s[res.data[0]]+"<br/>和"+%s[4-res.data[1]]+"<br/>"+res.data[2]
                    }
        },
        "animation": "false",
        "grid": {
            "height": "80%%",
            "top": "20%%"
        },
        "xAxis": {
            "show": "",
            "position": "top",
            "type": "category",
            "data": %s,
            "splitArea": {
                "show": "true"
            }
        },
        "yAxis": {
            "show": "",
            "type": "category",
            "data": %s,
            "splitArea": {
                "show": "true"
            }
        },
        "visualMap": {
            "min": %s,
            "max": 1,
            "precision": 2,
            "calculable": "true",
            "orient": "",
            "left": "right",
            "bottom": "50%%"
        },
        "series": [
            {
                "name": "Correlation",
                "type": "heatmap",
                "data": %s,
                "label": {
                    "show": "true"
                },
                "emphasis": {
                    "itemStyle": {
                        "shadowBlur": 10,
                        "shadowColor": "rgba(0, 0, 0, 0.5)"
                    }
                }
            }
        ]
    }""" % (name_out, name_out, data_g6_1, data_g6_2, data_g6_len, data_g6_3)

    g7 = {
        "title": {
            "text": "年度收益分布",
            "left": "center",
            "y": "top"
        },
        "tooltip": {
            "formatter": "{b}<br/>{a0} : {c0}%<br/>{a1} : {c1}%<br/>{a2} : {c2}%",
            "trigger": "axis",
            "axisPointer": {
                "type": "cross",
                "crossStyle": {
                    "color": "#999"
                }
            }
        },
        "legend": {
            "top": 35,
            "data": [
                "我的组合",
                "基准1(沪深300)",
                "基准2(标普500)"
            ]
        },
        "grid": {
            "left": "4%",
            "right": "6%",
            "bottom": "5%",
            "top": "25%",
            "containLabel": "true"
        },
        "xAxis": [
            {
                "type": "category",
                "data": [2019, 2018, 2017, 2016, 2015, 2014],
                "axisPointer": {
                    "type": "shadow"
                }
            }
        ],
        "yAxis": [
            {
                "type": "value",
                "axisLabel": {
                    "show": "true",
                    "interval": "auto",
                    "formatter": "{value} %"
                }
            }
        ],
        "series": [
            {
                "name": "我的组合",
                "type": "bar",
                "data": list(data_all[13])
            },
            {
                "name": "基准1(沪深300)",
                "type": "bar",
                "data": list(data_all_b1[13])
            },
            {
                "name": "基准2(标普500)",
                "type": "bar",
                "data": list(data_all_b2[13])
            }
        ]
    };

    result = {
        'a': g1,
        'b': g2,
        'c': g3,
        'd': g4,
        'e': g5,
        'f': g6,
        'g': t4,
        'h': t3,
        'i': t2,
        'j': t1,
        'k': t6,
        'l': g7,
        'm': t7,
        'date_now': date_now,
        'date_begin': date_begin
    }

    return json.dumps(result)
    # return render_template('g2.html', result=result)


if __name__ == '__main__':
    app.run(host='0.0.0.0', port='9000', debug=False)
