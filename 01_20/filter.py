import pandas as pd
import numpy as np
import math
import datetime
import time

init = pd.read_csv("filter.csv")
source = pd.read_csv("bloomberg.csv", low_memory=False)


def filter(init, source, asset_arr=[4, 2, 3, 1], geo_arr=[1, 6, 7, 4, 2, 3, 5], score=3):
    ### Filter according to user's rank
    asset_class = ["Equity", "Fixed Income", "Mixed Allocation", "Money Market"]
    geo_class = ["Africa& Middle West Region", "Asian Pacific Region", "European Region", "Greater China",
                 "International", "Latin American Region", "U.S."]
    fund_num = init.shape[0]
    filter_re = []
    for i in range(0, fund_num):
        asset_tmp = init['Asset Class'][i]
        geo_tmp = init['Geographical Focus'][i]
        if ((asset_tmp == asset_class[asset_arr[0] - 1] or asset_tmp == asset_class[asset_arr[1] - 1] or asset_tmp ==
             asset_class[asset_arr[2] - 1]) and (
                geo_tmp == geo_class[geo_arr[0] - 1] or geo_tmp == geo_class[geo_arr[1] - 1] or
                geo_tmp == geo_class[geo_arr[2] - 1] or geo_tmp == geo_class[geo_arr[3] - 1])):
            filter_re.append(init['ISIN'][i])

    ### If number is smaller than 50, choose again
    for i in range(4, 7):
        if (len(filter_re) < 50):
            for j in range(0, fund_num):
                asset_tmp = init['Asset Class'][j]
                if ((asset_tmp == asset_class[asset_arr[0] - 1] or asset_tmp == asset_class[
                    asset_arr[1] - 1] or asset_tmp ==
                     asset_class[asset_arr[2] - 1]) and geo_class[geo_arr[i] - 1] == init['Geographical Focus'][j]):
                    filter_re.append(init['ISIN'][j])
        else:
            break

    ### data: names after filter + their risks
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

    data_date_tmp = list(tmp['date']).copy()
    for i in range(0, len(data_date_tmp)):
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

    # filter1.to_csv("filter1.csv")
    # print(filter1)

    # df1 = pd.DataFrame({'d': ['2018/1/1', np.nan,'2019/8/3'], 'd1': [1,2,np.nan]})
    # df2 = pd.DataFrame({'d': ['2018/1/1', '2019/1/3'], 'd2': [1,3]})
    # df=pd.merge(df1,df2, on='d', how='outer')
    # df=df.sort_values(axis=0, ascending=True, by='d').reset_index(drop=True)
    # print(df)

filter(init, source)
