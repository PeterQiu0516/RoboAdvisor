from flask import Flask, render_template, request, redirect, url_for

app = Flask(__name__)


@app.route('/')
def login():
    return render_template('option.html')


# 获取提交参数及处理
@app.route('/sub', methods=['POST'])
def getLoginRequest():
    q1 = request.values.get("q1")
    q2 = request.values.get("q2")
    q3 = request.values.get("q3")
    q4 = request.values.get("q4")
    q5 = request.values.get("q5")
    q6 = request.values.get("q6")
    q7 = request.values.get("q7")
    q8 = request.values.getlist("q8")
    q9 = request.values.get("q9")
    q10 = request.values.get("q10")
    q11 = request.values.get("q11")
    q12 = request.values.get("q12")
    q13 = request.values.getlist("q13")
    q14 = request.values.getlist("q14")
    # if q1!=None and q2!=None and q3!=None and q4!=None and q5!=None and q6!=None and q7!=None and q8!=[] and q9!=[]and q10!=[]:

    ## Calculate the result and transform it into an integer
    sum = int(q1) + int(q2) + int(q3) + int(q4) + int(q5) + int(q6) + int(q7) +  int(q9) + int(q10) + int(q11) + int(q12)
    ## Classify
    num=12
    if sum in range(12, 17):
        level = 1
    elif sum in range(17, 22):
        level = 2
    elif sum in range(22, 27):
        level = 3
    elif sum in range(27, 31):
        level = 4
    elif sum in range(31, 35):
        level = 5
    elif sum in range(35, 39):
        level = 6
    elif sum in range(39, 43):
        level = 7
    elif sum in range(43, 45):
        level = 8
    elif sum in range(45, 46):
        level = 9
    elif sum in range(46, 48):
        level = 10
    else:
        level = 1

    from rpy2 import robjects
    robjects.r('''
library("xml2")
library("dplyr")
library("stringr")
library("rlist")
library("xts")
library("quantmod")
library("PortfolioAnalytics")
library("DEoptim")
library("ROI")
require("ROI.plugin.glpk")
require("ROI.plugin.quadprog")

weights<-function(ins) {
    ### Read data from last milestone
    data = read.csv("complete.csv",header = TRUE, fileEncoding = "UTF-8", sep=",", na.strings = "")
    data_print_out = data

    #Number of funds in the final portforlio, can be specified
    portfolio_num = 5
    fund_num = portfolio_num*10

    fund_num_tmp = floor(fund_num/10)
    fund_num_upper = fund_num_tmp * ins
    if(ins == 10) 
    {
        fund_num_upper = fund_num
    }
    selected <- data[,(fund_num_tmp*(ins-1)+1+1):(fund_num_upper+1)] #first plus one： begin with 6-10; second plus one: first column of data is date 
    selected <- cbind(data[,1],selected)
    data <- selected
    
    data_print_out = data

    ### Construct the system by Markowitz model
    #Initialization
    rows = nrow(data)
    returns_tmp = c()
    return_tmp = data.frame()

    for (j in 1:portfolio_num){
        data_tmp <- data.frame(data[,1],data[,j+1])
        data_tmp <- xts(data_tmp[,2],as.Date(data_tmp[,1]))
        return_tmp <- periodReturn(data_tmp,period="daily",type="log")
        returns_tmp <- cbind(returns_tmp,return_tmp)
    }

    returns <- returns_tmp[,1:5]
    rownames(returns) <- data[,1]
    colnames(returns) <- c("fd1", "fd2", "fd3", "fd4","fd5")
    funds <- colnames(returns)
    init <- portfolio.spec(assets = funds)
    init <- add.constraint(portfolio=init, type="leverage",min_sum=1.00, max_sum=1.00)
    init <- add.constraint(portfolio=init, type="box", min=0.10, max=0.40)

    #6.3 Minimize variance with ROI
    minvar <- add.objective(portfolio=init, type="risk", name="var")
    opt_minvar <- optimize.portfolio(R=returns, portfolio=minvar, optimize_method="ROI", trace=TRUE)
    
    w = extractWeights(opt_minvar)
    data_print_out_final = rbind(data_print_out,w)
    return_data = data_print_out_final[,-1]
    return_data = rbind(colnames(return_data),return_data)
    return (return_data)
}
        ''')
    import pandas as pd
    import numpy as np
    return_data = robjects.r['weights'](level)
    return_data=list(return_data)
    length=len(return_data[0])


    ## Change data type
    data=np.array(return_data[:])
    data=data[:,1:(length+100)].astype(float)
    data=data.transpose()

    ## Construct an empty array
    data_all = np.zeros((10, data.shape[0] - 1))  # here minus 1: space for weights

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

    ###initialization
    nav = data[:-1, :]  # here -1: space for weights
    weight = data[-1, :]
    date = nav.shape[0]
    fund_num = nav.shape[1]
    data_all = np.zeros((10, date))  # here minus 1: space for weights

    ###daily returns
    daily_returns = np.zeros((date - 1, fund_num))
    for j in range(0, fund_num):
        daily_returns[:, j] = np.diff(nav[:, j]) / nav[:-1, j]
    data_all[9, :-1] = weighted_avg(daily_returns, weight)

    ###---Step 1
    ###accumulative daily returns
    accu_daily_returns = np.zeros((date - 1, fund_num))
    for j in range(0, fund_num):
        for i in range(0, date - 1):
            accu_daily_returns[i, j] = float(nav[i + 1, j] / nav[0, j] - 1)
    data_all[1, :-1] = weighted_avg(accu_daily_returns, weight)

    ###---Step 2
    ###annualized return & std
    annual_return = ((1 + (data_all[1, -2])) ** (1 / (
                date / 252)) - 1)  # data_all[1,-2] is the second last number of the data_all row, which is the acumulative rate
    data_all[2, 0] = annual_return
    annual_std = np.std(data_all[9, :-1]) * np.sqrt(252)
    data_all[2, 3] = annual_std

    ## Sharpe Ratio
    SR = data_all[9].mean() / data_all[9].std()
    data_all[2, 1]=SR
    data_all[6, 0] = SR

    ## 最大回撤
    index_j = np.argmax(np.maximum.accumulate(data_all[9]) - data_all[9])  # 结束位置
    index_i = np.argmax(data_all[9][:index_j])  # 开始位置
    d = data_all[9][index_j] - data_all[9][index_i]  # 最大回撤
    data_all[2, 2] = d

    ## 95% VAR
    d_sort=data_all[9,:-1].copy()
    d_sort.sort()
    va=d_sort[int((date-1)*0.05)]
    data_all[2, 4] = va

    t2=[]
    t2.append(data_all[2, 0])
    t2.append(data_all[2, 1])
    t2.append(data_all[2, 2])
    t2.append(data_all[2, 3])
    t2.append(data_all[2, 4])

    ###---Step 3: Chosen date return
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
    t3=[]
    t3.append(data_all[3, 0])
    t3.append(data_all[3, 1])
    t3.append(data_all[3, 2])
    t3.append(data_all[3, 3])
    t3.append(data_all[3, 4])
    t3.append(data_all[3, 5])

    ###---Step 4: Calculate date for Step 3
    date_g = pd.read_csv("complete.csv", header=None).astype(str)
    ##48个日期
    date_tmp = []
    for i in range(0, 47):
        date_tmp.append(date_g[0][i * int(date / 48) + 1])
    date_tmp.append(date_g[0][date])

    ###---Step 5: Recent value
    within_0 = np.zeros(fund_num)
    for i in range(0, fund_num):
        within_0[i] = float(nav[date - 1, j] / nav[date - 1 - 5, j] - 1)  # 5 weekdays
    data_all[5, 0] = weighted_avg(within_0, weight)  # within a week

    within_1 = np.zeros(fund_num)
    for i in range(0, fund_num):
        within_1[i] = float(nav[date - 1, j] / nav[date - 1 - 21, j] - 1)  # 21 month days
    data_all[5, 1] = weighted_avg(within_1, weight)  # within a month
    within_2 = np.zeros(fund_num)
    for i in range(0, fund_num):
        within_2[i] = float(nav[date - 1, j] / nav[date - 1 - 63, j] - 1)  # 63 3months
    data_all[5, 2] = weighted_avg(within_2, weight)  # within 3 months

    within_3 = np.zeros(fund_num)
    for i in range(0, fund_num):
        within_3[i] = float(nav[date - 1, j] / nav[date - 1 - 126, j] - 1)  # 126 half a year
    data_all[5, 3] = weighted_avg(within_3, weight)  # within half a year

    within_4 = np.zeros(fund_num)
    for i in range(0, fund_num):
        within_4[i] = float(nav[date - 1, j] / nav[date - 1 - 252, j] - 1)  # 252 days a year
    data_all[5, 4] = weighted_avg(within_4, weight)  # within a year

    within_5 = np.zeros(fund_num)
    for i in range(0, fund_num):
        within_5[i] = float(nav[date - 1, j] / nav[date - 1 - (date % 252), j] - 1)  # this year
    data_all[5, 5] = weighted_avg(within_5, weight)  # this year

    ###---Step 6: Sharpe Ratio
    ##Sortino Ratio
    df = pd.DataFrame(columns=['downside_returns', 'Returns'])
    df['downside_returns'] = 0
    df['Returns'] = data_all[9]
    df.loc[df['Returns'] < 0, 'downside_returns'] = df['Returns'] ** 2
    expected_return = df['Returns'].mean()
    down_stdev = np.sqrt(df['downside_returns'].mean())
    sortino_ratio = expected_return / down_stdev
    data_all[6, 1] = sortino_ratio

    t4=[]
    t4.append(data_all[6, 0])
    t4.append(data_all[6, 1])

    ###---Step 7: Moving Volatility
    moving_std = np.zeros(date - 1)
    for i in range(0, date - 1):
        moving_std[i] = np.std(data_all[9, :(i + 2)])* np.sqrt(252)
    data_all[7, :-1] = moving_std


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

    g1 = {
        'title': {
            'text': '我的组合',
            'left': 'center'
        },
        'tooltip': {
            'trigger': 'item',
            'formatter': '{a} <br/>{b} : {c} ({d}%)'
        },
        'legend': {
            'orient': 'vertical',
            'left': 'left',
            'data': ['fd1', 'fd2', 'fd3', 'fd4', 'fd5']
        },
        'series': [
            {
                'name': '访问来源',
                'type': 'pie',
                'radius': '55%',
                'center': ['50%', '60%'],
                'data': [
                    {'value': return_data[0][length - 1], 'name': return_data[0][0]},
                    {'value': return_data[1][length - 1], 'name': return_data[1][0]},
                    {'value': return_data[2][length - 1], 'name': return_data[2][0]},
                    {'value': return_data[3][length - 1], 'name': return_data[3][0]},
                    {'value': return_data[4][length - 1], 'name': return_data[4][0]}
                ],
                'emphasis': {
                    'itemStyle': {
                        'shadowBlur': 10,
                        'shadowOffsetX': 0,
                        'shadowColor': 'rgba(0, 0, 0, 0.5)'
                    }
                }
            }
        ]
    };

    g2 = {
        'title': {
            'text': '折线图堆叠'
        },
        'tooltip': {
            'trigger': 'axis'
        },
        'legend': {
            'data': ['我的组合', '基准']
        },
        'grid': {
            'left': '3%',
            'right': '4%',
            'bottom': '3%',
            'containLabel': 'true'
        },
        'toolbox': {
            'feature': {
                'saveAsImage': {}
            }
        },
        'xAxis': {
            'type': 'category',
            'boundaryGap': 'false',
            'data': list(date_g[0][2:len(date_g)])
        },
        'yAxis': {
            'type': 'value'
        },
        'series': [
            {
                'name': '我的组合',
                'type': 'line',
                'stack': '总量',
                'data': list(data_all[1])
            },
            {
                'name': '基准',
                'type': 'line',
                'stack': '总量',
                'data': 0
            }
        ]
    };

    g3 = {
        'tooltip': {
            'trigger': 'axis',
            'axisPointer': {
                'type': 'cross',
                'crossStyle': {
                    'color': '#999'
                }
            }
        },
        'toolbox': {
            'feature': {
                'dataView': {'show': 'true', 'readOnly': 'false'},
                'magicType': {'show': 'true', 'type': ['line', 'bar']},
                'restore': {'show': 'true'},
                'saveAsImage': {'show': 'true'}
            }
        },
        'legend': {
            'data': ['我的组合', '降水量']
        },
        'xAxis': [
            {
                'type': 'category',
                'data': date_tmp,
                'axisPointer': {
                    'type': 'shadow'
                }
            }
        ],
        'yAxis': [
            {
                'type': 'value',
                'name': '我的组合'
            },
            {
                'type': 'value',
                'name': '基准'
            }
        ],
        'series': [
            {
                'name': '我的组合',
                'type': 'bar',
                'data': list(data_all[3])
            },
            {
                'name': '基准',
                'type': 'bar',
                'data': 0
            }
        ]
    };

    g4 = {
        'tooltip': {
            'trigger': 'axis',
            'axisPointer': {
                'type': 'cross',
                'crossStyle': {
                    'color': '#999'
                }
            }
        },
        'toolbox': {
            'feature': {
                'dataView': {'show': 'true', 'readOnly': 'false'},
                'magicType': {'show': 'true', 'type': ['line', 'bar']},
                'restore': {'show': 'true'},
                'saveAsImage': {'show': 'true'}
            }
        },
        'legend': {
            'data': ['我的组合', '降水量']
        },
        'xAxis': [
            {
                'type': 'category',
                'data': ['Sharpe Ratio','Sortino Ratio'],
                'axisPointer': {
                    'type': 'shadow'
                }
            }
        ],
        'yAxis': [
            {
                'type': 'value',
                'name': '我的组合'
            },
            {
                'type': 'value',
                'name': '基准'
            }
        ],
        'series': [
            {
                'name': '我的组合',
                'type': 'bar',
                'data': list(data_all[6])
            },
            {
                'name': '基准',
                'type': 'bar',
                'data': 0
            }
        ]
    };

    g5 = {
        'title': {
            'text': '折线图堆叠'
        },
        'tooltip': {
            'trigger': 'axis'
        },
        'legend': {
            'data': ['我的组合', '基准']
        },
        'grid': {
            'left': '3%',
            'right': '4%',
            'bottom': '3%',
            'containLabel': 'true'
        },
        'toolbox': {
            'feature': {
                'saveAsImage': {}
            }
        },
        'xAxis': {
            'type': 'category',
            'boundaryGap': 'false',
            'data': list(date_g[0][2:len(date_g)])
        },
        'yAxis': {
            'type': 'value'
        },
        'series': [
            {
                'name': '我的组合',
                'type': 'line',
                'stack': '总量',
                'data': list(data_all[7])
            },
            {
                'name': '基准',
                'type': 'line',
                'stack': '总量',
                'data': 0
            }
        ]
    };


    result = {
        'a': g1,
        'b': g2,
        'c': g3,
        'd': g4,
        'e': g5,
        'f': t5,
        'g':t4,
        'h':t3,
        'i':t2
    }
    return render_template('g1.html', result=result)


# else:
#     return render_template('option.html')


if __name__ == '__main__':
    app.run(debug=True)
