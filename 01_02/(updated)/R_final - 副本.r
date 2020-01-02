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


###first filter
first_filter<-function(data,ins){

    risk_all<-c() #annual volatility
    outline_all<-data.frame()
    fund_num<-ncol(data)/2

    outfund_all<-data.frame()
    sel_all<-c()
    data2<-data.frame()

    ###calculate return and volatility
    ###first, eliminate funds begin after 2014-12-01, and convert data to standard format to prepare for later functions
    for(k1 in 1:fund_num){
        if(is.null(data[2,(2*k1-1)])){break}
        if(as.Date(data[1,(2*k1-1)])>="2014-12-01"){next}
        tmp<-as.character(data[,(2*k1-1)])
        temp<-as.Date(tmp,format="%Y/%m/%d")
        data1<-data.frame(temp,data[,2*k1])
        data1<-na.omit(data1,data1[,1:2])
        data1<-xts(data1[,2],as.Date(data1[,1]))
        colnames(data1)<-colnames(data[(2*k1-1)])
        return<-periodReturn(data1,period="yearly",type="log")
        data2<-merge.xts(data1,data2)
        risk<-var(return)#risk
        risk_all<-c(risk_all,risk)
    }
    fund_num<-max(index(risk_all))

    ###sort by volatility, and choose by input
    sorted_risk_all<-sort(risk_all,decreasing=FALSE) #in decreasing order  3��2*79+1:3*79 
    selected<-head(sorted_risk_all,ins*fund_num/10)[floor((ins-1)*fund_num/10+1):ceiling(ins*fund_num/10)]#because fund_num/10 might not be integer
    selected_index<-match(selected,risk_all)
    for(k in selected_index){
        outfund<-data2[,k]#
        outfund<-na.omit(outfund,outfund[,1])
        outfund_all<-merge.xts(outfund,outfund_all)}
        outfund_all<-as.data.frame(outfund_all)
        #write.csv(outfund_all,"outfund.csv",na="")
    return (outfund_all)
}



###second filter
second_filter<-function(data,single_fund_num){
    
    #print(rownames(data))
    date_tmp = as.Date(rownames(data),format="%Y-%m-%d")
    data = cbind(date_tmp,data)


    data2<-data.frame()
    return_all<-c()
    fund_num<-ncol(data)
    final_fund_all<-data.frame()

for(k1 in 1:fund_num){
    if(is.null(data[2,(k1+1)])){break}
    data1<-data.frame(data[,1],data[,(k1+1)])
    data1<-na.omit(data1,data1[,1:2])
    data1<-xts(data1[,2],as.Date(data1[,1]))
    return<-periodReturn(data1,period="yearly",type="log")
    return_avg<-mean(return)
    return_all<-c(return_avg,return_all)
    data2<-merge.xts(data1,data2)
}
    #Debugging
    if(FALSE){            
        print("rows for data")
        print(nrow(data))
        print(ncol(data))
        print(head(data[,1]))
        print(typeof(data[,1]))
        print(rownames(data))
        print(typeof(rownames(data)))
        print("rows for data")
        print(nrow(data))
        print(ncol(data))
        print("rows for data 2")
        print(nrow(data2))
        print(ncol(data2))
        print("data 2")
        print(head(data2,5))
    }

    #print(return_all)
    sorted_return_all<-sort(return_all,decreasing=TRUE)
    #print(sorted_return_all)
    selected_index<-match(head(sorted_return_all,single_fund_num),return_all)
    #print(selected_index)

    for(k in selected_index){
    final_fund<-data2[,(fund_num - k)]
    final_fund<-na.omit(final_fund,final_fund[,1])
    colnames(final_fund)<-colnames(data)[(k+1)]
    final_fund_all<-merge.xts(final_fund,final_fund_all)
    }
    final_fund_all<-as.data.frame(final_fund_all)
    
    #Debugging
    #dataall<-rbind(final_fund_all,return_all)
    #for(k in selected_index){
    #final_fund<-data.frame(data[,1],data[,(k+1)])
    #final_fund<-na.omit(final_fund,final_fund[,1:2])
    #colnames(final_fund)<-c("Date",colnames(data[k+1]))
    #final_fund_all<-bind_cols(final_fund,final_fund_all)
    #}

    #write.csv(final_fund_all,"finalfund.csv",na="")
    return (final_fund_all)
}



weights<-function(data) {
    ### Read data from last milestone
    #setwd("C:/Users/EDZ/Desktop/理享家/Portfolio_Proj") #can be specified
    #data = read.csv("finalfund.csv",header = TRUE, fileEncoding = "UTF-8", sep=",", na.strings = "")
    #data = final_fund_all

    #print(rownames(data))
    date_tmp = as.Date(rownames(data),format="%Y-%m-%d")
    data = cbind(date_tmp,data)

    rows = nrow(data)
    fund_num = ncol(data) - 1

    #Debugging
    if(FALSE){
        print("rows:")
        print(rows)
        print("cols:")
        print(ncol(data))
    }
    #tmp = as.character(data[,1])
    #data[,1] = as.Date(tmp, format="%Y/%m/%d")


    ###Step 1: Complete all funds
    for (j in 1:fund_num){
        for (i in 1:rows){
            if(is.na(data[i,j+1])){
                if(i != rows){
                    #last_data = data[i-1,j+1]
                    i_tmp = i + 1;
                    while(is.na(data[i_tmp,j+1])) i_tmp = i_tmp + 1
                    data[i,j+1] = (data[i-1,j+1] + data[i_tmp,j+1]) / 2
                }
                else{
                    data[i,j+1] = data[i-1,j+1]
                }
            }
        }
    }


    ###last version
    if(FALSE){
        if(FALSE){
            ###Step 1: find the fund with most dates
            fund_most_date = 1
            MAX_date = 1
            for (j in 1:fund_num){
                i = 1
                while(!is.na(data[i,j+1])) 
                    i = i + 1
                if(i > MAX_date) {
                    MAX_date = i
                    fund_most_date = j
                }
            }
        }
        if(fund_most_date != fund_num){
        for (j in (fund_most_date+1):fund_num){
            for (i in i:rows){
                if(is.na(data[i,j+1])){
                    if(i != rows){
                        #new input format do not require data moving
                        if(FALSE)
                        {
                            #move the dates
                            data[(i+1):rows,2*j] = data[i:(rows-1),2*j]
                            data[(i+1):rows, 2*j+1] = data[i:(rows-1),2*j+1]
                            data[i,2*j] = data[i,2*fund_most_date]
                            data[i,2*j+1] = (data[i-1,2*j+1] + data[i+1,2*j+1]) /2
                        }
                        data[i,j+1] = (data[i-1,j+1] + data[i+1,j+1]) /2
                    }
                    else{
                        data[i,j+1] = data[i-1,j+1]
                    }
                }
            }
        }
    }

        if(fund_most_date != fund_num){
            for (j in (fund_most_date+1):fund_num){

                for (i in i :rows){
                    if(is.na(data[i,2*j]) || (data[i,2*j] > data[i, 2*fund_most_date])){
                        if(i != rows){
                            #move the dates
                            data[(i+1):rows,2*j] = data[i:(rows-1),2*j]
                            data[(i+1):rows, 2*j+1] = data[i:(rows-1),2*j+1]
                            data[i,2*j] = data[i,2*fund_most_date]
                            data[i,2*j+1] = (data[i-1,2*j+1] + data[i+1,2*j+1]) /2
                        }
                        else{
                            data[i,2*j] = data[i,2*fund_most_date]

                            data[i,2*j+1] = data[i,2*fund_most_date+1]
                        }
                    }
                }
            }
        }
    }

    ### Step 2: Construct the system by Markowitz model
    #Initialization
    rows = nrow(data)
    returns_tmp = c()
    return_tmp = data.frame()

    for (j in 1:fund_num){
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
    init <- add.constraint(portfolio=init, type="leverage",min_sum=0.99, max_sum=1.01)
    init <- add.constraint(portfolio=init, type="box", min=0.10, max=0.40)
    #print.default(init)

    #6.2 Maximize mean return with ROI
    #max_ret <- add.objective(portfolio=init, type="return", name="mean")
    #opt_maxret <- optimize.portfolio(R=returns, portfolio=max_ret, optimize_method="ROI",trace=TRUE)
    #print(opt_maxret)
    #plot(opt_maxret, risk.col="StdDev", return.col="mean", main="Maximum Return Optimization", chart.assets=TRUE, xlim = c(0,1), ylim = c(0,30))

    #6.3 Minimize variance with ROI
    minvar <- add.objective(portfolio=init, type="risk", name="var")
    opt_minvar <- optimize.portfolio(R=returns, portfolio=minvar, optimize_method="ROI", trace=TRUE)
    #print(opt_minvar)
    #plot(opt_minvar, risk.col="StdDev", return.col="mean", main="Minimum Variance Optimization", chart.assets=TRUE, xlim = c(0,1), ylim = c(0,200))

    #6.4 Maximize quadratic utility with ROI
    #qu <- add.objective(portfolio=init, type="return", name="mean")
    #qu <- add.objective(portfolio=qu, type="risk", name="var", risk_aversion=0.25) #can be specified
    #opt_qu <- optimize.portfolio(R=returns, portfolio=qu, optimize_method="ROI", trace=TRUE)
    #print(opt_qu)
    #plot(opt_qu, risk.col="StdDev", return.col="mean", main="Quadratic Utility Optimization", chart.assets=TRUE, xlim = c(0,1), ylim = c(0,200))

    #6.5 Minimize expected tail loss with ROI
    #etl <- add.objective(portfolio=init, type="risk", name="ETL")
    #opt_etl <- optimize.portfolio(R=returns, portfolio=etl, optimize_method="ROI", trace=TRUE)
    #print(opt_etl)
    #plot(opt_etl, risk.col="ES", return.col="mean", main="ETL Optimization", chart.assets=TRUE, xlim = c(0,1), ylim = c(0,200))

    #6.6 Maximize mean return per unit ETL with random portfolios
    #meanETL <- add.objective(portfolio = init, type = "return", name = "mean")
    #meanETL <- add.objective(portfolio = meanETL, type="risk", name="ETL", arguments=list(p=0.95))
    #opt_meanETL <- optimize.portfolio(R=returns, portfolio = meanETL, optimize_method = "random", trace = TRUE, search_size = 2000)
    #print(opt_meanETL)
    #stats_meanETL <-extractStats(opt_meanETL)
    #head(stats_meanETL)
    #plot(opt_meanETL, risk.col="ETL", return.col="mean", main="mean-ETL Optimization", neighbors=25)

    w = extractWeights(opt_minvar)

    return (w)
}


R_final<-function(data,ins){
    filted_1 <- first_filter(data,ins)
    filted_2 <- second_filter(filted_1,5)
    w<-weights(filted_2)
    return (w)
}

data<-read.csv("test.csv",header=TRUE,sep=",")
#print(R_final(data,5))