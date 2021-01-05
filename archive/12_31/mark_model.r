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


#function(finnal_fund_all){
    ### Read data from last milestone
    setwd("C:/Users/EDZ/Desktop/理享家/Portfolio_Proj") #can be specified
    data = read.csv("finalfund.csv",header = TRUE, fileEncoding = "UTF-8", sep=",", na.strings = "")
    #data = final_fund_all
    rows = nrow(data)
    fund_num = ncol(data) - 1
    #tmp = as.character(data[,1])
    #data[,1] = as.Date(tmp, format="%Y/%m/%d")


#####These are the code for last version


###Complete all funds
    for (j in 1:fund_num){
        for (i in 1:rows){
            if(is.na(data[i,j+1])){
                if(i != rows){
                    data[i,j+1] = (data[i-1,j+1] + data[i+1,j+1]) /2
                }
                else{
                    data[i,j+1] = data[i-1,j+1]
                }
            }
        }
    }




###These are the code for last version
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

### Construct the system by Marko model
#Initialization
rows = nrow(data)
returns_tmp = c()
return = data.frame()
for (j in 1:fund_num){
    #return<-data[,j+1]
    #for(i in 1:rows-1){
    #    return[i+1] = data[i+1,j+1] / data[i,j+1] - 1.00
    #}
    data_tmp <- data.frame(data[,1],data[,j+1])
    data_tmp <- xts(data_tmp[,2],as.Date(data_tmp[,1]))
    return <- periodReturn(data_tmp,period="daily",type="log")
    returns_tmp <- cbind(returns_tmp,return)
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
max_ret <- add.objective(portfolio=init, type="return", name="mean")
opt_maxret <- optimize.portfolio(R=returns, portfolio=max_ret, optimize_method="ROI",trace=TRUE)
#print(opt_maxret)
#plot(opt_maxret, risk.col="StdDev", return.col="mean", main="Maximum Return Optimization", chart.assets=TRUE, xlim = c(0,1), ylim = c(0,30))

#6.3 Minimize variance with ROI
minvar <- add.objective(portfolio=init, type="risk", name="var")
opt_minvar <- optimize.portfolio(R=returns, portfolio=minvar, optimize_method="ROI", trace=TRUE)
print(opt_minvar)
plot(opt_minvar, risk.col="StdDev", return.col="mean", main="Minimum Variance Optimization", chart.assets=TRUE, xlim = c(0,1), ylim = c(0,200))

#6.4 Maximize quadratic utility with ROI
qu <- add.objective(portfolio=init, type="return", name="mean")
qu <- add.objective(portfolio=qu, type="risk", name="var", risk_aversion=0.25) #can be specified
opt_qu <- optimize.portfolio(R=returns, portfolio=qu, optimize_method="ROI", trace=TRUE)
#print(opt_qu)
#plot(opt_qu, risk.col="StdDev", return.col="mean", main="Quadratic Utility Optimization", chart.assets=TRUE, xlim = c(0,1), ylim = c(0,200))

#6.5 Minimize expected tail loss with ROI
etl <- add.objective(portfolio=init, type="risk", name="ETL")
opt_etl <- optimize.portfolio(R=returns, portfolio=etl, optimize_method="ROI", trace=TRUE)
#print(opt_etl)
#plot(opt_etl, risk.col="ES", return.col="mean", main="ETL Optimization", chart.assets=TRUE, xlim = c(0,1), ylim = c(0,200))

#6.6 Maximize mean return per unit ETL with random portfolios
meanETL <- add.objective(portfolio = init, type = "return", name = "mean")
meanETL <- add.objective(portfolio = meanETL, type="risk", name="ETL", arguments=list(p=0.95))
opt_meanETL <- optimize.portfolio(R=returns, portfolio = meanETL, optimize_method = "random", trace = TRUE, search_size = 2000)
#print(opt_meanETL)
stats_meanETL <-extractStats(opt_meanETL)
#head(stats_meanETL)
#plot(opt_meanETL, risk.col="ETL", return.col="mean", main="mean-ETL Optimization", neighbors=25)

w = extractWeights(min_risk)

#return w

