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
    #setwd("C:/Users/EDZ/Desktop/理享�?/Portfolio_Proj") #can be specified
    data = read.csv("complete.csv",header = TRUE, sep=",", na.strings = "")
    seven_data = data[,52:53]
    #data = final_fund_all

    #Number of funds in the final portforlio, can be specified
    portfolio_num = 5
    fund_num = portfolio_num*10


    fund_num_tmp = floor(fund_num/10)
    fund_num_upper = fund_num_tmp * ins
    if(ins == 10) 
    {
        fund_num_upper = fund_num
    }
    selected <- data[,(fund_num_tmp*(ins-1)+1+1):(fund_num_upper+1)] #first plus one�? begin with 6-10; second plus one: first column of data is date 
    selected <- cbind(data[,1],selected)
    print(selected)
    data <- selected
    
    data_print_out <- data
    data_print_out <- cbind(data_print_out,seven_data)
    #print(rownames(data))
    #date_tmp = as.Date(rownames(data),format="%Y-%m-%d")
    #data = cbind(date_tmp,data)

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
    #print.default(init)

    #6.3 Minimize variance with ROI
    minvar <- add.objective(portfolio=init, type="risk", name="var")
    opt_minvar <- optimize.portfolio(R=returns, portfolio=minvar, optimize_method="ROI", trace=TRUE)
    #print(opt_minvar)
    #plot(opt_minvar, risk.col="StdDev", return.col="mean", main="Minimum Variance Optimization", chart.assets=TRUE, xlim = c(0,1), ylim = c(0,200))

    w = extractWeights(opt_minvar)
    data_print_out_final = rbind(data_print_out,w)
    return_data = data_print_out_final[,-1]
    write.csv(return_data,"weight.csv",na="")
    return_data = rbind(colnames(return_data),return_data)
    return (return_data)
}
