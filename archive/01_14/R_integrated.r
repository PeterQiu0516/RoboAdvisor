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


filter<-function(data,sour,ac,gf,riskave){
    ###initialize
    ac_all<-c()
    gf_all<-c()

    risk_all<-c() #annual volatility
    outline_all<-data.frame()
    fund_num<-ncol(sour)/2

    outfund_all<-data.frame()
    sel_all<-c()
    data2<-data.frame()
    reorderfund_all<-data.frame()
    return_all<-c()
    sorted_return_all<-c()
    writefund_all<-data.frame()
    finalfund_all<-data.frame()


    ###select asset class
    for(i in 1:4){
        if(ac[i]>3){next}
        acfilter<-switch(i,match(data[which(data$Asset.Class == "Equity"), ][,1],data[,1]),
            match(data[which(data$Asset.Class == "Fixed Income"), ][,1],data[,1]),
            match(data[which(data$Asset.Class == "Mixed Allocation"), ][,1],data[,1]),
            match(data[which(data$Asset.Class == "Money Market"), ][,1],data[,1]))
        ac_all<-c(ac_all,acfilter)
    }

    ###select geographical focus
    for(i in 1:7){
        if(gf[i]>4){next}
        gffilter<-switch(i,match(data[which(data$Geographical.Focus == "Africa& Middle West Region"), ][,1],data[,1]),
            match(data[which(data$Geographical.Focus == "Asian Pacific Region"), ][,1],data[,1]),
            match(data[which(data$Geographical.Focus == "Greater China"), ][,1],data[,1]),
            match(data[which(data$Geographical.Focus == "International"), ][,1],data[,1]),
            match(data[which(data$Geographical.Focus == "Latin American Region"), ][,1],data[,1]),
            match(data[which(data$Geographical.Focus == "U.S."), ][,1],data[,1]))
        gf_all<-c(gf_all,gffilter)
    }

    ###first selected 
    fil<-intersect(gf_all,ac_all)
    cn<-max(index(fil))#total amount of funds we've got in the first selection

    ###continue to choose if first selected funds are less than 50

    while(cn<50){
        for(i in 1:7){
            if(gf[i]<5){next}
            gffilter<-switch(i,match(data[which(data$Geographical.Focus == "Africa& Middle West Region"), ][,1],data[,1]),
                match(data[which(data$Geographical.Focus == "Asian Pacific Region"), ][,1],data[,1]),
                match(data[which(data$Geographical.Focus == "European Region"), ][,1],data[,1]),
                match(data[which(data$Geographical.Focus == "Greater China"), ][,1],data[,1]),
                match(data[which(data$Geographical.Focus == "International"), ][,1],data[,1]),
                match(data[which(data$Geographical.Focus == "Latin American Region"), ][,1],data[,1]),
                match(data[which(data$Geographical.Focus == "U.S."), ][,1],data[,1]))
            gf_all<-c(gf_all,gffilter)
            fil<-intersect(gf_all,ac_all)
            cn<-max(index(fil))
            if(cn>50){break}
        }
    }




    for(k1 in fil){
        if(is.null(sour[2,(2*k1-1)])){break}
        if(as.Date(sour[1,(2*k1-1)])>="2014-12-01"){next}
        #print(111)
        #print(sour[1,(2*k1-1)])}
        tmp<-as.character(sour[,(2*k1-1)])
        temp<-as.Date(tmp,format="%Y/%m/%d")
        #print(222)
        
        data1<-data.frame(temp,sour[,2*k1])
        data1<-na.omit(data1,data1[,1:2])
        data1<-xts(data1[,2],data1[,1])
        #data1<-xts(data1[,2],as.Date(data1[,1]))
        colnames(data1)<-colnames(sour[(2*k1-1)])
        return<-Return.calculate(data1)
        #print(555)
        return_cu<-Return.cumulative(return)
        data2<-merge.xts(data2,data1)
        risk<-StdDev.annualized(return)#risk
        risk_all<-c(risk_all,risk)
    }
    #print("out!")
    fund_num<-ncol(data2)
    #print("hi")
    ###sort by volatility, and choose by input

    sorted_risk_all<-sort(risk_all,decreasing=FALSE) 
    reorder<-match(sorted_risk_all,risk_all)

    ###reform the initial data, ranking by volatility, 1 for the least volatility[1552*708]
    for(k in reorder){
        reorderfund<-data2[,(k)]
        reorderfund<-na.omit(reorderfund,reorderfund[,1])
        reorderfund_all<-merge.xts(reorderfund_all,reorderfund)
    }
    #print("hi")

    group_num = 5###客户风险接受程度划分组数
    itv<-ceiling((fund_num)/group_num)

    ###for every 风险接受程度, find the first 5 funds
    
    for(ins in 1:group_num){
        return_all<-c()
        sorted_return_all<-c()
        outfund_all<-c()
        #risk_all<-c()
        index_upper<-itv*ins
        if(ins==group_num)
        {
            index_upper<-fund_num
        }
        selected<-((ins-1)*itv+1):index_upper#because fund_num/10 might not be integer
        
        ###for every ins, 3 for instance, find funds for this ins and calculate their average return
        for(k in selected){ 
            outfund<-reorderfund_all[,k]#
            outfund<-na.omit(outfund,outfund[,1])
            #outfund<-xts(outfund[,2],as.Date(outfund[,1]))
            return<-Return.calculate(outfund)
            return_cu<-Return.cumulative(return)
            return_all<-c(return_all,return_cu)
            outfund_all<-merge.xts(outfund_all,outfund)
        }
        sorted_return_all<-sort(return_all,decreasing=TRUE)
        selected_index<-match(head(sorted_return_all,5),return_all)
        
        
        ###find those 5 funds
        for(i in selected_index){
            writefund<-outfund_all[,i]
            writefund_all<-merge.xts(writefund_all,writefund)
            fund1<-na.omit(writefund,writefund[,1])
            ###for debugging
            #r<-Return.calculate(fund1)
            #rcu<-Return.cumulative(r)
            #print(rcu)
            #ri<-StdDev.annualized(r)#risk
            #print(ri)
        }
}

#writefund_all<-as.data.frame(writefund_all)
#write.csv(writefund_all,file="writefund.csv",na="")
    #print(ncol(writefund_all))
    #print(final_index)
    intv<-5
    final_index<-((riskave-1)*intv+1):(riskave*intv)
    for(i in final_index){
        final<-writefund_all[,i]
        finalfund_all<-merge.xts(finalfund_all,final)
    }

    finalfund_all<-as.data.frame(finalfund_all)
    #write.csv(finalfund_all,file="fivefund.csv",na="")
    finalfund_all = cbind(rownames(finalfund_all),finalfund_all)
    return(finalfund_all)
}

seven<-function(data){
    #data = read.csv("fund_5.csv",header = TRUE, fileEncoding = "UTF-8", sep=",", na.strings = "")
    data1_2 = read.csv("newfund.csv",header = TRUE, fileEncoding = "UTF-8", sep=",", na.strings = "")

    #print(data1_2[1,4])
    #print(data1_2[1,8])
    #print(ncol(data))
    data[,1] = as.Date(data[,1])
    fund_1 = data1_2[,8:9]
    fund_1[,1] = as.Date(fund_1[,1])
    fund_2 = cbind(as.Date(data1_2[,1]),data1_2[,5])

    max_date = nrow(data)
    fund_tmp = data.frame(base1 = -1*(1:max_date),base2 = -1*(1:max_date))
    date_tmp = data.frame(base1 = -1*(1:max_date),base2 = -1*(1:max_date))

    fund_1_date = 1
    fund_2_date = nrow(data1_2)
    while(!is.na(fund_1[fund_1_date,1])) fund_1_date = fund_1_date+1
    fund_1_date = fund_1_date - 1
    fund_tmp[1:fund_1_date,1] = fund_1[1:fund_1_date,2]
    fund_tmp[1:fund_2_date,2] = fund_2[1:fund_2_date,2]
    date_tmp[1:fund_1_date,1] = fund_1[1:fund_1_date,1]
    date_tmp[1:fund_2_date,2] = fund_2[1:fund_2_date,1]

    i = 1
    while (i <= max_date){
        if(is.na(date_tmp[i,1])||date_tmp[i,1]<0){
            date_tmp[i,1] = data[i,1]
            fund_tmp[i,1] = NA
            fund_1_date = fund_1_date + 1
        }
        else if(date_tmp[i,1] > data[i,1]){
            if(i < max_date){
                fund_tmp[(i+1):(fund_1_date+1),1] = fund_tmp[(i):(fund_1_date),1]
                date_tmp[(i+1):(fund_1_date+1),1] = date_tmp[(i):(fund_1_date),1]
                date_tmp[i,1] = data[i,1]
                fund_tmp[i,1] = NA
            }
            else if (i == max_date){
                date_tmp[i,1] = data[i,1]
                fund_tmp[i,1] = NA
            }
            fund_1_date = fund_1_date + 1
        }
        else if(date_tmp[i,1] < data[i,1]){
            #print(i)
            #print(date_tmp[i,1])
            #print(data[i,1])
            #print(fund_1_date)
            fund_tmp[(i):(fund_1_date),1] = fund_tmp[(i+1):(fund_1_date+1),1]
            date_tmp[(i):(fund_1_date),1] = date_tmp[(i+1):(fund_1_date+1),1]
            fund_tmp[fund_1_date,1] = NA
            date_tmp[fund_1_date,1] = NA
            fund_1_date = fund_1_date - 1
            i = i - 1
        }
        i = i + 1
        #print(i)
    }

    i = 1
    while (i <= max_date){
        if(is.na(date_tmp[i,2])||date_tmp[i,2]<0){
            date_tmp[i,2] = data[i,1]
            fund_tmp[i,2] = NA
            fund_2_date = fund_2_date + 1
        }
        else if(date_tmp[i,2] > data[i,1]){
            if(i < max_date){
                fund_tmp[(i+1):(fund_2_date+1),2] = fund_tmp[(i):(fund_2_date),2]
                date_tmp[(i+1):(fund_2_date+1),2] = date_tmp[(i):(fund_2_date),2]
                date_tmp[i,2] = data[i,1]
                fund_tmp[i,2] = NA
            }
            else if (i == max_date){
                date_tmp[i,2] = data[i,1]
                fund_tmp[i,2] = NA
            }
            fund_2_date = fund_2_date + 1
        }
        else if(date_tmp[i,2] < data[i,1]){
            #print(i)
            #print(date_tmp[i,2])
            #print(data[i,1])
            #print(fund_2_date)
            fund_tmp[(i):(fund_2_date),2] = fund_tmp[(i+1):(fund_2_date+1),2]
            date_tmp[(i):(fund_2_date),2] = date_tmp[(i+1):(fund_2_date+1),2]
            fund_tmp[fund_2_date,2] = NA
            date_tmp[fund_2_date,2] = NA
            fund_2_date = fund_2_date - 1
            i = i - 1
        }
        i = i + 1
        #print(i)
    }

    fund_tmp = fund_tmp[1:max_date,]
    data = cbind(data,fund_tmp)
    return (data)
}

fund_completion<-function(data){
    #data<-read.csv("fund_5.csv",header=TRUE,sep=",")
    data <- seven(data)
    #print(data)
    
    rows = nrow(data)
    fund_num = ncol(data) - 1
    ###Step 1: Complete all funds
    for (j in 1:fund_num){
        for (i in 1:rows){
            if(is.na(data[i,j+1])){
                if(i != rows){
                    #last_data = data[i-1,j+1]
                    i_tmp = i + 1;
                    while(is.na(data[i_tmp,j+1]) && i_tmp<=rows) 
                    {
                        i_tmp = i_tmp + 1
                        #print(j)
                        #print(i_tmp)
                    }
                    if(i == 1) 
                    {
                        data[i,j+1] = data[i_tmp,j+1]
                    }
                    else if (i_tmp == rows + 1) {
                        data[i,j+1] = data[i-1,j+1]
                    }
                    else{
                        data[i,j+1] = (data[i-1,j+1] + data[i_tmp,j+1]) / 2
                    }
                }
                else{
                    data[i,j+1] = data[i-1,j+1]
                }
            }
        }
    }

    return(data)
    #write.csv(data,"complete.csv",na="",row.names = F)
}

weights<-function(data) {
    ### Read data from last milestone
    #setwd("C:/Users/EDZ/Desktop/理享�?/Portfolio_Proj") #can be specified
    #data = read.csv("complete.csv",header = TRUE, sep=",", na.strings = "")
    seven_data = data[,7:8]
    #data = final_fund_all

    #Number of funds in the final portforlio, can be specified
    portfolio_num = 5
    fund_num = portfolio_num*10

    if(FALSE){
        fund_num_tmp = floor(fund_num/10)
        fund_num_upper = fund_num_tmp * ins
        if(ins == 10) 
        {
            fund_num_upper = fund_num
        }
        selected <- data[,(fund_num_tmp*(ins-1)+1+1):(fund_num_upper+1)] #first plus one�? begin with 6-10; second plus one: first column of data is date 
        selected <- cbind(data[,1],selected)
        #print(selected)
        data <- selected
    }
    
    data_print_out <- data
    #data_print_out <- cbind(data_print_out,seven_data)
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
    #print(head(data_print_out,5))
    #print(typeof(data_print_out[1,1]))
    #print(data_print_out[1,1])
    #print(head(w))
    #print(typeof(w[1]))

    data_print_out[,1] = as.integer(data_print_out[,1])
    data_print_out_final = rbind(data_print_out,w)
    #print("hi")
    return_data = data_print_out_final[,-1]
    #write.csv(return_data,"weight.csv",na="")
    return_data = rbind(colnames(return_data),return_data)
    return (return_data)
}

R_final<-function(init,source,asset_class,geo,risk_aversion){
    init<-read.csv("filter.csv",header=TRUE, sep=",")
    source<-read.csv("bloomberg.csv",header=T,sep=",")
    if(FALSE){
        asset_class<-c(1,2,3,4)
        geo<-c(2,4,3,1,6,5,7)
        risk_aversion<-3 #may get specified
    }
    level_1<-filter(init,source,asset_class,geo,risk_aversion)
    level_2<-fund_completion(level_1)
    level_3<-weights(level_2)
    return(level_3)
}