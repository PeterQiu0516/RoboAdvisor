library("dplyr")
library("PerformanceAnalytics")
library("rlist")
library("xts")
library("quantmod")

###read in initial file
data<-read.csv("filter.csv",header=TRUE,sep=",")
sour<-read.csv("bloomberg.csv",header=T,sep=",")
#a<-read.table("D:\\JI\\1.csv",sep=",",header=TRUE,fileEncoding = "UTF-8",fill=TRUE)
###input
ac<-c(1,2,3,4)
gf<-c(2,4,3,1,6,5,7)
riskave<-5


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
    write.csv(finalfund_all,file="fivefund.csv",na="")
    finalfund_all = cbind(rownames(finalfund_all),finalfund_all)
    return(finalfund_all)
}

finalfund_all<-filter(data,sour,ac,gf,riskave)
