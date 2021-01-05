library("dplyr")
library("PerformanceAnalytics")
library("rlist")
library("xts")
library("quantmod")

###read in initial file
data<-read.csv("0.csv",header=TRUE,sep=",")
sour<-read.csv("1.csv",header=TRUE,sep=",")
###input
ac<-c(1,2,3,4) #asset class
gf<-c(2,4,3,1,6,5,7) #geographical focus
riskave<-3 #risk aversion


filter<-function(data,sour,ac,gf,riskave){

###initialize
ac_all<-c()
gf_all<-c()

risk_all<-c() #annual volatility
outline_all<-data.frame()
fund_num<-ncol(data)/2

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
  print(gf[i])
  gffilter<-switch(i,match(data[which(data$Geographical.Focus == "Africa& Middle West Region"), ][,1],data[,1]),
                 match(data[which(data$Geographical.Focus == "Asian Pacific Region"), ][,1],data[,1]),
                 match(data[which(data$Geographical.Focus == "European Region"), ][,1],data[,1]),
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
    if(cn>50){break}}
}



for(k1 in fil){
  if(is.null(sour[2,(2*k1-1)])){break}
  if(as.Date(sour[1,(2*k1-1)])>="2014-12-01"){next}
  tmp<-as.character(sour[,(2*k1-1)])
  temp<-as.Date(tmp,format="%Y/%m/%d")
  data1<-data.frame(temp,sour[,2*k1])
  data1<-na.omit(data1,data1[,1:2])
  data1<-xts(data1[,2],data1[,1])
  #data1<-xts(data1[,2],as.Date(data1[,1]))
  colnames(data1)<-colnames(sour[(2*k1-1)])
  return<-Return.calculate(data1)
  return_cu<-Return.cumulative(return)
  data2<-merge.xts(data2,data1)
  risk<-StdDev.annualized(return)#risk
  risk_all<-c(risk_all,risk)
}

###calculate return and volatility
for(k1 in 1:fund_num){
  if(is.null(data[2,(2*k1-1)])){break}
  if(as.Date(data[1,(2*k1-1)])>="2014-12-01"){next}
  tmp<-as.character(data[,(2*k1-1)])
  temp<-as.Date(tmp,format="%Y/%m/%d")
  data1<-data.frame(temp,data[,2*k1])
  data1<-na.omit(data1,data1[,1:2])
  data1<-xts(data1[,2],data1[,1])
  #data1<-xts(data1[,2],as.Date(data1[,1]))
  colnames(data1)<-colnames(data[(2*k1-1)])
  return<-Return.calculate(data1)
  return_cu<-Return.cumulative(return)
  data2<-merge.xts(data2,data1)
  risk<-StdDev.annualized(return)#risk
  risk_all<-c(risk_all,risk)
}

fund_num<-max(index(risk_all))

###sort by volatility, and choose by input
sorted_risk_all<-sort(risk_all,decreasing=FALSE) 
reorder<-match(sorted_risk_all,risk_all)

###reform the initial data, ranking by volatility, 1 for the least volatility[1552*708]
for(k in reorder){
  reorderfund<-data2[,(k)]
  reorderfund<-na.omit(reorderfund,reorderfund[,1])
  reorderfund_all<-merge.xts(reorderfund_all,reorderfund)
}

itv<-ceiling((fund_num)/10)

###for every ins, find the first 5 funds
for(ins in 1:10){
  return_all<-c()
  sorted_return_all<-c()
  outfund_all<-c()
  #risk_all<-c()
  index_upper<-ceiling(fund_num/10)*ins
  if(ins==10)
  {index_upper<-fund_num}
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

intv<-5
final_index<-((riskave-1)*intv+1):(riskave*intv)
for(i in final_index){
  final<-writefund_all[,i]
  finalfund_all<-merge.xts(finalfund_all,final)
}

finalfund_all<-as.data.frame(finalfund_all)
write.csv(finalfund_all,file="fund_5.csv",na="")

return(finalfund_all)

}
