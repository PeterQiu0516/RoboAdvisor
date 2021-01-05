library("dplyr")
library("PerformanceAnalytics")
library("rlist")
library("xts")
library("quantmod")



###read in initial file
data<-read.csv("test.csv",header=TRUE,sep=",")

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
  write.csv(outfund_all,"outfund.csv",na="")
return (outfund_all)}

outfund_all<-first_filter(data,5)



###second filter
data<-read.csv("outfund.csv",header=TRUE,sep=",")

second_filter<-function(data,single_fund_num){



data2<-data.frame()
return_all<-c()
fund_num<-ncol(data)
final_fund_all<-data.frame()

  for(k1 in 1:fund_num){
    if(is.null(data[2,(k1+1)])){break}
    data1<-data.frame(data[,1],data[,(k1+1)])
    data1<-na.omit(data1,data1[,1:2])
    data1<-xts(data1[,2],data1[,1])
    return<-periodReturn(data1,period="yearly",type="log")
    return_avg<-mean(return)
    return_all<-c(return_avg,return_all)
    data2<-merge.xts(data1,data2)
  }

    if(FALSE){
          print(head(data[,1]))
    print(typeof(data[,1]))
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
#dataall<-rbind(final_fund_all,return_all)


#for(k in selected_index){
 #final_fund<-data.frame(data[,1],data[,(k+1)])
 #final_fund<-na.omit(final_fund,final_fund[,1:2])
 #colnames(final_fund)<-c("Date",colnames(data[k+1]))
 #final_fund_all<-bind_cols(final_fund,final_fund_all)
#}

write.csv(final_fund_all,"finalfund_1.csv",na="")
return (final_fund_all)
}

final_fund_all<-second_filter(data,5)
