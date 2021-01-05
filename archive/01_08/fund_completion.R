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

#fund_completion<-function(data){

    data<-read.csv("writefund.csv",header=TRUE,sep=",")

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

    write.csv(data,"complete.csv",na="",row.names = F)
#}