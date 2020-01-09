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

seven<-function(){
    data = read.csv("writefund.csv",header = TRUE, fileEncoding = "UTF-8", sep=",", na.strings = "")
    data1_2 = read.csv("newfund.csv",header = TRUE, fileEncoding = "UTF-8", sep=",", na.strings = "")

    #print(data1_2[1,4])
    #print(data1_2[1,8])
    #print(ncol(data))
    data[,1] = as.Date(data[,1])
    fund_1 = data1_2[,8:9]
    fund_1[,1] = as.Date(fund_1[,1])
    fund_2 = cbind(as.Date(data1_2[,1]),data1_2[,5])

    max_date = nrow(data)
    fund_tmp = data.frame(沪深300 = 1:max_date,标普500 = 1:max_date)
    date_tmp = data.frame(沪深300 = 1:max_date,标普500 = 1:max_date)

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
        if(is.na(date_tmp[i,1]) || date_tmp[i,1] > data[i,1]){
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
        else if(date_tmp[i,1] < data[i,1]) {
            fund_tmp[(i):(fund_1_date),1] = fund_tmp[(i+1):(fund_1_date+1),1]
            date_tmp[(i):(fund_1_date),1] = date_tmp[(i+1):(fund_1_date+1),1]
            fund_tmp[fund_1_date,1] = NA
            date_tmp[fund_1_date,1] = NA
            fund_1_date = fund_1_date - 1
            i = i - 1
        }
        i = i + 1
    }

    i = 1
    while (i <= max_date){
        if(is.na(date_tmp[i,2]) || date_tmp[i,2] > data[i,1]){
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

#fund_completion<-function(data){

    #data<-read.csv("writefund.csv",header=TRUE,sep=",")
    data <- seven()
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

    write.csv(data,"complete.csv",na="",row.names = F)
#}