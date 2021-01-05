library("xml2")
library("dplyr")
library("stringr")
library("rlist")
library("xts")
library("quantmod")
library("PortfolioAnalytics")
library("DEoptim")

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

#data = cbind(data, fund_1)
#data = cbind(data, fund_2)
#print(ncol(data))
#print(data[1,ncol(data)+1])
#print(data[1,ncol(data)+2])