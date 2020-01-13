library("xml2")
library("dplyr")
library("stringr")
library("PortfolioAnalytics")

data <- read.csv("C:/Users/qiuch/Desktop/portfolio_proj/input/finalfund.csv",header = T, encoding="UTF-8")

setwd("C:/Users/qiuch/Desktop/portfolio_proj")

print(is.data.frame(data))
rows = nrow(data)

#a = as.Date('2013/1/25',format='%Y/%m/%d')

#date
fund_num = 5 # can be specified
for (j in 1:fund_num){
    tmp = as.character(data[,2*j])
    data[,2*j] = as.Date(tmp,format='%Y/%m/%d')
}

#date completion

#Step 1: find the fund with most dates 
fund_most_date = 1
for (fund_most_date in 1:fund_num){
    i = 1
    while(!is.na(data[i,2*fund_most_date])) {i = i + 1}
    print(fund_most_date)

    
    if (i == rows) break;
}

print(fund_most_date)
#Step 2: Complete all the other funds
if(fund_most_date != 1) {
    for (j in 1:(fund_most_date-1)){
        for(i in 1:rows){
            if(is.na(data[i,2*j]) || (data[i,2*j] > data[i,2*fund_most_date])){
                if(i != rows){
                    #move the date
                    data[(i+1):rows,2*j] = data[i:(rows-1),2*j]
                    data[(i+1):rows,2*j+1] = data[i:(rows-1),2*j+1]
                    #use inflation to estimate the NAV for the lacking date
                    data[i,2*j] = data[i,2*fund_most_date]
                    data[i,2*j+1] = (data[i-1,2*j+1] + data[i+1,2*j+1]) / 2
                }
                else{
                    data[i,2*j] = data[i,2*fund_most_date]
                }
            }
        }
    }
}

if(fund_most_date != fund_num){
    for (j in (fund_most_date+1):fund_num){
        for(i in 1:rows){
            if(is.na(data[i,2*j]) || (data[i,2*j] > data[i,2*fund_most_date])){
                if(i != rows){
                    #move the date
                    data[(i+1):rows,2*j] = data[i:(rows-1),2*j]
                    data[(i+1):rows,2*j+1] = data[i:(rows-1),2*j+1]
                    #use inflation to estimate the NAV for the lacking date
                    data[i,2*j] = data[i,2*fund_most_date]
                    data[i,2*j+1] = (data[i-1,2*j+1] + data[i+1,2*j+1]) / 2
                }
                else{
                    data[i,2*j] = data[i,2*fund_most_date]
                }
            }
        }
    }
}


