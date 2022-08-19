#### library                                                     ####
library(readr)
library(dplyr)
library(ggplot2)
library(readxl)
library(reshape2)
library(writexl)
library(lubridate)
library(Hmisc)
library(tidyr)
#### entry data                                                  ####
df<- read_excel("data/08_2018_08_2021.xls", 
                col_names = FALSE)
#### extract data info                                           ####
station       <-df[6,4]
period        <-df[5,1]
code_watershed<-df[7,4]
watershed     <-df[8,4]
sub_watershed <-df[9,4]
#### delete useless info                                         ####
df<-df[-c(seq(1,9)),-c(4,6,7,12,13,14,15,20,21)] #delete col and rows
#### rows with month data                                        ####
row_month<-which(df$'...1' == 'MES:')
#### vector process                                              ####
vp<- append(row_month, nrow(df))
#### process data                                                ####
df_processed<-data.frame()
for (i in 2:length(vp))
{
  #### extract data of one month                                 ####
  a<-vp[i-1]                                 #auxiliary value
  b<-vp[i]                                   #auxiliary value
  month_data<-df[c(seq(a,b)),]               #data between months
  #### extract month and year data                               ####
  month_year<-data.frame(month_data[1,3])
  colnames(month_year)<-("x")
  month_year<-month_year %>% separate(x, c("month", "year"))
  month<-as.numeric(month_year[1,1])
  year<-as.numeric(month_year[1,2])
  #### delete useless info                                       ####
  month_data<-month_data[-c(seq(1,2)),]      #delete first 2 row
  month_data<-month_data[-nrow(month_data),] #delete last row
  month_data<-month_data[,-c(3,7,11)]        #delete height col
  #### column names                                              ####
  colnames(month_data) <-rep(c("day","hour","flow"),3)
  #### sort data using col names                                 ####                                            
  month_data<-rbind(month_data[1:3],
            setNames(month_data[4:6], names(month_data)[1:3]),
            setNames(month_data[7:9], names(month_data)[1:3]))     
                                             #join data 

  #### add month and year data                                   #### 
  month_data$year<-year                      #add year col
  month_data$month<-month                    #add month col
  month_data<-month_data %>% separate(hour, c("hour", "minute"))  
  month_data$day<-as.numeric(month_data$day) #numeric value day
  month_data$hour<-as.numeric(month_data$hour)  
                                             #numeric value hour
  month_data$minute<-as.numeric(month_data$minute)  
                                             #numeric value minute
  month_data$flow<-signif(as.numeric(month_data$flow),digits = 2)
  #### add date col                                              #### 
  month_data$date<-with(month_data, 
                        ymd_hm(paste(year, month, day, hour, minute, sep= ' ')))
  month_data<-month_data[order(month_data$date),]
  #### sort data                                                 ####
  month_data<-month_data[,c(7,5,6,1,2,3,4)]    #sort 
  #### merge months of data                                      ####
  df_processed<-merge(df_processed,month_data,all = TRUE)
}


