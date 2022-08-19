################################################################
##Writen by Vicente Saenger Derache, Civil Engineering Student##                                                        ##
##        Univerity of Concepción, Concepción, Chile          ##
################################################################

#### library                                                    ####
library(readr)
library(dplyr)
library(ggplot2)
library(readxl)
library(reshape2)
library(writexl)
library(lubridate)
library(Hmisc)
library(tidyr)
#### entry data                                                 ####
df<- read_excel("data/08_2018_08_2021.xls", 
                col_names = FALSE)
#### extract data info                                          ####
station       <-df[5,5]
period        <-df[4,2]
code_watershed<-df[4,7]
watershed     <-df[4,8]
sub_watershed <-df[4,9]
#### seccion meses sin nombrar ####
month_data    <-df[which(df$...1== 'MES:'), ]
month_data    <-month_data[,3] 
colnames(month_data)<-"month/year"
#### delete useless info                                        ####
df<-df[-c(seq(1,9)),-c(4,6,7,12,13,14,15,20,21)] #del col and rows
#### column names                                               ####                                       
colnames(df) <-rep(c(
         "day",
         "hour",
         "height",
         "flow"),3)
#### sort data                                                  ####



#### trabajo anterior

#### extraer mes y ano ####
month_year          <-data.frame(df[10,3])
colnames(month_year)<-("x")
month_year          <-month_year %>% separate(x, c("month", "year"))
month               <-as.numeric(month_year[1,1])
year                <-as.numeric(month_year[1,2])

#### ####


df$year <-year                                    #a?ado columna a?o
df$month<-month                                   ###a?ado columna mes
df<-df[, c(4,5,1,2,3)]                            ###reordeno
df<-df %>% separate(hour, c("hour", "minute"))    ###separo hora/minuto
df<-df %>% separate(hour, c("hour", "minute"))    ###separo hora/minuto



df<-df[, c(4,5, 1, 2, 3)]                         ###reordeno
df<-df %>% separate(hour, c("hour", "minute"))    ###separo hora/minuto

df$day<-as.numeric(df$day)                        ###valor numerico dia
df$hour<-as.numeric(df$hour)                      ###valor numerico hora
df<-df[, -5] 
df$flow<-signif(as.numeric(df$flow))
df<-df[-1,-c(3,7,11)]  
