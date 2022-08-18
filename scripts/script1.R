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
#### entry data                                                 #####
df<- read_excel("data/08_2018_08_2021.xls")
#### extract data info                                          #####
station       <-df[5,4]
period        <-df[4,1]
code_watershed<-df[4,6]
watershed     <-df[4,7]
sub_watershed <-df[4,8]

month_year          <-data.frame(df[10,3])
colnames(month_year)<-("x")
month_year          <-month_year %>% separate(x, c("month", "year"))
month               <-as.numeric(month_year[1,1])
year                <-as.numeric(month_year[1,2])
#### delete useless info                                        #####
df<-df[-c(seq(1,10)),-c(4,6,7,12,13,14,15,20,21)] #del col and rows
#### column names                                               ####                                       
colnames(df) <-rep(c(
         "day",
         "hour",
         "height",
         "flow"),3)
#### sort data ####
df<-df[-1,-c(3,7,11)]                             ###borro encabezado y altura
df<-rbind(df[1:3],
          setNames(df[4:6], names(df)[1:3]),
          setNames(df[7:9], names(df)[1:3]))      ###junto datos serie de columnas
df$year<-year                                       ###a?ado columna a?o
df$month<-month                                      ###a?ado columna mes
df<-df[, c(4,5, 1, 2, 3)]                         ###reordeno
df<-df %>% separate(hour, c("hour", "minute"))    ###separo hora/minuto

df<-df %>% separate(hour, c("hour", "minute"))    ###separo hora/minuto

aaaaa
d


df<-df[, c(4,5, 1, 2, 3)]                         ###reordeno
df<-df %>% separate(hour, c("hour", "minute"))    ###separo hora/minuto

df$day<-as.numeric(df$day)                        ###valor numerico dia
df$hour<-as.numeric(df$hour)                      ###valor numerico hora
df<-df[, -5] 
df$flow<-signif(as.numeric(df$flow))
df<-df[-1,-c(3,7,11)]  
