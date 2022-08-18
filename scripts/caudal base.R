
##### libraries ####
library(dplyr)
library(readxl)
library(writexl)
library(lubridate)
library(Hmisc)
library(tidyr)

##### datos de entrada #####
dfc1<-data.frame(read_excel("G:/Mi unidad/005 UDEC/2022_11/011_3_PROYECTO HIDRÁULICO AMBIENTAL/01 PROYECTO EMBALSE PUNILLA/1001 MATERIAL DE TRABAJO/ENTREGA 3/002 MATERIAL DE TRABAJO/caudal base/altura_caudal_punilla_1.xls"))
dfc2<-data.frame(read_excel("G:/Mi unidad/005 UDEC/2022_11/011_3_PROYECTO HIDRÁULICO AMBIENTAL/01 PROYECTO EMBALSE PUNILLA/1001 MATERIAL DE TRABAJO/ENTREGA 3/002 MATERIAL DE TRABAJO/caudal base/altura_caudal_punilla_2.xls"))
dfc3<-data.frame(read_excel("G:/Mi unidad/005 UDEC/2022_11/011_3_PROYECTO HIDRÁULICO AMBIENTAL/01 PROYECTO EMBALSE PUNILLA/1001 MATERIAL DE TRABAJO/ENTREGA 3/002 MATERIAL DE TRABAJO/caudal base/altura_caudal_punilla_3.xls"))
dfc4<-data.frame(read_excel("G:/Mi unidad/005 UDEC/2022_11/011_3_PROYECTO HIDRÁULICO AMBIENTAL/01 PROYECTO EMBALSE PUNILLA/1001 MATERIAL DE TRABAJO/ENTREGA 3/002 MATERIAL DE TRABAJO/caudal base/altura_caudal_punilla_4.xls"))
dfc5<-data.frame(read_excel("G:/Mi unidad/005 UDEC/2022_11/011_3_PROYECTO HIDRÁULICO AMBIENTAL/01 PROYECTO EMBALSE PUNILLA/1001 MATERIAL DE TRABAJO/ENTREGA 3/002 MATERIAL DE TRABAJO/caudal base/altura_caudal_punilla_5.xls"))
dfc6<-data.frame(read_excel("G:/Mi unidad/005 UDEC/2022_11/011_3_PROYECTO HIDRÁULICO AMBIENTAL/01 PROYECTO EMBALSE PUNILLA/1001 MATERIAL DE TRABAJO/ENTREGA 3/002 MATERIAL DE TRABAJO/caudal base/altura_caudal_punilla_6.xls"))

##### ploteo #####

p1<-f_datos_instantaneos_DGA(dfc1)
p2<-f_datos_instantaneos_DGA(dfc2)
p3<-f_datos_instantaneos_DGA(dfc3)
p4<-f_datos_instantaneos_DGA(dfc4)
p5<-f_datos_instantaneos_DGA(dfc5)
p6<-f_datos_instantaneos_DGA(dfc6)

plot(p1[[1]])
plot(p2[[1]])
plot(p3[[1]])
plot(p4[[1]])
plot(p5[[1]])
plot(p6[[1]])

##### nombres data frame #####
df1<-p1[[2]]
df2<-(p2[[2]])
df3<-(p3[[2]])
df4<-(p4[[2]])
df5<-(p5[[2]])
df6<-(p6[[2]])
##### caudal_base####
qbase1<-f_caudal_base_prom(df1,"2006-06-04 13:00:00","2006-06-05 04:00:00")
qbase4<-f_caudal_base_prom(df4,"2009-08-11 02:00:00","2009-08-12 12:00:00")
qbase5<-f_caudal_base_prom(df5,"2009-07-01 21:00:00","2009-07-04 00:00:00")

qbase_prom<-mean(c(qbase1,qbase4,qbase5))


