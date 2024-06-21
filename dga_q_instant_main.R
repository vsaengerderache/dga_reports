# library ----

library(tidyverse)
library(readxl)

# 1) ruta a carpeta con reportes dga ----

# cambiar segun caso

folder_files <- "dga_q_instant_reports_example" # path a carpeta con archivos xls

# 2) leer archivos ----

archivos_xls <- list.files(folder_files, full.names = TRUE, pattern = "\\.xls$") # lista de archivos xls
datos_por_archivo <- lapply(archivos_xls, function(archivo) {

  hojas <- excel_sheets(archivo) # Obtener la lista de hojas del archivo Excel
  datos_por_hoja <- setNames(lapply(hojas, function(hoja) { # Leer cada hoja y almacenarla en una lista con nombre de hoja
    read_xls(archivo, sheet = hoja, col_names = FALSE)
  }), hojas)  # asignar el nombre de la hoja como nombre de la lista
  
  return(datos_por_hoja)
}) # leer cada archivo xls

# 3) ordenar datos ----

# function sort data    
sort_data <- function(df) 
{
  #df <- estaciones[[1]][[1]]
  
  df_name <- as.character(df[6,4])  
  df <- df[-c(seq(1,9)),-c(4,6,7,12,13,14,15,20,21)] #delete col and rows
  
  #rows with month data                                       
  row_month<-which(df$'...1' == 'MES:')
  
  # vector process                                              
  vp<- append(row_month, nrow(df))
  
  # process data
  df_processed<-data.frame()
  for (i in 2:length(vp))
  {
    #extract data of one month 
    a <-vp[i-1]                                 #auxiliary value
    b <-vp[i]                                   #auxiliary value
    month_data<-df[c(seq(a,b)),]                #data between months
    
    #extract month and year data  
    month_year <- data.frame(month_data[1,3])
    colnames(month_year) <- ("x")
    month_year <- month_year %>% separate(x, c("month", "year"))
    month <- as.numeric(month_year[1,1])
    year <- as.numeric(month_year[1,2])
    
    #delete useless info  
    month_data <- month_data[-c(seq(1,2)),]      #delete first 2 row
    month_data <- month_data[-nrow(month_data),] #delete last row
    month_data <- month_data[,-c(3,7,11)]        #delete height col
    
    # column names 
    colnames(month_data) <-rep(c("day","hour","flow"),3)
    
    # sort data using col names                                                         
    month_data<-rbind(month_data[1:3],
                      setNames(month_data[4:6], names(month_data)[1:3]),
                      setNames(month_data[7:9], names(month_data)[1:3]))     
    #join data 
    # add month and year data 
    month_data$year <-year                      #add year col
    month_data$month <-month                    #add month col
    month_data<-month_data %>% separate(hour, c("hour", "minute"))  
    month_data$day<-as.numeric(month_data$day) #numeric value day
    month_data$hour<-as.numeric(month_data$hour) 
    
    #numeric value hour
    month_data$minute<-as.numeric(month_data$minute)  
    #numeric value minute
    month_data$flow<-signif(as.numeric(month_data$flow),digits = 2)
    # add date col                                              #### 
    month_data$date<-with(month_data, 
                          ymd_hm(paste(year, month, day, hour, minute, sep= ' ')))
    month_data<-month_data[order(month_data$date),]
    
    # sort data    
    month_data <- month_data[,c(7,5,6,1,2,3,4)]   
    
    # merge months of data    
    df_processed <- bind_rows(df_processed, month_data)
  }
  
  df_output <- df_processed %>% select('date','flow') %>%
    mutate(station = df_name) %>%
    select(date, station, flow)
  
  return(df_output)
}

q_instant <- map(datos_por_archivo, ~ map(., sort_data)) %>%
  map(., ~ reduce(.x, bind_rows)) %>%
  bind_rows(.) %>%
  unique(.) %>%
  pivot_wider(names_from = station, values_from = flow) %>%
  arrange(date)
