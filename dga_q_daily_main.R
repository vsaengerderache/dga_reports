# library ----

library(tidyverse)
library(readxl)

# 1) ruta a carpeta con reportes dga ----
# cambiar segun caso

folder_files <- "dga_q_daily_reports_example" # path a carpeta con archivos xls

# 2) leer archivos ----

archivos_xls <- list.files(folder_files, full.names = TRUE, pattern = "\\.xls$") # lista de archivos xls
datos_por_archivo <- lapply(archivos_xls, function(archivo) {
  # Obtener la lista de hojas del archivo Excel
  hojas <- excel_sheets(archivo)
  # Leer cada hoja y almacenarla en una lista con nombre de hoja
  datos_por_hoja <- setNames(lapply(hojas, function(hoja) {
    read_xls(archivo, sheet = hoja, col_names = FALSE)
  }), hojas)  # asignar el nombre de la hoja como nombre de la lista
  # Devolver la lista de datos por hoja para este archivo
  return(datos_por_hoja)
}) # leer cada archivo xls

# 3) ordenar datos ----

# function sequence sort year data 
sequence_sort <- function()
{
  cont_1 <- 1
  cont_2 <- 2
  cont_3 <- 3
  cont_4 <- 4
  sequence  <- vector()
  
  for (i in seq(1:12)) 
  {
    i_aux_0   <- i
    i_aux_1   <- i+12
    i_aux_2   <- i+24
    i_aux_3   <- i+36
    
    sequence[cont_1] <- i_aux_1
    cont_1 <- cont_1 + 4
    
    sequence[cont_2] <- i_aux_2
    cont_2 <- cont_2 + 4
    
    sequence[cont_3] <- i_aux_3
    cont_3 <- cont_3 + 4
    
    sequence[cont_4] <- i_aux_0
    cont_4 <- cont_4 + 4
  }
  return(sequence)
}

# function sort data    
sort_data <- function(df) 
{
  #df <- datos_por_archivo[[1]][[1]]
  
  df_name <- as.character(df[6,3])  
  df_caudal <- df[11:nrow(df),]
  
  # ordenar datos de caudal                                   ####
  del_row   <- seq(3,22,2)
  del_row   <- c(del_row,23,24,26)
  df_caudal <- df_caudal[,-(del_row)]
  
  df_caudal$let <- gsub("[^[:alpha:]]","",df_caudal$...1)
  df_caudal$num <- gsub("[^0-9]","",df_caudal$...1)
  df_caudal     <- df_caudal[,-1]
  df_caudal     <- df_caudal[, c(13, 14,1:12)]
  # datos por year                                             ####
  row_year  <- which(df_caudal$let == 'AÑO')
  vp        <- append(row_year, nrow(df_caudal))
  df_processed <- data.frame(matrix(nrow = 0, ncol = 5))
  colnames(df_processed) = c("year",
                             "month",
                             "day",
                             "date",
                             "caudal")
  for (i in seq(2,5))
  {
    ## extract data of one year                                            ####
    a <- vp[i-1]                                 #auxiliary value
    b <- vp[i]                                   #auxiliary value
    
    year_data <- df_caudal[c(seq(a,b)),]         #data between months
    
    ## extract month and year data                                         ####
    year <- year_data[[1,2]]
    ## delete                                                              ####
    year_data <- year_data[-c(1,2,nrow(year_data)),-c(1,2)]
    ## date matriz                                                               ####
    matriz_day   <- matrix(rep(seq(1,31), 12), ncol = 12)
    df_day_aux   <- data.frame(matriz_day)
    
    matriz_month <- matrix(seq(1:12), ncol = 12)
    matriz_month <- matrix(rep(matriz_month[1,], 31), ncol = ncol(matriz_month), byrow = TRUE)
    df_month_aux <- data.frame(matriz_month)
    
    matriz_year <- matrix(rep(year, 31*12), ncol = 12)
    df_year_aux <- data.frame(matriz_year)
    
    year_data <- cbind.data.frame(year_data,df_day_aux,df_month_aux,df_year_aux)
    ## sequence of sort                                                    ####
    sort      <- sequence_sort()
    year_data <- year_data[,sort]
    ## noname                                                              ####
    colnames(year_data) <-rep(c("day","month","year","caudal"),12)
    
    year_data<-rbind(year_data[1:4],
                     setNames(year_data[5:8], names(year_data)[1:4]),
                     setNames(year_data[9:12], names(year_data)[1:4]),
                     setNames(year_data[13:16], names(year_data)[1:4]),
                     setNames(year_data[17:20], names(year_data)[1:4]),
                     setNames(year_data[21:24], names(year_data)[1:4]),
                     setNames(year_data[25:28], names(year_data)[1:4]),
                     setNames(year_data[29:32], names(year_data)[1:4]),
                     setNames(year_data[33:36], names(year_data)[1:4]),
                     setNames(year_data[37:40], names(year_data)[1:4]),
                     setNames(year_data[41:44], names(year_data)[1:4]),
                     setNames(year_data[45:48], names(year_data)[1:4])) 
    ## noname                                                              ####
    
    year_data$date <- as.Date(with(year_data,paste(year,month,day,sep="/")),"%Y/%m/%d")
    year_data      <- year_data[!is.na(year_data$date),]
    
    year_data$year    <- as.numeric(year_data$year)
    year_data$caudal <- round(as.numeric(year_data$caudal),2)
    year_data <- year_data[,c(3,2,1,5,4)]   
    ## merge years of data ####
    df_processed <- merge(df_processed,year_data,all = TRUE)
  } 
  
  df_processed <- df_processed[, c("date", "caudal")]
  # Renombrar la columna con el nombre del DataFrame
  
  colnames(df_processed)[2] <- df_name
  df_processed$date <- as.Date(df_processed$date, "%Y-%m-%d")
  
  return(df_processed)
}

q_daily <- map(datos_por_archivo, ~ map(., sort_data)) %>%
  #map(~ map(., na.omit)) %>%
  map(., ~ reduce(.x, merge, by = "date", all = TRUE)) %>%
  bind_rows(.) %>%
  group_by(date) %>%
  summarise_all (~ if(is.numeric(.)) first(.[!is.na(.)]) else first(.[!is.na(.)])) %>%
  arrange(date) %>%
  remove_rownames()
