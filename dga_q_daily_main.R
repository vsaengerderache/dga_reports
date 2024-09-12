# library 
library(tidyverse)
library(readxl)

# folder path 
folder_files <- "dga_q_daily_reports_example" # path to folder with xls files (dga reports)
folder_files <- "C:\\010_r\\project_sociohydro_abm_flood_risk_r_discharge\\q_daily_dga_aux" # path to folder with xls files (dga reports)

# read files 
list_xls_file <- list.files(folder_files, full.names = TRUE, pattern = "\\.xls$") # list xls file
data_per_xls_file <- lapply(list_xls_file, function(xls_file) {
  sheets <- excel_sheets(xls_file)
  data_per_sheet <- setNames(lapply(sheets, function(sheet) {
    read_xls(xls_file, sheet = sheet, col_names = FALSE)
  }), sheets)  
  return(data_per_sheet)
}) # read each xls file

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
  #df <- data_per_xls_file[[1]][[1]]
  
  df_discharge <- df[11:nrow(df),]
  
  station <- as.character(df[6,3])  
  code_bna <- as.character(df[7,3])
  basin <- as.character(df[8,3])
  sub_basin <- as.character(df[9,3])
  
  elevation <- as.numeric(df[7,16])
  longitude <- as.character(df[8,16])
  latitude <- as.character(df[9,16])
  
  utm_north <- as.numeric(df[7,25])
  utm_east <- as.numeric(df[8,25])
  drainage_area <- as.numeric(df[9,25])
  
  # sort
  df_discharge <- df_discharge %>%
    dplyr::select(-c(seq(3, 22, 2), 23, 24, 26)) %>%
    mutate(let = str_remove_all(...1, "[^[:alpha:]]"),
           num = str_remove_all(...1, "[^0-9]")) %>%
    dplyr::select(-...1) %>%
    dplyr::select(let, num, everything())
  
  # filter
  row_year <- which(df_discharge$let == "DIA")
  row_year <- row_year - 1
  vp <- append(row_year, nrow(df_discharge))
  
  # create
  df_processed <- tibble(
    year = numeric(),
    month = numeric(),
    day = numeric(),
    date = Date(),
    discharge = numeric()
  )
  
  for (i in seq(2,length(row_year)))
  {
    ## extract data of one year 
    a <- as.numeric(vp[i-1]) #auxiliary value
    b <- as.numeric(vp[i]) #auxiliary value
    
    year_data <- df_discharge[c(seq(a,b)),] #data between months
    year <- year_data[[1,2]] # extract month and year data 

    year_data <- year_data[-c(1,2,nrow(year_data)),-c(1,2)] # delete 
    matriz_day   <- matrix(rep(seq(1,31), 12), ncol = 12) # date matriz
    df_day_aux   <- data.frame(matriz_day)
    
    matriz_month <- matrix(seq(1:12), ncol = 12)
    matriz_month <- matrix(rep(matriz_month[1,], 31), ncol = ncol(matriz_month), byrow = TRUE)
    df_month_aux <- data.frame(matriz_month)
    
    matriz_year <- matrix(rep(year, 31*12), ncol = 12)
    df_year_aux <- data.frame(matriz_year)
    
    year_data <- cbind.data.frame(year_data,df_day_aux,df_month_aux,df_year_aux)

    sort      <- sequence_sort() # sequence of sort 
    year_data <- year_data[,sort]
    
    # noname   
    colnames(year_data) <-rep(c("day","month","year","discharge"),12)
    
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
    
    # noname 
    year_data$date <- as.Date(with(year_data,paste(year,month,day,sep="/")),"%Y/%m/%d")
    year_data      <- year_data[!is.na(year_data$date),]
    
    year_data$year    <- as.numeric(year_data$year)
    year_data$discharge <- round(as.numeric(year_data$discharge),2)
    year_data <- year_data[,c(3,2,1,5,4)]   
    # merge years of data 
    df_processed <- merge(df_processed,year_data,all = TRUE)
  } 
  
  df_processed <- df_processed %>%
    mutate (date = as.Date(date, "%Y-%m-%d")) %>%
    mutate (station = station) %>%
    mutate (code_bna =  code_bna) %>%
    mutate (basin = basin) %>%
    mutate (sub_basin = sub_basin) %>%
    mutate (elevation = elevation) %>%
    mutate (latitude = latitude) %>%
    mutate (longitude = longitude) %>%
    mutate (drainage_area = drainage_area) %>%
    mutate (utm_north = utm_north) %>%
    mutate (utm_east = utm_east) %>%
    dplyr::select("date", "station", "code_bna", "basin", "sub_basin", "elevation", "latitude", "longitude", "drainage_area", "utm_north", "utm_east", "discharge") 
  
  return(df_processed)
}

# tidy data
q_daily <- map(data_per_xls_file, ~ map(., sort_data)) %>%
  unlist(.,recursive = FALSE) %>%
  bind_rows(.) %>%
  distinct(.) %>%
  arrange(date) %>%
  remove_rownames()
