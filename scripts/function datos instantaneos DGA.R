f_datos_instantaneos_DGA<-function(df) {
  ##### calculos intermedios #####
  ### se puede sacar periodo, coordenadas, estación, codigo, cuenca,area drenaje etc###
  estación<-df[5,4]
  periodo<-df[4,1]
  mes_año<-data.frame(df[10,3])
  colnames(mes_año)<-("x")
  mes_año<-mes_año %>% separate(x, c("mes", "año"))
  mes<-as.numeric(mes_año[1,1])
  año<-as.numeric(mes_año[1,2])
  ##### borrar información no necesaria #####
  df<-df[-c(seq(1,10)),-c(4,6,7,12,13,14,15,20,21)] ###borro filas superiores
  df<-df[-(nrow(df)),]                              ###borro fila inferior
  ###
  a<-rep(c("DIA",
           "HORA",
           "ALTURA",
           "CAUDAL"),3)
  colnames(df) <- a                                 ###nombre encabezado
  df<-df[-1,-c(3,7,11)]                             ###borro encabezado y altura
  ##### ordenar data frame #####
  df<-rbind(df[1:3],
            setNames(df[4:6], names(df)[1:3]),
            setNames(df[7:9], names(df)[1:3]))      ###junto datos serie de columnas
  df$AÑO<-año                                       ###añado columna año
  df$MES<-mes                                       ###añado columna mes
  df<-df[, c(4,5, 1, 2, 3)]                         ###reordeno
  df<-df %>% separate(HORA, c("HORA", "MINUTO"))    ###separo hora/minuto
  
  df$DIA<-as.numeric(df$DIA)                        ###valor numerico dia
  df$HORA<-as.numeric(df$HORA)                      ###valor numerico hora
  df<-df[, -5] 
  df$CAUDAL<-signif(as.numeric(df$CAUDAL))
  #df <-df[order(df$ymdh),]
  
  df$FECHA<-with(df, ymd_h(paste(AÑO, MES, DIA, HORA, sep= ' ')))
  df <-df[order(df$FECHA),]
  ##### plotear ####
  grafico<-ggplot(data = df, aes(x = FECHA, y = CAUDAL))+
    geom_line() +
    labs(title="Caudal en estación:",subtitle=estación,
         x=periodo, 
         y="CAUDAL (m3/s)")+
    scale_y_continuous(breaks=c(seq(0,2000,100)))+
    theme_bw()
  ##### output #####
  df<-data.frame(df$FECHA,df$CAUDAL) 
  return(list(grafico,df))
}