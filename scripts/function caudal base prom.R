f_caudal_base_prom<-function(df,date1,date2) {
  rng <- interval(date1, date2)               #desired range
  q_base1<- df[df$df.FECHA %within% rng,]
  qprom<-mean(q_base1$df.CAUDAL)
  return(qprom)
}