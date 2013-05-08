# Rolling Correlation
jf.rolling.correlation<-function(DF,primary,n=12) {
  df.dcast<-na.omit(dcast(DF,date~variable,value.var='value'))
  df.zoo<-zoo(df.dcast[,-1],df.dcast[,1])
  primary=primary
  n=n
  dates<-index(df.zoo)[(n+1):nrow(df.zoo)]
  df.cor<-as.list(rep(NA,nrow(df.zoo)-n))
  for (i in 1:(nrow(df.zoo)-n)) {
    df.cor[[i]]<-cor(df.zoo[i:(i+(n-1)),])[,primary,drop=FALSE]
  }
  names(df.cor)<-dates
  df.cor<-llply(df.cor, function (x) { data.frame(variable=row.names(x),cor=x) })
  df.cor<-ldply(df.cor)
  names(df.cor)<-c("date","variable","value")
  df.cor$date<-as.Date(df.cor$date)
  df.cor
}