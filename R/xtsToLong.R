xtsToLong<-function (Xts) { 
  df<-data.frame(date=index(Xts),coredata(Xts)) 
  names(df)<-c("date",names(Xts))
  df<-melt(df,id.vars="date")
  names(df)<-c("date","variable","value")
  df
}