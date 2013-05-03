dt.rolling.table<-function(DT,common=TRUE,n=12) {
  DT<-data.table(date=DT$date,variable=DT$variable,value=DT$value,key=c("variable","date"))
  DT.dates<-jf.dates(DT)
  start_date<-DT.dates$max_start_date
  end_date<-DT.dates$min_end_date
  ifelse(common,DT<-DT[date>=start_date & date<=end_date],DT)
  DT[,roll:=roll.cror(value,n),by=variable]
  DT.summary<-DT[,list(max.roll=max(roll,na.rm=TRUE),
                       min.roll=min(roll,na.rm=TRUE),
                       avg.roll=mean(roll,na.rm=TRUE),
                       up.roll=sum(roll>0,na.rm=TRUE)/length(na.omit(roll)),
                       start=min(date),
                       end=max(date)),by=variable]
  setnames(DT.summary,old=c("max.roll","min.roll","avg.roll","up.roll"),new=c(paste0("max.",n),paste0("min.",n),paste0("avg.",n),paste0("up.",n)))
  as.data.frame(DT.summary)
}