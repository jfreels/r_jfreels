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
  DT.summary
}

jf.tr.table<-function(DT) {
  setkey(DT,variable,date)
  max.date<-max(DT$date)
  MTD<-DT[,list(MTD=tail(value,1)),by=variable]
  YTD<-DT[year(date)==year(max.date),list(YTD=cror(value)),by=variable]
  dat<-join(MTD,YTD,by='variable')
  # managers with track records > 12 months
  names12<-as.character(DT[,.N-11,by=variable][V1>0]$variable)
  last12<-DT[names12,.SD[(.N-11):(.N)]][,list(last12=cror(value)),by=variable]
  dat<-join(dat,last12,by='variable')
  names24<-as.character(DT[,.N-23,by=variable][V1>0]$variable)
  last24<-DT[names24,.SD[(.N-23):(.N)]][,list(last24=cror(value)),by=variable]
  dat<-join(dat,last24,by='variable')
  names36<-as.character(DT[,.N-35,by=variable][V1>0]$variable)
  last36<-DT[names36,.SD[(.N-35):(.N)]][,list(last36=cror(value)),by=variable]
  dat<-join(dat,last36,by='variable')
  names60<-as.character(DT[,.N-59,by=variable][V1>0]$variable)
  last60<-DT[names60,.SD[(.N-59):(.N)]][,list(last60=cror(value)),by=variable]
  dat<-join(dat,last60,by='variable')
  dat
}