jf.rolling.table<-function(DF,n=12,common=TRUE) {
	DF<-data.frame(date=DF$date,variable=DF$variable,value=DF$value)
  DF<-arrange(DF,variable,date)
  DF.dates<-jf.dates(DF)
  start_date<-DF.dates$max_start_date
  end_date<-DF.dates$min_end_date
  ifelse(common,DF<-subset(DF,date>=start_date & date<=end_date),DF)
  DF<-ddply(DF,.(variable),transform,roll=rollapplyr(value,FUN=aror,width=n,fill=NA))
  DF<-ddply(DF,.(variable),summarise,
            max.roll=max(roll,na.rm=TRUE),
            min.roll=min(roll,na.rm=TRUE),
            avg.roll=mean(roll,na.rm=TRUE),
            up.roll=sum(roll>0,na.rm=TRUE)/length(na.omit(roll)),
            start=min(date),
            end=max(date))
  names(DF)<-c("variable",paste0("max.",n),paste0("min.",n),paste0("avg.",n),paste0("up.",n),"start","end")
  DF
}
