jf.dates<-function(DF) {
  dat<-data.frame(date=DF$date,variable=DF$variable,value=DF$value)
  dat<-ddply(dat,.(variable),summarise,start_date=min(date),end_date=max(date))
  list(all=dat,
    max_start_date=max(dat$start_date),
    min_end_date=min(dat$end_date)
  )
}