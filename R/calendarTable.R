calendarTable<-function (DT,ITD=FALSE) {
  DT<-DT[,list(date,value)]
  if(nrow(DT<12)) {
    calendarBlank<-data.frame(date=(max(DT$date)+days(1))+months(1:12)-days(1),value=rep(NA,12))
    calendarTable<-rbind(DT,calendarBlank)
  }
  calendarTable<-dcast(calendarTable,year(date)~month(date),value.var="value")
  names(calendarTable)<-c("Year",month.abb)
  calendarTable<-arrange(calendarTable,-Year)
  row.names(calendarTable)<-calendarTable[,1]
  calendarTable<-calendarTable[,-1]
  calendarTable<-calendarTable[apply(calendarTable,1,function(x) sum(is.na(x)))!=12,]
  calendarTable$YTD<-apply(calendarTable,1,aror)
  if(ITD==TRUE) { calendarTable$ITD<-rev(vami(rev(calendarTable$YTD))-1) }
  calendarTable
}