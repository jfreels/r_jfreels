calendarTable<-function (oneFundLongDataFrame,ITD=FALSE) {
  calendarTable<-dcast(oneFundLongDataFrame,year(date)~month(date),value.var="value")
  names(calendarTable)<-c("Year",month.abb)
  calendarTable<-arrange(calendarTable,-Year)
  row.names(calendarTable)<-calendarTable[,1]
  calendarTable<-calendarTable[,-1]
  calendarTable$YTD<-apply(calendarTable,1,aror)
  if(ITD==TRUE) { calendarTable$ITD<-rev(vami(rev(calendarTable$YTD))-1) }
  calendarTable
}