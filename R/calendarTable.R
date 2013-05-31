calendarTable<-function (oneFundLongDataFrame) {
  calendarTable<-dcast(oneFundLongDataFrame,year(date)~month(date),value.var="value")
  names(calendarTable)<-c("Year",month.abb)
  calendarTable<-arrange(calendarTable,-Year)
  row.names(calendarTable)<-calendarTable[,1]
  calendarTable<-calendarTable[,-1]
  calendarTable$YTD<-apply(calendarTable,1,aror)
  calendarTable$ITD<-rev(vami(rev(calendarTable$YTD))-1)
  calendarTable
}