calendarTable<-function (df,ITD=FALSE) {
  if(length(unique(df$variable))>1) stop('More than one variable in data frame.')
  df<-df %>%
    group_by() %>%
    select(date,value)
  df_dates<-jf.dates.completed(df)
  df_blank<-data.frame(date=df_dates)
  df_joined<-left_join(df_blank,df,by='date')
  calendarTable<-dcast(df_joined,year(date)~month(date),value.var="value")
  names(calendarTable)<-c("Year",month.abb)
  calendarTable<-arrange(calendarTable,desc(Year))
  row.names(calendarTable)<-calendarTable[,1]
  calendarTable<-calendarTable[,-1]
  calendarTable$YTD<-apply(calendarTable,1,cror)
  if(ITD==TRUE) { calendarTable$ITD<-rev(vami(rev(calendarTable$YTD))-1) }
  calendarTable
}