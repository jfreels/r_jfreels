jf.dd.chart<-function(longDataFrame,common=TRUE) {
  DF<-as.data.frame(longDataFrame)
  DT<-data.table(date=DF$date,variable=DF$variable,value=DF$value)
  common.start.date<-as.Date(max(DT[,list(start_date=min(date)),by=variable]$start_date))
  common.end.date<-as.Date(min(DT[,list(end_date=max(date)),by=variable]$end_date))
  ifelse(common,DT<-DT[date>=common.start.date&date<=common.end.date],DT)
  DT.start<-min(DT$date)
  DT.end<-max(DT$date)
  DT[,dd:=dd(value),by=variable]
  p<-ggplot(DT,aes(x=as.Date(date),y=dd,group=variable))+geom_area(fill=col.brew[4],color="black")+
    theme(legend.position="none",
          plot.title = element_text(size=16, face="bold", hjust=0))+
    labs(x=NULL,y="Drawdown",title=paste0("Drawdown: ",DT.start," to ",DT.end))+
    scale_y_continuous(labels=percent)+
    facet_wrap(~variable,ncol=1)
    #scale_x_date(expand=c(0,0))
  print(p) 
}