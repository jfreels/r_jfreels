jf.vami.chart<-function(longDataFrame,common=TRUE) {
  DF<-longDataFrame
  DT<-data.table(date=DF$date,variable=DF$variable,value=DF$value)
  DT$date<-as.Date(DT$date)
  DT2<-DT[,list(start_date=head(date,1),end_date=tail(date,1)),by=variable]
  common.start.date<-max(DT2[,start_date])
  common.end.date<-min(DT2[,end_date])
  ifelse(common,DT<-DT[date>=common.start.date&date<=common.end.date],DT)
  DT.start<-min(DT$date)
  DT.end<-max(DT$date)
  DT[,vami:=vami(value)-1,by=variable]
  p<-ggplot(DT,aes(x=as.Date(date),y=vami,group=variable))+geom_area(fill=col.brew[8],color="black")+
    theme(legend.position="none",
          plot.title = element_text(size=16, face="bold", hjust=0))+
    labs(x=NULL,y="Total Return",title=paste0("Total Return: ",DT.start," to ",DT.end))+
    scale_y_continuous(labels=percent)+
    facet_wrap(~variable,ncol=1)
    #scale_x_date(expand=c(0,0))
  print(p)
}
