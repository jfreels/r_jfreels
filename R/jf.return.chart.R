jf.return.chart<-function(longDataFrame,common=TRUE) {
  DF<-as.data.frame(longDataFrame)
  DT<-data.table(date=DF$date,variable=DF$variable,value=DF$value)
  common.start.date<-as.Date(max(DT[,list(start_date=min(date)),by=variable]$start_date))
  common.end.date<-as.Date(min(DT[,list(end_date=max(date)),by=variable]$end_date))
  ifelse(common,DT<-DT[date>=common.start.date&date<=common.end.date],DT) # common time period or full time period
  DT.start<-min(DT$date)
  DT.end<-max(DT$date)
  DT[value>0,sign:="positive",by=variable] # add the sign column
  DT[value<0,sign:="negative",by=variable] # add the sign column
  p<-ggplot(DT,aes(x=as.Date(date),y=value,group=variable,fill=sign))+geom_bar(stat='identity',position='identity')+
    theme(legend.position="none",
          plot.title = element_text(size=16, face="bold", hjust=0))+
    #strip.text.y=element_text(angle=0,hjust=1))+ # rotate strip text horizontal
    labs(x=NULL,y=paste0("Monthly Returns"),title=paste0("Monthly Returns: ",DT.start," to ",DT.end))+
    scale_y_continuous(labels=percent)+ # make the y labels percentage
    scale_fill_manual(values=c("positive"=col.brew[8],"negative"=col.brew[4]))+ # positive values blue, negative values red
    facet_wrap(~variable,ncol=1)
  #scale_x_date(expand=c(0,0))
  print(p) 
}