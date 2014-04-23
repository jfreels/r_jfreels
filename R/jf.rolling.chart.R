jf.rolling.chart<-function(longDataFrame,common=TRUE,width=12) {
  DF<-as.data.frame(longDataFrame)
  DT<-data.table(date=DF$date,variable=DF$variable,value=DF$value)
  common.start.date<-as.Date(max(DT[,list(start_date=min(date)),by=variable]$start_date))
  common.end.date<-as.Date(min(DT[,list(end_date=max(date)),by=variable]$end_date))
  ifelse(common,DT<-DT[date>=common.start.date&date<=common.end.date],DT) # common time period or full time period
  DT.start<-min(DT$date)
  DT.end<-max(DT$date)
  DT[,roll:=rollapplyr(value,width=width,FUN=aror,fill=NA),by=variable] # add the roll column
  DT[roll>0,sign:="positive",by=variable] # add the sign column
  DT[roll<0,sign:="negative",by=variable] # add the sign column
  p<-ggplot(DT,aes(x=as.Date(date),y=roll,group=variable,fill=sign))+geom_bar(stat='identity',position='identity')+
    theme(legend.position="none",
          plot.title = element_text(size=16, face="bold", hjust=0))+
    labs(x=NULL,y=paste0(width," Month Rolling Return (Annualized)"),title=paste0(width," Month Rolling Return (Annualized): ",DT.start," to ",DT.end))+
    scale_y_continuous(labels=percent)+ # make the y labels percentage
    scale_fill_manual(values=c("positive"=col.brew[8],"negative"=col.brew[4]))+ # positive values blue, negative values red
    facet_wrap(~variable,ncol=1)
    #scale_x_date(expand=c(0,0))
  print(p) 
}