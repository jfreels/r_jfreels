jf.calendar.chart<-function(longDataFrame,yearly=FALSE) {
  if(length(unique(longDataFrame$variable))>1) stop("Data Frame has more than 1 variable.")
  DF<-as.data.frame(longDataFrame)
  DT<-data.table(date=DF$date,variable=DF$variable,value=DF$value)
  DT.name<-droplevels(unique(DT$variable))[1]
  DT.start<-min(DT$date)
  DT.end<-max(DT$date)
  DT.dcast<-dcast(DT,year(date)~month(date),value.var="value")
  colnames(DT.dcast)<-c("year",month.abb)
  DT.melt<-melt(DT.dcast,id.var="year")
  DT.melt$name<-DT.name
  ifelse(yearly,DT.melt<-DT[,list(variable="year",value=cror(value),name=DT.name),by=year(date)],DT.melt)
  # chart the data
  p<-ggplot(DT.melt,aes(x=as.factor(variable),y=as.factor(year),fill=value,label=ifelse(is.na(value),NA,paste0(round(value*100,2),"%"))))+
    geom_tile(color="black")+geom_text()+
    theme_bw()+
    theme(legend.position="none",
          plot.title = element_text(size=16, face="bold", hjust=0))+
    labs(x=NULL,y=NULL,title=paste0("Track Record: ",DT.start," to ",DT.end))+
    facet_wrap(~name,ncol=1)+
    scale_fill_gradient2(low=col.brew[3],mid=col.brew[6],high=col.brew[9])
  print(p) 
}