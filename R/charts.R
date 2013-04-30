### Charting functions
  libs<-c("reshape2","plyr","quantmod","ggplot2","scales","data.table")
  lapply(libs,require,character.only=TRUE)

# colors
  require(RColorBrewer)
  col.brew = brewer.pal(name="RdBu",n=11)

# Test functions
  jf_dates<-function(DT) {
    list(all=DT[,list(start_date=min(date),end_date=max(date)),by=variable],
         max_start_date=max(DT[,list(start_date=min(date),end_date=max(date)),by=variable]$start_date),
         min_end_date=min(DT[,list(start_date=min(date),end_date=max(date)),by=variable]$end_date)
         )
  }

# Total Return Chart
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

# Drawdown Chart
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

# Rolling returns Chart
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

# Monthly Returns Chart
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

# Monthly Return Chart
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