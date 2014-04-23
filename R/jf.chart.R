### Charting functions
  libs<-c("reshape2","plyr","quantmod","ggplot2","scales","data.table")
  lapply(libs,require,character.only=TRUE)
# colors
  require(RColorBrewer)
  col.brew = brewer.pal(name="RdBu",n=11)
# data.frame Charts
jf.chart<-function(DF,type="cror",common=TRUE) {
  DF<-data.frame(date=DF$date,variable=DF$variable,value=DF$value)
  DF<-arrange(DF,variable,date)
  DF.dates<-jf.dates(DF)
  start_date<-DF.dates$max_start_date
  end_date<-DF.dates$min_end_date
  ifelse(common,DF<-subset(DF,date>=start_date & date<=end_date),DF)
  DF<-ddply(DF,.(variable),transform,
            cror=vami(value)-1,
            dd=dd(value),
            roll=roll.cror(value,n=12)
  )
  DF$value.sign<-NA
  DF$value.sign[which(DF$value>0)]<-"positive"
  DF$value.sign[which(DF$value<0)]<-"negative"
  DF$roll.sign<-NA
  DF$roll.sign[which(DF$roll>0)]<-"positive"
  DF$roll.sign[which(DF$roll<0)]<-"negative"
  # Cumulative Rate of Return Chart
  switch(type,
         cror = {
           p<-ggplot(DF,aes(x=as.Date(date),y=cror,group=variable))+geom_area(fill=col.brew[8],color="black")+
             theme(legend.position="none",
                   plot.title = element_text(size=16, face="bold", hjust=0))+
             labs(x=NULL,y="Total Return",title=paste0("Total Return: ",start_date," to ",end_date))+
             scale_y_continuous(labels=percent)+
             facet_wrap(~variable,ncol=1)
           #scale_x_date(expand=c(0,0))
           print(p)
         },
         dd = {
           p<-ggplot(DF,aes(x=as.Date(date),y=dd,group=variable))+geom_area(fill=col.brew[4],color="black")+
             theme(legend.position="none",
                   plot.title = element_text(size=16, face="bold", hjust=0))+
             labs(x=NULL,y="Drawdown",title=paste0("Drawdown: ",start_date," to ",end_date))+
             scale_y_continuous(labels=percent)+
             facet_wrap(~variable,ncol=1)
           #scale_x_date(expand=c(0,0))
           print(p)
         },
         roll = {
           p<-ggplot(DF,aes(x=as.Date(date),y=roll,group=variable,fill=roll.sign))+geom_bar(stat='identity',position='identity')+
             theme(legend.position="none",
                   plot.title = element_text(size=16, face="bold", hjust=0))+
             labs(x=NULL,y=paste0(12," Month Rolling Return (Total Return)"),title=paste0(12," Month Rolling Return (Total Return): ",start_date," to ",end_date))+
             scale_y_continuous(labels=percent)+ # make the y labels percentage
             scale_fill_manual(values=c("positive"=col.brew[8],"negative"=col.brew[4]))+ # positive values blue, negative values red
             facet_wrap(~variable,ncol=1)
           #scale_x_date(expand=c(0,0))
           print(p) 
         },
         return = {
           p<-ggplot(DF,aes(x=as.Date(date),y=value,group=variable,fill=value.sign))+geom_bar(stat='identity',position='identity')+
             theme(legend.position="none",
                   plot.title = element_text(size=16, face="bold", hjust=0))+
             #strip.text.y=element_text(angle=0,hjust=1))+ # rotate strip text horizontal
             labs(x=NULL,y=paste0("Monthly Returns"),title=paste0("Monthly Returns: ",start_date," to ",end_date))+
             scale_y_continuous(labels=percent)+ # make the y labels percentage
             scale_fill_manual(values=c("positive"=col.brew[8],"negative"=col.brew[4]))+ # positive values blue, negative values red
             facet_wrap(~variable,ncol=1)
           #scale_x_date(expand=c(0,0))
           print(p) 
         },
         correlation = {
           dat<-DF[c('date','variable','value')]
           dat<-jf.cor(dat)
           p<-ggplot(dat,aes(x=Var1,y=Var2,fill=value,label=round(value,2)))+geom_tile()+geom_text()+
             theme(legend.position="none",
                   plot.title = element_text(size=16, face="bold", hjust=0),
                   axis.text.x = element_text(angle=-90,hjust=0,vjust=0.5))+
             labs(x=NULL,y=NULL,title=paste0("Correlation Matrix: ",start_date," to ",end_date))+
             scale_fill_gradient2(low=col.brew[8],mid=col.brew[6],high=col.brew[4],midpoint=0.3)
           print(p)
         }
  )
}