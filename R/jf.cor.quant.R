jf.cor.quant<-function(DF,primary) {
  dat<-data.frame(date=DF$date,variable=DF$variable,value=DF$value)
  dat<-na.omit(dcast(dat,date~variable,value.var="value"))
  dat<-melt(dat,id.vars="date")
  cor.table<-jf.cor(dat)
  cor.table<-subset(cor.table,Var2==primary)
  cor.table<-arrange(cor.table[-2],-value)
  names(cor.table)<-c("variable","cor")
  quant.table<-jf.stats(dat)
  cor.quant.table<-merge(cor.table,quant.table,by="variable")
  arrange(cor.quant.table,-cor)
}