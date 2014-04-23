jf.cor<-function(DF) {
  dat<-dcast(DF,date~variable,value.var="value")
  dat<-na.omit(dat)
  dat<-melt(cor(dat[-1]))
  dat
}