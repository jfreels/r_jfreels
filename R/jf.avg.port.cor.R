jf.avg.port.cor<-function(DT) {
  dat<-dt.dates.filter(DT)
  dat.cor<-dt.cor(dat)
  dat.melt<-melt(dat.cor)
  dat.dlply<-dlply(dat.melt,.variables='Var2')
  dat.ldply<-ldply(dat.dlply,function (x) (sum(x$value)-1)/length(x$value-1))
  dat.ldply$start=start(dt.xts(dat))
  dat.ldply$end=end(dt.xts(dat))
  names(dat.ldply)<-c('variable','avg.port.cor','start','end')
  data.table(dat.ldply)
}

# weighted average correlation
#a<-jf.tr.table(cgm,asof='2013-05-31',allocation=TRUE)[,list(variable,Allocation)]
#x<-jf.avg.port.cor(cgm)
#setkey(a,variable);setkey(x,variable)
#sum(a[x][,Allocation*avg.port.cor])