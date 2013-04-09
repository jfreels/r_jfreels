# Drought function: longest flat period in a time series
# for use with plyr
drought<-function (ror) {
  dat<-data.frame(ror=ror,cror=cumprod(ror+1)-1)
  dat2<-transform(dat,index=ldply(apply(dat[,2,drop=FALSE],1,function (y) { which(y<dat[,2,drop=FALSE])}),function (x) { x[1] }))
  names(dat2)<-c("ror","cror","index")
  dat3<-transform(dat2,drought=as.integer(row.names(dat2))-index)
  dat3$drought[dat3$drought<0]<-NA
  dat3$index[is.na(dat3$drought)]<-NA
  dat3
}

droughtMax<-function (ror) { as.integer(max(drought(ror)$drought,na.rm=TRUE)) }
droughtMaxPct<-function (ror) { droughtMax(ror)/length(ror) }