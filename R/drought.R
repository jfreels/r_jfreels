# Drought function: longest flat period in a time series
# for use with plyr

# All drought values
drought<-function (ror) {
  dat<-data.frame(ror=ror,cror=vami(ror)-1)
  dat2<-transform(dat,index=ldply(apply(dat[,2,drop=FALSE],1,function (y) { which(y<dat[,2,drop=FALSE])}),function (x) { x[1] }))
  names(dat2)<-c("ror","cror","index")
  dat3<-transform(dat2,value=as.integer(row.names(dat2))-index)
  dat3$value[dat3$value<0]<-NA
  dat3$index[is.na(dat3$value)]<-NA
  dat3[,2:4]
}

# Maximum Drought
drought.max<-function (ror,percent=FALSE) { 
	drought.max<-max(drought(x)$value,na.rm=TRUE)
	percent<-percent
	ifelse(percent==TRUE,
		drought.max/length(ror),
		drought.max)
}


# Current Drought
drought.current<-function (ror,percent=FALSE) {
	drought.current<-tail(drought(x)$value,1)
	percent<-percent
	ifelse(percent==TRUE,
		drought.current/length(ror),
		drought.current)
}