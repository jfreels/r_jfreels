# Maximum Drought
drought.max<-function (ror,percent=FALSE) { 
	drought.max<-max(drought(ror),na.rm=TRUE)
	percent<-percent
	ifelse(percent==TRUE,
		drought.max/length(ror),
		drought.max)
}
