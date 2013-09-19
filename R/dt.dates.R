dt.dates<-function(DT) {
	dat<-DT[,list(data_start=min(date),data_end=max(date)),by=variable]
	setkey(dat,data_end)
	dat
}