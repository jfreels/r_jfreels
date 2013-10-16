xts.dt <- function (xts) {
	dat<-data.frame(date=index(xts),coredata(xts))
	names(dat)<-c('date',names(xts))
	dat<-data.table(melt(dat,id.vars='date'))
	setkey(dat,variable,date)
	dat
}