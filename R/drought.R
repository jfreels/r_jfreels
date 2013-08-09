# Drought function: longest flat period in a time series
# for use with plyr / data.table

# All drought values
drought <- function (ror) {
	dat<-data.frame(ror=ror,cror=vami(ror)-1)
	dat$drought_cror<-NA # create a column called drought_cror
	dat$drought_cror[which(dat$cror<0)]<-0 # set negative cror values to 0 for drought_cor column
	# create dat$index which gives the row number of the cror that is higher than the current cror
	dat<-transform(dat,index=ldply(apply(dat[,2,drop=FALSE],1,function (y) { which(y<dat[,2,drop=FALSE])}),function (x) { x[1] }))
	names(dat)<-c('ror','cror','drought_cror','index')
	dat$drought_index<-apply(dat[,c('drought_cror','index')],1,function (x) min(na.omit(x)))
	dat$drought_index[dat$drought_index>=as.integer(row.names(dat))]<-NA
	dat$drought_value<-as.integer(row.names(dat))-dat$drought_index
	dat$drought_pct<-dat$drought_value/length(dat$drought_value)
	dat<-dat[,c('ror','cror','drought_index','drought_value','drought_pct')]
	dat$drought_value
}

# Maximum Drought
drought.max<-function (ror,percent=FALSE) { 
	drought.max<-max(drought(ror)$drought,na.rm=TRUE)
	percent<-percent
	ifelse(percent==TRUE,
		drought.max/length(ror),
		drought.max)
}

# Current Drought
drought.current<-function (ror,percent=FALSE) {
	drought.current<-tail(drought(ror)$drought,1)
	percent<-percent
	ifelse(percent==TRUE,
		drought.current/length(ror),
		drought.current)
}