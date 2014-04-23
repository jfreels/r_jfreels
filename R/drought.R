# Drought function: longest flat period in a time series
# for use with plyr / data.table

# All drought values
drought <- function (ror) {
	ror<-as.vector(ror)
	dat<-data.frame(ror=ror,cror=vami(ror)-1)
	dat$drought_cror<-NA # create a column called drought_cror
	dat$drought_cror[which(dat$cror<0)]<-0 # set negative cror values to 0 for drought_cor column
	# create dat$index which gives the row number of the cror that is higher than the current cror
	dat<-transform(dat,index=ldply(apply(dat[,2,drop=FALSE],1,function (y) { which(y<dat[,2,drop=FALSE])}),function (x) { x[1] }))
	names(dat)<-c('ror','cror','drought_cror','index')
	dat$drought_index<-apply(dat[,c('drought_cror','index')],1,function (x) min(na.omit(x)))
	dat$drought_index[dat$drought_index>=as.integer(row.names(dat))]<-NA
	dat$drought_value<-as.integer(row.names(dat))-dat$drought_index
	dat$drought_value[is.na(dat$drought_value)]<-0
	dat$drought_value
}


