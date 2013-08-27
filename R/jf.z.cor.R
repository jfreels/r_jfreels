jf.z.cor <- function (name,DT=z,returns=FALSE) {
	dat_start<-min(DT[name]$date)
	dat_end<-max(DT[name]$date)
	dat_orig<-DT[date>=dat_start][date<=dat_end][,list(date,variable,value)]
	dat_dcast<-dcast(dat_orig,date~variable,value.var='value')
	dat_xts<-xts(dat_dcast[,-1],dat_dcast[,1])
	dat_melt<-melt(cor(dat_xts,method='spearman'))
	dat<-data.table(subset(dat_melt,Var1==name))
	dat$Var1<-NULL
	setnames(dat,old=c('Var2','value'),new=c('variable','correlation'))
	setkey(dat,variable)
	dat$start_date<-dat_start
	dat$end_date<-dat_end
	if(returns) {
		dat_ar<-jf.ar.table(dat_orig,asof=dat_end)
		setkey(dat_ar,variable)
		arrange(dat[dat_ar],-correlation)
	}
	else arrange(dat,-correlation)	
}