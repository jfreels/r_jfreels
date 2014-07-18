jf.ar.table<-function(DT,asof,allocation=FALSE) {
  DT<-data.table(DT)
  setkey(DT,variable,date)
	DT.variables<-data.table(variable=unique(DT$variable),key='variable')
	asof<-as.Date(asof)
	ifelse(allocation==TRUE,
	       MTD<-DT[date==asof,list(date,variable,Allocation=allocation,MTD=value)],
	       MTD<-DT[date==asof,list(date,variable,MTD=value)])
	QTD<-DT[quarter(date)==quarter(asof)][year(date)==year(asof)][,list(QTD=cror(value)),by=variable]
	dat<-join(MTD,QTD,by='variable')
	YTD<-DT[year(date)==year(asof),list(YTD=cror(value)),by=variable]
	dat<-join(dat,YTD,by='variable')
	# managers with track records > 12 months
	names12<-as.character(DT[,.N-11,by=variable][V1>0]$variable)
	last12<-DT[names12,.SD[(.N-11):(.N)]][,list(Last12=aror(value)),by=variable]
	dat<-join(dat,last12,by='variable')
	names24<-as.character(DT[,.N-23,by=variable][V1>0]$variable)
	#last24<-DT[names24,.SD[(.N-23):(.N)]][,list(Last24=aror(value)),by=variable]
	#dat<-join(dat,last24,by='variable')
	names36<-as.character(DT[,.N-35,by=variable][V1>0]$variable)
	last36<-DT[names36,.SD[(.N-35):(.N)]][,list(Last36=aror(value)),by=variable]
	dat<-join(dat,last36,by='variable')
	names60<-as.character(DT[,.N-59,by=variable][V1>0]$variable)
	last60<-DT[names60,.SD[(.N-59):(.N)]][,list(Last60=aror(value)),by=variable]
	dat<-join(dat,last60,by='variable')
	setkey(dat,variable)
	dat<-dat[DT.variables] # merge dat and DT.variables in order to keep all the names in the original dataset
	ifelse(allocation==TRUE,
	       setcolorder(dat,neworder=c('date','variable','Allocation','MTD','QTD','YTD','Last12','Last36','Last60')),
	       setcolorder(dat,neworder=c('date','variable','MTD','QTD','YTD','Last12','Last36','Last60')))
	dat
}