jf.table.drought<-function(DF,type='max') {
  require(mondate)
  DT<-data.table(date=DF$date,variable=DF$variable,value=DF$value) # create a data.table using only columns 'date','variable','value' from the original data
  setkey(DT,variable,date) # set the keys for the data table so it is sorted first by variable, then by date
  DT.dates<-DT[,list(data_start=min(date),data_end=max(date),n_months=length(date)),by=variable] # start and end dates of the data to later be merged
  DT[,drought.value:=drought(value),by=variable] # add drought value and index columns
  DT[,value:=NULL] # drop the value column
  #DT[drought.start==0,drought.start2:=head(date,1)-months(1),by=variable] # set droght.start 0 months to equal the month prior to the track record inception
  DT$drought.start<-as.Date(mondate(DT$date)-DT$drought) # add drought.start column by subtracting the drought value from the current date
  if(type=='max') { DT<-ddply(DT,.(variable),function(x) { tail(subset(x,drought.value==max(drought.value,na.rm=TRUE)),1) }) } # keep only the max drought rows for each variable
  if(type=='current') { DT<-ddply(DT,.(variable),function (x) { tail(x,1) }) } # keep only the current drought rows for each variable
  DT<-data.table(DT,key='variable') # create a new data.table and key it
  DT<-DT[DT.dates] # merge DT.dates into DT in order to add the start and end dates of the dataset
  DT[,drought.pct:=drought.value/n_months,by=variable]
  setnames(DT,old='date',new='drought.end')
  setcolorder(DT,c('variable','drought.pct','drought.start','drought.value','drought.end','data_start','data_end','n_months')) # rearrange columns
  arrange(DT,drought.pct) # arrange the table by drought.value
}