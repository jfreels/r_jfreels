# Financial Time Series Functions
ror<-function (price,period=1) { 
  head(c(rep(NA,period),price[-c(1:period)]/price-1),-period) 
}
vami<-function (ror) { cumprod(na.omit(ror) + 1) }
aror<-function (ror) { (1 + cror(ror))^(12/length(ror)) - 1 }
cror<-function (ror) { tail(vami(ror), 1) - 1 }
asd<-function (ror) { sd(ror) * sqrt(12) }
sharpe<-function (ror) { aror(ror)/asd(ror) }
maxdd<-function (ror) { min(dd(ror)) }
dd<-function (ror) { -(1 - vami(ror)/cummax(c(1, cummax(vami(ror))))[-1]) }
omega<-function (ror) { sum(ror[ror>0])/sum(abs(ror[ror<0])) }
percentUp<-function (ror) { values<-na.omit(ror); length(values[values>0])/length(values) }
cror.roll<-function(ror,...) { rollapplyr(ror,...,FUN=cror,fill=NA) }
aror.roll<-function(ror,...) { rollapplyr(ror,...,FUN=aror,fill=NA) }

jf.cor<-function(DF) {
  dat<-dcast(DF,date~variable,value.var="value")
  dat<-na.omit(dat)
  dat<-melt(cor(dat[-1]))
  dat
}


jf.stats<-function (longDataFrame) {
  ddply(longDataFrame[,1:3],.(variable),summarise,
        # data should have columns "date","variable","value"
        cror=cror(value),
        aror=aror(value),
        asd=asd(value),
        sharpe=sharpe(value),
        maxdd=maxdd(value),
        #droughtMaxPct=drought.max(value,percent=true),
        omega=omega(value),
        start=min(date),
        end=max(date))
}

jf.cor.quant<-function(DF,primary) {
  dat<-data.frame(date=DF$date,variable=DF$variable,value=DF$value)
  dat<-na.omit(dcast(dat,date~variable,value.var="value"))
  dat<-melt(dat,id.vars="date")
  cor.table<-jf.cor(dat)
  cor.table<-subset(cor.table,Var2==primary)
  cor.table<-arrange(cor.table[-2],-value)
  names(cor.table)<-c("variable","cor")
  quant.table<-jf.stats(dat)
  cor.quant.table<-merge(cor.table,quant.table,by="variable")
  arrange(cor.quant.table,-cor)
}

dt.stats<-function (dataTable) {
  # data should have columns "date","variable","value"
  dataTable[,list(cror=cror(value),
            aror=aror(value),
            asd=asd(value),
            sharpe=sharpe(value),
            maxdd=maxdd(value),
            omega=omega(value),
            start=min(date),
            end=max(date)),
      by=variable]
}

calendarTable<-function (oneFundLongDataFrame) {
  calendarTable<-dcast(oneFundLongDataFrame,year(date)~month(date),value.var="value")
  names(calendarTable)<-c("Year",month.abb)
  calendarTable<-arrange(calendarTable,-Year)
  row.names(calendarTable)<-calendarTable[,1]
  calendarTable<-calendarTable[,-1]
  calendarTable$Year<-apply(calendarTable,1,aror)
  calendarTable<-round(calendarTable,2)
  calendarTable
}

# Data formatting functions
longToXts<-function (longDataFrame) { xts(longDataFrame[,-1],longDataFrame[,1]) }
xtsToLong<-function (Xts) { 
  df<-data.frame(date=index(Xts),coredata(Xts)) 
  names(df)<-c("date",names(Xts))
  df<-melt(df,id.vars="date")
  names(df)<-c("date","variable","value")
  df
}