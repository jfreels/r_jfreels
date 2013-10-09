# Financial Time Series Functions
ror<-function (price,period=1) { 
  head(c(rep(NA,period),price[-c(1:period)]/price-1),-period) 
}

vami<-function (ror) { cumprod(na.omit(ror) + 1) }

aror<-function (ror,periods=12) {
  (1+cror(ror))^(periods/length(ror))-1
}
  
tr<-function(ror) { cumprod(na.omit(ror)+1)-1 } # total return
cror<-function (ror) { tail(vami(ror), 1) - 1 }
asd<-function (ror,periods=12) { sd(ror) * sqrt(periods) }
sharpe<-function (ror,periods=12) { aror(ror,periods)/asd(ror,periods) }
maxdd<-function (ror) { min(dd(ror)) }
dd<-function (ror) { -(1 - vami(ror)/cummax(c(1, cummax(vami(ror))))[-1]) }
calmar<-function (ror,...) { aror(ror,...)/abs(maxdd(ror)) }
pain<-function (ror,...) { aror(ror,...)/abs(mean(dd(ror))) }
hwm<-function (ror) { 1/(1+tail(dd(ror),1))-1 }
omega<-function (ror) { sum(ror[ror>0])/sum(abs(ror[ror<0])) }
percentUp<-function (ror) { values<-na.omit(ror); length(values[values>0])/length(values) }
percentDown<-function (ror) { values<-na.omit(ror); length(values[values<0])/length(values)}
roll.cror<-function(ror,n) {
  vami<-c(1,vami(ror))
  head(c(rep(NA,n-1),vami[-c(1:n)]/vami-1),-n)
}

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

# Data formatting functions
longToXts<-function (longDataFrame) { xts(longDataFrame[,-1],longDataFrame[,1]) }
xtsToLong<-function (Xts) { 
  df<-data.frame(date=index(Xts),coredata(Xts)) 
  names(df)<-c("date",names(Xts))
  df<-melt(df,id.vars="date")
  names(df)<-c("date","variable","value")
  df
}