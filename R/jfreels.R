# Financial Time Series Functions
cror<-function (ror) { tail(vami(ror), 1) - 1 }
asd<-function (ror,periods=12) { sd(ror) * sqrt(periods) }
sharpe<-function (ror,periods=12) { aror(ror,periods)/asd(ror,periods) }
maxdd<-function (ror) { min(dd(ror)) }
currentdd<-function (ror) { tail(dd(ror),1) }
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

