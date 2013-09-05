fv<-function(pv,r,t) {
  pv*(1+r)^t
}

pv<-function(fv,r,t) {
  fv/(1+r)^t
}

ldply(1:30, function (x) fv(1,.05,t=x))

fees<-function(pv,r,mf,pf) {
  pv*(1+r)*(1-mf)*(1-pf)
}


