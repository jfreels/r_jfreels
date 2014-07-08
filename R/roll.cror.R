roll.cror<-function(ror,n) {
  vami<-c(1,vami(ror))
  head(c(rep(NA,n-1),vami[-c(1:n)]/vami-1),-n)
}