# Calculate rate of return
ror<-function (price,period=1) { 
  head(c(rep(NA,period),price[-c(1:period)]/price-1),-period) 
}