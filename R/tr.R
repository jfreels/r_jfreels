# Total (cumulative) return
tr<-function(ror) { cumprod(na.omit(ror)+1)-1 } # total return