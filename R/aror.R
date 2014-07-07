# Annualized rate of return
aror<-function (ror,periods=12) {
  (1+cror(ror))^(periods/length(ror))-1
}