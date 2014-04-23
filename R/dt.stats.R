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