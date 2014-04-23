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
