jf.stats<-function (longDataFrame) {
  longDataFrame %>%
  	group_by(variable) %>%
  	summarise(
        cror=cror(value),
        aror=aror(value),
        asd=asd(value),
        sharpe=sharpe(value),
        maxdd=maxdd(value),
        #droughtMaxPct=drought.max(value,percent=true),
        omega=omega(value),
        start=min(date),
        end=max(date)
    )
}
