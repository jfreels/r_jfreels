dt.stats<-function (df) {
  # data should have columns "date","variable","value"
  df %>%
  	tbl_df() %>%
  	group_by(variable) %>%
  	summarise(
  		cror=cror(value),
      aror=aror(value),
      asd=asd(value),
      sharpe=sharpe(value),
      maxdd=maxdd(value),
      omega=omega(value),
      start=min(date),
      end=max(date))
}