jf.dates<-function(df) {
	df<-df %>%
		tbl_df %>%
		select(date,variable,value) %>%
		group_by(variable) %>%
		summarise(start_date=min(date),
							end_date=max(date))
	list(all=df,
			 max_start_date=max(df$start_date),
			 min_end_date=min(df$end_date))
}