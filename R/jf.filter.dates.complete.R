jf.filter.dates.complete<-function(df) {
	y<-jf.dates(df)
	df %>%
		group_by(variable) %>%
		filter(date>=y$max_start_date,
					 date<=y$min_end_date)
}