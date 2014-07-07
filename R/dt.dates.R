dt.dates<-function(dt) {
	dt %>%
		tbl_df() %>%
		group_by(variable) %>%
		summarise(data_start=min(date),data_end=max(date)) %>%
		arrange(data_end)
}