dt.dates.filter<-function(dt) {
	start_date<-jf.dates(dt)$max_start_date
	end_date<-jf.dates(dt)$min_end_date
  dt %>%
  	tbl_df() %>%
  	group_by(variable) %>%
  	filter(date>=start_date,date<=end_date)
}
