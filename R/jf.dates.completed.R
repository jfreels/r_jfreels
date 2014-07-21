# create complete set of "Y-m-d" dates from an incomplete set of "Y-m-d" dates (all months of each potentially shortened year)
jf.dates.completed<-function (df) {
	df %>%
		use_series(date) %>%
		year %>%
		unique %>%
		lapply(function(x) { paste(x,seq(1,12),'01',sep='-') }) %>%
		unlist %>%
		as.Date %>%
		add(months(1)) %>%
		subtract(days(1))
}