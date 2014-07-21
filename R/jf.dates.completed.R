# create complete set of "Y-m-d" dates from an incomplete set of "Y-m-d" dates (all months of each potentially shortened year)
jf.dates.completed<-function (df) {
	df %>%
		use_series(date) %>%
		year %>%
		unique %>%
		lapply(function(x) {
			as.Date(paste(x,seq(1,12),'01',sep='-'),format='%Y-%m-%d')+months(1)-days(1)
		}) %>%
		unlist %>%
		as.Date
}