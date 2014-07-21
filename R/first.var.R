# return the first variable from a dataset
first.var<-function(df) {
	df %>%
		filter(variable==first.var.name(df))
}