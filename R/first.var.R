# return the first variable from a dataset
first.var<-function(df) {
	df_name<-first.var.name(df)
	df %>%
		filter(variable==df_name)
}