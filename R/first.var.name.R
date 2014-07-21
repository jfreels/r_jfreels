# return the name of the first variable from a dataset
first.var.name <- function (df) {
	as.character(unique(df$variable)[1])
}