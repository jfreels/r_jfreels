# convert a data.table to a correlation matrix
dt.cor <- function (dt) {
	cor(na.omit(dt.xts(dt)))
}