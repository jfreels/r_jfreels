dt.cor <- function (dt) {
	cor(na.omit(dt.xts(dt)))
}