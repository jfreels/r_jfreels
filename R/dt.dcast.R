# dcast a data.table
dt.dcast <- function (dt) {
	dcast(dt,date~variable,value.var='value')
}