# convert data.table to xts object
dt.xts <- function (dt) {
  require(xts)
  dt_names<-unique(dt$variable)
  dt_dcast <- dcast(dt,date~variable,value.var='value')
  dt_xts <- xts(dt_dcast[,-1],dt_dcast[,1])
  names(dt_xts)<-dt_names
  dt_xts
}
