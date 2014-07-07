# convert data.table to xts object
dt.xts <- function (dt) {
  require(xts)
  dt_dcast<-dt.dcast(dt)
  dt_xts <- xts(dt_dcast[,-1],dt_dcast[,1])
  names(dt_xts)<-names(dt_dcast)[-1]
  dt_xts
}