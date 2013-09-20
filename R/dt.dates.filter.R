dt.dates.filter<-function(DT) {
  DT[date>=max(dt.dates(DT)$data_start)][date<=min(dt.dates(DT)$data_end)]
}
