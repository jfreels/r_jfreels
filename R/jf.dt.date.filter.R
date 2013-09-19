dt.date.filter<-function(DT) {
  DT[date>=max(jf.table.dates(DT)$data_start)][date<=max(jf.table.dates(DT)$data_end)]
}