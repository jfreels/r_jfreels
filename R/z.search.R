z.search<-function(df,name) {
  x<-unique(df$variable)
  x[grep(name,x)]
}