require(RJSONIO)
require(plyr)
json<-function(data.frame,sink=FALSE) {
	modified<-list(
  	keys = colnames(data.frame),
  	values = unname(alply(data.frame,1,identity))
  )
  if(sink!=FALSE) {
  	sink(sink)
  	cat(toJSON(modified,pretty=TRUE))
  	sink()
  }
  if(sink==FALSE) {
  	cat(toJSON(modified,pretty=TRUE))
  }
}