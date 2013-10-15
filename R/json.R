json<-function(data.frame,sink=FALSE) {
	require(plyr)
	require(RJSONIO)
	# convert date columns to character
	d1<-as.data.frame(data.frame)
	d1_date_columns<-names(d1[,c(sapply(d1,class)=='Date'),drop=FALSE])
	for (i in d1_date_columns) {
	  d1[,i] <- as.character(d1[,i])
	}
	# convert to json
	json_data<-unname(alply(d1,1,identity))
	# write to file
  if(sink==TRUE) {
  	sink(sink)
  	cat(toJSON(json_data,pretty=TRUE))
  	sink()
  } else {
  	# print json to console
 	  cat(toJSON(json_data,pretty=TRUE))
  }
}