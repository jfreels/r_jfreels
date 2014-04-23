# Current Drought
drought.current<-function (ror,percent=FALSE) {
	drought.current<-tail(drought(ror),1)
	percent<-percent
	ifelse(percent==TRUE,
		drought.current/length(ror),
		drought.current)
}