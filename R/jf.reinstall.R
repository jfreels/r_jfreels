# reinstall jfreels packages
jf.reinstall<-function () {
	library(devtools)
	install_github('jfreels/r_jfreels')
	library(jfreels)
}