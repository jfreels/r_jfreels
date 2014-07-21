# reinstall jfreels packages
jf.reinstall<-function () {
	require(devtools)
	install_github('jfreels/r_jfreels')
	require(jfreels)
}