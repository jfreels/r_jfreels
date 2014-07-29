jf.pct <- function(numeric) { 
	if(!is.numeric(numeric)) stop('Function requires numeric value.')
	numeric %>%
	  multiply_by(100) %>%
	  round(2) %>%
	  format(nsmall=2) %>%
	  paste0('%')
}