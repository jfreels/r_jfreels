jf.portfolio.equal <- function(df,portfolio_name) {
	df %>%
		select(date,variable,value) %>%
		mutate(allocation=1/length(unique(df$variable)),
			     attribution=allocation*value) %>%
		group_by(date) %>%
		summarise(variable=portfolio_name,
							value=sum(attribution))
}