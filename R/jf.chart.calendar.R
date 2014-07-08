jf.chart.calendar<-function(df) {
	# error catch
	if(length(unique(df$variable))>1) stop('More than 1 variable in data frame.')
	# data manipulation
	df_melt<-df %>%
		calendarTable_df() %>%
		melt(id.var='year') %>%
		mutate(name=unique(df$variable))
	df_start_date<-min(df$variable)
	df_end_date<-max(df$date)
	# plot
	p<-df_melt %>%
    filter(variable!='YTD') %>%
    ggplot(aes(x=as.factor(variable),
                        y=as.factor(year),
                        fill=value,
                        label=ifelse(is.na(value),NA,jf.pct(value))))+
      geom_tile(color="black")+
      geom_text()+
      theme_bw()+
      theme(legend.position="none",
        plot.title = element_text(size=16, face="bold", hjust=0))+
      labs(x=NULL,y=NULL,title=paste0("Track Record: ",df_start_date," to ",df_end_date))+
      facet_wrap(~name,ncol=1)+
      scale_fill_gradient2(low=col.brew[3],mid=col.brew[6],high=col.brew[9])
  print(p) 
}