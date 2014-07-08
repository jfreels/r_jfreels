# Plot a cumulative return chart
jf.chart.cror<-function(df) {
  # seteup
  require(scales)
  # error catch
  if(length(unique(df$variable))>1) stop('More than 1 variable in data frame.')
  # data manipulation
  df<-select(df,date,variable,value)
  df_dates<-jf.dates(df)
  df_start_date<-as.Date(df_dates$max_start_date)
  df_end_date<-as.Date(df_dates$min_end_date)
  df<-df %>%
    group_by(variable) %>%
    mutate(cror=vami(value)-1)
  df_cror_max<-filter(df,cror==max(df$cror))
  df_cror_end<-filter(df,date==df_end_date)
  if(identical(df_cror_max,df_cror_end)) rm(df_cror_max)
  # plot
  p<-ggplot(data=df)+
    geom_line(aes(x=as.Date(date),
                  y=cror,
                  group=variable))+
    geom_hline(yintercept=0,linetype='dotted')+
    geom_text(data=df_cror_end,
              aes(x=as.Date(date),
                  y=cror,
                  label=jf.pct(cror)),
              hjust = 0,
              vjust = 0)+
    scale_x_date(expand=c(0.1,0.1))+
    scale_y_continuous(labels=percent)+
    theme_bw()+
    theme(legend.position='none',
          plot.title=element_text(size=16, face='bold', hjust=0))+
    labs(x=NULL,
         y='Total Return',
         title=paste0('Total Return: ', df_start_date,' to ', df_end_date))+
    facet_wrap(~variable, ncol=1)
  if(exists('df_cror_max')) p<-p+geom_text(data=df_cror_max,
                                           aes(x=as.Date(date),
                                               y=cror,
                                               label=jf.pct(cror)),
                                           hjust = 1,
                                           vjust = 0)
  print(p)
}
