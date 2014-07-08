# Plot a drawdown chart
jf.chart.drawdown<-function(df) {
  # seteup
  require(RColorBrewer)
  col.brew = brewer.pal(name="RdBu",n=11)
  color_positive = col.brew[8]
  color_negative = col.brew[4]
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
    mutate(dd=dd(value))
  df_dd_max<-filter(df,dd==min(df$dd))
  df_dd_end<-filter(df,date==df_end_date)
  if(identical(df_dd_max,df_dd_end)) rm(df_dd_max)
  # plot
  p<-ggplot(data=df)+
    geom_area(aes(x=as.Date(date),
                  y=dd,
                  group=variable),
              color='black',
              fill=color_negative)+
    geom_hline(yintercept=0,linetype='dotted')+
    geom_text(data=df_dd_end,
              aes(x=as.Date(date),
                  y=dd,
                  label=jf.pct(dd)),
              hjust = 0,
              vjust = 1)+
    scale_x_date(expand=c(0.1,0.1))+
    scale_y_continuous(labels=percent)+
    theme_bw()+
    theme(legend.position='none',
          plot.title=element_text(size=16, face='bold', hjust=0))+
    labs(x=NULL,
         y='Drawdown',
         title=paste0('Drawdown: ', df_start_date,' to ', df_end_date))+
    facet_wrap(~variable, ncol=1)
  if(exists('df_dd_max')) p<-p+geom_text(data=df_dd_max,
                                           aes(x=as.Date(date),
                                               y=dd,
                                               label=jf.pct(dd)),
                                           hjust = 0.5,
                                           vjust = 1)
  print(p)
}
