# Plot a rolling returns bar chart
jf.chart.rolling<-function(df,n_months=12) {
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
    mutate(roll=roll.cror(value,n=n_months),
           type='positive')
  df$type[which(df$roll<0)]<-'negative'
  # plot
  p<-ggplot(data=df)+
    geom_bar(aes(x=as.Date(date),
                 y=roll,
                 group=variable,
                 fill=type),
             stat='identity')+
    geom_hline(yintercept=0,linetype='dotted')+
    scale_fill_manual(values=c('positive'=color_positive,'negative'=color_negative))+
    scale_x_date(expand=c(0.1,0.1))+
    scale_y_continuous(labels=percent)+
    theme_bw()+
    theme(legend.position='none',
          plot.title=element_text(size=16, face='bold', hjust=0))+
    labs(x=NULL,
         y=paste0(n_months,' Months Rolling Returns'),
         title=paste0('Rolling Returns (', n_months, '): ', df_start_date,' to ', df_end_date))+
    facet_wrap(~variable, ncol=1)
  print(p)
}