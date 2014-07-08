require(RColorBrewer)
col.brew = brewer.pal(name="RdBu",n=11)
jf.z.histogram <- function (df) {
  # assign values to the last 3 dfes to color them later
  df_n<-nrow(df)
  df_n_sequence<-c(rep('tx',df_n-3),'t-2','t-1','t')
  df<-mutate(df,time=df_n_sequence)
  # expected shortfall
  require(PerformanceAnalytics)
  VaR_method <- 'gaussian' # options are: 'historical','gaussian','modified'
  VaR_p <- .01 # confidence level
  df_VaR_t<- VaR(dt.xts(df[1:(df_n),]),p=VaR_p,method=VaR_method)[1,1]
  df_VaR_t1<- VaR(dt.xts(df[1:(df_n-1),]),p=VaR_p,method=VaR_method)[1,1]
  df_VaR_t2<- VaR(dt.xts(df[1:(df_n-2),]),p=VaR_p,method=VaR_method)[1,1]
    # center the histogram by keeping the min and max values consistent
  df_limits<-max(abs(c(min(df$value),max(df$value),df_VaR_t,df_VaR_t1,df_VaR_t2)))
  # title of the chart
  df_title <- paste0(unique(df$variable),"\nHistogram of Monthly Returns\n",head(df$date,1)," to ",tail(df$date,1))
  # ggplot
  p<-ggplot(df,aes(x=value,fill=factor(time)))+
    geom_bar(color='white',binwidth=0.01)+
    scale_fill_manual(values=c(col.brew[2],col.brew[3],col.brew[4],'gray'),name='Time Period:',breaks=c('t','t-1','t-2',NULL),labels=c('t','t-1','t-2',NULL))+
    geom_vline(xintercept=df_VaR_t,color=col.brew[2])+ # add 99% expected shortfall line using dfa available at time t
    geom_vline(xintercept=df_VaR_t1,color=col.brew[3])+ # add 99% expected shortfall line using dfa available at time t-1
    geom_vline(xintercept=df_VaR_t2,color=col.brew[4])+ # add 99% expected shortfall line using dfa available at time t-2
    geom_vline(xintercept=0,color='black',linetype=3)+ # vertical line at 0
    geom_hline(yintercept=0,color='black')+ # horizontal line at 0
    scale_x_continuous(labels=percent,limits=c(-df_limits-.01,df_limits+.01))+ # center the histogram with 'limits'
    labs(title=df_title,x='Monthly Returns',y='Number of Observations')+
    theme_bw()
  print(p)
}