require(RColorBrewer)
col.brew = brewer.pal(name="RdBu",n=11)

jf.z.histogram <- function (name, DT=z) {
  dat <- DT[name]
  # assign values to the last 3 dates to color them later
  dat_n <- dat[,.N]
  dat_n_sequence<-c(rep('tx',dat_n-3),'t-2','t-1','t')
  dat[,time:=dat_n_sequence]
  # expected shortfall
  require(PerformanceAnalytics)
  VaR_method <- 'gaussian' # options are: 'historical','gaussian','modified'
  VaR_p <- .01 # confidence level
  dat_VaR_t<-VaR(dt.xts(dat[,.SD[1:(.N)],by=variable]),p=VaR_p,method=VaR_method)[1,1]
  dat_VaR_t1<-VaR(dt.xts(dat[,.SD[1:(.N-1)],by=variable]),p=VaR_p,method=VaR_method)[1,1]
  dat_VaR_t2<-VaR(dt.xts(dat[,.SD[1:(.N-2)],by=variable]),p=VaR_p,method=VaR_method)[1,1]
  # center the histogram by keeping the min and max values consistent
  dat_limits<-max(abs(c(min(dat$value),max(dat$value),dat_VaR_t,dat_VaR_t1,dat_VaR_t2)))
  # title of the chart
  dat_title <- paste0(unique(dat$variable),"\nHistogram of Monthly Returns\n",head(dat$date,1)," to ",tail(dat$date,1))
  # ggplot
  p<-ggplot(dat,aes(x=value,fill=factor(time)))+
    geom_bar(color='white',binwidth=0.01)+
    scale_fill_manual(values=c(col.brew[2],col.brew[3],col.brew[4],'gray'),name='Time Period:',breaks=c('t','t-1','t-2',NULL),labels=c('t','t-1','t-2',NULL))+
    geom_vline(xintercept=dat_VaR_t,color=col.brew[2])+ # add 99% expected shortfall line using data available at time t
    geom_vline(xintercept=dat_VaR_t1,color=col.brew[3])+ # add 99% expected shortfall line using data available at time t-1
    geom_vline(xintercept=dat_VaR_t2,color=col.brew[4])+ # add 99% expected shortfall line using data available at time t-2
    geom_vline(xintercept=0,color='black',linetype=3)+ # vertical line at 0
    geom_hline(yintercept=0,color='black')+ # horizontal line at 0
    scale_x_continuous(labels=percent,limits=c(-dat_limits-.01,dat_limits+.01))+ # center the histogram with 'limits'
    labs(title=dat_title,x='Monthly Returns',y='Number of Observations')+
    theme_bw()
  print(p)
}