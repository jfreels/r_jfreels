require(RColorBrewer)
col.brew = brewer.pal(name="RdBu",n=11)

jf.z.histogram <- function (name, DT=z) {
  dat <- DT[name]
  dat_n <- dat[,.N]
  dat_n_sequence<-c(rep('tx',dat_n-3),'t-2','t-1','t')
  dat[,time:=dat_n_sequence]
  dat_limits<-max(abs(min(dat$value)),abs(max(dat$value)))
  dat_title <- paste0(unique(dat$variable),"\nHistogram of Monthly Returns\n",head(dat$date,1)," to ",tail(dat$date,1))
  p<-ggplot(dat,aes(x=value,fill=factor(time)))+
    geom_bar(color='white',binwidth=0.01)+
    scale_fill_manual(values=c(col.brew[2],col.brew[3],col.brew[4],'gray'),name='Time Period:',breaks=c('t','t-1','t-2',NULL),labels=c('t','t-1','t-2',NULL))+
    geom_vline(xintercept=0,color='black',linetype=3)+
    geom_hline(yintercept=0,color='black')+
    scale_x_continuous(labels=percent,limits=c(-dat_limits-.01,dat_limits+.01))+
    labs(title=dat_title,x='Monthly Returns',y='Number of Observations')+
    theme_bw()
  print(p)
}