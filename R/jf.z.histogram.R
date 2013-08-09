require(RColorBrewer)
col.brew = brewer.pal(name="RdBu",n=11)

jf.z.histogram <- function (name, DT=z) {
  dat <- DT[name]
  dat_title <- paste0(unique(dat$variable),"\nHistogram of monthly returns\nTrailing 60 Months\n",head(dat$date,1)," to ",tail(dat$date,1))
  dat_last <- tail(dat,1)
  p<-ggplot(dat,aes(x=value))+
    geom_histogram(fill='gray',color='white',binwidth=0.01)+
    geom_histogram(data=dat_last,aes(x=value),fill=col.brew[4],color='white',binwidth=0.01)+
    geom_vline(xintercept=0,color='black',linetype=3)+
    scale_x_continuous(labels=percent)+
    labs(title=dat_title,x="Monthly Returns")+
    theme_bw()
  print(p)
}