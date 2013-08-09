require(RColorBrewer)
col.brew = brewer.pal(name="RdBu",n=11)

jf.z.rolling <- function (name, DT=z) {
  dat <- DT[name]
  dat[,roll.ror:=rollapply(value,width=12,fill=NA,align='right',FUN=aror),by=variable]
  dat[,roll.asd:=rollapply(value,width=12,fill=NA,align='right',FUN=asd),by=variable]
  dat_title <- paste0(unique(dat$variable),"\nRolling 12 Month Return and Standard Deviation (Annualized)\nTrailing 60 Months\n",head(dat$date,1)," to ",tail(dat$date,1))
  p<-ggplot(dat,aes(x=date))+
    geom_line(aes(y=roll.ror),color=col.brew[8])+
    geom_line(aes(y=roll.asd),color=col.brew[4])+
    geom_hline(yintercept=0,color='black',linetype=3)+
    labs(title=dat_title,x=NULL,y='12 Month Rolling Return (blue) and Standard Deviation (orange)')+
    scale_y_continuous(labels=percent)+
    theme_bw()
  print(p)
}