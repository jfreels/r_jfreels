require(RColorBrewer)
col.brew = brewer.pal(name="RdBu",n=11)

jf.z.dd <- function (name, DT=z) {
  setkey(DT,variable,date)
  dat <- DT[name]
  dat[,dd:=dd(value),by=variable]
  dat_title <- paste0(unique(dat$variable),"\nDrawdown\n",head(dat$date,1)," to ",tail(dat$date,1))
  p<-ggplot(dat,aes(x=date,y=dd))+
    geom_area(fill=col.brew[4],color='black')+
    geom_hline(yintercept=0,color='black',linetype=3)+
    #annotate('text',label=round(min(dat$dd)*100,2),x=as.Date(head(dat$date,1)),y=min(dat$dd),color='red',size=4)+
    labs(title=dat_title,x=NULL,y=NULL)+
    scale_y_continuous(labels=percent)+
    scale_x_date(expand=c(0,0))+
    theme_bw()
  print(p)
}