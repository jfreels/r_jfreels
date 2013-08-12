require(RColorBrewer)
col.brew = brewer.pal(name="RdBu",n=11)

jf.z.cror <- function (name, DT=z) {
  dat <- DT[name]
  dat[,cror:=vami(value)-1,by=variable]
  dat_title <- paste0(unique(dat$variable),"\nCumulative Return & Drought\n",head(dat$date,1)," to ",tail(dat$date,1))
  drought_current<-jf.table.drought(dat,type='current')
  drought_current_start<-drought_current$drought.start
  drought_current_end<-drought_current$drought.end 
  drought_max<-jf.table.drought(dat,type='max')
  drought_max_start<-drought_max$drought.start
  drought_max_end<-drought_max$drought.end
  dat[,drought_max_start:=drought_max_start]
  dat[,drought_max_end:=drought_max_end]
  dat[,drought_max_value:=max(dat[date==drought_max_end,cror],0)]
  dat[,drought_current_start:=drought_current_start]
  dat[,drought_current_end:=drought_current_end]
  p<-ggplot(dat,aes(x=date,y=cror))+
    geom_line()+
    geom_hline(yintercept=0,color='black',linetype=3)+
    geom_segment(aes(x=drought_current_start,xend=drought_current_end,y=tail(cror,1),yend=tail(cror,1)),linetype=2,size=1,color=col.brew[4])+
    geom_segment(aes(x=drought_max_start,xend=drought_max_end,y=drought_max_value,yend=drought_max_value),linetype=2,size=1,color=col.brew[2])+
    labs(title=dat_title,x=NULL,y=NULL)+
    scale_y_continuous(labels=percent)+
    scale_x_date(expand=c(0,0))+
    theme_bw()
  print(p)
}