require(Hmisc)

trailing.return.allocation<-function(name,...) {
  name<-get(name)
  max.date<-asof
  dat<-jf.ar.table(name,...)
  dat$Allocation<-dat$Allocation*100
  dat$MTD<-dat$MTD*100
  dat$YTD<-dat$YTD*100
  dat$Last12<-dat$Last12*100
  dat$Last24<-dat$Last24*100
  dat$Last36<-dat$Last36*100
  dat$Last60<-dat$Last60*100
  dat<-arrange(dat,-Allocation)
  dat$date<-NULL
  setnames(dat,old='Allocation',new='%**')
  setnames(dat,old="variable",new="Underlying Manager*")
  setnames(dat,old=c('Last12','Last24','Last36','Last60'),new=c('1 Year','2 Year','3 Year','5 Year'))
  allocation.note<-paste0("Return values are total return for the period ending ",asof,". Allocations are as of ",asof+days(1),".")
  print(xtable(dat,align='cl|c|rrrrrr'))
}

trailing.return<-function(name,...) {
  name<-get(name)
  max.date<-asof
  dat<-jf.ar.table(name,...)
  dat$MTD<-dat$MTD*100
  dat$YTD<-dat$YTD*100
  dat$Last12<-dat$Last12*100
  dat$Last24<-dat$Last24*100
  dat$Last36<-dat$Last36*100
  dat$Last60<-dat$Last60*100
  dat<-arrange(dat,-MTD)
  dat$date<-NULL
  #dat[,list(variable,allocation,MTD,YTD,last12,last24,last36,last60)]
  setnames(dat,old="variable",new="Fund")
  total.return.note<-paste0("Return values are total return for the period ending ",asof,".")
  print(xtable(dat,align='cl|rrrrrr'))
}

calendar.table.itd<-function(name,DT=z) {
  ctable<-calendarTable(DT[name],ITD=TRUE)
  xt<-capture.output(xtable(ctable*100,align='c|rrrrrrrrrrrr|r|r'))
  xt_mod<-gsub("(\\s|^)(-\\d*\\.\\d*)","\\1\\\\textcolor{red}{\\2}",xt)
  cat(xt_mod,sep='\n')
}

calendar.table<-function(name,DT=z) {
  ctable<-calendarTable(DT[name])
  xt<-capture.output(print(xtable(ctable*100,align='c|rrrrrrrrrrrr|r')))
  xt_mod<-gsub("(\\s|^)(-\\d*\\.\\d*)","\\1\\\\textcolor{red}{\\2}",xt)
  cat(xt_mod,sep='\n')
}

roll_chart<-function(name,n=12) {
  dat<-zd[name]
  dat[,roll:=rollapplyr(value,fill=NA,width=n,FUN=aror,align='right')]
  ggplot(dat,aes(x=date,y=roll,group=variable))+
    geom_line()+
    geom_hline(yintercept=0,linetype=3)+
    theme_bw()+
    labs(x=NULL,y=NULL)+
    scale_y_continuous(labels=percent)+
    scale_x_date(expand=c(0,0))
}

dd_chart<-function(name) {
  dat<-zd[name]
  dat[,dd:=dd(value)]
  ggplot(dat,aes(x=date,y=dd,group=variable))+
    geom_line()+
    geom_hline(yintercept=0,linetype=3)+
    theme_bw()+
    labs(x=NULL,y=NULL)+
    scale_y_continuous(labels=percent)+scale_x_date(expand=c(0,0))
}

vami_chart<-function(name) {
  dat<-zd[name]
  dat[,vami:=vami(value)]
  ggplot(dat,aes(x=date,y=vami,group=variable))+
    geom_line()+
    geom_hline(yintercept=1,linetype=3)+
    theme_bw()+
    labs(x=NULL,y=NULL)+
    scale_y_continuous(labels=dollar)+scale_x_date(expand=c(0,0))
}


description <- function (name) {
  cat(as.character(z[name][,tail(.SD,1)]$description))
}