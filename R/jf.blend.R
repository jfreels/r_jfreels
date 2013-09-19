require(PerformanceAnalytics)

jf.blend<-function(names,DT=z,value) {
  dat_names<-names # pick names
  dat<-DT[variable %in% dat_names] # create data set using those names
  dat_start<-max(jf.table.dates(dat)$data_start) # find latest start date
  dat_end<-min(jf.table.dates(dat)$data_end) # find earliest end date
  dat<-dat[date>=dat_start][date<=dat_end] # filter the data set based on the dates
  dat<-dcast(dat,date~variable,value.var='value')
  dat_blend_value<-value
  dat$blend<-dat[,2]*(dat_blend_value)+dat[,3]*(1-dat_blend_value)
  dat
}

jf.blend.multiple<-function(names,DT=z,values) {
  dat_names<-names # pick names
  dat<-DT[variable %in% dat_names] # create data set using those names
  dat_start<-max(jf.table.dates(dat)$data_start) # find latest start date
  dat_end<-min(jf.table.dates(dat)$data_end) # find earliest end date
  dat<-dat[date>=dat_start][date<=dat_end] # filter the data set based on the dates
  dat<-dcast(dat,date~variable,value.var='value')
  dat$blend<-apply(dat[,-1]*values,1,sum)
  dat<-data.frame(date=dat$date,blend=dat$blend)
  dat<-melt(dat,id.vars='date')
  data.table(dat)
}

jf.mass.blend<-function(blend_names,blend_seq,...) {
  # blend stats
  blend<-jf.blend(names=blend_names,...,value=1)
  blend_date_start<-min(blend$date)
  blend_date_end<-max(blend$date)
  blend_first_name<-names(blend[2])
  blend_second_name<-names(blend[3])

  # correlation data.frame
  blend_cor<-llply(blend_seq,function(x) {
    dat<-jf.blend(names=blend_names,...,value=x)
    dat_cor<-cor(dat[,-1])[,3]
  })
  blend_cor<-ldply(blend_cor)
  blend_cor$mgr_cor<-cor(jf.blend(names=blend_names,DT=z,value=1)[,c(2:3)])[2,1]
  blend_cor$allocation<-blend_seq
  blend_cor<-melt(blend_cor,id.vars='allocation')
  blend_cor$type<-'correlation'

  # return data.frame
  blend_return<-llply(blend_seq,function(x) {
    dat<-jf.blend(names=blend_names,...,value=x)
    apply(dat[,-1],2,aror)    
  })
  blend_return<-ldply(blend_return)
  blend_return$allocation<-blend_seq
  blend_return<-melt(blend_return,id.vars='allocation')
  blend_return$type<-'ann. return'

  # std dev data.frame
  blend_sd<-llply(blend_seq,function(x) {
    dat<-jf.blend(names=blend_names,...,value=x)
    apply(dat[,-1],2,function(x) -1*asd(x))    
  })
  blend_sd<-ldply(blend_sd)
  blend_sd$allocation<-blend_seq
  blend_sd<-melt(blend_sd,id.vars='allocation')
  blend_sd$type<-'ann. std. dev.'

  # sharpe ratio data.frame
  blend_sharpe<-llply(blend_seq,function(x) {
    dat<-jf.blend(names=blend_names,...,value=x)
    apply(dat[,-1],2,sharpe)
  })
  blend_sharpe<-ldply(blend_sharpe)
  blend_sharpe$allocation<-blend_seq
  blend_sharpe<-melt(blend_sharpe,id.vars='allocation')
  blend_sharpe$type<-'sharpe'
 
  # max drawdown data.frame
  blend_max_dd<-llply(blend_seq,function(x) {
  	dat<-jf.blend(names=blend_names,...,value=x)
	apply(dat[,-1],2,maxdd)
  })
  blend_max_dd<-ldply(blend_max_dd)
  blend_max_dd$allocation<-blend_seq
  blend_max_dd<-melt(blend_max_dd,id.vars='allocation')
  blend_max_dd$type<-'max_drawdown'

  # average drawdown data.frame
  blend_avg_dd<-llply(blend_seq,function(x) {
  	dat<-jf.blend(names=blend_names,...,value=x)
  	apply(dat[,-1],2,function(x) mean(dd(x)))
  })
  blend_avg_dd<-ldply(blend_avg_dd)
  blend_avg_dd$allocation<-blend_seq
  blend_avg_dd<-melt(blend_avg_dd,id.vars='allocation')
  blend_avg_dd$type<-'avg_drawdown'

  # var data.frame
  blend_var<-llply(blend_seq,function(x) {
  	dat<-jf.blend(names=blend_names,...,value=x)
  	dat<-xts(dat[,-1],dat[,1])
	VaR(dat,p=.99,method='gaussian')
  })
  blend_var<-ldply(blend_var)
  blend_var$allocation<-blend_seq
  blend_var<-melt(blend_var,id.vars='allocation')
  blend_var$type<-'var_99'

  # drought data.frame
  blend_drought<-llply(blend_seq,function(x) {
  	dat<-jf.blend(names=blend_names,...,value=x)
  	dat<-xts(dat[,-1],dat[,1])
  	apply(dat,2,function(x) -max(drought(x),na.rm=TRUE))
  })
  blend_drought<-ldply(blend_drought)
  blend_drought$allocation<-blend_seq
  blend_drought<-melt(blend_drought,id.vars='allocation')
  blend_drought$type<-'max_drought'

  

  # combine data.frames
  blend_combined<-rbind(blend_return,blend_sharpe,blend_cor,blend_sd,blend_avg_dd,blend_max_dd,blend_var,blend_drought)
  blend_combined$type<-factor(blend_combined$type,levels=c('correlation','ann. return','ann. std. dev.','sharpe','max_drawdown','avg_drawdown','var_99','max_drought'))

  #plot
  ggplot(blend_combined,aes(x=allocation,y=value,group=variable,color=variable))+
    geom_line()+
 	facet_grid(type~.,scales='free_y')+
 	geom_vline(xintercept=0.50,linetype=2)+
 	theme_bw()+
 	scale_x_continuous(label=percent,expand=c(0,0))+
 	labs(title=paste('Portfolio blends of:\n',blend_first_name,"\nand\n",blend_second_name,"\n",format(blend_date_start,"%b %Y"),"-",format(blend_date_end,"%b %Y")),
 		x=paste("Allocation to",blend_first_name))
}