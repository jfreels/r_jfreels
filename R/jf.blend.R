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

jf.mass.blend<-function(blend_names,blend_seq,...) {
  # blend stats
  blend<-jf.blend(names=blend_names,...,value=1)
  blend_date_start<-min(blend$date)
  blend_date_end<-max(blend$date)
  blend_first_name<-names(blend[2])
  blend_second_name<-names(blend[3])
  # sharpe ratio data.frame
  blend_sharpe<-llply(blend_seq,function(x) {
    dat<-jf.blend(names=blend_names,...,value=x)
    dat_sharpe<-apply(dat[,-1],2,sharpe)
    dat_sharpe
  })
  blend_sharpe<-ldply(blend_sharpe)
  blend_sharpe$allocation<-blend_seq
  blend_sharpe<-melt(blend_sharpe,id.vars='allocation')
  blend_sharpe$type<-'sharpe'
  
  # correlation data.frame
  blend_cor<-llply(blend_seq,function(x) {
    dat<-jf.blend(names=blend_names,...,value=x)
    dat_cor<-cor(dat[,-1])
    dat_cor[,3]
  })
  blend_cor<-ldply(blend_cor)
  blend_cor$mgr_cor<-cor(jf.blend(names=blend_names,DT=z,value=1)[,c(2:3)])[2,1]
  blend_cor$allocation<-blend_seq
  blend_cor<-melt(blend_cor,id.vars='allocation')
  blend_cor$type<-'correlation'

  # drawdown data.frame
  blend_dd<-llply(blend_seq,function(x) {
  	dat<-jf.blend(names=blend_names,...,value=x)
  	dat_dd<-apply(dat[,-1],2,maxdd)
  	dat_dd
  })
  blend_dd<-ldply(blend_dd)
  blend_dd$allocation<-blend_seq
  blend_dd<-melt(blend_dd,id.vars='allocation')
  blend_dd$type<-'max_drawdown'

  # var data.frame
  blend_var<-llply(blend_seq,function(x) {
  	dat<-jf.blend(names=blend_names,...,value=x)
  	dat<-xts(dat[,-1],dat[,1])
  	dat_var<-VaR(dat,p=.99,method='gaussian')
  	dat_var
  })
  blend_var<-ldply(blend_var)
  blend_var$allocation<-blend_seq
  blend_var<-melt(blend_var,id.vars='allocation')
  blend_var$type<-'var_99'

  # drought data.frame
  blend_drought<-llply(blend_seq,function(x) {
  	dat<-jf.blend(names=blend_names,...,value=x)
  	dat<-xts(dat[,-1],dat[,1])
  	dat_drought<-apply(dat,2,function(x) -max(drought(x),na.rm=TRUE))
  	dat_drought
  	})
  blend_drought<-ldply(blend_drought)
  blend_drought$allocation<-blend_seq
  blend_drought<-melt(blend_drought,id.vars='allocation')
  blend_drought$type<-'max_drought'

  # combine data.frames
  blend_combined<-rbind(blend_sharpe,blend_cor,blend_dd,blend_var,blend_drought)

  #plot
  ggplot(blend_combined,aes(x=allocation,y=value,group=variable,color=variable))+
    geom_line()+
 	facet_grid(type~.,scales='free_y')+
 	geom_vline(xintercept=0.50,linetype=2)+
 	theme_bw()+
 	scale_x_continuous(label=percent)+
 	labs(title=paste('Blends of',blend_first_name,"and",blend_second_name,"\n",format(blend_date_start,"%b %Y"),"-",format(blend_date_end,"%b %Y")),
 		x=paste("Allocation to",blend_first_name))
}