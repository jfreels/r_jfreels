jf.blend<-function(names,DT,value) {
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

jf.mass.blend<-function(blend_names,blend_seq) {
  # sharpe ratio graph
  blend_sharpe<-llply(blend_seq,function(x) {
    dat<-jf.blend(names=blend_names,DT=z,value=x)
    dat_sharpe<-apply(dat[,-1],2,sharpe)
    dat_sharpe
  })
  blend_sharpe<-ldply(blend_sharpe)
  blend_sharpe$allocation<-blend_seq
  blend_sharpe<-melt(blend_sharpe,id.vars='allocation')
  blend_sharpe$type<-'sharpe'
  
  # correlation graph
  blend_cor<-llply(blend_seq,function(x) {
    dat<-jf.blend(names=blend_names,DT=z,value=x)
    dat_cor<-cor(dat[,-1])
    dat_cor[,3]
  })
  blend_cor<-ldply(blend_cor)
  blend_cor$mgr_cor<-cor(jf.blend(names=blend_names,DT=z,value=x)[,c(2:3)])[2,1]
  blend_cor$allocation<-blend_seq
  blend_cor<-melt(blend_cor,id.vars='allocation')
  blend_cor$type<-'cor'
  
  # combined plots
  blend_combined<-rbind(blend_sharpe,blend_cor)
  ggplot(blend_combined,aes(x=allocation,y=value,group=variable,color=variable))+
    geom_line()+
    facet_grid(type~.,scales='free_y')+
    geom_vline(xintercept=0.50,linetype=2)+
    theme_bw()
}