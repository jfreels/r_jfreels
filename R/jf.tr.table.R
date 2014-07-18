jf.tr.table<-function(df,asof,allocation=FALSE) {
  # set up variables
  asof<-as.Date(asof)
  df<-df %>%
    group_by(variable) %>%
    arrange(variable,date) %>%
    filter(date<=asof)
  # Month to date value
  MTD<-df %>%
    filter(date==as.Date(asof))
  ifelse(allocation==TRUE,
         {MTD<-MTD %>%
           select(date,variable,Allocation=allocation,MTD=value)},
         {MTD<-MTD %>%
           select(date,variable,MTD=value)})
  # Quarter to date value
  QTD<-df %>%
    filter(quarter(date)==quarter(asof),year(date)==year(asof)) %>%
    summarise(QTD=cror(value))
  dat<-left_join(MTD,QTD,by='variable')
  # Year to date value
  YTD<-df %>%
    filter(year(date)==year(asof)) %>%
    summarise(YTD=cror(value))
  dat<-left_join(dat,YTD,by='variable')
  # managers with track records > 12 months
  names12<-df %>%
    tally() %>%
    filter(n>=12) %>%
    .$variable
  last12<-df %>%
    filter(variable %in% names12,
           min_rank(desc(date))<=12) %>%
    summarise(Last12=cror(value))
  dat<-left_join(dat,last12,by='variable')
  # managers with track records > 36 months
  names36<-df %>%
    tally() %>%
    filter(n>=36) %>%
    .$variable
  last36<-df %>%
    filter(variable %in% names36,
           min_rank(desc(date))<=36) %>%
    summarise(Last36=cror(value))
  dat<-left_join(dat,last36,by='variable')
  # managers with track records > 60 months
  names60<-df %>%
    tally() %>%
    filter(n>=60) %>%
    .$variable
  last60<-df %>%
    filter(variable %in% names60,
           min_rank(desc(date))<=60) %>%
    summarise(Last60=cror(value))
  dat<-left_join(dat,last60,by='variable')
  ifelse(allocation==TRUE,
         dat<-arrange(dat,desc(Allocation)),
         dat<-arrange(dat,desc(MTD)))
  dat
}