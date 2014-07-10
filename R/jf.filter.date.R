jf.filter.date<-function(df,end_date,n_months_lookback,min_months_required) {
  start_date<-as.Date(end_date)+days(1)-months(n_months_lookback-1)-days(1) # start date is end date minus lookback minus 1
  df_names<-df %.%
    group_by(variable) %.%
    filter(date<=as.Date(end_date)) %.% # remove data newer than end_date
    filter(date>=start_date) %.% # remove data older than end_date - (n_months_lookback - 1)
    tally() %.% # count the number of dates in the data set
    filter(n>=min_months_required) # remove data without min_months_required
  df_names<-df_names$variable
  df %.%
    filter(variable %in% df_names) %.% 
    filter(date<=as.Date(end_date))%.% # remove data newer than end_date
    filter(date>=start_date) # remove data older than end_date - (n_months_lookback - 1)
}