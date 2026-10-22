#this files contains the settings we use throughout our analysis

Lags=5


#alpha for vuong test
alpha=0.05

#start and end times
if(Aggregation=="hour"){
  start = strptime("2020-06-01 00:00:01",format="%Y-%m-%d %H:%M:%S",tz="utc")
  end =  strptime("2021-06-01 00:00:00",format="%Y-%m-%d %H:%M:%S",tz="utc")
}else if(Aggregation=="5minute"){
  start = strptime("2021-01-11 00:00:01",format="%Y-%m-%d %H:%M:%S",tz="utc")
  start = start - 24*60*60*Lags  #to start the model from 11 Jan we need data including the previous days for lages AC_24
  end =  strptime("2021-02-13 00:00:00",format="%Y-%m-%d %H:%M:%S",tz="utc")
}
start_unix = as.numeric(start) 
end_unix = as.numeric(end)
n.period = (end_unix - start_unix+1)/(Aggregationmin*60)
period_num = c(1:n.period)

frenzy_start = strptime("2021-01-19 00:00:01",format="%Y-%m-%d %H:%M:%S",tz="utc")
frenzy_end =  strptime("2021-01-31 23:59:00",format="%Y-%m-%d %H:%M:%S",tz="utc")


#dataframe with all hours
ts <-data.frame(period_num=period_num,
                unix = 0,
                period_dt = 0)
ts$unix = fPeriodtoUNIX(ts$period_num,start,Aggregationmin)
ts$period_dt = fUNIXtoDT(ts$unix)

if(setting=="A"){
  #rolling window
  if(Aggregation == "hour"){
    rolling.window = 2*30*24 #robustness check include larger/smaller windows
    step=168   #steps between rolling windows week
    t.between.cps = 168 #minimum time between two consequtive changepoints
    cpstep = 12 #to speed up code first eveluate cps by step and then fill in the gap
  }else if (Aggregation == "5minute"){
    rolling.window = 7*24*12 #4*24*12 #3*24*60 #robustness check include larger/smaller windows
    step= 12*24 #12*4   #steps between rolling windows  = 1 day
    t.between.cps =12*24 #minimum time between two consequtive changepoints
    cpstep = 12 #to speed up code first eveluate cps by step and then fill in the gap
  }
}else if(setting== "B"){ #three months window
  #rolling window
  if(Aggregation == "hour"){
    rolling.window = 3*30*24 #three months instead of two month 
    step=168   #steps between rolling windows week
    t.between.cps = 168 #minimum time between two consequtive changepoints
    cpstep = 12 #to speed up code first eveluate cps by step and then fill in the gap
  }else if (Aggregation == "5minute"){
    rolling.window = 14*24*12 #two weeks instead of standard one
    step= 12*24 #12*4   #steps between rolling windows  = 1 day
    t.between.cps =12*24 #minimum time between two consequtive changepoints
    cpstep = 12 #to speed up code first eveluate cps by step and then fill in the gap
  }
} else if(setting== "C"){ #three months window
  #rolling window
  if(Aggregation == "hour"){
    rolling.window = 1*30*24 #one month instead of two month 
    step=168   #steps between rolling windows week
    t.between.cps = 168 #minimum time between two consequtive changepoints
    cpstep = 12 #to speed up code first eveluate cps by step and then fill in the gap
  }else if (Aggregation == "5minute"){
    rolling.window = 3*24*12 #half a  week instead of standard one
    step= 12*24 #12*4   #steps between rolling windows  = 1 day
    t.between.cps =12*24 #minimum time between two consequtive changepoints
    cpstep = 12 #to speed up code first eveluate cps by step and then fill in the gap
  }
}
