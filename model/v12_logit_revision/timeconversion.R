#function to take UTC-UNIX time and make a period number for analysis
#input: UTC-Unix time stamp (to be converted to period number)
#start: start date and time of whole analysis in UTC timezone
#format start =strptime("2020-06-01 00:00:00",format="%Y-%m-%d %H:%M:%S",tz="utc")
#aggregation: time window of aggregation in minutes (eg 5 or 60)

fUNIXtoPeriod= function(input,
                        start,
                        aggregation){
  start_unix = as.numeric(start)
  input2 =  (input - start_unix+1)
  step = aggregation*60
  period = plyr::round_any(input2, step, f = ceiling)
  period2 = period/step
  return(period2)
}


#function to take period number and make UTC-UNIX timestamp outof it
#input: period number (to be converted toUTC-Unix time stamp)
#start: start date and time of whole analysis in UTC timezone
#format start =strptime("2020-06-01 00:00:00",format="%Y-%m-%d %H:%M:%S",tz="utc")
#aggregation: time window of aggregation in minutes (eg 5 or 60)
fPeriodtoUNIX = function(input,
                         start,
                         aggregation){
  step = aggregation*60
  start_unix = as.numeric(start)
  dum1 = input*step
  unix = dum1 + start_unix -1
  return(unix)
}

fUNIXtoDT = function(unix){
  dt = as.POSIXct(unix, origin="1970-01-01",tz="utc")
  return(dt)
}

fPeriodtoDT = function(input, start,aggregation){
  unix = fPeriodtoUNIX(input,start,aggregation)
  dt = fUNIXtoDT(unix)
  return(dt)
}



