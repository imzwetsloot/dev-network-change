################
##This code take the webscraped data and processes it into
## a dataframe with edgelist
## this dataframe is saved as an Rds file
###############

#load and install libraries
libraries = c("tidyr","knitr","lubridate","dplyr","plyr")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#manual defined function to filter on 'not in'
`%notin%` <- Negate(`%in%`)

#set working directory 
setwd("F:/Reddit_project/Github/model/v11_logit_utc")

source('timeconversion.R')
source('settings.R')

#set working directory 
setwd("F:/Reddit_project/Github")

############################
##### DATA PREPARATION #####
############################

#####
#Aggregation = "hour" ; Aggregationmin=60
Aggregation = "5minute";Aggregationmin=5  

#Read the data
dum <- read.csv(file = 'data/interactions_periods_userno_updated.csv')
#dumor<-dum
#dum<-dum[1:10000,]

#threats with response exception see "exception_final.xls"
excl_threat = c("m1xhlc","l71c4z","lm7n51","l6vhy3",
                "l5c0nr","l4lmrx","l2ljpt","l65p3u")

#process data into correct format
dum1 <- dum %>%
  #mutate(date=strptime(period,format="%Y-%m-%d",tz="utc"))%>%
  mutate(datetime=as.POSIXct(utc, origin="1970-01-01",tz="utc"))%>%
  #mutate(period=as.POSIXct(strptime(period,format="%Y-%m-%d")))%>%
  mutate(from = as.integer(from_no.))%>%
  mutate(to = as.integer(to_no.))%>%
  mutate(threat = as.character(post.id))%>%
  mutate(comment = as.character(comment.id)) %>%
  mutate(period_num = fUNIXtoPeriod(utc,start=start,Aggregationmin))%>%
  #mutate(hour =  ifelse(hour>=9,hour-9,hour+15)) %>%
  #mutate(period_hour = period+hour*60*60)%>%
  dplyr::select(datetime,from,to,threat,comment,period_num,utc)

#filter data to 
##1) include correct period 
dum2 <- dum1 %>%
  filter(utc >= start_unix) %>% 
  filter(utc <= end_unix) %>%
  arrange(period_num)

#filter data to 
###2) exclude none/automoderatore and 
###3) exclude threats with response exception
dum3 <- dum2 %>%
  filter(to != 7)%>% #take out none
  filter(from != 7)%>%
  filter(to != 87491)%>% #take out automoderator
  filter(from != 87491)%>%
  filter(to != 0)%>% #take out VisualMod
  filter(from != 0)%>%
  filter(threat %notin% excl_threat)%>%
  arrange(period_num)


#create timeseries of all hours
ts <- data.frame(period_num=1:n.period)
data_with_missing_days <- full_join(ts,dum3, by="period_num")

dat <- data_with_missing_days %>%
  arrange(period_num)

head(dat)
if(Aggregation=="hour"){
  saveRDS(dat,file="data/processed_edgelist_hour.rds")
}else if(Aggregation=="5minute"){
  saveRDS(dat,file="data/processed_edgelist_5min.rds")
}
