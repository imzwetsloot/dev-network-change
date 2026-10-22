################
##This code takes the processed edgelist
## and makes inputmatrices for followers and influencers
## these inputmatrices are save as csv files
###############

rm(list=ls(all=TRUE))

#load library
libraries = c("kableExtra","doSNOW", "foreach","MASS","tidyr","knitr","lubridate","dplyr","xts","ggplot2","igraph")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#####
#Aggregation = "hour" ; Aggregationmin=60; folder="data_hour"
Aggregation = "5minute";Aggregationmin=5; folder= "data_5min"  

#set working directory 
setwd("F:/Reddit_project/Github")
#setwd("C:/Users/inez_/Documents/2 Research/1 Papers/44. Network change - Simon/Github")
source('model/v11_logit_utc/timeconversion.R')
source('model/v11_logit_utc/settings.R')

#manual defined function to filter on 'not in'
`%notin%` <- Negate(`%in%`)

#load processing data
if(Aggregation=="hour"){
  dat1<-readRDS(file="data/processed_edgelist_hour.rds")
}else if(Aggregation=="5minute"){
  dat1<-readRDS(file="data/processed_edgelist_5min.rds")
}


#### IDENTIFY INFLUENCERS ####
influencers <- read.csv(file="data/influencers_list.csv")
infl.threat <- read.csv(file="data/infl_threat.csv")

###############################################
####### MAKE AC column for INPUT MATRIX########
###############################################

dum1 <- dat1%>%
  group_by(period_num)%>%
  dplyr::summarize(logAC=log(n()+1))

AC <- full_join(ts,dum1, by="period_num")
#AC$logAC[is.na(AC$logAC)]<-0

#plot(x=AC$period_num,y=AC$logAC)

write.csv(AC,file=paste0("data/AC_",Aggregation,".csv"))


#############################################
### Creat dummy wallstreet open
############################################
dt.utc = seq(start,end,by="min")
df.WlStr = data.frame(unix=0,
                      dt.utc=dt.utc,
                      period_num=0,
                      dt.ny=0,
                      dOpen=0)
df.WlStr$unix = as.numeric(df.WlStr$dt.utc)

df.WlStr$dt.ny=as.POSIXct(df.WlStr$unix, origin="1970-01-01",tz="EST5EDT")
df.WlStr$period_num = fUNIXtoPeriod(df.WlStr$unix,start,Aggregationmin)

#view daylight saving 1 nov 1am
#View(df.WlStr[c(3670:3900),])
#View daylight svaing March 14, 2021, 3:00:00 am
#View(df.WlStr[c(6870:6900),])

#opening hours of wallstreet 9:30-16:00
#opening hours robinhood 9:00-17:59
Tradeopen = "09:00"
Tradeopen = format(as.POSIXct(Tradeopen,format="%H:%M"),format="%H:%M")
Tradeclose = "17:59"
Tradeclose = format(as.POSIXct(Tradeclose,format="%H:%M"),format="%H:%M")

bank_holiday = c("2020-07-02",#not holiday 
                 "2020-07-03",
                 "2020-09-07",
                 "2020-10-12",#not holiday
                 "2020-11-11",#not holiday
                 "2020-11-26",
                 "2020-11-27", #early closing
                 "2020-12-24", #early closing
                 "2020-12-25",
                 "2020-12-31",#not holiday
                 "2021-01-01",
                 "2021-01-18",
                 "2021-02-15",
                 "2021-04-02",
                 "2021-05-31")
bank_holiday<-date(strptime(bank_holiday,format="%Y-%m-%d"))

for(t in 1:nrow(df.WlStr)){
  dt = df.WlStr$dt.ny[t]
  if(date(dt) %notin% bank_holiday){ #exclude bank holiday
    if(wday(dt) %in% c(2,3,4,5,6)){ #exclude sat  & sunday
      dt.hm = format(dt,format="%H:%M")
      currenthour = hour(dt)
      currentminute = minute(dt)
      currentime = paste0(currenthour,":",currentminute)
      currentime = format(as.POSIXct(currentime,format="%H:%M"),format="%H:%M")
      
      if(currentime>=Tradeopen && currentime<=Tradeclose){ #include opening hours only
        df.WlStr$dOpen[t]=1
      }
    }
  }
}
View(df.WlStr)

#create dummy variable for each period
df.dummy<-df.WlStr %>%
  filter(period_num>0) %>%
  filter(period_num<= n.period)%>%
  group_by(period_num)%>%
  dplyr::summarise(dummy = mean(dOpen))%>%
  dplyr::select(period_num,dummy)

if(nrow(df.dummy)!=n.period){stop("something wrong")}
if(all(df.dummy$period_num!=ts$period_num)){stop("Something wrong")}

dummyWallStreet <- df.dummy$dummy

#test length of weekends and nights
# vN=rep(0,400)
# idx = 1
# for(i in 1:nrow(df.dummy)){
#   if(df.dummy$dummy[i]==0){
#     switch=FALSE
#     vN[idx]=vN[idx]+1
#   }else{
#     if(!switch){
#       idx=idx+1
#     }
#     switch=TRUE
#   }
# }
# table(vN)

#############################################
### Create dummy matrix for each hour
############################################
dummyHours = matrix(0,nrow=n.period,ncol=24)

for(t in 1:n.period){
  hour.idx= hour(ts$period_dt[t])
  if(hour.idx==0){
    col.idx=24
  }else{col.idx=hour.idx}
  dummyHours[t,col.idx] = 1
}

dummyHours = as.data.frame(dummyHours)
colnames(dummyHours) <- c("hr1","hr2","hr3","hr4","hr5",
                          "hr6","hr7","hr8","hr9","hr10","hr11",
                          "hr12","hr13","hr14","hr15","hr16","hr17",
                          "hr18","hr19","hr20","hr21","hr22","hr23","hr24")

############################################
####### MAKE INFLUENCER INPUT MATRIX########
############################################

n.infl=length(influencers$influencer)

#loop over each influencer
for(i in 1:n.infl){
  print(paste(i," out of ",n.infl))
  infl = influencers$influencer[i]
  
  #create input.matrix for each influencer
  X= data.frame(period_num = ts$period_num,
                unix = ts$unix,
                X_ii=numeric(length=n.period),
                AC = AC$logAC,
                ACin = numeric(length=n.period),
                ACthr =numeric(length=n.period),
                ACout = numeric(length=n.period),
                ACoutother=numeric(length=n.period),
                dWLSopen = numeric(length=n.period))
  
  #X_ii
  x_infl <- dat1%>%
    filter(from==infl)%>%
    filter(to==infl)%>%
    group_by(period_num)%>%
    dplyr::summarize(x=n())%>%
    dplyr::select(period_num,x)
  
  x_ii <- full_join(ts,x_infl, by="period_num")
  x_ii[is.na(x_ii)]<-0
  x_ii$x[which(x_ii$x>0)]<-1 #binarize
  X$X_ii = x_ii$x

  #count number of communications
  #if(sum(x_ii$x) <= 3){next} #skip this VN as less than 4 communications 
  
  #AC
  X$AC = AC$logAC
  
  #Dummy open wallstreet
  X$dWLSopen = dummyWallStreet
  
  #ACin
  dum <- dat1%>%
    filter(to == infl)%>%
    filter(from != infl)%>%
    group_by(period_num)%>%
    dplyr::summarize(logACin=log(n()+1))
  ACin <- full_join(ts,dum,by="period_num")
  ACin$logACin[is.na(ACin$logACin)]<-0
  X$ACin = ACin$logACin
  
  #ACthreat
  threat.id <- infl.threat$threat[which(infl.threat==infl)] #all threat send by infl
  
  dum <- dat1%>%
    filter(threat %in% threat.id)%>%
    filter(to != infl)%>%
    group_by(period_num)%>%
    dplyr::summarize(logACthr=log(n()+1)) 
  ACthreat <- full_join(ts,dum,by="period_num")
  ACthreat$logACthr[is.na(ACthreat$logACthr)]<-0
  X$ACthr = ACthreat$logACthr
  
  #ACout
  dum <- dat1%>%
    filter(from == infl)%>%
    filter(to != infl)%>%
    group_by(period_num)%>%
    dplyr::summarize(logACout=log(n()+1))
  ACout <- full_join(ts,dum,by="period_num")
  ACout$logACout[is.na(ACout$logACout)]<-0
  X$ACout = ACout$logACout
  
  #ACout_others
  dum <- dat1%>%
    filter(from %in% influencers$influencer)%>%
    filter(from != infl)%>%
    group_by(period_num)%>%
    dplyr::summarize(logACout=log(n()+1))
  ACout <- full_join(ts,dum,by="period_num")
  ACout$logACout[is.na(ACout$logACout)]<-0
  X$ACoutother = ACout$logACout
  
  X <- cbind(X,dummyHours)
  write.csv(X,file=paste0("data/",folder,"/inputmatrix/infl_",infl,".csv"))
  
}


############################################
####### MAKE FOLLOWERS INPUT MATRIX########
############################################

#number of processes to use
processes = 10

# Parallelization over the rolling windows
if (processes > 1) {
  cl <- makeCluster(processes)
  clusterEvalQ(cl, library(foreach))
  # clusterEvalQ(cl, library(MASS))
  clusterEvalQ(cl, library(dplyr))
  clusterEvalQ(cl, library(tidyr))
  clusterEvalQ(cl, library(lubridate))
  clusterEvalQ(cl, library(stats))
  clusterEvalQ(cl, library(gtools))
  #  registerDoParallel(cl)
  registerDoSNOW(cl)
} else {
  registerDoSEQ()
}

##All threats posted by senders
posted.threats <- dat1%>% 
  filter(threat==comment)%>%
  dplyr::select(from,threat,period_num)

############################################
####### MAKE FOLLOWERS INPUT MATRIX#########
############################################

#influencers = read.csv(file="influencers_list.csv")
n.infl=nrow(influencers)

#total no of nodes
nodes = unique(c(dat1$from,dat1$to))
nodes<-nodes[!is.na(nodes)] #remove NA
n=length(nodes)

#loop over each influencer
for(i in 1:n.infl){
  
  infl = influencers$influencer[i]
  
  #Threats send by influencer
  threat.id <- posted.threats$threat[which(posted.threats$from==infl)] #all threat send by infl
  
  #nodes sending message in threat
  dum <- dat1%>%
    filter(threat %in% threat.id)%>%
    dplyr::select(from)
  from.nodes = unique(dum$from)
  
  #nodes sending messages to influencer (irrespective of threat)
  dum <- dat1%>%
    filter(to==infl)%>%
    dplyr::select(from)
  from.nodes = unique(c(dum$from,from.nodes))
  
  #potential non vital nodes
  followers <- from.nodes[-which(from.nodes==infl)]
  n.fol = length(followers)
  if(n.fol==0){next}
  print(paste("Influencer ",i," | ",n.infl, "with ID ",infl,"and n.fol = ",n.fol))
  
  #loop over all followers
  pb <- txtProgressBar(max = n.fol, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  list.results = foreach(j = 1:n.fol, .options.snow = opts) %dopar% {
    progress(j)
    fol = followers[j]
    
    #create input.matrix 
    X= data.frame(period_num = period_num,
                  unix = ts$unix,
                  X_ij=numeric(length=n.period),
                  AC = AC$logAC,
                  #AC_24 = AC$logAC,
                  ACin_i = numeric(length=n.period),
                  ACout_i = numeric(length=n.period),
                  ACin_j = numeric(length=n.period),
                  ACout_j = numeric(length=n.period),
                  ACthr_j = numeric(length=n.period),
                  ACoutother=numeric(length=n.period),
                  dWLSopen = numeric(length=n.period))
    
    #X_ij
    dum <- dat1%>%
      filter(from==fol)%>%
      filter(threat %in% threat.id)%>%
      group_by(period_num)%>%
      dplyr::summarize(x=n())%>%
      dplyr::select(period_num,x)
  
    #count number of communications
    if(nrow(dum) <= 3){return("done")} #skip this follower as a less than 4 communications with influencer
    
    x_ij <- full_join(ts,dum, by="period_num")
    x_ij[is.na(x_ij)]<-0
    x_ij$x[which(x_ij$x>0)]<-1 #binarize
    X$X_ij = x_ij$x
  
    
    #AC
    X$AC = AC$logAC
    
    #AC_t_minus23
    #X$AC_24 = c(rep(NA,23),AC$logAC[1:(nrow(AC)-23)])
    
    #Dummy open wallstreet
    X$dWLSopen = dummyWallStreet
    
    #ACin_i
    dum <- dat1%>%
      filter(to == fol)%>%
      group_by(period_num)%>%
      dplyr::summarize(logAC=log(n()+1))
    ACin<- full_join(ts,dum,by="period_num")
    ACin$logAC[is.na(ACin$logAC)]<-0
    X$ACin_i = ACin$logAC
    
    #ACout_i
    dum <- dat1%>%
      filter(from == fol)%>%
      filter(!(threat %in% threat.id))%>%
      group_by(period_num)%>%
      dplyr::summarize(logAC=log(n()+1))
    ACout <- full_join(ts,dum,by="period_num")
    ACout$logAC[is.na(ACout$logAC)]<-0
    X$ACout_i = ACout$logAC
    
    #ACin_j
    dum <- dat1%>%
      filter(to == infl)%>%
      filter(from != fol)%>%
      group_by(period_num)%>%
      dplyr::summarize(logAC=log(n()+1))
    ACin <- full_join(ts,dum,by="period_num")
    ACin$logAC[is.na(ACin$logAC)]<-0
    X$ACin_j = ACin$logAC
    
    #ACout_j
    dum <- dat1%>%
      filter(from == infl)%>%
      group_by(period_num)%>%
      dplyr::summarize(logAC=log(n()+1))
    ACout <- full_join(ts,dum,by="period_num")
    ACout$logAC[is.na(ACout$logAC)]<-0
    X$ACout_j = ACout$logAC
    
    #ACthr_j
    dum <- dat1%>%
      filter(threat %in% threat.id)%>%
      filter(to != infl)%>%
      filter(from != fol)%>%
      group_by(period_num)%>%
      dplyr::summarize(logACthr=log(n()+1))
    ACthr <- full_join(ts,dum,by="period_num")
    ACthr$logACthr[is.na(ACthr$logACthr)]<-0
    X$ACthr_j = ACthr$logACthr
    
    #ACout_others
    dum <- dat1%>%
      filter(from %in% influencers$influencer)%>%
      filter(from != infl)%>%
      group_by(period_num)%>%
      dplyr::summarize(logACout=log(n()+1))
    ACout <- full_join(ts,dum,by="period_num")
    ACout$logACout[is.na(ACout$logACout)]<-0
    X$ACoutother = ACout$logACout
    
    X <- cbind(X,dummyHours)
    
    write.csv(X,file=paste0("data/",folder,"/inputmatrix/fol_",fol,"_infl_",infl,".csv"))
    return(paste0("done_fol",fol,"_infl_",infl))
  }
}

close(pb)

if (processes > 1) {
  stopCluster(cl)
}

