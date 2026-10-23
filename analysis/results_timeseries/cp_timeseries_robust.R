#this code creates data series ordereb y time (hour) for the number of changepoints occuring at a timepoitn
#split by VN and NVN
#THIS splits the timeseries into three part: Keith, Moderator, rest

rm(list=ls(all=TRUE))

libraries = c("tidyr","knitr","lubridate","dplyr","xts","ggplot2","scales","stringr")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#####
Aggregation = "hour" ; Aggregationmin=60; folder="data_hour"
#Aggregation = "5minute";Aggregationmin=5; folder= "data_5min"  

#set working directory and load files
setwd("C:/Users/izwetsl/OneDrive - UvA/Documenten/2 Research/1 Papers/44. Network change - Simon/network_change")
source('model/v11_logit_utc/timeconversion.R')
source('model/v11_logit_utc/settings.R')

##########FOR VN ################
#load data
result<- readRDS(paste0("data/",folder,"/results_final_VN.Rds"))
#load influencer list
influencers<-read.csv(file="data/influencers_list.csv")


#####################################################################
## function to make cp list
######################################################################
get_cp_timeseries <- function(n,res){
  
  stopifnot(is.list(res))        # ensure it's a list
  if (length(res) == 0) return(NULL)
  

  #################################
  ####All change points by aggregation period for VN####
  #################################
  df.cp <- cbind(ts,
                 cp.count.all=0,
                 cp.count.lik=0,
                 cp.count.aic=0,
                 cp.count.bic=0)
  df.vn <- data.frame(cbind(vn=rep(NA,n),
                 n.cps=rep(0,n)))
  
  for( i in 1:n){
    if(n==1){
      dum = res
    } else {
      dum = res[[i]]
    }
    cps= dum[[3]]
    df.vn$vn[i] = dum[[11]]
    if(!is.na(cps)[1]){     #check if cps are present
      for(j in 1:length(cps)){  #
        df.cp$cp.count.all[cps[j]] = df.cp$cp.count.all[cps[j]]+1
      }
      for(v in 1:3){          #do for each vuong test
        if(dum[[9]][v]<alpha){  #check if model is significant
          for(j in 1:length(cps)){  #
            df.cp[cps[j],v+4] = df.cp[cps[j],v+4]+1
            if(v==1){
              df.vn$n.cps[i] = length(cps)
            }
          }
        }
      }
    }
  }
  

  #################################################
  ####results  Identified changepoints versus time
  ####################################################
  return(df.cp)
  
}

######################
#results all VN
#####################
df.cp <- get_cp_timeseries(length(result),result)
  
###### save CP's ###
file.fig = paste0("analysis/results_timeseries/",folder,"/VN_significant_cps_all.png")
file.csv = paste0("analysis/results_timeseries/",folder,"/VN_significant_cps_all.csv")
file.rds = paste0("analysis/results_timeseries/",folder,"/VN_significant_cps_all.rds")

#save RDS and CSV
saveRDS(df.cp,file=file.rds)
write.csv(df.cp,file=file.csv)
  
#make plot and save
fig<-df.cp %>%
  gather(test,count,-c(period_dt,period_num,unix))%>%
  ggplot(aes(period_dt,count))+
    geom_line(size=1)+
    ylab("Number of changepoints")+
    scale_y_continuous(breaks=c(0,1))+
    facet_wrap(~ test,nrow=3,strip.position = "right")

png(file.fig,width=600,height=600)  
plot(fig)
dev.off()

######################
#results ONLY KG
#####################

if(result[[25]][[11]]==paste0("infl_4300")){result.onlyKG <-result[[25]]}
file.fig = paste0("analysis/results_timeseries/",folder,"/VN_significant_cps_onlyKG.png")
file.csv = paste0("analysis/results_timeseries/",folder,"/VN_significant_cps_onlyKG.csv")
file.rds = paste0("analysis/results_timeseries/",folder,"/VN_significant_cps_onlyKG.rds")

df.cp <- get_cp_timeseries(1,result.onlyKG)


#save RDS and CSV
saveRDS(df.cp,file=file.rds)
write.csv(df.cp,file=file.csv)

#make plot and save
fig<-df.cp %>%
  gather(test,count,-c(period_dt,period_num,unix))%>%
  ggplot(aes(period_dt,count))+
  geom_line(size=1)+
  ylab("Number of changepoints")+
  scale_y_continuous(breaks=c(0,1))+
  facet_wrap(~ test,nrow=3,strip.position = "right")

png(file.fig,width=600,height=600)  
plot(fig)
dev.off()

######################
#results ONLY Mod
#####################

if(result[[7]][[11]]==paste0("infl_1583")){result.onlyMod <-result[[7]]}
file.fig = paste0("analysis/results_timeseries/",folder,"/VN_significant_cps_onlyMod.png")
file.csv = paste0("analysis/results_timeseries/",folder,"/VN_significant_cps_onlyMod.csv")
file.rds = paste0("analysis/results_timeseries/",folder,"/VN_significant_cps_onlyMod.rds")

df.cp <- get_cp_timeseries(1,result.onlyMod)


#save RDS and CSV
saveRDS(df.cp,file=file.rds)
write.csv(df.cp,file=file.csv)

#make plot and save
fig<-df.cp %>%
  gather(test,count,-c(period_dt,period_num,unix))%>%
  ggplot(aes(period_dt,count))+
  geom_line(size=1)+
  ylab("Number of changepoints")+
  scale_y_continuous(breaks=c(0,1))+
  facet_wrap(~ test,nrow=3,strip.position = "right")

png(file.fig,width=600,height=600)  
plot(fig)
dev.off()

######################
#results Ex KG and Ex Mog
#####################

#Results rest only
result.rest <- result
result.rest[[25]] <-NULL  #remove KG
result.rest[[7]] <-NULL   #remove Mod
file.fig = paste0("analysis/results_timeseries/",folder,"/VN_significant_cps_rest.png")
file.csv = paste0("analysis/results_timeseries/",folder,"/VN_significant_cps_rest.csv")
file.rds = paste0("analysis/results_timeseries/",folder,"/VN_significant_cps_rest.rds")

df.cp <- get_cp_timeseries(length(result.rest),result.rest)


#save RDS and CSV
saveRDS(df.cp,file=file.rds)
write.csv(df.cp,file=file.csv)

#make plot and save
fig<-df.cp %>%
  gather(test,count,-c(period_dt,period_num,unix))%>%
  ggplot(aes(period_dt,count))+
  geom_line(size=1)+
  ylab("Number of changepoints")+
  scale_y_continuous(breaks=c(0,1))+
  facet_wrap(~ test,nrow=3,strip.position = "right")

png(file.fig,width=600,height=600)  
plot(fig)
dev.off()

##################################
##########FOR NVN ################
##################################


result<- readRDS(paste0("data/",folder,"/results_final_NVN.Rds"))
result.all<-result

######################
#prepare various result lists
#####################
result.rest <- result
result.onlyKG <-c(list(result[[1]]),list(result[[2]]))
result.onlyMod <-c(list(result[[1]]),list(result[[2]]))

#remove to nodepairs ANVNs-KG
for (i in length(result.rest):1){
  nodepair=result.rest[[i]][[11]]
  if(str_sub(nodepair,-5)=="_4300"){
    result.rest[[i]]<-NULL
    result.onlyKG <- c(result.onlyKG,list(result[[i]]))
  }
  if(str_sub(nodepair,-5)=="_1583"){
    result.rest[[i]]<-NULL
    result.onlyMod<-c(result.onlyMod,list(result[[i]]))
    }
}
#delete what was added as structure setting
result.onlyKG[[2]]<-NULL 
result.onlyKG[[1]]<-NULL
result.onlyMod[[2]]<-NULL
result.onlyMod[[1]]<-NULL

######################
#results all ANVN - VN
#####################

df.cp <- get_cp_timeseries(length(result.all),result.all)
saveRDS(df.cp,file=paste0("analysis/results_timeseries/",folder,"/NVN_significant_cps_all.RDS"))
write.csv(df.cp,file=paste0("analysis/results_timeseries/",folder,"/NVN_significant_cps_all.csv"))

fig<-df.cp %>%
  gather(test,count,-c(period_num,unix,period_dt))%>%
  ggplot(aes(period_dt,count))+
  geom_line(size=1)+
  ylab("Number of changepoints")+
  #scale_y_continuous(breaks=c(0,1,2,3,4,5))+
  facet_wrap(~ test,nrow=3,strip.position = "right")

file.fig = paste0("analysis/results_timeseries/",folder,"/NVN_significant_cps_all.png")
png(file.fig,width=600,height=600)  

plot(fig)
dev.off()

######################
#results ANVN - KG nodepairs only (4300)
#####################

df.cp <- get_cp_timeseries(length(result.onlyKG),result.onlyKG)
saveRDS(df.cp,file=paste0("analysis/results_timeseries/",folder,"/NVN_significant_cps_onlyKG.RDS"))
write.csv(df.cp,file=paste0("analysis/results_timeseries/",folder,"/NVN_significant_cps_onlyKG.csv"))

fig<-df.cp %>%
  gather(test,count,-c(period_num,unix,period_dt))%>%
  ggplot(aes(period_dt,count))+
  geom_line(size=1)+
  ylab("Number of changepoints")+
  #scale_y_continuous(breaks=c(0,1,2,3,4,5))+
  facet_wrap(~ test,nrow=3,strip.position = "right")

file.fig = paste0("analysis/results_timeseries/",folder,"/NVN_significant_cps_onlyKG.png")
png(file.fig,width=600,height=600)  

plot(fig)
dev.off()

######################
#results ANVN- Mod nodepairs only (1583)
#####################

df.cp <- get_cp_timeseries(length(result.onlyMod),result.onlyMod)
saveRDS(df.cp,file=paste0("analysis/results_timeseries/",folder,"/NVN_significant_cps_onlyMod.RDS"))
write.csv(df.cp,file=paste0("analysis/results_timeseries/",folder,"/NVN_significant_cps_onlyMod.csv"))

fig<-df.cp %>%
  gather(test,count,-c(period_num,unix,period_dt))%>%
  ggplot(aes(period_dt,count))+
  geom_line(size=1)+
  ylab("Number of changepoints")+
  #scale_y_continuous(breaks=c(0,1,2,3,4,5))+
  facet_wrap(~ test,nrow=3,strip.position = "right")

file.fig = paste0("analysis/results_timeseries/",folder,"/NVN_significant_cps_onlyMod.png")
png(file.fig,width=600,height=600)  

plot(fig)
dev.off()

######################
#results ANVN - NOT KG or  Mod Nodepair only
#####################

df.cp <- get_cp_timeseries(length(result.rest),result.rest)
saveRDS(df.cp,file=paste0("analysis/results_timeseries/",folder,"/NVN_significant_cps_rest.RDS"))
write.csv(df.cp,file=paste0("analysis/results_timeseries/",folder,"/NVN_significant_cps_rest.csv"))

fig<-df.cp %>%
  gather(test,count,-c(period_num,unix,period_dt))%>%
  ggplot(aes(period_dt,count))+
  geom_line(size=1)+
  ylab("Number of changepoints")+
  #scale_y_continuous(breaks=c(0,1,2,3,4,5))+
  facet_wrap(~ test,nrow=3,strip.position = "right")

file.fig = paste0("analysis/results_timeseries/",folder,"/NVN_significant_cps_rest.png")
png(file.fig,width=600,height=600)  

plot(fig)
dev.off()

