#this code creates data series ordereb y time (hour) for the number of changepoints occuring at a timepoitn
#split by VN and NVN

rm(list=ls(all=TRUE))

libraries = c("tidyr","knitr","lubridate","dplyr","xts","ggplot2","scales")
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

#number of VN
n=length(result)
n.withcp = 0
for(infl in 1:n){
  if(length(result[[infl]][[9]])>2){
    n.withcp=n.withcp+1}
}

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
  dum = result[[i]]
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
####results for table: Identified changepoints
####################################################
table(df.vn$n.cps)

#################################
####Plot : all change points VN####
#################################

# cp2 = as.POSIXct(unlist(cp), origin = "1970-01-01")
# cp2 = data.frame(table(cp2))
# 
# cp.freq <- cp2%>%
#   mutate(dates=strptime(cp2,format="%Y-%m-%d"))%>%
#   dplyr::select(dates,Freq)
# 
# cp.freq <- full_join(dates,cp.freq, by="dates")
# cp.freq$Freq[which(is.na(cp.freq$Freq))]<-0
# 
# ts.cp = xts(cp.freq$Freq,order.by=cp.freq$dates)
# 
# file.fig = "analysis/results_timeseries/change_points_all_VN.png"
# png(file.fig,width=600,height=300)  
# 
# plot.xts(ts.cp)
# dev.off()

#################################
####Plot 2: significant change points VN####
#################################

fig<-df.cp %>%
  gather(test,count,-c(period_dt,period_num,unix))%>%
  ggplot(aes(period_dt,count))+
    geom_line(size=1)+
    ylab("Number of changepoints")+
    scale_y_continuous(breaks=c(0,1))+
    facet_wrap(~ test,nrow=3,strip.position = "right")

file.fig = paste0("analysis/results_timeseries/",folder,"/VN_significant_cps.png")
png(file.fig,width=600,height=600)  

plot(fig)
dev.off()

##########FOR NVN ################

result<- readRDS(paste0("data/",folder,"/results_final_NVN.Rds"))

#no NVN
n=length(result)
n.withcp = 0
for(i in 1:n){
  if(length(result[[i]][[9]])>2){
    n.withcp=n.withcp+1}
}

#################################
####All change points by hour for NVN####
#################################
#cp.index = vector(mode="list",length=n)
#cp =vector(mode="list",length=n)
df.cp <- cbind(ts,
               cp.count.all=0,
               cp.count.lik=0,
               cp.count.aic=0,
               cp.count.bic=0)

for( i in 1:n){
  dum = result[[i]]
  cps= dum[[3]]
  if(!is.na(dum[[3]])[1]){     #check if cps are present
    #cp.index[[i]] = dum[[3]]
    #cp[[i]] = hours[cp.index[[i]]]
    for(j in 1:length(cps)){  #
      df.cp[cps[j],4] = df.cp[cps[j],4]+1
    }

    for(v in 1:3){          #do for each vuong test
      if(dum[[9]][v]<alpha){  #check if model is significant
        for(j in 1:length(cps)){  #
          df.cp[cps[j],v+4] = df.cp[cps[j],v+4]+1
        }
      }
    }
  }
}
#cp.index = data.frame(table(unlist(cp.index)))
#colnames(cp.index)<-c("changepoint","freq")

#saveRDS(cp.index,file="analysis/results_timeseries/all_cp_NVN.RDS")
#write.csv(cp.index,file="analysis/results_timeseries/all_cp_NVN.csv")
saveRDS(df.cp,file=paste0("analysis/results_timeseries/",folder,"/NVN_significant_cps.RDS"))
write.csv(df.cp,file=paste0("analysis/results_timeseries/",folder,"/NVN_significant_cps.csv"))

#################################
####Plot 1: all change points VN####
#################################

# cp2 = as.POSIXct(unlist(cp), origin = "1970-01-01")
# cp2 = data.frame(table(cp2))
# 
# cp.freq <- cp2%>%
#   mutate(dates=strptime(cp2,format="%Y-%m-%d"))%>%
#   dplyr::select(dates,Freq)
# 
# cp.freq <- full_join(dates,cp.freq, by="dates")
# cp.freq$Freq[which(is.na(cp.freq$Freq))]<-0
# 
# ts.cp = xts(cp.freq$Freq,order.by=cp.freq$dates)
# 
# file.fig = "analysis/results_timeseries/change_points_all_NVN.png"
# png(file.fig,width=600,height=300)  
# 
# plot.xts(ts.cp)
# dev.off()

#################################
####Plot 2: significant change points VN####
#################################

fig<-df.cp %>%
  gather(test,count,-c(period_num,unix,period_dt))%>%
  ggplot(aes(period_dt,count))+
  geom_line(size=1)+
  ylab("Number of changepoints")+
  #scale_y_continuous(breaks=c(0,1,2,3,4,5))+
  facet_wrap(~ test,nrow=3,strip.position = "right")

file.fig = paste0("analysis/results_timeseries/",folder,"/NVN_significant_cps.png")
png(file.fig,width=600,height=600)  

plot(fig)
dev.off()

