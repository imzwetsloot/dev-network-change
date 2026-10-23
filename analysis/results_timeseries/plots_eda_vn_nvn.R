#this code gives plots for:
#   number of cps per VN
#   number of cps per  NVN
#   followers per VN
#   number of following per NVN
#   model estimates for VN

rm(list=ls(all=TRUE))

libraries = c("tidyr","knitr","lubridate","dplyr","xts","ggplot2","scales")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#####
Aggregation = "hour" ; Aggregationmin=60; folder="data_hour"
#Aggregation = "5minute";Aggregationmin=5; folder= "data_5min"  

setwd("F:/Reddit_project/Github")
#setwd("C:/Users/inez_/Documents/2 Research/1 Papers/44. Network change - Simon/Github")
source('model/v11_logit_utc/timeconversion.R')
source('model/v11_logit_utc/settings.R')

#load data
res.vn<- readRDS(paste0("data/",folder,"/results_final_VN.Rds"))
res.nvn<- readRDS(paste0("data/",folder,"/results_final_NVN.Rds"))
n = length(res.vn)
m = length(res.nvn)

#vuong test to use 
v=1

#no influencers with cp
n.withcp = 0
for(infl in 1:n){
  if(length(res.vn[[infl]][[5]])>2){
    n.withcp=n.withcp+1}
}

#get number of unique nodes
counter=0
dfNodes = data.frame(Name=rep(NA,n+m),VN=NA,NoFollowing=NA,NoFollowers=NA)
#run through all VN models
for(i in 1:n){
  counter=counter+1
  name = res.vn[[i]][[11]]
  name = strsplit(name,split="_")[[1]][2]
  dfNodes$Name[counter] = name
  dfNodes$VN[counter]=1
  dfNodes$NoFollowing[counter]=0
  dfNodes$NoFollowers[counter]=0
}
#run through all NVN results
counter=counter+1
for(i in 1:m){
  name = res.nvn[[i]][[11]]
  name.nvn = strsplit(name,split="_")[[1]][2]
  name.vn = strsplit(name,split="_")[[1]][4]
  dum=length(which(dfNodes$Name==name.nvn))==0 #already existing in df?
  if(dum){ #new to add to df
    dfNodes$Name[counter] = name.nvn
    dfNodes$VN[counter]=0
    dfNodes$NoFollowing[counter]=1
    dfNodes$NoFollowers[counter]=0
    idx=which(dfNodes$Name==name.vn) #vital node that is followed
    dfNodes$NoFollowers[idx] = dfNodes$NoFollowers[idx] + 1 
    counter= counter+1
  }else{ #existing name in df
    #print(i)
    idx = which(dfNodes$Name==name.nvn)
    dfNodes$NoFollowing[idx]=dfNodes$NoFollowing[idx]+1
    idx = which(dfNodes$Name==name.vn)
    dfNodes$NoFollowers[idx]=dfNodes$NoFollowers[idx]+1
  }
}
#Histogram of number of followers for VNs
file.fig=paste0("analysis/results_timeseries/",folder,"/no_followers_by_vn.png")
png(file.fig,width=600,height=600)  

fig <- dfNodes %>%
  filter(VN==1)%>%
  filter(NoFollowers<1000)%>%
  ggplot(aes(x=NoFollowers)) + 
  geom_histogram()+
  theme_bw()+
  ylab("Number of Followers")+
  annotate("text", x=40, y=15,label="Ommiting 2 influencers with")+
  annotate("text", x=40, y=13,label="2464(!) and 20177(!) followers") ### NEEDS TO BE UPDATES
plot(fig)
dev.off() 
plot(fig)


#Histogram of number of following for NVNs
file.fig=paste0("analysis/results_timeseries/",folder,"/no_following_by_nvn.png")
png(file.fig,width=600,height=600)  

fig<-dfNodes %>%
  ggplot(aes(x=NoFollowing)) + 
  geom_histogram()+
  theme_bw()+
  ylab("Number of nodes following")
plot(fig)
dev.off() 
plot(fig)

####
# Number of cps per VN and per NVN
####
df.vn <- data.frame(vn.idx = c(1:n),
                    vn.name = NA,
                    n.cps = NA,
                    n.sig.cps = NA,
                    n.nvn = NA)
for(i in 1:n){
  dum = res.vn[[i]]
  df.vn$vn.name[i] = dum[[11]]
  df.vn$n.cps[i]   = length(dum[[3]])

  #get number of significant cps
  if(length(dum[[5]])==2){next}
  if(dum[[9]][v]<alpha){ #insignificant cps
    df.vn$n.sig.cps[i] = length(dum[[3]])
  }
}
df.nvn <- data.frame(nvn.idx = c(1:m),
                     nvn.name = NA,
                     n.cps = NA,
                     n.sig.cps = NA)
for(i in 1:m){
  dum = res.nvn[[i]]
  df.nvn$nvn.name[i] = dum[[11]]
  df.nvn$n.cps[i]   = length(dum[[3]])

  #get number of significant cps
  if(length(dum[[5]])==2){next}
  if(dum[[9]][v]<alpha){ #insignificant cps
    df.nvn$n.sig.cps[i] = length(dum[[3]])
  }
}


file.fig=paste0("analysis/results_timeseries/",folder,"/no_cps_vn.png")
png(file.fig,width=600,height=600)  

fig<-df.vn %>%
  mutate(n.cps = as.factor(n.cps))%>%
  group_by(n.cps)%>%
  summarize(n=n())%>%
  ggplot(aes(x=n.cps, y=n)) + 
  geom_bar(stat = "identity")+
  theme_bw()+
  ylab("Number of VN")
plot(fig)
dev.off() 
plot(fig)

file.fig=paste0("analysis/results_timeseries/",folder,"/no_cps_nvn.png")
png(file.fig,width=600,height=600)  

fig<-df.nvn %>%
  mutate(n.cps = as.factor(n.cps))%>%
  group_by(n.cps)%>%
  summarize(n=n())%>%
  ggplot(aes(x=n.cps, y=n)) + 
  geom_bar(stat = "identity")+
  theme_bw()+
  ylab("Number of NVN")
plot(fig)
dev.off() 
plot(fig)

###########################
#Make plot cps over time
###########################
df <- cbind(ts,cps.vn=0,cps.nvn=0)
for(i in 1:n){
  dum = res.vn[[i]]
  if(length(dum[[5]])==2){next} #skip if no change points
  
  n.cps   = length(dum[[3]])
  #get number of significant cps only
  if(dum[[9]][v]<alpha){ 
    for(j in 1:n.cps){
      idx = dum[[3]][j]
      df$cps.vn[idx] =  df$cps.vn[idx] +1
    }
  } 
}
for(i in 1:m){
  dum = res.nvn[[i]]
  if(length(dum[[5]])==2){next} #skip if no change points
  
  n.cps   = length(dum[[3]])
  #get number of significant cps only
  if(dum[[9]][v]<alpha){ 
    for(j in 1:n.cps){
      idx = dum[[3]][j]
      df$cps.nvn[idx] =  df$cps.nvn[idx] +1
    }
  } 
}
file.fig=paste0("analysis/results_timeseries/",folder,"/change_points_all_influencers.png")
png(file.fig,width=600,height=600)  

fig <- df%>%
  ggplot(aes(x=period_dt,y=cps.vn))+
  geom_line()+
  theme_bw()

plot(fig)
dev.off()
plot(fig)

file.fig=paste0("analysis/results_timeseries/",folder,"/change_points_all_followers.png")
png(file.fig,width=600,height=600)  

fig <- df%>%
  ggplot(aes(x=period_dt,y=cps.nvn))+
  geom_line()+
  theme_bw()

plot(fig)
dev.off()
plot(fig)

