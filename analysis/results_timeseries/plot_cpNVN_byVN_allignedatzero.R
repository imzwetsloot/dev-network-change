#Code create CP on NVN in time after CP of Vital node

rm(list=ls(all=TRUE))

libraries = c("tidyr","knitr","lubridate","dplyr","xts","ggplot2","scales")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#####
Aggregation = "hour" ; Aggregationmin=60; folder="data_hour"
Aggregation = "5minute";Aggregationmin=5; folder= "data_5min"  

#setwd("F:/Reddit_project/Github")
setwd("C:/Users/inez_/Documents/2 Research/1 Papers/44. Network change - Simon/Github")
source('model/v11_logit_utc/timeconversion.R')
source('model/v11_logit_utc/settings.R')
#alpha=5
#load data
result<- readRDS(paste0("data/",folder,"/results_final_VN.Rds"))

#load VN info
vn.data <- read.csv("data/influencers_list.csv") 
vn.data <-as.data.frame(vn.data)
vn.data <- cbind(vn.data,vn.name=paste0("infl_",vn.data$influencer))

#no influencers
n=length(result)

#vuong test to use 
v=1

###########################
#make dataframe
###########################

# #number of significant cps
# n.cp = 0
# for(i in 1:n){
#   dum =  result[[i]]
#   n.cps = length(dum[[5]])-2
#   if(n.cps>0){
#     if (dum[[9]][v]<alpha){
#       n.cp = n.cp + length(dum[[3]])
#     }
#   }
# }
# 
# #set up dataframe with each estimated model
# df.cp <- data.frame(vn.idx   = rep(NA,n.cp),
#                       vn.name  = NA,
#                       vn.group = NA,
#                       n.cps = NA,
#                       cp.yw    = NA,
#                       cp.dt =NA)
#set up dataframe with info of vn
df.vn <- data.frame(vn.idx = c(1:n),
                    vn.name = NA,
                    n.cps = NA)
for(i in 1:n){
    dum = result[[i]]
    vn.name = dum[[11]]
    df.vn$vn.name[i]=vn.name
    df.vn$n.cps[i]=length(dum[[3]])
}

#results NVN
res.nvn<- readRDS(paste0("data/",folder,"/results_final_NVN.Rds"))
m = length(res.nvn)

#number of cps per nvn
df.nvn = data.frame(nvn.idx = c(1:m),
                     nvn.name = NA,
                     vn.name=NA,
                     ncps.nvn = NA,
                     tbe =NA,
                     cp.vn=NA,
                     cp.nvn=NA)

for(i in 1:m){
  dum=res.nvn[[i]]
  nvn.name = dum[[11]]
  df.nvn$nvn.name[i] =  nvn.name
  vn.name = paste0("infl_",strsplit(nvn.name,split="_")[[1]][4])
  df.nvn$vn.name[i] = vn.name
  
  #check if NVN has significant cps
  ncps.nvn = length(dum[[3]])
  df.nvn$ncps.nvn[i] = ncps.nvn
  if(length(dum[[5]])==2){next} #skip nvn without cps
  if(dum[[9]][[v]]>alpha){next} #skip if nvn non significant cps
  
  #check if VN has significant cps
  idx.vn = which(df.vn$vn.name == vn.name)
  res.vn = result[[idx.vn]]
  cp.vn = res.vn[[4]][1]
  if(length(cp.vn)==0){next} #skip if vn without cps
  if(res.vn[[9]][[v]]>alpha){next} #skip if vn non significant cps
  
  #add Time between VN cp and NVN cp to df
  df.nvn$cp.vn[i] = cp.vn
  
  #first cp NVN
  cp.nvn = dum[[4]][1]
  df.nvn$cp.nvn[i] = cp.nvn

  #tb
  df.nvn$tbe[i] = (cp.nvn-cp.vn)/3600
  
  #second and more cps
  if(ncps.nvn>1){
    for(j in 2:ncps.nvn){
      cp.nvn = dum[[4]][j]
      tb = (cp.nvn-cp.vn)/3600
      
      row = df.nvn[i,]
      row$cp.nvn = cp.nvn
      row$tbe = tb
      df.nvn = rbind(df.nvn,row)
    }
  }
}

df.full<-left_join(df.nvn,vn.data,by="vn.name")
df.full<-df.full%>%
  mutate(cp.vn.dt = fUNIXtoDT(cp.vn))%>%
  mutate(cp.nvn.dt = fUNIXtoDT(cp.nvn))%>%
  #mutate(VitalNode = as.factor(ifelse(med.comments>10000,1,ifelse(med.comments>500,2,ifelse(med.comments>100,3,4)))))
  mutate(VitalNode = as.factor(ifelse(med.comments>10000,"1583",ifelse(med.comments>500,"4300","others"))))
#df.nvn$vn.group = as.character(df.cp$n.cps)
#df$vn.group = as.character(df$n.cps)
#df.cp$cp.yw <-as.factor(df.cp$cp.yw)
file.fig = paste0("figures/",folder,"_folcp_by_infl_allignedatzero.png")
#file.fig = paste0("analysis/results_timeseries/",folder,"/folcp_by_infl_allignedatzero.png")
png(file.fig,width=600,height=300)  
library(hrbrthemes)
fig<-df.full %>%
  ggplot( aes(x = tbe,group=VitalNode, fill=VitalNode)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()+
  xlab("Hours between VN change point and NVN change points")+
  geom_vline(xintercept=0)+
  facet_wrap(~ VitalNode)+
  theme(legend.position="none")
  #geom_point(data=df.cp,aes(x=cp.yw,y=vn.name), size=1, color="black",inherit.aes=F)+
#  facet_grid(rows = vars(vn.group), scales = "free") +#facets to add gaps
  #scale_fill_gradient(low = "white", high = "green")+
  #scale_fill_gradient('ln(no.cp)', breaks = c(1:8),  low = "white", high = "lightskyblue4")+
  #scale_x_discrete(breaks = levels(df$weeks)[c(T, rep(F, 9))])+
  #xlab("")+
  #ylab("Vital Node")+
  #theme(axis.text.y = element_text(color = "grey20", size = 6, face = "plain"))
plot(fig)
dev.off() 
plot(fig)


