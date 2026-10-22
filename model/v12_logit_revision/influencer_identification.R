################
##This code takes the processed edgelist
## and identifies all vital nodes
###############

rm(list=ls(all=TRUE))

#load library
libraries = c("kableExtra","doSNOW", "foreach","MASS","tidyr","knitr","lubridate","dplyr","xts","ggplot2","igraph")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#set working directory 
setwd("F:/Reddit_project/Github/model/v11_logit_utc")
source('timeconversion.R')
source('settings.R')

#set working directory
setwd("F:/Reddit_project/Github")
#setwd("C:/Users/inez_/Documents/2 Research/1 Papers/44. Network change - Simon/Github")

#manual defined function to filter on 'not in'
`%notin%` <- Negate(`%in%`)

#####
Aggregation = "hour" ; Aggregationmin=60; folder="data_hour"
#Aggregation = "5minute";Aggregationmin=5; folder= "data_5min"  


#load processing data
if(Aggregation=="hour"){
  dat1<-readRDS(file="data/processed_edgelist_hour.rds")
}else if(Aggregation=="5minute"){
  dat1<-readRDS(file="data/processed_edgelist_5min.rds")
}


##############################
#### IDENTIFY INFLUENCERS ####
##############################

##all threats in dataset with person posting and week
infl.threat <- dat1%>%
  filter(threat==comment)%>%
  mutate(influencer = from)%>%
  mutate("y_w" = paste0(year(datetime),"_",week(datetime),sep=""))%>%
  dplyr::select(influencer,threat,y_w)
write.csv(infl.threat,file="data/infl_threat.csv")

#No of comments in each threat 
comm.threat <- dat1%>%
  filter(threat %in% infl.threat$threat)%>%
  group_by(threat)%>%
  dplyr::summarize(n.comments=n()-1) #omit 1 for the initial threat post

#No of comments per threat and poster of threat
dum<-full_join(infl.threat,comm.threat, by="threat")

#Information organised per threat-sender 
threat.posters <- dum%>%
  group_by(influencer)%>%
  dplyr::summarize(n.threat = n_distinct(threat),
                   med.comments = median(n.comments),
                   tot.comments = sum(n.comments),
                   # mean.comments = mean(n.comments),
                   n.week = n_distinct(y_w))%>%
  arrange(desc(n.threat))
write.csv(threat.posters,file="data/comments_per_threatsender.csv")

#get latex table 
threat.posters %>%
  arrange(desc(n.threat))%>%
  filter(n.threat>28)%>%
  kbl(caption="Top 10 nodes with most threats",
      format="latex",
      col.names = c("Node","n.threats","med.comments","tot.comments","n.weeks"),
      align="r") %>%
  kable_minimal(full_width = F,  html_font = "Source Sans Pro")

top10.mediancomments = quantile(threat.posters$med.comments,0.9)

#influencers - selected from trheat.posters
influencers <- threat.posters %>%
  filter(med.comments>=top10.mediancomments)%>%
  filter(n.week>=5)%>%
  arrange(desc(n.threat))
nrow(influencers)

write.csv(influencers,file="data/influencers_list.csv")

