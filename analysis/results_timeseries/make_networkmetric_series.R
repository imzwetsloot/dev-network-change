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


#Aggregation = "hour" ; Aggregationmin=60; folder="data_hour"
Aggregation = "5minute";Aggregationmin=5; folder= "data_5min"  


#set working directory 
setwd("F:/Reddit_project/Github/model/v11_logit_utc")
source('timeconversion.R')
source('settings.R')

#set working directory
setwd("F:/Reddit_project/Github")
#setwd("C:/Users/inez_/Documents/2 Research/1 Papers/44. Network change - Simon/Github")


#load processing data
if(Aggregation =="hour"){
  dat1<-readRDS(file="data/processed_edgelist_hour.rds")
}else if(Aggregation=="5minute"){
  dat1<-readRDS(file="data/processed_edgelist_5min.rds")
}

#dataframe with all hours
# ts <-data.frame(period.num=hours.idx,
#                 period_hour = hours,
#                 day.num = NA,
#                 day.date = hours)
# ts$day.date = date(ts$period_hour)
# 
# start= as.numeric(ts$day.date[1])-1
# ts$day.num = as.numeric(ts$day.date)-start
# n.days=length(unique(ts$day.num)) 
# #VN <- read.csv(file="data/influencers_list.csv")

###############################################
####### MAKE Network metrics column per hour########
###############################################
names <-c(colnames(ts),"meandist","totalDegree","meandegree","activenodes","betweennes")
netstat <- data.frame(matrix(0,nrow=n.period,ncol=length(names)))
netstat[,1:ncol(ts)] <-ts
colnames(netstat) <- names

# el<-dat1%>%
#   dplyr::select(from,to)%>%
#   na.omit()
# net <- graph_from_data_frame(el,directed=TRUE)
# nodes <- V(net)

for(t in 1:n.period){
  el <- dat1 %>%
    filter(period_num == t)%>%
    dplyr::select(from,to)%>%
    na.omit()
  if(nrow(el)>0){
    net <- graph_from_data_frame(el,directed=TRUE)
  
    netstat[t,"meandist"] <- mean_distance(net) #calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices
    netstat[t,"totalDegree"] <- sum(degree(net,mode="in")) #total number of posts
    netstat[t,"meandegree"] <- mean(degree(net,mode="in")) #mean number of posts send per active node
    netstat[t,"activenodes"] <- length(V(net)) #number of nodes active in network at this time points
   # netstat[t,6] <- mean(harmonic_centrality(net,vids=V(net)),na.rm=TRUE) #The harmonic centrality of a vertex is the mean inverse distance to all other vertices. The inverse distance to an unreachable vertex is considered to be zero
  #  netstat[t,7] <- max(harmonic_centrality(net,vids=V(net)),na.rm=TRUE)
    netstat[t,"betweennes"] <- mean(betweenness(net,v=V(net),directed=TRUE)) #average number of shortest paths going through nodes
   # netstat[t,9] <- max(betweenness(net,v=V(net),directed=TRUE)) #maximum number of shortest paths going thorugh one node
   # netstat[t,10] <- log(diameter(net)+1) #longest shortest path
  }
}
#AC$logAC[is.na(AC$logAC)]<-0

for(i in 4:ncol(netstat)){
  plot(x=netstat$period_dt,y=netstat[,i])
  title(names[i])
}

write.csv(netstat,file=paste0("data/",folder,"/network_stat.csv"))
saveRDS(netstat,file=paste0("data/",folder,"/network_stat.Rds"))



###############################################
####### MAKE Network metrics column per day########
###############################################

# names <-c("day","meandist","totalDegree","meandegree","activenodes","betweennes")
# netstat <- data.frame(matrix(0,nrow=n.days,ncol=length(names)))
# netstat[,1] <-unique(ts$day.date)
# colnames(netstat) <- names
# 
# # el<-dat1%>%
# #   dplyr::select(from,to)%>%
# #   na.omit()
# # net <- graph_from_data_frame(el,directed=TRUE)
# # nodes <- V(net)
# 
# for(t in n.days:244){
#   el <- dat1 %>%
#     filter(day.num == t)%>%
#     dplyr::select(from,to)%>%
#     na.omit()
#   if(nrow(el)>0){
#     net <- graph_from_data_frame(el,directed=TRUE)
#     
#     netstat[t,2] <- mean_distance(net) #calculates the average path length in a graph, by calculating the shortest paths between all pairs of vertices
#     netstat[t,3] <- sum(degree(net,mode="in")) #total number of posts
#     netstat[t,4] <- mean(degree(net,mode="in")) #mean number of posts send per active node
#     netstat[t,5] <- length(V(net)) #number of nodes active in network at this time points
#     # netstat[t,6] <- mean(harmonic_centrality(net,vids=V(net)),na.rm=TRUE) #The harmonic centrality of a vertex is the mean inverse distance to all other vertices. The inverse distance to an unreachable vertex is considered to be zero
#     #  netstat[t,7] <- max(harmonic_centrality(net,vids=V(net)),na.rm=TRUE)
#  #   netstat[t,6] <- mean(betweenness(net,v=V(net),directed=TRUE)) #average number of shortest paths going through nodes
#     # netstat[t,9] <- max(betweenness(net,v=V(net),directed=TRUE)) #maximum number of shortest paths going thorugh one node
#     # netstat[t,10] <- log(diameter(net)+1) #longest shortest path
#   }
# }
# #AC$logAC[is.na(AC$logAC)]<-0
# 
# for(i in 1:ncol(netstat)){
#   plot(x=netstat$day,y=netstat[,i])
#   title(names[i])
# }
# 
# write.csv(netstat,file="data/network_stat_per_day.csv")
# saveRDS(netstat,file="data/network_stat_per_day.Rds")
