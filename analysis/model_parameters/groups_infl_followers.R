#group VN based on model parameteres
rm(list=ls(all=TRUE))

libraries = c("tidyr","knitr","lubridate","dplyr","xts","ggplot2","scales")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

setwd("F:/Reddit_project/Github")
setwd("C:/Users/inez_/Documents/2 Research/1 Papers/44. Network change - Simon/Github")
source('model/v11_logit_utc/timeconversion.R')

#####
Aggregation = "hour" ; Aggregationmin=60; folder="data_hour"
#Aggregation = "5minute";Aggregationmin=5; folder= "data_5min"  

Lags=5

#take string minus the last n characters
substrRight <- function(x, n){
  substr(x,1, nchar(x)-n)
}

#load data
res.vn<- readRDS(paste0("data/",folder,"/results_final_VN.Rds"))
#res.nvn<- readRDS(paste0("data/",folder,"/results_final_NVN.Rds"))
n = length(res.vn)
#m = length(res.nvn)

#no influencers
n.inf.withcp = 0
for(i in 1:n){
  if(length(res.vn[[i]][[5]])>2){
    n.inf.withcp=n.inf.withcp+1}
}


####################################################################
####Three categories of influencers (see email 23 June) ####
####################################################################
infl.info = vector(mode="list",length=n.inf.withcp)
j=0
for( i in 1:n.inf){
  dum = resultinf[[i]]
  if(length(dum)>2){
    j=j+1
    infl.info[[j]][[1]] = dum[[4]] #infl name
    infl.info[[j]][[2]] = dum[[3]] # cp index
    infl.info[[j]][[3]] = hours_lag24[dum[[3]]] #cp datetime
    infl.info[[j]][[4]] = length(dum[[3]]) #n.cp
    
    #influencer groups
    #set up boleans to test
    bIntercept_is_0 = sum(abs(unlist(dum[[2]])))==0 #zero intercept
    par = unlist(dum[[1]])
    n.par = length(par)
    bXii_is_0 = sum(abs(par[seq(from=1,to=n.par,by=6)])) == 0
    bAC_is_0 =  sum(abs(par[c(seq(from=2,to=n.par,by=6),
                              seq(from=3,to=n.par,by=6))])) == 0
    bACinfl_is_0 =  sum(abs(par[c(seq(from=4,to=n.par,by=6),
                                  seq(from=5,to=n.par,by=6),
                                  seq(from=6,to=n.par,by=6))])) == 0
    #identify groups
    if( bAC_is_0 &bACinfl_is_0){
      infl.info[[j]][[5]] = "group 1"
    } else if (bACinfl_is_0){
      infl.info[[j]][[5]] = "group 2"
    } else{
      infl.info[[j]][[5]] = "group 3"}
  } 
}

#make dataframe with cp's and group of influencer
n.cp = length(unlist(sapply(infl.info, "[[", 2) ))
df.cpinfl <- data.frame(cp.index  = rep(0,n.cp),
                    cp = as.POSIXct(rep(0,n.cp), origin = "1970-01-01"),
                    infl = rep(NA,n.cp),
                    infl.type = rep(NA,n.cp))
k=0
for(i in 1:n.inf.withcp){
  for(j in 1:infl.info[[i]][[4]]){
    k=k+1
    df.cpinfl$cp.index[k]=   infl.info[[i]][[2]][[j]]
    df.cpinfl$cp[k]=         as.POSIXct(infl.info[[i]][[3]][[j]], origin = "1970-01-01")
    df.cpinfl$infl[k]=       infl.info[[i]][[1]]
    df.cpinfl$infl.type[k]=  infl.info[[i]][[5]]
  }
}
df.cpinfl$infl=as.factor(df.cpinfl$infl)
df.cpinfl$infl.type=as.factor(df.cpinfl$infl.type)
df.cpinfl <- as_tibble(df.cpinfl)
write.csv(df.cpinfl,file="data/influencers_by_group.csv")



lims <- as.POSIXct(strptime(c("2020-11-01 00:00", "2021-05-31 23:00"), 
                            format = "%Y-%m-%d %H:%M"))

### histogram of change points
file.fig = "figures/cp_categorized_by_infl_group_histo.png"
png(file.fig,width=600,height=300)  
fig<-df.cpinfl %>%
  ggplot(aes(x=cp, fill=infl.type)) +
  geom_histogram() +
  scale_x_datetime(labels = date_format("%y-%m"), 
                   breaks = date_breaks("1 months"), 
                   limits = lims) +
  labs(x="",title=paste("CPs by influencer group"))
plot(fig)
dev.off() 


#### compute mean cp per infl group
df.groupsinfl <- df.cpinfl %>%
  group_by(infl.type)%>%
  summarise(n_infl = n_distinct(infl),
            n_cp=n(),
            mean.cp.index=round(mean(cp.index)))
df.groupsinfl <- df.groupsinfl %>% mutate(mean.cp = hours_lag24[df.groupsinfl$mean.cp.index])
df.groupsinfl
write.csv(df.cpinfl,file="data/mean_cp_influencers_by_group.csv")

### fitted distr of change points with mean
gme_date = as_tibble(as.POSIXct(strptime("2021-01-28 00:00", format = "%Y-%m-%d %H:%M")))

file.fig = "figures/cp_categorized_by_infl_group_fit.png"
png(file.fig,width=600,height=300)  
fig<-df.cpinfl %>%
  ggplot(aes(x=cp, colour=infl.type, fill=infl.type)) +
  geom_density(adjust = 2,alpha = 0) +
  geom_vline(data=df.groupsinfl, aes(xintercept=mean.cp,  colour=infl.type),
             linetype="dashed", size=1)+
  geom_vline(data=gme_date,aes(xintercept=value))+
  labs(x="",title=paste("CPs by influencer type"))+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
plot(fig)
dev.off()  

###########################
#Make groups Followers  by influencer groups
###########################

#make list of influencers and groups
infl.group<-df.cpinfl %>%
              select(infl,infl.type)%>%
              mutate(infl=substrRight(as.character(infl),4))%>%
              rename(group=infl.type)%>%
              distinct()

fol.info = vector(mode="list",length=n.fol.withcp)
j=0
for( i in 1:n.fol){
  dum = resultfol[[i]]
  if(length(dum)>2){
    j=j+1
    fol.info[[j]][[1]] = dum[[4]] #fol name
    fol.info[[j]][[2]] = dum[[3]] # cp index
    fol.info[[j]][[3]] = hours_lag24[dum[[3]]] #cp datetime
    fol.info[[j]][[4]] = length(dum[[3]]) #n.cp
    fol.info[[j]][[5]] = paste("infl",substrRight(strsplit(dum[[4]],split = "_")[[1]][4],4),sep="_") #influencer
    fol.info[[j]][[6]] = infl.group$group[infl.group$infl==fol.info[[j]][[5]]]
    
    #follower groups
    #set up boleans to test
    bIntercept_is_0 = sum(abs(unlist(dum[[2]])))==0 #zero intercept
    par = unlist(dum[[1]])
    n.par = length(par)
    bXij_AC_out_is_0 =  sum(abs(par[c(seq(from=1,to=n.par,by=8), #X_ij
                                      seq(from=5,to=n.par,by=8))])) == 0 #AC_out_i
    bAC_is_0 =  sum(abs(par[c(seq(from=2,to=n.par,by=8), #AC
                              seq(from=3,to=n.par,by=8))])) == 0 #AC_24
    bACfol_is_0 =  sum(abs(par[c(seq(from=4,to=n.par,by=8))])) == 0
    bACinfl_is_0 =  sum(abs(par[c(seq(from=6,to=n.par,by=8),
                                  seq(from=7,to=n.par,by=8),
                                  seq(from=8,to=n.par,by=8))])) == 0
    #identify groups
    if(bAC_is_0 & bACfol_is_0&bACinfl_is_0){
      fol.info[[j]][[7]] = "group A"
    } else if (bACfol_is_0&bACinfl_is_0){
      fol.info[[j]][[7]] = "group B"
    } else if (bACinfl_is_0){
      fol.info[[j]][[7]] = "group C"  
    } else{
      fol.info[[j]][[7]] = "group D"}
  } 
}

#make dataframe with cp's and group of followers
n.fol.cp = length(unlist(sapply(fol.info, "[[", 2) ))
df.fol.cp <- data.frame (cp.index  = rep(0,n.fol.cp),
                     cp = as.POSIXct(rep(0,n.fol.cp), origin = "1970-01-01"),
                     fol = rep(NA,n.fol.cp),
                     fol.group = rep(NA,n.fol.cp),
                     infl.group  = rep(NA,n.fol.cp))
k=0
for(i in 1:n.fol.withcp){
  for(j in 1:fol.info[[i]][[4]]){
    k=k+1
    df.fol.cp$cp.index[k]=   fol.info[[i]][[2]][[j]]
    df.fol.cp$cp[k]=         as.POSIXct(fol.info[[i]][[3]][[j]], origin = "1970-01-01")
    df.fol.cp$fol[k]=       fol.info[[i]][[1]]
    df.fol.cp$fol.group[k]=  fol.info[[i]][[7]]
    if(length(fol.info[[i]][[6]])==1){
      df.fol.cp$infl.group[k]=  fol.info[[i]][[6]]
    } else if (length(fol.info[[i]][[6]])==0){
      df.fol.cp$infl.group[k]= "infl no cp"
    }
  }
}
df.fol.cp$fol=as.factor(df.fol.cp$fol)
df.fol.cp$fol.group = as.factor(df.fol.cp$fol.group)
df.fol.cp$infl.group = as.factor(df.fol.cp$infl.group)

####Plot 4: all change points categorized by type of follower ####
##### one plot for each influence group


write.csv(df.fol.cp,file="data/fol_by_group.csv")



lims <- as.POSIXct(strptime(c("2020-11-01 00:00", "2021-05-31 23:00"), 
                            format = "%Y-%m-%d %H:%M"))

### histogram of change points
file.fig = "figures/cp_categorized_by_fol_group_histo.png"
png(file.fig,width=600,height=300)  
fig<-df.fol.cp %>%
  #filter(infl.group=="infl no cp")%>%
  ggplot(aes(x=cp, fill=infl.group)) +
  geom_histogram() +
  scale_x_datetime(labels = date_format("%y-%m"), 
                   breaks = date_breaks("1 months"), 
                   limits = lims) +
  labs(x="",title=paste("CPs by follower group"))
plot(fig)
dev.off() 


#### compute mean cp per infl group
df.fol.groups <- df.fol.cp %>%
  group_by(infl.group)%>%
  summarise(n_fol = n_distinct(fol),
            n_cp=n(),
            mean.cp.index=round(mean(cp.index)))
df.fol.groups <- df.fol.groups %>% mutate(mean.cp = hours_lag24[df.fol.groups$mean.cp.index])
df.fol.groups

### fitted distr of change points with mean
gme_date = as_tibble(as.POSIXct(strptime("2021-01-28 00:00", format = "%Y-%m-%d %H:%M")))

file.fig = "figures/cp_categorized_by_fol_group_fit.png"
png(file.fig,width=600,height=300)  
fig<-df.fol.cp %>%
  ggplot(aes(x=cp, colour=infl.group, fill=infl.group)) +
  geom_density(adjust = 2,alpha = 0) +
  geom_vline(data=df.fol.groups, aes(xintercept=mean.cp,  colour=infl.group),
             linetype="dashed", size=1)+
  geom_vline(data=gme_date,aes(xintercept=value))+
  labs(x="",title=paste("CPs by influencer type"))+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
plot(fig)
dev.off()  



