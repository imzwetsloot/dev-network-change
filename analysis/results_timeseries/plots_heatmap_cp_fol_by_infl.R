#Code create heatmap with horizontal axis is time and 
#vertical axis is influencer 
#colour is number of NVN change points at that time period (weeks)

rm(list=ls(all=TRUE))

libraries = c("tidyr","knitr","lubridate","dplyr","xts","ggplot2","scales")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#####
#Aggregation = "hour" ; Aggregationmin=60; folder="data_hour"
Aggregation = "5minute";Aggregationmin=5; folder= "data_5min"  

#setwd("F:/Reddit_project/Github")
setwd("C:/Users/inez_/Documents/2 Research/1 Papers/44. Network change - Simon/Github")
source('model/v11_logit_utc/timeconversion.R')
source('model/v11_logit_utc/settings.R')

#load data
result<- readRDS(paste0("data/",folder,"/results_final_VN.Rds"))

#no influencers
n=length(result)

#function obtain week-year from hours
fYW <-function(timestamp){
  week = week(timestamp)
  year = year(timestamp)
  return(yw=year*100+week)
}

#vuong test to use we use 
v=1 # 1= likelihood

###########################
#make dataframe
###########################

#number of significant cps
n.cp = 0
for(i in 1:n){
  dum =  result[[i]]
  n.cps = length(dum[[5]])-2
  if(n.cps>0){
    if (dum[[9]][v]<alpha){
      n.cp = n.cp + length(dum[[3]])
    }
  }
}

#set up dataframe with each estimated model
df.cp <- data.frame(vn.idx   = rep(NA,n.cp),
                      vn.name  = NA,
                      vn.group = NA,
                      n.cps = NA,
                      cp.yw    = NA)
#set up dataframe with info of vn
df.vn <- data.frame(vn.idx = c(1:n),
                    vn.name = NA,
                    vn.group=NA,
                    n.cps = NA)
cntr=0
for(i in 1:n){
    #print(i)
    dum = result[[i]]
    infl = dum[[11]]
    df.vn$vn.name[i]=infl
    df.vn$n.cps[i]=length(dum[[3]])
    
    #get number of cps
    n.cps = length(dum[[5]])-2
    if (n.cps>0){
      if(dum[[9]][v]<alpha){ #significant cps only
        cntr = cntr+1
        df.cp$n.cps[cntr] = n.cps
        #first cp
        cp = dum[[3]][[1]]
        cp = fYW(fUNIXtoDT(dum[[4]][[1]]))
        df.cp$cp.yw[cntr]=cp
        df.cp$vn.name[cntr] = infl
        df.cp$vn.idx[cntr] = i
        
        #for other cps
        if(n.cps>1){
          for(j in 2:n.cps){
            cntr=cntr+1
            cp = dum[[3]][[j]]
            cp = fYW(fUNIXtoDT(dum[[4]][[j]]))
            df.cp$vn.name[cntr] = infl
            df.cp$vn.idx[cntr] = i
            df.cp$n.cps[cntr] = n.cps
            df.cp$cp.yw[cntr]=cp
            
          }
        }
      }
    }
}

#obtain vector with weeks
#note week 53 and week 1 need to be combined later!
weeks <- unique(fYW(fPeriodtoDT(period_num, start,Aggregationmin)))

#results NVN
res.nvn<- readRDS(paste0("data/",folder,"/results_final_NVN.Rds"))
m = length(res.nvn)

#number of cps per nvn
df.nvn = data.frame(nvn.idx = c(1:m),
                     nvn.name = NA,
                     nvn.group=NA,
                     n.cps = NA)

#set up dataframe for heatmap to count for each VN the cp's of the followers
df<-cbind(df.vn[rep(1:n, length(weeks)), ], weeks=rep(weeks, each = n))
df$cp.nvn <-0
df$weeks = as.factor(df$weeks)
df$vn.name = as.factor(df$vn.name)

#get changepoints for followers
for(i in 1:m){
  dum=res.nvn[[i]]
  nvn.name = dum[[11]]
  df.nvn$name[i] =  nvn.name
  df.nvn$n.cps[i] = length(dum[[3]])
  if(length(dum[[5]])==2){next} #skip nvn without cps
  if(dum[[9]][[v]]<alpha){ #include significant cps only
    cps = dum[[3]]
    vn = strsplit(dum[[11]],split="_")[[1]][4]
    for( j in 1:length(cps)){
      #cp.nvn = hours[cps[j]]
      cp.week = fYW(fUNIXtoDT(dum[[4]][j]))
      idx = which(df$weeks==cp.week & df$vn.name==paste0("infl_",vn))
      if(length(idx)!=1){print("SOMETHING wrong")
        print(i)}
      df$cp.nvn[idx]= df$cp.nvn[idx]+1
    }
  }
}


#order influencers based on different groups
#df.vn$vn.group = "group 0" #default is group 0 (no cp)
#dum = read.csv(file="data/influencers_by_group.csv")
#add group for each influencer in df.infl
# for(i in 1:nrow(dum)){
#   inf = substr(dum$infl[i], start = 6, stop = nchar(dum$infl[i])-4)
#   group= dum$infl.type[i]
#   idx = which(df.vn$vn.name == inf)
#   df.vn$vn.group[idx] = group
# }

#add group for each influencer in df.infl.cp
# df.cp$vn.group = "group 0" #default is group 0 (no cp)
# for(i in 1:nrow(dum)){
#   inf = substr(dum$infl[i], start = 6, stop = nchar(dum$infl[i])-4)
#   group= dum$infl.type[i]
#   idx = which(df.cp$vn.name == inf)
#   df.cp$vn.group[idx] = group
# }
# 
# #add group for each influencer in df
# df$vn.group = "group 0" #default is group 0 (no cp)
# for(i in 1:nrow(dum)){
#   inf = substr(dum$infl[i], start = 6, stop = nchar(dum$infl[i])-4)
#   group= dum$infl.type[i]
#   idx = which(df$vn.name == inf)
#   df$vn.group[idx] = group
# }

df.cp$vn.group = as.character(df.cp$n.cps)
df$vn.group = as.character(df$n.cps)
df.cp$cp.yw <-as.factor(df.cp$cp.yw)

file.fig = paste0("analysis/results_timeseries/",folder,"/heatmap_folcp_by_infl.png")
png(file.fig,width=600,height=600)  

fig<-df %>%
  ggplot( aes(x = weeks, y = vn.name, fill = log(cp.nvn+1))) +
  geom_tile() +
  geom_point(data=df.cp,aes(x=cp.yw,y=vn.name), size=1, color="black",inherit.aes=F)+
#  facet_grid(rows = vars(vn.group), scales = "free") +#facets to add gaps
  scale_fill_gradient(low = "white", high = "green")+
  scale_fill_gradient('ln(no.cp)', breaks = c(1:8),  low = "white", high = "lightskyblue4")+
  scale_x_discrete(breaks = levels(df$weeks)[c(T, rep(F, 9))])+
  xlab("")+
  ylab("Vital Node")+
  theme(axis.text.y = element_text(color = "grey20", size = 6, face = "plain"))
plot(fig)
dev.off() 
plot(fig)


