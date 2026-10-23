#Code create CP on NVN in time after CP of Vital node

rm(list=ls(all=TRUE))

libraries = c("tidyr","knitr","lubridate","dplyr","xts","ggplot2","scales")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#setwd("F:/Reddit_project/Github")
setwd("C:/Users/inez_/Documents/2 Research/1 Papers/44. Network change - Simon/Github")
source('model/v11_logit_utc/timeconversion.R')
source('model/v11_logit_utc/settings.R')

start = strptime("2021-01-11 00:00:01",format="%Y-%m-%d %H:%M:%S",tz="utc")
end =  strptime("2021-02-13 00:00:00",format="%Y-%m-%d %H:%M:%S",tz="utc")

#load data - HOUR
Aggregation = "hour" ; Aggregationmin=60; folder="data_hour"
result.hour<- readRDS(paste0("data/",folder,"/results_final_VN.Rds"))
#load data - 5 MIN
Aggregation = "5minute";Aggregationmin=5; folder= "data_5min"  
result.5min<- readRDS(paste0("data/",folder,"/results_final_VN.Rds"))

#no influencers
n.hour=length(result.hour)
n.5min=length(result.5min)

#vuong test to use 
v=1

#data frame vn with cps
df <- data.frame(vn.idx = c(1:n),
                    vn.name = NA,
                    n.cps.hour = NA,
                    n.cps.5min =NA,
                    cp.hour=NA,
                    cp.5min=NA,
                    diff=NA)
for(i in 1:n.hour){
    #hour
    dum.h = result.hour[[i]]
    vn.name = dum.h[[11]]
    df$vn.name[i]=vn.name
    if(length(dum.h[[5]])>2){ #include only if n.cp>=1
      if(dum.h[[9]][[v]]<alpha){ #include only if significant cps
        if(dum.h[[4]][1]>as.numeric(start)){ #include only if in 5min interval
          if(dum.h[[4]][1]<as.numeric(end)){
            df$n.cps.hour[i]=length(dum.h[[3]])
            df$cp.hour[i] = dum.h[[4]][1]
          }
        }
      }
    }
    
    #5min
    dum.5m = result.5min[[i]]
    if(length(dum.5m[[5]])>2){ #include only if n.cp>=1
      if(dum.5m[[9]][[v]]<alpha){ #include only if significant cps
        df$n.cps.5min[i]=length(dum.5m[[3]])
        df$cp.5min[i] = dum.5m[[4]]
      }
    }
    #check VN same in hour and 5min
    if(vn.name!=dum.5m[[11]]){warning("something wrong")}
}
#remove NA rows
remove.idx=c()
for(i in 1:n.hour){
  if(is.na(df$n.cps.hour[i])&is.na(df$n.cps.5min[i])){remove.idx=c(remove.idx,i)}
}
df<-df[-remove.idx,]
df<-df%>%
  mutate(diff=(cp.hour-cp.5min)/60)%>%
  mutate(cp.hour.dt =fUNIXtoDT(cp.hour))%>%
  mutate(cp.5min.dt =fUNIXtoDT(cp.5min))


