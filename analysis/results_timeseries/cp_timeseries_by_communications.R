#this code recreates the cp_timeseries (number of changepoints per hour, split by VN and NVN)
#but only uses the models with at least a minimum number of communications (ones in X_ii / X_ij)
#robustness check for reviewer 1, comment 4 (sparsity)
#the number of communications per inputmatrix comes from analysis/count_communications_inputmatrix.R

rm(list=ls(all=TRUE))

libraries = c("tidyr","dplyr","ggplot2")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#####
Aggregation = "hour" ; Aggregationmin=60; folder="data_hour"
#Aggregation = "5minute";Aggregationmin=5; folder= "data_5min"

#minimum number of communications a model needs to be included
#4 is the baseline (inputmatrices are only made with more than 3 communications)
thresholds = c(4,5,10,20)

#set working directory and load files
setwd("C:/Users/izwetsl/OneDrive - UvA/Documenten/2 Research/1 Papers/44. Network change - Simon/network_change")
source('model/v12_logit_revision/timeconversion.R')
source('model/v12_logit_revision/settings.R')

outputfolder = paste0("analysis/results_timeseries/",folder,"/by_communications")
dir.create(outputfolder, showWarnings = FALSE, recursive = TRUE)

#number of communications per inputmatrix
count.comm <- read.csv(paste0("analysis/robustness/",folder,"/count_communications_inputmatrix.csv"))
count.comm$name <- sub("\\.csv$","",count.comm$file)

#function that counts the changepoints by period for a list of models
fCountCps = function(result){
  df.cp <- cbind(ts,
                 cp.count.all=0,
                 cp.count.lik=0,
                 cp.count.aic=0,
                 cp.count.bic=0)
  for( i in seq_along(result)){
    dum = result[[i]]
    cps= dum[[3]]
    if(!is.na(cps)[1]){     #check if cps are present
      for(j in 1:length(cps)){  #
        df.cp$cp.count.all[cps[j]] = df.cp$cp.count.all[cps[j]]+1
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
  return(df.cp)
}

df.models = data.frame()   #number of models (and with sign. cp) per threshold
df.all = data.frame()      #all timeseries stacked for the comparison plot

for(type in c("VN","NVN")){

  result<- readRDS(paste0("data/",folder,"/results_final_",type,".Rds"))
  names.result = sapply(result, function(x) x[[11]])
  n.comm = count.comm$n.ones[match(names.result, count.comm$name)]

  #all models need a count, otherwise rerun count_communications_inputmatrix.R on all inputmatrices
  if(any(is.na(n.comm))){
    stop(paste(sum(is.na(n.comm)),"of",length(n.comm),type,"models have no communication count"))
  }

  for(thr in thresholds){

    result.thr = result[n.comm >= thr]
    df.cp = fCountCps(result.thr)

    n.sign = sum(sapply(result.thr, function(x) !is.na(x[[3]])[1] && x[[9]][1]<alpha))
    df.models = rbind(df.models, data.frame(type=type, threshold=thr,
                                            n.models=length(result.thr),
                                            n.models.signcp.lik=n.sign))

    saveRDS(df.cp,file=paste0(outputfolder,"/",type,"_significant_cps_thr",thr,".RDS"))
    write.csv(df.cp,file=paste0(outputfolder,"/",type,"_significant_cps_thr",thr,".csv"))

    #################################
    ####Plot: significant change points (same as cp_timeseries.R)####
    #################################
    fig<-df.cp %>%
      gather(test,count,-c(period_dt,period_num,unix))%>%
      ggplot(aes(period_dt,count))+
      geom_line(size=1)+
      ylab("Number of changepoints")+
      ggtitle(paste0(type,", at least ",thr," communications (",length(result.thr)," models)"))+
      facet_wrap(~ test,nrow=4,strip.position = "right")

    file.fig = paste0(outputfolder,"/",type,"_significant_cps_thr",thr,".png")
    png(file.fig,width=600,height=600)
    plot(fig)
    dev.off()

    df.all = rbind(df.all, data.frame(type=type, threshold=thr,
                                      period_dt=df.cp$period_dt,
                                      count=df.cp$cp.count.lik))
  }
}

#################################
####results for table: number of models by threshold####
#################################
df.models
write.csv(df.models,file=paste0(outputfolder,"/n_models_by_threshold.csv"),row.names=FALSE)

#################################
####Plot: compare thresholds (significant cps, likelihood vuong test)####
#################################
fig<-df.all %>%
  mutate(threshold=factor(paste0(">= ",threshold), levels=paste0(">= ",thresholds)))%>%
  ggplot(aes(period_dt,count))+
  geom_line(size=0.5)+
  ylab("Number of significant changepoints (lik)")+
  facet_grid(threshold ~ type, scales="free_y")

file.fig = paste0(outputfolder,"/cps_by_threshold.png")
png(file.fig,width=900,height=200*length(thresholds))
plot(fig)
dev.off()
