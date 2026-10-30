#this code recreates the cp_timeseries (number of changepoints per hour, split by VN and NVN)
#but only uses the models with at least a minimum number of communications (ones in X_ii / X_ij)
#robustness check for reviewer 1, comment 4 (sparsity)
#the number of communications per inputmatrix comes from analysis/count_communications_inputmatrix.R

rm(list=ls(all=TRUE))

libraries = c("knitr","tidyr","dplyr","ggplot2")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#####
Aggregation = "hour" ; Aggregationmin=60; folder="data_hour"
#Aggregation = "5minute";Aggregationmin=5; folder= "data_5min"

#minimum number of communications a model needs to be included
#4 is the baseline (inputmatrices are only made with more than 3 communications)
thresholds = c(4,5,10,15, 25, 50)

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

    saveRDS(df.cp,file=paste0(outputfolder,"/",type,"_significant_cps_thr",thr,".RDS"))
    write.csv(df.cp,file=paste0(outputfolder,"/",type,"_significant_cps_thr",thr,".csv"))

    df.all = rbind(df.all, data.frame(type=type, threshold=thr,
                                      period_dt=df.cp$period_dt,
                                      count=df.cp$cp.count.lik))
  }
}
###########################
#### compare cp sequences
##########################
#this code reads the changepoint timeseries made by cp_timeseries_by_communications.R
#and computes how many changepoints we loose as the minimum number of communications (thr) increases
#split by VN and NVN
types =c("VN" ,"NVN" )
#read and combine all files
df.all = data.frame()
for(type in types){
  for(thr in thresholds){
    df.cp = readRDS(paste0(outputfolder,"/",type,"_significant_cps_thr",thr,".RDS"))
    df.all = rbind(df.all, data.frame(type=type, threshold=thr, df.cp))
  }
}

#total number of changepoints per type, threshold and test
df.total <- df.all %>%
  group_by(type, threshold) %>%
  dplyr::summarize(across(c(cp.count.all, cp.count.lik, cp.count.aic, cp.count.bic), sum), .groups="drop") %>%
  gather(test, n.cps, -c(type, threshold)) %>%
  filter(test == "cp.count.lik")

#create latex table

tab <- df.total %>%
  dplyr::select(type, threshold, n.cps) %>%
  pivot_wider(names_from = type, values_from = n.cps) %>%   # one row per threshold, VN and NVN columns
  dplyr::select(threshold, VN, NVN)

latex.tab <- kable(tab, format = "latex", booktabs = TRUE,
                   col.names = c("Min. communications", "VN", "NVN"),
                   caption = "Number of significant change points (likelihood test) by communication threshold",
                   label = "cps_by_threshold")
latex.tab
writeLines(latex.tab, paste0(outputfolder, "/table_cps_by_threshold.tex"))


#### check on if the sequence for threshold =4 is the same as before

setwd("C:/Users/izwetsl/OneDrive - UvA/Documenten/2 Research/1 Papers/44. Network change - Simon/network_change")

new <- readRDS("analysis/results_timeseries/data_hour/by_communications/NVN_significant_cps_thr4.RDS")
old <- readRDS("analysis/results_timeseries/data_hour/NVN_significant_cps_all.RDS")

cols <- c("period_num","unix","cp.count.all","cp.count.lik","cp.count.aic","cp.count.bic")

all.equal(new[, cols], old[, cols], check.attributes = FALSE)   # TRUE if identical
colSums(new[, cols[-(1:2)]]) - colSums(old[, cols[-(1:2)]])      # difference in total cps per test (should be all 0)

#if not identical: show the hours where they differ
#diff.rows <- which(rowSums(new[, cols] != old[, cols]) > 0)
#cbind(period_dt = new$period_dt[diff.rows], new = new[diff.rows, cols[-(1:2)]], old = old[diff.rows, cols[-(1:2)]])

new <- readRDS("analysis/results_timeseries/data_hour/by_communications/VN_significant_cps_thr4.RDS")
old <- readRDS("analysis/results_timeseries/data_hour/VN_significant_cps_all.RDS")

cols <- c("period_num","unix","cp.count.all","cp.count.lik","cp.count.aic","cp.count.bic")

all.equal(new[, cols], old[, cols], check.attributes = FALSE)   # TRUE if identical
colSums(new[, cols[-(1:2)]]) - colSums(old[, cols[-(1:2)]])      # difference in total cps per test (should be all 0)
