rm(list=ls(all=TRUE))

libraries = c( "Matrix","glmnet", "MASS", "doSNOW", "foreach", "dplyr", "igraph", "gtools","tictoc")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


Aggregation = "hour" ; Aggregationmin=60; folder="data_hour"
#Aggregation = "5minute";Aggregationmin=5; folder= "data_5min"  

#set working directory 
setwd("C:/Users/izwetsl/OneDrive - UvA/Documenten/2 Research/1 Papers/44. Network change - Simon/network_change/model/v12_logit_revision")

#cp_setting 
setting="A"

source('settings_sparsity_cp_robustness.R')


#number of cores
processes = 1

#setwd with all input matrices
setwd("C:/Users/izwetsl/OneDrive - UvA/Documenten/2 Research/1 Papers/44. Network change - Simon/network_change/data")

# Parallelization over the rolling windows
if (processes > 1) {
  cl <- makeCluster(processes)
  clusterEvalQ(cl, library(foreach))
#  clusterEvalQ(cl, library(glmnet))
  clusterEvalQ(cl, library(MASS))
  clusterEvalQ(cl, library(stats))
  clusterEvalQ(cl, library(gtools))
  clusterEvalQ(cl, library(tictoc))
  #  registerDoParallel(cl)
  registerDoSNOW(cl)
} else {
  registerDoSEQ()
}

#all files to run code on
files <- list.files(paste0(folder,"/inputmatrix"))
files<-files[length(files):1]
n.files= length(files)

tic.clearlog()


print("loop over files")
pb <- txtProgressBar(max = n.files, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

outputfolder="revision_cp_sparsity"

df.result<-data.frame(
  Name  = files[1],
  total.ones = NA,
  window= NA
)

list.results = for(l in 1:n.files){
  
  output.vector = read.csv(paste0(folder,"/inputmatrix/",files[l]))
  output.vector = output.vector[,1] 
  
  ### rolling window to identify possible change points (candidates)
  window.start = seq(from= Lags*24*60/Aggregationmin,
                     to = n.period-rolling.window+step,
                     by=step)
  idx = which((window.start+rolling.window)>n.period) #remove windows that end after jun 30th
  if(length(idx)>1){   window.start = window.start[-idx]} #remove windows that end after jun 30th except the final one
  number.windows = length(window.start)
   

  for(current.window in window.start){
    
    regress.window.full.start = 1 + current.window
    regress.window.full.end = min(rolling.window + current.window, n.period)
    regress.window = regress.window.full.start:regress.window.full.end
    numb.ones = sum(output.vector[regress.window] == 1)
    df.result<-rbind(df.result, data.frame(Name=files[l],total.ones=numb.ones,window=current.window))
    
  }
}  
 
saveRDS(df.result,file=paste0(folder,"/", outputfolder,"/cpsettings_sparcity_setting",setting,".RData"))

close(pb)

if (processes > 1) {
  stopCluster(cl)
}
table(df.result$total.ones)
