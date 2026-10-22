rm(list=ls(all=TRUE))

libraries = c( "Matrix","glmnet", "MASS", "doSNOW", "foreach", "dplyr", "igraph", "gtools","tictoc", "sandwich", "CompQuadForm")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#####
#Aggregation = "hour" ; Aggregationmin=60; folder="data_hour"
Aggregation = "5minute";Aggregationmin=5; folder= "data_5min"  

#set wd
setwd("F:/Reddit_project/Github/model/v11_logit_utc")
#setwd("C:/Users/inez_/Documents/2 Research/1 Papers/44. Network change - Simon/Github")

source("VuongTest.R")
source('timeconversion.R')
source('settings.R')

setwd("F:/Reddit_project/Github")

#number of cores
processes = 10

# Parallelization initializeres[][]
if (processes > 1) {
  cl <- makeCluster(processes)
  clusterEvalQ(cl, library(foreach))
  clusterEvalQ(cl, library(glmnet))
  clusterEvalQ(cl, library(MASS))
  clusterEvalQ(cl, library(stats))
  clusterEvalQ(cl, library(gtools))
  clusterEvalQ(cl, library(sandwich))
  clusterEvalQ(cl, library(CompQuadForm))
  #  registerDoParallel(cl)
  registerDoSNOW(cl)
} else {
  registerDoSEQ()
}

files_todo <- list.files(paste0("data/",folder,"/result_files_final"))
files_done <- list.files(paste0("data/",folder,"/result_files_vuongtest"))
files<-files_todo[!(files_todo%in%files_done)]
n.files <-length(files)
files<-files[length(files):1]

#need to do this in parralel especially for followers (3047 models)
print("loop over files")
pb <- txtProgressBar(max = n.files, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

list.results = foreach(l = 1:n.files, 
                       .options.snow = opts) %dopar% {
  print(paste(l, "|", n.files, "files"))
                    
  res = readRDS(file= paste0("data/", folder,"/result_files_final/",files[l]))                            
  
  store.logit.glm.res = res[[6]]
  store.logit.glm.object =  res[[8]]
  data_start_end = res[[5]]
  store.names.selected.coef = res[[10]]
  name= res[[11]]
  
  if (length(data_start_end) == 2) {
    store.vuong.test = list("No changepoint present")
  } else {
    store.vuong.test = VuongTest(store.logit.glm.res[[1]], store.logit.glm.object, store.names.selected.coef)
  }
  
  res[[9]] = store.vuong.test
  
  
  saveRDS(res,file=paste0("data/",folder,"/result_files_vuongtest/",name,".RData"))
  return(res)
}

close(pb)

if (processes > 1) {
  stopCluster(cl)
}

