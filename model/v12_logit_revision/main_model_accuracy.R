rm(list=ls(all=TRUE))

libraries = c( "Matrix","glmnet", "MASS", "doSNOW", "foreach", "dplyr", "igraph", "gtools","tictoc", "sandwich", "CompQuadForm")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#####
Aggregation = "hour" ; Aggregationmin=60; folder="data_hour"
#Aggregation = "5minute";Aggregationmin=5; folder= "data_5min"  

#set wd
#setwd("F:/Reddit_project/Github/model/v11_logit_utc")
#setwd("C:/Users/inez_/Documents/2 Research/1 Papers/44. Network change - Simon/Github")
setwd("C:/Users/izwetsl/OneDrive - UvA/Documenten/2 Research/1 Papers/44. Network change - Simon/network_change/model/v11_logit_utc")

source('timeconversion.R')
source('settings.R')

#directory for data loading from dropbox
#setwd("C:/Users/izwetsl/Dropbox/reddit")
setwd("C:/Users/izwetsl/OneDrive - UvA/Documenten/2 Research/1 Papers/44. Network change - Simon/network_change")

#number of cores
processes = 1

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

files_todo <- list.files(paste0("data/",folder,"/result_files_vuongtest"))
files_done <- list.files(paste0("data/",folder,"/result_files_modelaccuracy"))
files<-files_todo[!(files_todo%in%files_done)]
n.files <-length(files)
files<-files[length(files):1]

#need to do this in parralel especially for followers (3047 models)
print("loop over files")
pb <- txtProgressBar(max = n.files, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

df.deviance = data.frame(withcp=rep(0,n.files),
                         nocp=rep(0,n.files),
                         interceptonly = rep(0,n.files))
df.accuracy = data.frame(withcp=rep(0,n.files),
                         nocp=rep(0,n.files),
                         interceptonly = rep(0,n.files),
                         No1s=rep(0,n.files))
list.results = foreach(l = 1:n.files, 
                       .options.snow = opts) %dopar% {
  print(paste(l, "|", n.files, "files"))
                    
  res = readRDS(file= paste0("data/", folder,"/result_files_vuongtest/",files[l]))                            
  
  store.logit.glm.object =  res[[8]]
  data_start_end = res[[5]]
  name= res[[11]]
  no.cp = length(res[[5]])-2
  
  if (length(data_start_end) == 2) {
   # store.deviance = list("No changepoint present")
    store.deviance.withcp=NA
    store.deviance.nocp = store.logit.glm.object[[2]]$deviance
    store.deviance.null = store.logit.glm.object[[2]]$null.deviance
  } else {
    store.deviance.withcp=0
    for(i in 1:(no.cp+1)){
      glmres = store.logit.glm.object[[i]]
      store.deviance.withcp = store.deviance.withcp+glmres$deviance
    }
    store.deviance.nocp = store.logit.glm.object[[no.cp+2]]$deviance
    store.deviance.null = store.logit.glm.object[[no.cp+2]]$null.deviance
  }
  
  res[[12]] = store.deviance.withcp
  res[[13]] = store.deviance.nocp
  res[[14]] =  store.deviance.null
  
  df.deviance[l,1]=res[[12]]
  df.deviance[l,2]=res[[13]]
  df.deviance[l,3]=res[[14]]
  
  #prediction accuracy
  glm.object.full = store.logit.glm.object[[no.cp+2]]
  y.full = glm.object.full$y
  Index.1s = which(y.full==1)
  thres = 0.5
  
  #for null model
  logit.glm = glm(y.full ~ 1, family = 'binomial')
  Index.pred = which(logit.glm$fitted.values>thres)
  pred.1s.null = length(which(Index.pred %in% Index.1s)) #overlap of Index.1s and Index.pred
  
  #for no changepoint model
  Index.pred = which(glm.object.full$fitted.values>thres)
  pred.1s.nocps = length(which(Index.pred %in% Index.1s)) #overlap of Index.1s and Index.pred
  
  #for cp model
  if (length(data_start_end) == 2) {
    pred.1s.cps=NA
  } else {
    fitted.values =c()
    for(i in 1:(no.cp+1)){
      glm.object.cp = store.logit.glm.object[[i]]
      fitted.values = c(fitted.values,glm.object.cp$fitted.values)
    }
    Index.pred = which(fitted.values>thres)
    pred.1s.cps = length(which(Index.pred %in% Index.1s)) #overlap of Index.1s and Index.pred
    
  }
  res[[15]] = list(no.1s = sum(y.full),
                   pred.1s.null = pred.1s.null,
                   pred.1s.cps  = pred.1s.cps,
                   pred.1s.nocps = pred.1s.nocps)
  df.accuracy[l,1] =pred.1s.cps
  df.accuracy[l,2] =pred.1s.nocps
  df.accuracy[l,3] =pred.1s.null
  df.accuracy[l,4] =sum(y.full)
  
  saveRDS(res,file=paste0("data/",folder,"/result_files_modelaccuracy/",name,".RData"))
  return(res)
}

saveRDS(df.deviance,file=paste0("data/",folder,"/result_files_modelaccuracy/dfDeviance.RData"))
saveRDS(df.accuracy,file=paste0("data/",folder,"/result_files_modelaccuracy/dfAccuracy.RData"))
close(pb)

if (processes > 1) {
  stopCluster(cl)
}

