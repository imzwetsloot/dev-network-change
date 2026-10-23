#create one file with all results for VN
#create one file wiht all results for NVN

rm(list=ls(all=TRUE))

libraries = c(  "MASS", "doSNOW", "foreach" )
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#set wd
setwd("F:/Reddit_project/Github")

#hours
start = strptime("2020-06-01 00:00:00",format="%Y-%m-%d %H:%M:%S")
end =  strptime("2021-05-31 23:00:00",format="%Y-%m-%d %H:%M:%S")
hours <- seq.POSIXt(start, end, by="hour")

files <- list.files("data/result_files_vuongtest")
n=length(files)
#files = files[n:1]

#FOR VN
resultVN=list()
counterVN=0
for (i in 1:n){
  fileType = strsplit(files[i],"_")[[1]][1]
  if(fileType=="infl"){
    counterVN=counterVN+1
    dum=readRDS(paste0("data/result_files_vuongtest/",files[i]))
    dum[[1]]=dum[[2]]=dum[[4]]=dum[[5]]=dum[[8]]=NA
    resultVN[[counterVN]]=dum
  }
}

saveRDS(resultVN,file="data/results_final_VN.Rds")


#FOR NVN

#number of processes to use
processes = 45

# Parallelization over the rolling windows
if (processes > 1) {
  cl <- makeCluster(processes)
  clusterEvalQ(cl, library(foreach))
  # clusterEvalQ(cl, library(MASS))
#  clusterEvalQ(cl, library(dplyr))
#  clusterEvalQ(cl, library(tidyr))
#  clusterEvalQ(cl, library(lubridate))
  clusterEvalQ(cl, library(stats))
  clusterEvalQ(cl, library(gtools))
  #  registerDoParallel(cl)
  registerDoSNOW(cl)
} else {
  registerDoSEQ()
}

#loop over all followers
pb <- txtProgressBar(max = n, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

resultNVN = foreach(i = 1:n, .options.snow = opts) %dopar% {
  progress(i)
  
#for (i in 1:n){
  fileType = strsplit(files[i],"_")[[1]][1]
  if (fileType=="fol"){
    #counterNVN=counterNVN+1
    dum=readRDS(paste0("data/result_files_vuongtest/",files[i]))
    dum[[1]]=dum[[2]]=dum[[4]]=dum[[5]]=dum[[8]]=NA
    #resultNVN[[counterNVN]] = dum
    return(dum)
  }
}

close(pb)

if (processes > 1) {
  stopCluster(cl)
}

#remove NULL elements (the influencers)
resultNVN=resultNVN[-(which(sapply(resultNVN,is.null),arr.ind=TRUE))]

saveRDS(resultNVN,file="data/results_final_NVN.Rds")
