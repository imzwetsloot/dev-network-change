rm(list=ls(all=TRUE))

libraries = c( "tidyr" , "psych", "Matrix","glmnet", "MASS", "doSNOW", "foreach", "dplyr", "igraph", "gtools","tictoc", "sandwich", "CompQuadForm")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#set wd
setwd("F:/Reddit_project/Github")

#####
Aggregation = "hour" ; Aggregationmin=60; folder="data_hour"
Aggregation = "5minute";Aggregationmin=5; folder= "data_5min"  

files <- list.files(paste0("data/",folder,"/result_files_main_cp"))
n.files <-length(files)
#files<-files[length(files):1]

df = data.frame(T1=rep(NA,n.files),
                T2=rep(NA,n.files))
for(l in 1:n.files){
  res = readRDS(file= paste0("data/", folder,"/result_files_main_cp/",files[l]))                            
  if(length(res)==6){
    if(length(res[[6]])==1){
      df$T1[l] =unlist(lapply(res[[6]], function(x) x$toc - x$tic))
      
    }else{
      timings =unlist(lapply(res[[6]], function(x) x$toc - x$tic))
      df$T1[l] = timings[1]
      df$T2[l] = timings[2]
    }
  }
}

hist(df$T1)
hist(df$T2)
mean(df$T1,na.rm=TRUE)
mean(df$T2,na.rm=TRUE)

df2 <- df%>%
  gather("time", "part")

describeBy(df2$part,df2$time)

