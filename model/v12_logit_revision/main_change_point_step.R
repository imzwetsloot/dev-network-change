rm(list=ls(all=TRUE))

libraries = c( "Matrix","glmnet", "MASS", "doSNOW", "foreach", "dplyr", "igraph", "gtools","tictoc")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#define functions
substrRightReplace <- function(x, n,text){
  paste0(substr(x, 1, nchar(x)-n),text)
}

Aggregation = "hour" ; Aggregationmin=60; folder="data_hour"
#Aggregation = "5minute";Aggregationmin=5; folder= "data_5min"  

#set working directory 
setwd("C:/Users/izwetsl/OneDrive - UvA/Documenten/2 Research/1 Papers/44. Network change - Simon/network_change/model/v11_logit_utc")

source('models_change_point.R')
source('single_change_point_parallel_lrt.R')
source('timeconversion.R')
source('settings.R')

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
files_todo <- list.files(paste0(folder,"/inputmatrix"))
files_done <- list.files(paste0(folder,"/result_files_main_cp"))
files_done <- substrRightReplace(files_done,6,".csv")
files<-files_todo[!(files_todo%in%files_done)]
files<-files[length(files):1]
n.files= length(files)

tic.clearlog()

print("loop over files")
pb <- txtProgressBar(max = n.files, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

#l=51206 #VN 4300
list.results = foreach(l = 1:n.files, 
                             .options.snow = opts) %dopar% {
  #print(l)
  name= strsplit(files[l],"[.]")[[1]][1]
  
  #check if file already ran, if yes skip
  dum =paste0(folder,"/result_files_main_cp/",name,".RData")
  if(file.exists(file=dum)){
    #result<-readRDS(file=dum)
    return("skipped because already done")
  }
  # #start
  # start_time <- Sys.time()
  
  X = read.csv(paste0(folder,"/inputmatrix/",files[l]))
  ts = X[,2:3]
  
  #create output.vector and input.matrix
  n=nrow(X)
  X = X[,4:ncol(X)]
  output.vector = X[,1] 
  input.matrix = as.matrix(X) 
  
  #check if enough ones
  # numb.zeros = sum(output.vector == 0)
  # numb.ones = sum(output.vector == 1)
  # if (numb.ones <= 3 || numb.zeros <= 3) {
  #   results = list(" No of 1 or 0 <=3",files[l])
  #   saveRDS(results,file=paste0(outputfolder,"/",name,".RData"))
  #   return(results) #MODIFIED
  # }
  
  ### rolling window to identify possible change points (candidates)
  window.start = seq(from= Lags*24*60/Aggregationmin,
                     to = n.period-rolling.window+step,
                     by=step)
  idx = which((window.start+rolling.window)>n.period) #remove windows that end after jun 30th
  if(length(idx)>1){   window.start = window.start[-idx]} #remove windows that end after jun 30th except the final one
  number.windows = length(window.start)
   
  ######
  #print("single change point indentification")
  #pb <- txtProgressBar(max = number.windows, style = 3)
  #progress <- function(n) setTxtProgressBar(pb, number.windows)
  #opts <- list(progress = progress)
  tic(1)
  change.point.store = foreach(current.window = window.start, 
                               .options.snow = opts) %do% {
                                 
    print(paste(current.window, "|", number.windows))
    
    regress.window.full.start = 1 + current.window
    regress.window.full.end = min(rolling.window + current.window, n.period)
    regress.window = regress.window.full.start:regress.window.full.end
    
    numb.zeros = sum(output.vector[regress.window] == 0)
    numb.ones = sum(output.vector[regress.window] == 1)
    #print(numb.ones)
    if (numb.ones <= 3 || numb.zeros <= 3) { # at least 4 obs 
      #print("in if stat")
      return(NULL)
    }
    
    #take midle 50% of window as possible change points
    change.point.sequence = regress.window
    change.point.sequence.step = seq(from=regress.window.full.start,
                                     to = regress.window.full.end,
                                     by=cpstep)
    q25 = floor(quantile(1:length(change.point.sequence.step),prob=0.25))
    q75 = ceiling(quantile(1:length(change.point.sequence.step),prob=0.75))
    change.point.sequence.step =  change.point.sequence.step[q25:q75]
    #change.point.sequence = floor(rolling.window* 0.25):ceiling(rolling.window* 0.75) + current.window

    change.point.step.res = single_change_point_lrt(
                          Lags = Lags, 
                          regress.window.full.start = regress.window.full.start, 
                          regress.window.full.end = regress.window.full.end, 
                          change.point.sequence = change.point.sequence.step, 
                          input.matrix = input.matrix,
                          output.vector = output.vector,
                          Aggregationmin = Aggregationmin)
    
    #now evaluate changepoints that we skipped around the found cp
    change.point.sequence.step.fill = seq(from = change.point.step.res[[1]]-cpstep+1,
                                          to = change.point.step.res[[1]]+cpstep-1,
                                          by=1)
    
    change.point.res = single_change_point_lrt(
                                  Lags = Lags, 
                                  regress.window.full.start = regress.window.full.start, 
                                  regress.window.full.end = regress.window.full.end, 
                                  change.point.sequence = change.point.sequence.step.fill, 
                                  input.matrix = input.matrix,
                                  output.vector = output.vector,
                                  Aggregationmin = Aggregationmin)
                                
    return(change.point.res)
  }
  toc(log = TRUE, quiet = TRUE)
  #close(pb)
  
  #print("change point unique")
  change.point.candidates = c()
  change.point.candidates = unlist(change.point.store)
  change.point.candidates = na.omit(change.point.candidates)
  change.point.unique = sort(unique(change.point.candidates))
  
  #' evaluate the possible changepoints and compare if several changepoints out
  #' of the candidates are present in the model.
  
  if(length(change.point.unique)==0){  
    
    #make results list
    data_start_end = c(window.start[1],dim(input.matrix)[1])
    results = vector(mode="list",length=5)
    results[[1]] = NA
    results[[2]] = NA
    results[[3]] = data_start_end
    results[[4]] = change.point.unique
    results[[5]] = name
    results[[6]] = tic.log(format = FALSE)
    tic.clearlog()
    
    saveRDS(results,file=paste0(folder,"/result_files_main_cp/",name,".RData"))
    return(results)
  }
 
   #continue if 1 or more changepints identified
  tic(2)
  store.models.change.point = list()
  numb.change.points = c()
  store.skip = c()
  counter = 0
  regress.window.full.start = window.start[1]
  regress.window.full.end = n.period
  
  #print(length(change.point.unique))
  #set maximum number of changepoints in whole data set
  max.cp = min(4,length(change.point.unique)) #THIS IS ARBITRARY number
  
  store.models.change.point = vector(mode="list",length=max.cp)
  #print("multiple change points")
  for (i in 1:max.cp) {
    #print(i)
    if(length(change.point.unique)==1){ #if only one change point identified
      change.point.iter.which = TRUE
      change.point.iter = matrix(change.point.unique,nrow=i,ncol=1)
    }else{
    change.point.iter.which = combn(change.point.unique, i, 
                                    FUN = function(x) !any(abs(diff(x)) <= t.between.cps))
    change.point.iter = matrix(combn(change.point.unique, i)[,change.point.iter.which], nrow = i, byrow = FALSE)}
   # REFERENCE FOR ABOVE combn(1:100, 1)[,combn(1:100, 1, FUN = function(x) !any(diff(x) <= 20))]
    
    if (dim(change.point.iter)[2]==0){next} #if no change points identified than next
    #pb <- txtProgressBar(max = dim(change.point.iter)[2], style = 3)
    
    #print(dim(change.point.iter)[2])
    
    models.change.point = foreach(j = 1:dim(change.point.iter)[2], 
                                     .options.snow = opts) %do% {    
          #    print(paste(j, "/", dim(change.point.iter)[2]))
          change.point.temp = change.point.iter[,j]
            
          models.change.point.res = models_change_point(
                                            Lags = Lags, 
                                            regress.window.full.start = regress.window.full.start, 
                                            regress.window.full.end = regress.window.full.end, 
                                            change.points = change.point.temp, 
                                            input.matrix = input.matrix,
                                            output.vector = output.vector,
                                            Aggregationmin = Aggregationmin)
          return(list(models.change.point.res[[1]],models.change.point.res[[2]],models.change.point.res[[3]],change.point.temp))
    
    }
    store.models.change.point[[i]] = models.change.point
    #close(pb)
  }
  toc(log = TRUE, quiet = TRUE)
  
  # Choice of best fitting model with AIC (at the moment)
  PenlogLiks = c()
  logLiks = c()
  cp.list =list()
  penalty = 2
  no.cp =c()
  coef.logit=list()
  coef.logit.a0=list()
  counter = 0
  for (i in 1:max.cp) {
    if (length(store.models.change.point[[i]]) != 0) {
      for (j in 1:length(store.models.change.point[[i]])) {
        counter = counter + 1
        logLiks = cbind(logLiks, Reduce("+", store.models.change.point[[i]][[j]][[1]])) #sum(unlist(store.models.change.point[[i]][[j]][[1]]))
        coef.logit[[counter]]= store.models.change.point[[i]][[j]][[2]]
        coef.logit.a0[[counter]]=store.models.change.point[[i]][[j]][[3]]
        no.cp[counter] = length(unlist(store.models.change.point[[i]][[j]][[4]]))
        cp.list[[counter]] = unlist(store.models.change.point[[i]][[j]][[4]])
        paras = sum(unlist(Reduce("rbind", coef.logit[[counter]])) != 0)
        paras.a0 = sum(unlist(Reduce("rbind", coef.logit.a0[[counter]])) != 0)
        
        PenlogLiks = cbind(PenlogLiks, -2*Reduce("+", store.models.change.point[[i]][[j]][[1]]) + penalty*(paras + paras.a0) ) # -2*sum(unlist(store.models.change.point[[i]][[j]][[1]])) + penalty*(i+1)*ncol(input.matrix)
      }
    }
  }
  
  results = vector(mode="list",length=5)
  PenlogLiks_optModel = which(PenlogLiks == min(PenlogLiks), arr.ind = TRUE)[1,]
  cps = cp.list[[PenlogLiks_optModel[2]]]
  data_start_end=c(window.start[1],cps,dim(input.matrix)[1])
  results[[1]] = cps
  results[[2]] = ts$unix[cps]
  results[[3]] = data_start_end
  results[[4]] = change.point.unique
  results[[5]] = name
  results[[6]] = tic.log(format = FALSE)
  tic.clearlog()
  
  saveRDS(results,file=paste0(folder,"/result_files_main_cp/",name,".RData"))
  
  return(results)
  }

close(pb)

if (processes > 1) {
  stopCluster(cl)
}

