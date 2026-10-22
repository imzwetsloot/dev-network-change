rm(list=ls(all=TRUE))

libraries = c( "Matrix","glmnet", "MASS", "doSNOW", "foreach", "dplyr", "igraph", "gtools","tictoc", "sandwich", "CompQuadForm")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#set wd
###############
# Inez machine
###############
# setwd("F:/Reddit_project/Github")

###############
# Simons machine
###############
setwd("~/Github/network_change")
#setwd("C:/Users/inez_/Documents/2 Research/1 Papers/44. Network change - Simon/Github")

#number of cores
processes = 1

#####
Aggregation = "hour" ; Aggregationmin=60; folder="data_hour"
#Aggregation = "5minute";Aggregationmin=5; folder= "data_5min"  

source('model/v11_logit_utc/timeconversion.R')
source('model/v11_logit_utc/settings.R')

# Parallelization initializeres[][]
if (processes > 1) {
  cl <- makeCluster(processes)
  clusterEvalQ(cl, library(foreach))
  clusterEvalQ(cl, library(glmnet))
  clusterEvalQ(cl, library(MASS))
  clusterEvalQ(cl, library(stats))
  clusterEvalQ(cl, library(gtools))
  #  registerDoParallel(cl)
  registerDoSNOW(cl)
} else {
  registerDoSEQ()
}

files_todo <- list.files(paste0("data/",folder,"/result_files_main_cp"))
files_done <- list.files(paste0("data/",folder,"/result_files_final"))
files<-files_todo[!(files_todo%in%files_done)]
n.files <-length(files)
files<-files[length(files):1]

#need to do this in parralel especially for followers 
print("loop over files")
pb <- txtProgressBar(max = n.files, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

list.results = foreach(l = 1:n.files, 
                       .options.snow = opts) %dopar% {
  #print(paste(l, "|", n.files, "files"))
  
  res = readRDS(file= paste0("data/", folder,"/result_files_main_cp/",files[l]))                            
  
  data_start_end = res[[3]]
  name=res[[5]]
  
  X = read.csv(paste0("data/",folder,"/inputmatrix","/",name,".csv"))

  #create output.vector and input.matrix
  n=nrow(X)
  X = X[,4:ncol(X)]
  output.vector = X[,1] 
  input.matrix = as.matrix(X)
  
  #initialize storage space
  store.logit.lasso.res = list()
  store.logit.lasso.a0.res = list()
  store.logit.glm.res = list()
  store.logit.glm.a0.res = list()
  store.logit.glm.object = list()
  store.names.selected.coef = vector(mode = "list", length(data_start_end))
  
  # iter.change = length(data_start_end) : no changepoint model
  # iter.change = 1,2,3,.... : models before and after changepoints
  for (iter.change in 1:(length(data_start_end))) {
    #print(iter.change)
    # set variables for parameter estimation as well as evaluation via Log Likelihood
    # separation for no cp model and the one with cps
    if (iter.change == length(data_start_end)) {
      regress.window.start = data_start_end[1] + 1
      regress.window.end = data_start_end[length(data_start_end)]
    } else {
      regress.window.start = data_start_end[iter.change] + 1
      regress.window.end = data_start_end[iter.change + 1]
    }
    
    regress.window = regress.window.start:regress.window.end
    est.window = matrix(NA, nrow = Lags, ncol = length(regress.window))
    est.window2 = matrix(NA, nrow = Lags, ncol = length(regress.window))
    
    for (i in 1:Lags) {
      est.window[i,] = regress.window - i
    }
    
    for (i in 1:Lags) { #this is to index AC_24, taking AC 24,28,72 hours etc before current modeling time
      est.window2[i,] = regress.window - (24*i*(60/Aggregationmin)) #minus -1 because AC are time lagged in the input_matrix owing to previous construction
    } #60/Aggregationmin to either take 24 lines up for hourly model or 24*12 up for 5 min model
    
    output.data = output.vector[regress.window]
    
    nvar = ncol(input.matrix)
    ndum = 24+1 #24 hourly dummies + WallstreetDummy
    nvarlag = nvar - ndum
    store.logit.lasso = matrix(0, nrow = 1, ncol = ((nvarlag + 1) * Lags + ndum)) #+1 for AC_24
    store.logit.lasso.a0 = c()
    store.logit.glm = matrix(0, nrow = 1, ncol = ((nvarlag + 1) * Lags + ndum))#+1 for AC_24
    store.logit.glm.a0 = c()
    
    #create input matrix with lagging some columns (but not the dummy columns)
    input.data.lagged = input.matrix[,1:nvarlag]
    input.data.dummies = input.matrix[regress.window,(nvarlag+1):nvar]
    input.data =c()
    for (k in 1:Lags) {
      input.data = cbind(input.data,
                         cbind(input.data.lagged[est.window[k,],], AC24=input.data.lagged[est.window2[k,],'AC'])
      )
    }
    lag.name.extension = c()
    for (kk in 1:Lags) {
      lag.name.extension = c(lag.name.extension, rep(paste0("_",kk), (nvarlag + 1)))
    }
    colnames(input.data) = paste0(colnames(input.data), lag.name.extension)
    input.data = cbind(input.data,input.data.dummies)
    names.all.coef = colnames(input.data)
    
    ### switch to intercept only model in case number of ones/zero are 
    ### smaller/equal 3, otherwise glmnet cannot handle it
    numb.zeros = sum(output.data == 0)
    numb.ones = sum(output.data == 1)
    if (numb.ones <= 3 | numb.zeros <= 3) {
      logit.glm = glm(output.data ~ 1,
                        family = 'binomial')
      
      store.logit.lasso = NA
      store.logit.lasso.a0 = NA

      store.logit.glm.a0 = coef(logit.glm)[1]
      colnames(store.logit.glm) = names.all.coef
      
      names.selected.paras = NULL
      
    } else {
      
      # we only consider the variables which display values different from 0
      # This formulation ensures the AC are not omitted and the
      # variables with only 0 are excluded
      # We will add later 0 as parameter for the excluded variables to ensure
      # we can analyse the data properly between changepoints
      # This formulation changes nothing, a variable with no changes would be 
      # estimated as 0 anyway
      included.coef = which(colSums(input.data) != 0)
      
      #' Currently set to leave-one-out cross validation. Probably a bit too much
      #' considering the data we have is quite sparse on the 1. It should
      #' be fine to have a different setting here, like 10 or 100 or something
      #' like that folds. 
      
      folds = max(10,sum(output.data))
      set.seed(5)
      logit.lasso = cv.glmnet(x = input.data[,included.coef], y = output.data,
                              family = 'binomial', alpha = 1,
                              intercept = TRUE, type.measure = "mse",
                              nfolds = folds, 
                              trace.it = TRUE)
      
      store.logit.lasso[1,included.coef] = c(as.matrix(coef(logit.lasso, s = "lambda.min")))[-1]
      store.logit.lasso.a0 = c(as.matrix(coef(logit.lasso, s = "lambda.min")))[1]
      colnames(store.logit.lasso) = names.all.coef
      
      #which varialbes have a nonzero coefficient in the lasso estimated model
      which.paras = which(store.logit.lasso != 0)
      names.selected.paras = colnames(input.data)[which.paras]
      
      if (length(which.paras) == 0) {
        #if lasso gives only 0 estimated parameters
        logit.glm = glm(output.data ~ 1, family = 'binomial')
      } else {
        #if lasso gives nonzero estimate parameters
        logit.glm = glm(output.data ~ input.data[,which.paras],family = 'binomial')
        store.logit.glm[1,which.paras] = coef(logit.glm)[-1]
      }
      
      #store.logit.glm[1,included.coef] = store.logit.lasso[1,included.coef]
      store.logit.glm.a0 = coef(logit.glm)[1]
      colnames(store.logit.glm) = names.all.coef
    }
    
    store.logit.lasso.res[[iter.change]] = store.logit.lasso
    store.logit.lasso.a0.res[[iter.change]] = store.logit.lasso.a0
    
    store.logit.glm.res[[iter.change]] = store.logit.glm
    store.logit.glm.a0.res[[iter.change]] = store.logit.glm.a0
    
    store.logit.glm.object[[iter.change]] = logit.glm
    
    if (length(names.selected.paras)>0) {
      store.names.selected.coef[[iter.change]] = names.selected.paras
    }
  }

    results=list(store.logit.lasso.res,  #1
               store.logit.lasso.a0.res, #2
               res[[1]], #cp.list        #3    
               res[[2]], #cp.utc         #4
               res[[3]], #data_start_end #5
               store.logit.glm.res,      #6
               store.logit.glm.a0.res,   #7
               store.logit.glm.object,   #8
               NA, #for vuong test results #9
               store.names.selected.coef,#10
               res[[5]]) #name            #11
  
  
  saveRDS(results,file=paste0("data/",folder,"/result_files_final/",name,".RData"))
  return(results)
}

close(pb)

if (processes > 1) {
  stopCluster(cl)
}



