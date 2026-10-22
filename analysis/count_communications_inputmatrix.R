################
##This code loops over all inputmatrices
## and counts the number of ones in the X_ii column (influencer files: infl_*.csv)
## or the X_ij column (follower files: fol_*_infl_*.csv)
## the result is saved as one csv file with a row per inputmatrix
###############

rm(list=ls(all=TRUE))

#load library
libraries = c("doSNOW", "foreach", "data.table")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#####
Aggregation = "hour" ; Aggregationmin=60; folder="data_hour"
#Aggregation = "5minute";Aggregationmin=5; folder= "data_5min"

#set working directory
setwd("C:/Users/izwetsl/OneDrive - UvA/Documenten/2 Research/1 Papers/44. Network change - Simon/dev_network_change/dev-network-change")

#number of cores
processes = 1

# Parallelization initializeres
if (processes > 1) {
  cl <- makeCluster(processes)
  clusterEvalQ(cl, library(data.table))
  registerDoSNOW(cl)
} else {
  registerDoSEQ()
}

files <- list.files(paste0("data/",folder,"/inputmatrix"), pattern = "\\.csv$")
n.files <- length(files)

print("loop over files")
pb <- txtProgressBar(max = n.files, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

list.results = foreach(l = 1:n.files,
                       .options.snow = opts) %dopar% {
  file = files[l]

  #influencer files have X_ii, follower files have X_ij
  if (startsWith(file, "infl_")) {
    column = "X_ii"
  } else if (startsWith(file, "fol_")) {
    column = "X_ij"
  } else {
    return(data.frame(file = file, column = NA, n.ones = NA, n.periods = NA))
  }

  #only read the column we need (fast for many files)
  X = fread(file = paste0("data/",folder,"/inputmatrix/",file), select = column)

  data.frame(file = file,
             column = column,
             n.ones = sum(X[[column]] == 1, na.rm = TRUE),
             n.periods = nrow(X))
}

close(pb)

if (processes > 1) {
  stopCluster(cl)
}

results <- rbindlist(list.results)
results <- results[order(results$n.ones), ]

write.csv(results, file = paste0("analysis/",folder,"/count_communications_inputmatrix.csv"), row.names = FALSE)
