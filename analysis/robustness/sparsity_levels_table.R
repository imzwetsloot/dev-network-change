################
##This code uses the counts of communications per inputmatrix (count_communications_inputmatrix.R)
## and computes for different sparsity levels (minimum number of communications between NVN and VN)
## how many NVN-VN models would have to be estimated
## the current threshold is 4 (make_inputmatrix.R skips pairs with less than 4 communications)
## the result is saved as a csv file and a latex table
## note: n.ones counts the hours with a communication (X_ij is binary per hour), while the cut in
## make_inputmatrix.R counts communications; these are equal if a pair communicates at most once per hour
###############

rm(list=ls(all=TRUE))

#load library
libraries = c("data.table", "knitr")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#####
Aggregation = "hour" ; Aggregationmin=60; folder="data_hour"
#Aggregation = "5minute";Aggregationmin=5; folder= "data_5min"

#set working directory
setwd("C:/Users/izwetsl/OneDrive - UvA/Documenten/2 Research/1 Papers/44. Network change - Simon/network_change")

#sparsity levels: minimum number of communications in an inputmatrix
thresholds = c(4, 5, 10, 15)
threshold.current = 4

#load counts
counts <- fread(paste0("analysis/robustness/",folder,"/count_communications_inputmatrix.csv"))

#only the follower files (fol_<NVN>_infl_<VN>.csv); the VN models do not depend on the threshold
fol <- counts[column == "X_ij"]
fol[, NVN := sub("^fol_([0-9]+)_infl_([0-9]+)\\.csv$", "\\1", file)]
fol[, VN  := sub("^fol_([0-9]+)_infl_([0-9]+)\\.csv$", "\\2", file)]

n.current = nrow(fol[n.ones >= threshold.current])

#table with one row per sparsity level
tab <- rbindlist(lapply(thresholds, function(k) {
  dum = fol[n.ones >= k]
  data.table(threshold    = k,
             n.models     = nrow(dum),
             perc.models  = 100 * nrow(dum) / n.current,
             n.NVN        = uniqueN(dum$NVN),
             n.VN         = uniqueN(dum$VN),
             mean.n.ones  = mean(dum$n.ones),
             median.n.ones = median(dum$n.ones))
}))
print(tab)

#number of VN models (unchanged by the threshold), only if the influencer files are counted
n.VN.models = nrow(counts[column == "X_ii"])
print(paste0("number of VN models (independent of threshold): ", n.VN.models))

#save
write.csv(tab, file = paste0("analysis/robustness/",folder,"/sparsity_levels_n_models.csv"), row.names = FALSE)

tab.tex <- kable(tab, format = "latex", booktabs = TRUE, digits = 1,
                 col.names = c("Min. communications", "No. NVN-VN models", "\\% of current (4)",
                               "No. NVNs", "No. VNs", "Mean communications", "Median communications"),
                 escape = FALSE,
                 caption = "Number of NVN-VN models to estimate for different sparsity levels")
writeLines(tab.tex, paste0("analysis/robustness/",folder,"/sparsity_levels_n_models.tex"))
