################
##This code uses the counts of communications per inputmatrix (count_communications_inputmatrix.R)
## and computes for different sparsity levels (minimum number of communications in an inputmatrix)
## how many VN models (infl_*.csv, X_ii) and NVN-VN models (fol_*_infl_*.csv, X_ij) would have to be estimated
## current setting: NVN-VN pairs need at least 4 communications (make_inputmatrix.R),
## VN models have no minimum (the cut-off in make_inputmatrix.R is commented out), so all VN models are estimated
## the result is saved as a csv file and a latex table
## note: n.ones counts the hours with a communication (X_ii / X_ij is binary per hour), while the cut in
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

#load counts
counts <- fread(paste0("analysis/robustness/",folder,"/count_communications_inputmatrix.csv"))

#VN files (infl_<VN>.csv)
vn <- counts[column == "X_ii"]
vn[, VN := sub("^infl_([0-9]+)\\.csv$", "\\1", file)]

#NVN-VN files (fol_<NVN>_infl_<VN>.csv)
fol <- counts[column == "X_ij"]
fol[, NVN := sub("^fol_([0-9]+)_infl_([0-9]+)\\.csv$", "\\1", file)]
fol[, VN  := sub("^fol_([0-9]+)_infl_([0-9]+)\\.csv$", "\\2", file)]

#current number of models: all VN models and the NVN-VN models with at least 4 communications
n.vn.current  = nrow(vn)
n.fol.current = nrow(fol[n.ones >= 4])

#table with one row per sparsity level
tab <- rbindlist(lapply(thresholds, function(k) {
  dum.vn  = vn[n.ones >= k]
  dum.fol = fol[n.ones >= k]
  data.table(threshold      = k,
             n.VN.models    = nrow(dum.vn),
             perc.VN        = if (n.vn.current > 0) 100 * nrow(dum.vn) / n.vn.current else NA,
             n.NVN.models   = nrow(dum.fol),
             perc.NVN       = 100 * nrow(dum.fol) / n.fol.current,
             n.NVN          = uniqueN(dum.fol$NVN),
             n.VN.with.NVN  = uniqueN(dum.fol$VN),
             n.models.total = nrow(dum.vn) + nrow(dum.fol))
}))
print(tab)

#save
write.csv(tab, file = paste0("analysis/robustness/",folder,"/sparsity_levels_n_models.csv"), row.names = FALSE)

tab.tex <- kable(tab, format = "latex", booktabs = TRUE, digits = 1,
                 col.names = c("Min. communications", "No. VN models", "\\% of current",
                               "No. NVN-VN models", "\\% of current",
                               "No. NVNs", "No. VNs with NVNs", "Total no. models"),
                 escape = FALSE,
                 caption = "Number of models to estimate for different sparsity levels. Current: all VN models and NVN-VN models with at least 4 communications.")
writeLines(tab.tex, paste0("analysis/robustness/",folder,"/sparsity_levels_n_models.tex"))
