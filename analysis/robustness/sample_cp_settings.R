################
##This code draws a stratified sample of inputmatrices for the change point settings robustness check
## (reviewer 2, minor comment 6): the change point analysis is re-run on this sample with settings B and C
## and compared to the change points of setting A
## - all VN models (infl_*.csv) are kept
## - NVN-VN models (fol_*_infl_*.csv) are sampled within strata: VN x activity level
##   activity level = number of communications in the inputmatrix (n.ones), binned as 4, 5-9, 10-14, 15+
##   (the same cut-offs as in sparsity_levels_table.R)
## - allocation is proportional to the stratum size, with a minimum per stratum (all models if the stratum is smaller)
## - the sampling weight (N.stratum / n.stratum) is saved to scale aggregated results back to the full network
## input: count_communications_inputmatrix.csv (run count_communications_inputmatrix.R on all files first)
###############

rm(list=ls(all=TRUE))

#load library
libraries = c("data.table")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#####
Aggregation = "hour" ; Aggregationmin=60; folder="data_hour"
#Aggregation = "5minute";Aggregationmin=5; folder= "data_5min"

#set working directory
setwd("C:/Users/izwetsl/OneDrive - UvA/Documenten/2 Research/1 Papers/44. Network change - Simon/network_change")

#sample settings
set.seed(20261023)
n.sample = 2000                     #target number of NVN-VN models in the sample
n.min.stratum = 3                   #minimum number of models per stratum
activity.breaks = c(4, 5, 10, 15, Inf)
activity.labels = c("4", "5-9", "10-14", "15+")

#load counts
counts <- fread(paste0("analysis/robustness/",folder,"/count_communications_inputmatrix.csv"))

#VN models: keep all
vn <- counts[column == "X_ii"]
vn[, VN := sub("^infl_([0-9]+)\\.csv$", "\\1", file)]
vn[, `:=`(type = "VN", NVN = NA_character_, activity = NA_character_,
          N.stratum = 1L, n.stratum = 1L, weight = 1)]

#NVN-VN models: stratify by VN x activity
fol <- counts[column == "X_ij"]
fol[, NVN := sub("^fol_([0-9]+)_infl_([0-9]+)\\.csv$", "\\1", file)]
fol[, VN  := sub("^fol_([0-9]+)_infl_([0-9]+)\\.csv$", "\\2", file)]
fol[, activity := as.character(cut(n.ones, breaks = activity.breaks, labels = activity.labels, right = FALSE))]
if (any(is.na(fol$activity))) {warning("models with less than 4 communications are not in a stratum")}
fol <- fol[!is.na(activity)]

#number of models to draw per stratum
strata <- fol[, .(N.stratum = .N), by = .(VN, activity)]
strata[, n.stratum := pmin(N.stratum, pmax(n.min.stratum, round(n.sample * N.stratum / nrow(fol))))]

#draw the sample
fol <- merge(fol, strata, by = c("VN", "activity"))
fol.sample <- fol[, .SD[sample(.N, n.stratum[1])], by = .(VN, activity)]
fol.sample[, `:=`(type = "NVN", weight = N.stratum / n.stratum)]

#combine and save
cols <- c("file", "type", "VN", "NVN", "n.ones", "activity", "N.stratum", "n.stratum", "weight")
sample.files <- rbind(vn[, ..cols], fol.sample[, ..cols])
sample.files <- sample.files[order(type, VN, activity)]

write.csv(sample.files, file = paste0("analysis/robustness/",folder,"/sample_cp_settings.csv"), row.names = FALSE)

#overview of the sample
print(paste0("VN models: ", nrow(vn), " | NVN-VN models: ", nrow(fol.sample), " of ", nrow(fol)))
overview <- fol.sample[, .(n.sample = .N), by = activity][
  fol[, .(n.population = .N), by = activity], on = "activity"]
overview[, perc.sampled := 100 * n.sample / n.population]
print(overview[order(match(activity, activity.labels))])
