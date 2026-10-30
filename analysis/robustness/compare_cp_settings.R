################
##This code compares the candidate change points (item 4 of the result lists, change.point.unique)
## of the change point setting A (main analysis) with another setting (e.g. B)
## (robustness check change point settings, reviewer 2 minor comment 6)
## - reads the result files of both settings and matches them on file name
## - builds a table with one row per model: the change points of A and B as list columns and as strings,
##   and the number of change points in A and B
## - computes per model: identical, same number of change points, matching within +/- delta hours
##   (precision, recall, F1) and the Hausdorff distance
## the table is saved as Rds (with list columns) and csv (change points as strings), the summary as csv
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

#settings to compare
setting.compare = "B"
folder.A = paste0("data/",folder,"/result_files_main_cp")
folder.B = paste0("data/",folder,"/result_files_main_cp_",setting.compare)
delta = 48     #tolerance in hours to count two change points as the same

outputfolder = paste0("analysis/robustness/",folder)

#############################################
### functions
#############################################

#read item 4 (candidate change points) of a result file, empty vector if there are none
fReadCP <- function(file){
  res = readRDS(file)
  if(!is.list(res) || length(res) < 4){return(NULL)}   #not a regular result file
  cp = unlist(res[[4]])
  cp = sort(unique(as.numeric(cp[!is.na(cp)])))
  return(cp)
}

#share of change points in x that have a change point in y within +/- delta
fShareMatched <- function(x, y, delta){
  if(length(x) == 0){return(NA)}
  if(length(y) == 0){return(0)}
  mean(sapply(x, function(a) any(abs(a - y) <= delta)))
}

#for each change point in x the distance to the closest change point in y (empty if x or y is empty)
fDistClosest <- function(x, y){
  if(length(x) == 0 || length(y) == 0){return(numeric(0))}
  sapply(x, function(a) min(abs(a - y)))
}

#Hausdorff distance: largest distance from a change point to the nearest change point of the other setting
fHausdorff <- function(x, y){
  if(length(x) == 0 && length(y) == 0){return(0)}
  if(length(x) == 0 || length(y) == 0){return(NA)}
  d.xy = max(sapply(x, function(a) min(abs(a - y))))
  d.yx = max(sapply(y, function(b) min(abs(b - x))))
  max(d.xy, d.yx)
}

#############################################
### read the result files and match them on file name
#############################################

files.A = list.files(folder.A)
files.B = list.files(folder.B)
files = intersect(files.A, files.B)
print(paste0("files A: ", length(files.A), " | files ", setting.compare, ": ", length(files.B),
             " | in both: ", length(files)))

pb <- txtProgressBar(max = length(files), style = 3)
cp.A = vector("list", length(files))
cp.B = vector("list", length(files))
for(l in seq_along(files)){
  cp.A[[l]] = fReadCP(paste0(folder.A,"/",files[l]))
  cp.B[[l]] = fReadCP(paste0(folder.B,"/",files[l]))
  setTxtProgressBar(pb, l)
}
close(pb)

#remove files that are not regular result lists in one of the settings
valid = !sapply(cp.A, is.null) & !sapply(cp.B, is.null)
if(any(!valid)){print(paste0("not a regular result file (removed): ", paste(files[!valid], collapse=", ")))}
files = files[valid]; cp.A = cp.A[valid]; cp.B = cp.B[valid]

#############################################
### table: one row per model
#############################################

name = sub("\\.[^.]*$", "", files)
tab <- data.table(file = name,
                  type = ifelse(startsWith(name, "infl_"), "VN", "NVN"),
                  cp_A = cp.A,
                  cp_B = cp.B)
tab[, cpstr_A := sapply(cp_A, paste, collapse = ";")]
tab[, cpstr_B := sapply(cp_B, paste, collapse = ";")]
tab[, n.cp_A := sapply(cp_A, length)]
tab[, n.cp_B := sapply(cp_B, length)]

#similarity metrics
tab[, identical := cpstr_A == cpstr_B]
tab[, same.n.cp := n.cp_A == n.cp_B]
tab[, precision := mapply(fShareMatched, cp_B, cp_A, MoreArgs = list(delta = delta))]  #share of B found in A
tab[, recall    := mapply(fShareMatched, cp_A, cp_B, MoreArgs = list(delta = delta))]  #share of A found in B
tab[, F1 := ifelse(n.cp_A == 0 & n.cp_B == 0, 1,
               ifelse(n.cp_A == 0 | n.cp_B == 0, 0,
                   ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))))]
tab[, hausdorff := mapply(fHausdorff, cp_A, cp_B)]
#for each change point in B: distance (hours) to the closest change point in A
tab[, dist_B := mapply(fDistClosest, cp_B, cp_A, SIMPLIFY = FALSE)]
tab[, diststr_B := sapply(dist_B, paste, collapse = ";")]
setnames(tab, c("cp_B", "cpstr_B", "n.cp_B", "dist_B", "diststr_B"),
         paste0(c("cp_", "cpstr_", "n.cp_", "dist_", "diststr_"), setting.compare))

#############################################
### distances from each change point in B to the closest change point in A
#############################################

#long table: one row per change point in B (models without change points in A or B are not included)
has.dist = sapply(tab[[paste0("dist_", setting.compare)]], length) > 0
dist.long <- tab[has.dist, .(cp = unlist(get(paste0("cp_", setting.compare))),
                             dist = unlist(get(paste0("dist_", setting.compare)))),
                 by = .(file, type)]

fDistStat <- function(x){
  q = quantile(x, probs = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95))
  data.table(n.cp = length(x), mean = mean(x), median = median(x), sd = sd(x),
             q05 = q[1], q10 = q[2], q25 = q[3], q50 = q[4], q75 = q[5], q90 = q[6], q95 = q[7],
             max = max(x), perc.within.delta = 100 * mean(x <= delta))
}
dist.stat <- rbind(cbind(type = "all", fDistStat(dist.long$dist)),
                   dist.long[, fDistStat(dist), by = type])
dist.stat[, setting := setting.compare]
print(dist.stat)

#histogram
file.fig = paste0(outputfolder,"/fig_dist_cp_",setting.compare,"_to_A.png")
png(file.fig, width = 600, height = 400)
hist(dist.long$dist, breaks = 50, col = "steelblue3", border = "white",
     main = paste0("Distance from each change point in ",setting.compare," to the closest change point in A"),
     xlab = "Distance (hours)", ylab = "Number of change points")
abline(v = median(dist.long$dist), lty = 2)
abline(v = mean(dist.long$dist), lty = 3)
legend("topright", legend = c("median", "mean"), lty = c(2, 3), bty = "n")
dev.off()

#############################################
### summary
#############################################

fSummary <- function(d){
  d[, .(n.models          = .N,
        perc.identical    = 100 * mean(identical),
        perc.same.n.cp    = 100 * mean(same.n.cp),
        mean.n.cp_A       = mean(n.cp_A),
        mean.n.cp_compare = mean(get(paste0("n.cp_", setting.compare))),
        mean.precision    = mean(precision, na.rm = TRUE),
        mean.recall       = mean(recall, na.rm = TRUE),
        mean.F1           = mean(F1),
        median.hausdorff  = median(hausdorff, na.rm = TRUE),
        perc.hausdorff.NA = 100 * mean(is.na(hausdorff)))]
}
summary.tab <- rbind(cbind(type = "all", fSummary(tab)),
                     tab[, fSummary(.SD), by = type])
summary.tab[, `:=`(setting = setting.compare, delta = delta)]
print(summary.tab)

#############################################
### save
#############################################

saveRDS(tab, file = paste0(outputfolder,"/compare_cp_A_",setting.compare,".Rds"))
tab.csv <- tab[, !c("cp_A", paste0("cp_", setting.compare), paste0("dist_", setting.compare)), with = FALSE]
write.csv(tab.csv, file = paste0(outputfolder,"/compare_cp_A_",setting.compare,".csv"), row.names = FALSE)
write.csv(summary.tab, file = paste0(outputfolder,"/compare_cp_A_",setting.compare,"_summary.csv"), row.names = FALSE)
write.csv(dist.long, file = paste0(outputfolder,"/dist_cp_",setting.compare,"_to_A.csv"), row.names = FALSE)
write.csv(dist.stat, file = paste0(outputfolder,"/dist_cp_",setting.compare,"_to_A_summary.csv"), row.names = FALSE)
