#this code reads the changepoint timeseries made by cp_timeseries_by_communications.R
#and computes how many changepoints we loose as the minimum number of communications (thr) increases
#split by VN and NVN

rm(list=ls(all=TRUE))

libraries = c("tidyr","dplyr")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#####
Aggregation = "hour" ; Aggregationmin=60; folder="data_hour"
#Aggregation = "5minute";Aggregationmin=5; folder= "data_5min"

#same thresholds as in cp_timeseries_by_communications.R, first one is the baseline
thresholds = c(4,5,10,20)
types = c("VN","NVN")

#set working directory
setwd("C:/Users/izwetsl/OneDrive - UvA/Documenten/2 Research/1 Papers/44. Network change - Simon/network_change")

outputfolder = paste0("analysis/results_timeseries/",folder,"/by_communications")

#read and combine all files
df.all = data.frame()
for(type in types){
  for(thr in thresholds){
    df.cp = readRDS(paste0(outputfolder,"/",type,"_significant_cps_thr",thr,".RDS"))
    df.all = rbind(df.all, data.frame(type=type, threshold=thr, df.cp))
  }
}

#total number of changepoints per type, threshold and test
df.total <- df.all %>%
  group_by(type, threshold) %>%
  dplyr::summarize(across(c(cp.count.all, cp.count.lik, cp.count.aic, cp.count.bic), sum), .groups="drop") %>%
  gather(test, n.cps, -c(type, threshold))

#loss compared to the baseline (first threshold) and compared to the previous threshold
df.loss <- df.total %>%
  group_by(type, test) %>%
  arrange(threshold, .by_group=TRUE) %>%
  mutate(n.cps.baseline = first(n.cps),
         lost.vs.baseline = n.cps.baseline - n.cps,
         pct.lost.vs.baseline = round(100*lost.vs.baseline/n.cps.baseline, 1),
         lost.vs.previous = lag(n.cps) - n.cps,
         pct.lost.vs.previous = round(100*lost.vs.previous/lag(n.cps), 1)) %>%
  ungroup() %>%
  arrange(type, test, threshold)

#################################
####results for table: changepoints lost by threshold####
#################################
print(as.data.frame(df.loss))

#compact table: number of significant cps (lik) by threshold, VN and NVN next to each other
df.loss %>%
  filter(test=="cp.count.lik") %>%
  dplyr::select(type, threshold, n.cps, pct.lost.vs.baseline) %>%
  pivot_wider(names_from=type, values_from=c(n.cps, pct.lost.vs.baseline)) %>%
  as.data.frame()

write.csv(df.loss, file=paste0(outputfolder,"/cps_lost_by_threshold.csv"), row.names=FALSE)
