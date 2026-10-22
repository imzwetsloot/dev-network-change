################
##This code visualizes the change in model parameters at the change points for the NVN
## figure 1: bars activated / deactivated / strengthened / weakened
## figure 2: ratio of parameters before and after the change points
## mat_par_NVN.Rds has one row per NVN x VN x model, so a model is identified by (NVN, VN, model)
###############

rm(list=ls(all=TRUE))

libraries = c("tidyr","dplyr","ggplot2")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#####
Aggregation = "hour" ; Aggregationmin=60; folder="data_hour"
#Aggregation = "5minute";Aggregationmin=5; folder= "data_5min"

#setwd("F:/Reddit_project/Github")
setwd("C:/Users/izwetsl/OneDrive - UvA/Documenten/2 Research/1 Papers/44. Network change - Simon/network_change")

folder2 = "analysis/model_parameters"
folder3="figures/par_plots"

#Vuong test to use
testnames=c("lik","aic","bic")
vuongtest=testnames[1]

fCount <- function(x){
  length(which(x!=0))
}

#variables in the NVN models (each with lag 1 to 5)
varnames <- c("Xij","AC","Din_i","Dout_i","Din_j","Dout_j","ACthr","ACother","AC24")

#############################################################################
##################Create figure bars activate not activate ####
#############################################################################

# Data: one row per node pair x model x variable
df.par <- readRDS(paste0(folder2,"/",folder,"/parameters/",vuongtest,"/mat_par_NVN.Rds"))

long <- df.par %>%
  select(-start, -end, -a0) %>%              # intercept is never 0: leave out
  pivot_longer(-c(NVN, VN, model), names_to = "par", values_to = "value")

# create: each model vs the previous model, per variable
# create types of changes in paramter value - variable
trans <- long %>%
  rename(after = value) %>%
  inner_join(long %>% mutate(model = model + 1) %>% rename(before = value),
             by = c("NVN", "VN", "model", "par")) %>%
  mutate(
    cat = case_when(
      before == 0 & after == 0      ~ "Inactive",
      before == 0                   ~ "Activated",
      after  == 0                   ~ "Deactivated",
     # sign(before) != sign(after)   ~ "Sign flip",
      abs(after) > abs(before)      ~ "Strengthened",
      TRUE                          ~ "Weakened"
    ),
    par_group = sub("\\.[0-9]+$", "", par),                          # Xij.3 -> Xij
    par_group = if_else(grepl("^hr[0-9]+$", par), "hour", par_group) # hr1..24 -> hour
  )

n_trans <- trans %>% distinct(NVN, VN, model) %>% nrow()

# create df for plot
cat_levels <- c("Deactivated", "Weakened", "Activated", "Strengthened")

plot_df <- trans %>%
  count(par_group, cat) %>%
  group_by(par_group) %>%
  mutate(share = 100 * n / sum(n)) %>%
  ungroup() %>%
  filter(cat != "Inactive") %>%
  mutate(
    x   = if_else(cat %in% c("Deactivated", "Weakened"), -n, n),  # left / right
    cat = factor(cat, levels = cat_levels)
  )

#largest bar on either side, used to place the annotations
xmax <- plot_df %>%
  group_by(par_group, x > 0) %>%
  summarize(tot = sum(abs(x)), .groups = "drop") %>%
  pull(tot) %>%
  max()

# create plot  Labels and colours
# i = NVN (follower), j = VN (influencer)
grp_levels <- c(varnames, "dWLSopen", "hour")
grp_labels <- c(Xij = "Own lag (Xij)", AC = "Overall activity (AC)",
                Din_i = "In-degree NVN (Din_i)", Dout_i = "Out-degree NVN (Dout_i)",
                Din_j = "In-degree VN (Din_j)", Dout_j = "Out-degree VN (Dout_j)",
                ACthr = "Comments in VN's posts (AC_post)", ACother = "Other VNs' activity (AC_vn)",
                AC24 = "Activity 24 hours before (AC24)",
                dWLSopen = "Market open (dum_WLS)", hour = "Hour of day (hr1-hr24)")
cat_cols <- c(Deactivated = "#c94a17", Weakened = "#f5b48f",
              Activated = "#1f5fae", Strengthened = "#8db8ea")

file.fig = paste0(folder3,"/",folder,"/fig_nvn_parameters_before_after.png")
png(file.fig,width=600,height=1000)

# create plot
fig<-ggplot(plot_df, aes(x = x, y = par_group, fill = cat)) +
  geom_col(position = position_stack(reverse = TRUE),   # first level sits next to 0
           width = 0.62, colour = "white", linewidth = 0.4) +
  geom_vline(xintercept = 0, colour = "grey20") +
  annotate("text", x = -xmax * 0.03, y = length(grp_levels) + 0.75,
           label = "less important after CP", hjust = 1, size = 3.2, colour = "grey35") +
  annotate("text", x =  xmax * 0.03, y = length(grp_levels) + 0.75,
           label = "more important after CP", hjust = 0, size = 3.2, colour = "grey35") +
  scale_y_discrete(limits = rev(grp_levels), labels = grp_labels,
                   expand = expansion(add = c(0.6, 1.2))) +
  scale_x_continuous(labels = abs)  +
  scale_fill_manual(values = cat_cols, breaks = cat_levels, drop = FALSE, name = NULL) +
  labs(title = "Changes in variable importance at the change point for non-vital nodes (NVNs)",
       x = "Number of NVN-VN models with changing variable strength at the change point (aggregated by type of variable)", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top",
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        plot.title.position = "plot")
plot(fig)
dev.off()
plot(fig)

############################################################
##################Create figure Ratio paragmerter before and after CPS ####
###########################################################

#create df4: one row per node pair x model x nonzero parameter
mat.par <- readRDS(paste0(folder2,"/",folder,"/parameters/",vuongtest,"/mat_par_NVN.Rds"))

parnames <- colnames(mat.par[,6:ncol(mat.par)])
parnamesfullordered <- c("a0",
                         paste0(rep(varnames, each = 5), ".", 1:5),  # Xij.1..Xij.5, AC.1..AC.5, ...
                         "dWLSopen",
                         paste0("hr", 1:24))

zero.col=which(colSums(mat.par)==0)
if(length(zero.col)>0){
  mat.par<-mat.par[,-zero.col]
  parnames<-parnames[-(zero.col-5)]
}
mat.par[mat.par==0]=NA

df2 <-data.frame(par = parnames,n=NA,No=NA)
for(i in 1:nrow(df2)){
  df2$n[i]=fCount(mat.par[,df2$par[i]])
}
df2<-df2%>%
  mutate(No=ifelse(n>100,"n>100",ifelse(n>10,"10<n<=100",ifelse(n>5,"5<n<=10","2<n<=5"))))%>%
  mutate(No=factor(No, levels=c("2<n<=5","5<n<=10","10<n<=100","n>100")))

df3<-  mat.par %>%
    gather(par, value,-c("NVN","VN","model","start","end"))%>%
    filter(!is.na(value))
df4<- left_join(df3,df2,by="par")
df4 <- df4%>%
  mutate(par=factor(par, levels=parnamesfullordered))
#add rows to df4 with the zero col
row=df4[1,]; row[1:ncol(row)]=NA
for(i in seq_along(zero.col)){
  row$par = names(zero.col[i])
  row$n = 0
  df4<-rbind(df4,row)
}

#create grouped variable names
df4 <- df4 %>%
  mutate(
    par_group = sub("\\.[0-9]+$", "", par),                          # Xij.3 -> Xij
    par_group = if_else(grepl("^hr[0-9]+$", par_group), "hour", par_group)  # hr1..hr24 -> hour
  ) %>%
  mutate(par_group = factor(par_group,
                            levels = c("a0", varnames, "dWLSopen", "hour")))

df_prev <- df4 %>%
  transmute(NVN, VN, par, par_group, model = model + 1, value_prev = value)

df_ratio <- df4 %>%
  filter(model %in% c(2, 3, 4)) %>%
  left_join(df_prev, by = c("NVN", "VN", "par", "model", "par_group")) %>%
  mutate(
    ratio = if_else(is.na(value_prev), NA_real_, value / value_prev)  # ratio vs previous model
  )

df_ratio %>%
  filter(!is.na(ratio), is.finite(ratio)) %>%      # drop NA / Inf from zero denominators
  ggplot(aes(x = par_group, y = ratio)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +  # ratio = 1: no change
  geom_boxplot(outlier.size = 1) +
  labs(x = "Parameter", y = "Ratio (model k / model k-1)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

plot_df <- df_ratio %>%
  filter(!is.na(value), is.finite(value)) %>%
  group_by(par_group) %>%
  mutate(
    n = n(),
    No = cut(n,
             breaks = c(-Inf, 4, 10, 100, Inf),
             labels = c("n<5", "5<=n<=10", "10<n<=100", "n>100"))
  ) %>%
  ungroup()


file.fig = paste0(folder3,"/",folder,"/fig_nvn_parameters_before_after_cp.png")
png(file.fig,width=600,height=300)


fig<- plot_df %>%
  ggplot(aes(x = par_group, y = value, fill = No)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_boxplot(outlier.size = 1) +
  scale_fill_manual(values = c("steelblue1", "steelblue2", "steelblue3", "steelblue4"),
                    drop = FALSE, name = "Data points") +
  labs(x = "Parameter", y = "Ratio (model k / model k-1)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot(fig)
dev.off()
plot(fig)
