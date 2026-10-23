################
##This code visualizes the change in model parameters at the change points for the NVN
## figure: bars activated / deactivated / strengthened / weakened
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

# % of the changes that are activations or strengthenings
# changes = all parameter transitions at a CP that are not Inactive (0 -> 0)
changes <- trans %>%
  filter(cat != "Inactive")
n_changes <- nrow(changes)
n_up <- sum(changes$cat %in% c("Activated", "Strengthened"))
print(paste0("NVN: ", n_up, " out of ", n_changes, " changes (",
             round(100 * n_up / n_changes, 1), "%) are activations or strengthenings"))

# split by type of change and by variable
print(changes %>%
        count(cat) %>%
        mutate(perc = round(100 * n / sum(n), 1)))
print(changes %>%
        group_by(par_group) %>%
        summarize(n_changes = n(),
                  perc_up = round(100 * mean(cat %in% c("Activated", "Strengthened")), 1)))

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


#############################################################################
##################Create figure boxplot change in parameter value ####
#############################################################################

# difference in parameter value at the change point: after - before
# leave out pairs that are 0 before and after (variable not selected in either model)
box_df <- trans %>%
  filter(cat != "Inactive") %>%
  mutate(diff     = after - before,
         par_group = factor(par_group, levels = grp_levels))

# number of NVN x VN x model x variable combinations per box, shown above the boxes
n_box <- box_df %>% count(par_group)

file.fig = paste0(folder3,"/",folder,"/fig_nvn_parameters_boxplot_diff.png")
png(file.fig,width=1000,height=600)

# create plot
fig2<-ggplot(box_df, aes(x = par_group, y = diff)) +
  geom_hline(yintercept = 0, colour = "grey20") +
  geom_boxplot(fill = "#8db8ea", colour = "grey20", width = 0.6,
               outlier.size = 0.8, outlier.alpha = 0.5) +
  geom_text(data = n_box, aes(x = par_group, y = Inf, label = paste0("n=", n)),
            vjust = 1.5, size = 3.2, colour = "grey35") +
  scale_x_discrete(limits = grp_levels, labels = grp_labels) +
  labs(title = "Change in parameter value at the change point for non-vital nodes (NVNs)",
       x = NULL, y = "Parameter value after CP - parameter value before CP") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        plot.title.position = "plot")
plot(fig2)
dev.off()
plot(fig2)
