### this file computes descriptive statistcs for our network
### as well as makes basic plots of our network data

rm(list=ls(all=TRUE))

libraries = c("kableExtra","tidyr","knitr","lubridate","dplyr","xts","ggplot2","scales")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#####
Aggregation = "hour" ; Aggregationmin=60; folder="data_hour"

setwd("F:/Reddit_project/Github")
#setwd("C:/Users/inez_/Documents/2 Research/1 Papers/44. Network change - Simon/Github")
source('model/v11_logit_utc/timeconversion.R')
source('model/v11_logit_utc/settings.R')

#load data
res.vn<- readRDS(paste0("data/",folder,"/results_final_VN.Rds"))
res.nvn<- readRDS(paste0("data/",folder,"/results_final_NVN.Rds"))
n = length(res.vn)
m = length(res.nvn)

#Load edgelist data data
dat<-readRDS(file="data/processed_edgelist_hour.rds")

#create data without NA rows for days without comments
dat.narm <- dat %>%
  dplyr::select(from,to,threat,comment)%>%
  drop_na()

#############################
## descriptive statistics
#############################

#number of nodes in network
from <-unique(dat.narm%>% 
                dplyr::select(from))
to <- unique(dat.narm %>%
               dplyr::select(to))
n.nodes <- length(unique(c(to$to,from$from)))

#print number of nodes
print(paste("total nodes = ", n.nodes))

#print number of comments and threats in network
print(paste("total comments and threats = ", nrow(dat.narm)))


#number of comments 
comments <- dat.narm%>%
  filter(threat != comment)
n.comments = length(unique(comments$comment))
print(paste("total comments = ", n.comments))

#number of threats in network
n.threats = nrow(dat.narm) - n.comments
threattest <- dat.narm%>%
  filter(threat == comment)
nrow(threattest)
#n.threats = length(unique(dat.narm$threat))
print(paste("total threats = ", n.threats))

#####################3
## number of x_ii and x_ij
######################

no.xii <- dat.narm%>%
  filter(from==to)
nrow(no.xii)
no.xij <- dat.narm %>%
  filter(from != to)
nrow(no.xij)

#####################3
## Table 2: number of nodes and NVN
######################
dat2<- as.data.frame(table(dat.narm$from))
colnames(dat2)<-c("node","Dout")

dat3<-dat2%>%
  filter(Dout>=1)
nrow(dat3)
dat4<-dat2%>%
  filter(Dout>=4)
nrow(dat4)

#number of activity NVN (ie number of inputmatrixes)
files <- list.files(paste0("data/",folder,"/inputmatrix"))
nfiles=length(files)
#filter out VN
files.nvn<-files[1:(nfiles-64)]
noNVN = length(files.nvn)

#number of active NVN who follow multiple VN
dum = strsplit(files.nvn,split="_")
NVN = sapply(dum, "[[", 2)
dum2<-as.data.frame(table(NVN))
dum3<-dum2%>%
  filter(Freq>1)
noNVNwithMultipleVN = nrow(dum3)
noNVNwithMultipleVN

#number of NVN who are also VN
infl.list = read.csv(file="data/influencers_list.csv")
VN<-infl.list$influencer
NVN=dum2$NVN
sum(NVN %in%VN)
NVN[NVN%in%VN]

#number of NVN for each VN

#extract pairs of follower and influencer
pair<-rbind()
for(f in files.nvn){
  e<-strsplit(f,"[_.]")
  p<-c(e[[1]][2],e[[1]][4]) #c(fol,inf, row number in fol)
  pair<-rbind(pair,p)
}

pair<-data.frame(pair)
colnames(pair)[1] <- "nvn"
colnames(pair)[2] <- "vn"

vn.table <- pair %>%
  group_by(vn)%>%
  summarize(n=n())%>%
  mutate(vn=as.numeric(vn))
View(vn.table)

infl.list<-infl.list %>%
  mutate(vn = as.numeric(influencer))

vn.tab<-full_join(vn.table,infl.list, by="vn")
vn.tab2 <- vn.tab %>%
  mutate(no.nvn = n)%>%
  mutate(no.nvn = ifelse(is.na(n),0,n))%>%
  dplyr::select(vn,n.threat,med.comments,tot.comments,n.week,no.nvn)%>%
  arrange(desc(no.nvn))
View(vn.tab2)

#get latex table 
vn.tab2 %>%
 # filter(n.threat>28)%>%
  kbl(caption="Overview VN and number of NVN",
      format="latex",
      col.names = c("Node","n.threats","med.comments","tot.comments","n.weeks","no.nvn"),
      align="l") %>%
  kable_minimal(full_width = F,  html_font = "Source Sans Pro")
