rm(list=ls(all=TRUE))

libraries = c("ggridges","hrbrthemes","viridis","tidyr","knitr","lubridate","dplyr","xts","ggplot2","scales","tidyverse")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#####
Aggregation = "hour" ; Aggregationmin=60; folder="data_hour"
#Aggregation = "5minute";Aggregationmin=5; folder= "data_5min"  

#setwd("F:/Reddit_project/Github")
setwd("C:/Users/izwetsl/OneDrive - UvA/Documenten/2 Research/1 Papers/44. Network change - Simon/network_change")
source('model/v12_logit_revision/timeconversion.R')
source('model/v12_logit_revision/settings.R')

folder2 = "analysis/model_parameters"
folder3="figures/par_plots"

#load data
res<- readRDS(paste0("data/",folder,"/results_final_VN.Rds"))

#take string minus the last n characters
substrRight <- function(x, n){
  substr(x,1, nchar(x)-n)
}

#Vuong test to use
testnames=c("lik","aic","bic")

#initalize results dataframe
df.models = data.frame(VN = rep(NA,length(res)),
                       models.lik=rep(0,length(res)),
                       models.aic=rep(0,length(res)),
                       models.bic=rep(0,length(res)))
#run for each vuong test
for(v in 1:1){
  print(v)  
  vuongtest=testnames[v]
  
  ##########################3
  #For the VN
  ##########################
  
  #count the number of models
  #dum1 <- colnames(res[[2]][[6]][[1]])[1:7]
  dum <- c("Xii","AC","Din","ACthr","Dout","Doutother","AC24")
  parnames_nolag<-dum
  intname <- "a0"
  parnames <- c(paste0(dum,".1"),paste0(dum,".2"),paste0(dum,".3"),paste0(dum,".4"),paste0(dum,".5"))
  dummynames <- colnames(res[[2]][[6]][[1]])[36:60]
  allparnames <- c(intname,parnames,dummynames)
  n.res=length(res)
  n.models=0
  
  for(i in 1:n.res){
    dum=res[[i]]
    df.models$VN[i] = dum[[11]]
    cps=dum[[3]]
    if(!is.na(dum[[3]])[1]){     #check if cps are present
      if(dum[[9]][v]<alpha){  #check if model is significant
        n.models=n.models+length(cps) + 1
        df.models[i,v+1]=length(cps) + 1
      }else{ #non significant cps, just full model
        n.models=n.models+1
        df.models[i,v+1]=1
      }
    }else { #no cps present, just full model
      n.models=n.models+1
      df.models[i,v+1]=1
    }
  }
    
  #create matrix with each model on 1 row
  npar = length(allparnames)
  mat.par <- matrix(NA,nrow=n.models,ncol=npar+4)
  colnames(mat.par) <-c("inf","model","start","end",allparnames)
  k=0
  for(i in 1:n.res){
    dum=res[[i]]
    n.mod = df.models[i,v+1]
    for(m in 1:n.mod){
      #for(j in 1:length(res[[i]][[1]])){
        k=k+1
        mat.par[k,1] = strsplit(dum[[11]],"_")[[1]][2]
        mat.par[k,2] = m
        if(n.mod==1){ #if only full model 
          mat.par[k,"start"] = as.numeric(dum[[5]][1]+1)
          idx = length(dum[[5]]) #get last element
          mat.par[k,"end"] = as.numeric(dum[[5]][idx])
          mat.par[k,"a0"] = as.numeric(dum[[7]][[idx]])
          mat.par[k,6:65] = as.numeric(dum[[6]][[idx]])
        } else if(n.mod>1){ #if cp model
          mat.par[k,"start"] = as.numeric(dum[[5]][m]+1)
          mat.par[k,"end"] = as.numeric(dum[[5]][m+1])
          mat.par[k,"a0"] = as.numeric(dum[[7]][[m]])
          mat.par[k,6:65] = as.numeric(dum[[6]][[m]])
        }
    }
  }

  mat.par = as.data.frame(matrix(as.numeric(mat.par),nrow=n.models))
  colnames(mat.par)  <-c("inf","model","start","end",allparnames)
  #View(mat.par)
  saveRDS(mat.par,paste0(folder2,"/",folder,"/parameters/",vuongtest,"/mat_par_VN.Rds"))
  write.csv(mat.par,paste0(folder2,"/",folder,"/parameters/",vuongtest,"/mat_par_VN.csv"))
  
  ## make counter for non zero
  df <-data.frame(ts,matrix(nrow = n.period, 
                         ncol = 1+length(allparnames))) 
  colnames(df)<-c(colnames(ts),allparnames)
  
  #lims <- as.POSIXct(strptime(c("2020-11-01 00:00", "2021-05-31 23:00"), 
  #                            format = "%Y-%m-%d %H:%M"))
  
  for(var in allparnames){
      
      vCounter = rep(0,n.period)
      
      for(i in 1:nrow(mat.par)){
        dum=mat.par[i,]
        if(mat.par[i,var]!=0){
          vCounter[dum$start:dum$end] = vCounter[dum$start:dum$end]+1
        }
      }
      df[,var] <- vCounter
      dum <- data.frame(ts,
                        count = vCounter)
      maxy = max(15,max(dum$count/n.res*100))
      
      # saveRDS(dum,paste0(folder2,"/parameters/",vuongtest,"/VN/dat_",var,".Rds"))
      # file.fig = paste0(folder2,"/parameters/",vuongtest,"/VN/fig_",var,".png")
      # png(file.fig,width=600,height=300)  
      # fig<-dum %>%
      #   ggplot(aes(x=period_dt,y=count/n.res*100))+
      #   geom_line()+
      #   ylab("%VN with nonzero par")+
      #   theme(axis.title.x=element_blank())+
      #   ggtitle(var)+
      #   ylim(min=0,max=maxy)+
      #   scale_x_datetime(labels = date_format("%y-%m"), 
      #                    breaks = date_breaks("2 months")) 
      # plot(fig)
      # dev.off() 
  }
  saveRDS(df,  paste0(folder2,"/",folder,"/parameters/",vuongtest,"/dat_ALL_PAR_VN.Rds"))
  write.csv(df,paste0(folder2,"/",folder,"/parameters/",vuongtest,"/dat_ALL_PAR_VN.csv"))
}

########
# cumulative representation of all parameters in lags as by variable
########

df<-readRDS(paste0(folder2,"/",folder,"/parameters/",vuongtest,"/dat_ALL_PAR_VN.Rds"))

par_names = colnames(df[4:64])

#make plot for intercept
var = "a0"
dum = cbind(df[1:3],count=df$a0)
file.fig = paste0(folder3,"/",folder,"/fig_vn_",vuongtest,"_",var,".png")
png(file.fig,width=600,height=300)
fig<-dum %>%
  ggplot(aes(x=period_dt,y=count))+
  geom_line()+
  theme_ipsum() +  
  ylab("No VN with nonzero par")+
  theme(axis.title.x=element_blank())+
  ggtitle(var)+
  ylim(min=0,max=65)
plot(fig)
dev.off()
plot(fig)

#make plots of other parameters
for(p in 2:8){
  idx = seq(from=p,to=length(par_names)-25,by=7)
  par_plot = par_names[idx]
  var = strsplit(par_plot[1],split="[.]")[[1]][1]
  dum = cbind(df[1:3],df[,idx+3])
  
  file.fig = paste0(folder3,"/",folder,"/fig_vn_",vuongtest,"_",var,".png")
  png(file.fig,width=600,height=300)
  fig<-dum %>%
    dplyr::select(!c(unix,period_num))%>%
    gather("var","count",-c(period_dt))%>%
    ggplot(aes(x=period_dt,y=count, fill = var))+
    geom_area(alpha=0.6 , size=.5, colour="white")+
    scale_fill_viridis(discrete = T) +
    theme_ipsum() + 
    ylab("No VN with nonzero par")+
    theme(axis.title.x=element_blank())+
    ggtitle(var)+
    ylim(min=0,max=35)
  plot(fig)
  dev.off()
  plot(fig)
}
#make plots of dummies
#for(p in 37:61){
for(p in 37:37){
  par_plot = par_names[p]
  var = par_plot
  dum = cbind(df[1:3],df[var])
  
  file.fig = paste0(folder3,"/",folder,"/fig_vn_",vuongtest,"_",var,".png")
  png(file.fig,width=600,height=300)
  fig<-dum %>%
    dplyr::select(!c(unix,period_num))%>%
    gather("var","count",-c(period_dt))%>%
    ggplot(aes(x=period_dt,y=count))+
    geom_line()+
    scale_fill_viridis(discrete = T) +
    theme_ipsum() + 
    ylab("No VN with nonzero par")+
    theme(axis.title.x=element_blank())+
    ggtitle(var)+
    ylim(min=0,max=35)
  plot(fig)
  dev.off()
  plot(fig)
}

#make plot all hours
idx = seq(from=38, to=61)
par_plot = par_names[idx]
var = par_plot
dum = cbind(df[1:3],df[,idx+3])

file.fig = paste0(folder3,"/",folder,"/fig_vn_",vuongtest,"_hour.png")
png(file.fig,width=600,height=300)
dum2<-dum %>%
  dplyr::select(!c(unix,period_num))%>%
  gather("var","count",-c(period_dt))%>%
  mutate(var=factor(var, levels=par_plot))
#dat_label <- subset(dum2, period_dt == max(period_dt))
#dat_label <- dat_label[c(1,4,8,12,16,20,24),]

fig<-dum2%>%  
  ggplot(aes(x=period_dt,y=count, fill = var))+
  geom_area(alpha=0.6 , size=.5, colour="white")+
  scale_fill_viridis(discrete = T) +
  theme_ipsum() + 
  ylab("No VN with nonzero hour dummy")+
  theme(axis.title.x=element_blank())+
  ylim(0,35)
plot(fig)
dev.off()
plot(fig)

#############################################
#Look at parameter values in the models by quantiles and descr stat
############################################
fCount <- function(x){
  length(which(x!=0))
}

mat.par <- readRDS(paste0(folder2,"/",folder,"/parameters/",vuongtest,"/mat_par_VN.Rds"))

parnames <- colnames(mat.par[,5:ncol(mat.par)])
parnamesfull <-parnames
parnamesfullordered<-parnamesfull[c(1, #a0
                                  2,9,16,23,30, #Xii
                                  4,11,18,25,32, #Di
                                  6,13,20,27,34, #Dout
                                  7,14,21,28,35, #Dout other
                                  5,12,19,26,33, #ACthr
                                  3,10,17,24,31, #Ac
                                  8,15,22,29,36, #AC24
                                  37, #dWLSopen
                                  c(38:61))] #hours
npar =length(parnames)
n.zero.row=length(which(rowSums(mat.par[,6:65])==0))
print(paste0("numer of constant models is ",n.zero.row," out of ",n.models))

zero.col=which(colSums(mat.par)==0)
print(zero.col)
#print(paste0("paramters never used:  ",zero.col," out of ",npar))
mat.par<-mat.par[,-zero.col]
parnames<-parnames[-(zero.col-4)]
ncols <-ncol(mat.par)
mat.par[mat.par==0]=NA

### Descriptive stats
stat =c("min","q5","q10","q25","q50","q75","q90","q95","max","n","mean","std")
des.stat = matrix(NA,nrow=ncols-4,ncol=length(stat))
colnames(des.stat)<-stat
rownames(des.stat)<-parnames
#compute descriptive stats
des.stat[,"min"]=apply(mat.par[,5:ncols],MARGIN=2,FUN=min,na.rm=TRUE)
des.stat[,"q5"]=apply(mat.par[,5:ncols],MARGIN=2,FUN=quantile,probs=0.05,na.rm=TRUE)
des.stat[,"q10"]=apply(mat.par[,5:ncols],MARGIN=2,FUN=quantile,probs=0.1,na.rm=TRUE)
des.stat[,"q25"]=apply(mat.par[,5:ncols],MARGIN=2,FUN=quantile,probs=0.25,na.rm=TRUE)
des.stat[,"q50"]=apply(mat.par[,5:ncols],MARGIN=2,FUN=quantile,probs=0.5,na.rm=TRUE)
des.stat[,"q75"]=apply(mat.par[,5:ncols],MARGIN=2,FUN=quantile,probs=0.75,na.rm=TRUE)
des.stat[,"q90"]=apply(mat.par[,5:ncols],MARGIN=2,FUN=quantile,probs=0.90,na.rm=TRUE)
des.stat[,"q95"]=apply(mat.par[,5:ncols],MARGIN=2,FUN=quantile,probs=0.95,na.rm=TRUE)
des.stat[,"max"]=apply(mat.par[,5:ncols],MARGIN=2,FUN=max,na.rm=TRUE)
des.stat[,"mean"]=apply(mat.par[,5:ncols],MARGIN=2,FUN=mean,na.rm=TRUE)
des.stat[,"std"]=apply(mat.par[,5:ncols],MARGIN=2,FUN=sd,na.rm=TRUE)
des.stat[,"n"]=apply(mat.par[,5:ncols],MARGIN=2,FUN=fCount)
View(des.stat)

#saveRDS(mat.par,paste0(folder2,"/",folder,"/parameters/",vuongtest,"/descr_stat_parameters_VN.Rds"))
#write.csv(mat.par,paste0(folder2,"/",folder,"/parameters/",vuongtest,"/descr_stat_parameters_VN.csv"))

##########################################################
###############Figure historgrams staacked paramger values
#########################################################

df2 <-data.frame(par = parnames,n=NA,No=NA)
for(i in 1:nrow(df2)){
  df2$n[i]=fCount(mat.par[,df2$par[i]])
}
df2<-df2%>%
  mutate(No=ifelse(n>100,"n>100",ifelse(n>10,"10<n<=100",ifelse(n>5,"5<n<=10","2<n<=5"))))%>%
  mutate(No=factor(No, levels=c("2<n<=5","5<n<=10","10<n<=100","n>100")))

df3<-  mat.par %>%
    gather(par, value,-c("inf","model","start","end"))%>%
    filter(!is.na(value))
df4<- left_join(df3,df2,by="par")
df4 <- df4%>%
  mutate(par=factor(par, levels=parnamesfullordered))
df2<-df2%>%
  mutate(par=factor(par,levels=parnamesfullordered))
df2<-df2[order(df2$par),]
#add rows to df4 and f2 with the zero col
row2=df2[1,]; row2[1:3]=NA
row=df4[1,]; row[1:8]=NA
for(i in 1:length(zero.col)){
  row$par = names(zero.col[i])
  row$n = 0
  row2$par = names(zero.col[i])
  row2$n =0
  df4<-rbind(df4,row)
  df2<-rbind(df2,row2)
}

col <- ifelse(df2$n < 3, "grey", "black")
file.fig = paste0(folder3,"/",folder,"/fig_vn_parameters.png")
png(file.fig,width=600,height=1000)
fig<-df4%>%
    ggplot(aes(x = value, y = par, fill = No)) +
    geom_density_ridges(scale = 4) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
  #  coord_cartesian(clip = "off") +
    theme_ridges(grid = F) +
   # guides(fill = F)+
    geom_vline(xintercept=0)+
    labs(x = "Paremeter estimate", y = "") +
    guides(fill=guide_legend("No. non-zero par"))+
    theme(axis.text = element_text(size = 10))  +
    xlim(-35,35)+
    theme(axis.text.y = element_text(colour = col))+
    scale_fill_manual(values = c("steelblue1", "steelblue2", "steelblue3","steelblue4"))+
    theme(legend.position="top")
plot(fig)
dev.off()
plot(fig)

