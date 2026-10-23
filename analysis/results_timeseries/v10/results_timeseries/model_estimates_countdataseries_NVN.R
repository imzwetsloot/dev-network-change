rm(list=ls(all=TRUE))

libraries = c("tidyr","knitr","lubridate","dplyr","xts","ggplot2","scales","tidyverse")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

setwd('F:/Reddit_project/Github/data')

#take string minus the last n characters
substrRight <- function(x, n){
  substr(x,1, nchar(x)-n)
}

#load data
res<- readRDS("results_final_NVN.Rds")

setwd('F:/Reddit_project/Github/analysis/results_timeseries')

#hours
start = as.POSIXct("2020-06-01 00:00:00")
end = as.POSIXct("2021-05-31 23:00:00")
hours<-seq.POSIXt(start, end, by="hour")

#Vuong test to use
testnames=c("lik","aic","bic")

df.models = data.frame(NVN = rep(NA,length(res)),
                       models.lik=rep(0,length(res)),
                       models.aic=rep(0,length(res)),
                       models.bic=rep(0,length(res)))
#run for each vuong test
for(v in 1:3){
print(v)  
vuongtest=testnames[v]
alpha=0.05


##########################3
#For the NVN
##########################

#count the number of models
#dum1 <- colnames(res[[2]][[6]][[1]])[1:9]
dum <- c("Xij","AC","Din_i","Dout_i","Din_j","Dout_j","ACthr","Doutother","AC24")
parnames_nolag<-dum
intname <- "int"
parnames <- c(paste0(dum,".1"),paste0(dum,".2"),paste0(dum,".3"),paste0(dum,".4"),paste0(dum,".5"))
dummynames <- colnames(res[[2]][[6]][[1]])[46:70]
allparnames <- c(intname,parnames,dummynames)
n=length(res)
n.models=0

for(i in 1:n){
  dum=res[[i]]
  df.models$NVN[i] = dum[[11]]
  cps=dum[[3]]
  if(!is.na(dum[[3]])[1]){     #check if cps are present
    if(dum[[12]][v]<alpha){  #check if model is significant
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
npar = length(res[[1]][[6]][[1]])
mat.par <- matrix(NA,nrow=n.models,ncol=npar+5)
colnames(mat.par) <-c("NVN","model","start","end",allparnames)
k=0
for(i in 1:n){
  dum=res[[i]]
  n.mod = df.models[i,v+1]
  for(m in 1:n.mod){
    #for(j in 1:length(res[[i]][[1]])){
      k=k+1
      mat.par[k,1] = strsplit(dum[[11]],"_")[[1]][2]
      mat.par[k,2] = m
      if(n.mod==1){ #if only full model 
        mat.par[k,"start"] = dum[[9]][1]+1
        idx = length(dum[[7]]) #get last element
        mat.par[k,"end"] = dum[[9]][idx]
        mat.par[k,"int"] = as.numeric(dum[[7]][[idx]])
        mat.par[k,6:75] = as.numeric(dum[[6]][[idx]])
      } else if(n.mod>1){ #if cp model
        mat.par[k,"start"] = dum[[9]][m]+1
        mat.par[k,"end"] = dum[[9]][m+1]
        mat.par[k,"int"] = as.numeric(dum[[7]][[m]])
        mat.par[k,6:75] = as.numeric(dum[[6]][[m]])
      }
  }
}

mat.par = as.data.frame(matrix(as.numeric(mat.par),nrow=n.models))
colnames(mat.par)  <-c("NVN","model","start","end",allparnames)
View(mat.par)

## make counter for non zero
df <-data.frame(matrix(nrow = length(hours), 
                       ncol = 1+length(allparnames))) 
colnames(df)<-c("hour",allparnames)
df$hour <-hours

lims <- as.POSIXct(strptime(c("2020-11-01 00:00", "2021-05-31 23:00"), 
                            format = "%Y-%m-%d %H:%M"))

for(var in allparnames){
    print(paste0(var," is ",which(allparnames==var), "|",length(allparnames)))
    vCounter = rep(0,length(hours))
    
    for(i in 1:nrow(mat.par)){
      #print(i)
      dum=mat.par[i,]
      if(dum[var]!=0 &&!is.na(dum[var])){
        vCounter[dum$start:dum$end] = vCounter[dum$start:dum$end]+1
      }
    }
    df[,var] <- vCounter
    dum <- data.frame(period.num = c(1:length(hours)),
                      period_hour = as.POSIXct(hours),
                             count = vCounter)
    
    saveRDS(dum,paste0("parameters/",vuongtest,"/NVN/dat_",var,".Rds"))
    file.fig = paste0("parameters/",vuongtest,"/NVN/fig_",var,".png")
    png(file.fig,width=600,height=300)  
    fig<-dum %>%
      ggplot(aes(x=period_hour,y=count/n*100))+
      geom_line()+
      ylab("%NVN with nonzero par")+
      theme(axis.title.x=element_blank())+
      ggtitle(var)+
    #  ylim(min=0,max=100)+
      scale_x_datetime(labels = date_format("%y-%m"), 
                       breaks = date_breaks("2 months")) 
    plot(fig)
    dev.off() 
}
saveRDS(df,paste0("parameters/",vuongtest,"/NVN/dat_ALL_PAR.Rds"))
write.csv(df,paste0("parameters/",vuongtest,"/NVN/dat_ALL_PAR.csv"))

}


##### TO BE UPDATED ####
########
# cumulative representation of all parameters in lags as by variable
########
# 
# par_names_ind = colnames(res[[2]][[1]][[1]])[1:6]
# par_names_ind[4] = "Din"
# par_names_ind[6] = "Dout"
# ## make counter for non zero
# df2 <- setNames(data.frame(matrix(nrow = length(hours_lag24), ncol = 1+length(par_names_ind))), c("hour",par_names_ind))
# df2$hour <-hours_lag24
# for(var in par_names_ind){
#   
#   vCounter = rep(0,length(hours_lag24))
#   
#   for (var2 in c(var, parnames[grep(paste0(var, "[.]"), parnames)])) {
#     for(i in 1:nrow(mat.par)){
#       dum=mat.par[i,]
#       if(mat.par[i,var2]!=0){
#         vCounter[dum$start:dum$end] = vCounter[dum$start:dum$end]+1
#       }
#     }
#   }
#   
#   df2[,var] <- vCounter
#   dum <- data.frame (hour = as.POSIXct(hours_lag24),
#                      count = vCounter)
#   
#   file.fig = paste0("../../figures/par_plots/vital/vital_",var,"_cum.png")
#   png(file.fig,width=600,height=300)  
#   fig<-dum %>%
#     ggplot(aes(x=hour,y=count/n*100))+
#     geom_line()+
#     ylab("Percent of vital nodes with nonzero parameters")+
#     theme(axis.title.x=element_blank())+
#     ggtitle(var)+
# #    ylim(min=0,max=15)+
#     scale_x_datetime(labels = date_format("%y-%m"), 
#                      breaks = date_breaks("1 months")) 
#   plot(fig)
#   dev.off() 
# }


