#3 in 1 plot showing probability of posting, cps and model estimates
rm(list=ls(all=TRUE))

libraries = c("wesanderson","gridExtra", "tidyr","knitr","lubridate","dplyr","xts","ggplot2","scales")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#####
#Aggregation = "hour" ; Aggregationmin=60; folder="data_hour"
Aggregation = "5minute";Aggregationmin=5; folder= "data_5min"  

#set working directory 
setwd("F:/Reddit_project/Github")
#setwd("C:/Users/inez_/Documents/2 Research/1 Papers/44. Network change - Simon/Github")
source('model/v11_logit_utc/timeconversion.R')
source('model/v11_logit_utc/settings.R')

#load data
results<- readRDS(paste0("data/",folder,"/results_final_VN.Rds"))

#function to select last n elements from string x
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


#no influencers
n=length(results)

#alpha for vuong test
v = 1 

#matrix with P
mP <- matrix(0,ncol=n,nrow=n.period)
colnameP <- rep(NA,n)
##Loop over vital nodes
for(VN in 1:n){
  
  resultinfl = results[[VN]]
  if(is.na(resultinfl[[3]][1])){next} #exclude nodes without change point
  inflname=resultinfl[[11]]
 # if(inflname=="infl_755"){print(VN)}
 #} ######################
  ### Plot 1: output vector with cp
  ######################
  
  cps =  resultinfl[[3]]
  
  #get outputvector
  X = read.csv(paste0("data/",folder,"/inputmatrix/",inflname,".csv"))
  X = X[,4:ncol(X)]#as we use AC_t-24, first 24 rows are not included in model
  output.vector = data.frame(X[,1])
  colnames(output.vector)<-"x"
  input.matrix = as.matrix(X)
  
  # dataframe for plotting output vector
  hours.x1 = fPeriodtoDT(which(output.vector==1),start,Aggregationmin)
  df <- data.frame(x=hours.x1,y=1)
  
  #get changepoints
  cps = fUNIXtoDT(resultinfl[[4]])
  
  #create figure
#  lims <- as.POSIXct(strptime(c(start, end), 
#                              format = "%Y-%m-%d %H:%M"))
  
  lims <- c(as.POSIXct(start),as.POSIXct(end))
  
  fig1<-df %>%
    # gather(key="Var_name",value="Var_value",-dates)%>%
    ggplot( aes( x=x, y=y)) +
    geom_point()+
    scale_x_datetime(labels = date_format("%y-%m"), 
                     breaks = date_breaks("1 months"), 
                     limits = lims) +
    # labs(x="",y="",title=paste("Influencer",inflname, cps, sep=" , "))+
    geom_vline(xintercept = cps, linetype="dashed", 
               color = "black", size=1.2)+
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  plot(fig1)
  ######################
  ### Plot 2: P[X=1] over time
  ######################
  
  nt=nrow(input.matrix)

  #make inputmatrix also with lags and AC24
  data_start_end = resultinfl[[5]]
  regress.window = (data_start_end[1]+1) : data_start_end[length(data_start_end)]
  est.window = matrix(NA, nrow = Lags, ncol = length(regress.window))
  est.window2 = matrix(NA, nrow = Lags, ncol = length(regress.window))
  
  for (i in 1:Lags) {
    est.window[i,] = regress.window - i
  }
  
  for (i in 1:Lags) { #this is to index AC_24, taking AC 24,28,72 hours etc before current modeling time
    est.window2[i,] = regress.window - (24*i*(60/Aggregationmin)) #minus -1 because AC are time lagged in the input_matrix owing to previous construction
  } #60/Aggregationmin to either take 24 lines up for hourly model or 24*12 up for 5 min model
  
  output.data = output.vector[regress.window,]
  
  nvar = ncol(input.matrix)
  ndum = 24+1 #24 hourly dummies + WallstreetDummy
  nvarlag = nvar - ndum
  
  #create input matrix with lagging some columns (but not the dummy columns)
  input.data.lagged = input.matrix[,1:nvarlag]
  input.data.dummies = input.matrix[regress.window,(nvarlag+1):nvar]
  input.data =c()
  for (k in 1:Lags) {
    input.data = cbind(input.data,
                       cbind(input.data.lagged[est.window[k,],], AC24=input.data.lagged[est.window2[k,],'AC'])
    )
  }
  lag.name.extension = c()
  for (kk in 1:Lags) {
    lag.name.extension = c(lag.name.extension, rep(paste0("_",kk), (nvarlag + 1)))
  }
  colnames(input.data) = paste0(colnames(input.data), lag.name.extension)
  input.data = cbind(input.data,input.data.dummies)
  names.all.coef = colnames(input.data)
  
  #make matrix with parameters estimates in it
  n.par = ncol(input.data)
  mPar = matrix(0,nrow=n.period,ncol=n.par+1)
  colnames(mPar) <- c("th0",names.all.coef)
  
  s = data_start_end[1] #start of first change period
  
  bCPmodel = resultinfl[[9]][v]<alpha
  
  if(bCPmodel){ #model with change points
    
    for(i in 1:(length(cps)+1)){
      s = data_start_end[i]+1
      e = data_start_end[i+1]
      
      #make th0
      th0 = resultinfl[[7]]
      mPar[s:e,"th0"] = th0[[i]][[1]]
      
      selected.par = resultinfl[[10]][[i]]
      
      for(col in selected.par){
        vTheta = resultinfl[[6]][[i]]
        idx =  which(colnames(vTheta)==col)
        mPar[s:e,col] = vTheta[idx]
      }
    }
  }else if(!bCPmodel){ #model without changepoints.
    s=data_start_end[1]
    e=data_start_end[length(data_start_end)]
    
    #make th0
    th0 = resultinfl[[7]]
    mPar[s:e,"th0"] = th0[[3]][[1]]
    
    selected.par = resultinfl[[10]][[3]]
    
    for(col in selected.par){
      vTheta = resultinfl[[6]][[3]]
      idx =  which(colnames(vTheta)==col)
      mPar[s:e,col] = vTheta[idx]
    }
  }
  dfP_Xis1 = data.frame(period=period_num,P=rep(NA,n.period),dt=rep(NA,n.period))
  dfP_Xis1$dt = fPeriodtoDT(dfP_Xis1$period, start,Aggregationmin)
  for(t in 1:length(regress.window)){
    idx = regress.window[t]
    a=as.numeric(c(1,input.data[t,]))
    b=as.numeric(mPar[idx,])
    z = sum(a*b)
    dfP_Xis1$P[idx] =1/(1+exp(-z))
  }
    
  #create figure  
  
  fig2<-dfP_Xis1 %>%
    ggplot(aes(x=dt,y=P))+
    geom_point()+
    scale_x_datetime(labels = date_format("%y-%m"), 
                     breaks = date_breaks("1 months"), 
                     limits = lims)+
    geom_vline(xintercept = cps, linetype="dashed", 
               color = "black", size=1.2)+
    ylim(0,max(dfP_Xis1$P)*1.1)
  plot(fig2)
  
  #save results in overall P matrix
  colnameP[VN]<-inflname
  mP[,VN] <- dfP_Xis1$P
  ######################
  ### Plot 2: Par estimates with cp
  ######################
  non.zero.par = which(colSums(mPar)!=0)
  hm = cbind(data.frame(mPar[,non.zero.par]),period=period_num,dt=rep(NA,n.period))
  hm$dt = fPeriodtoDT(hm$period, start,Aggregationmin)
  #head(hm)
  maxlim = ceiling(max(abs(hm[,1:(ncol(hm)-2)])))
  
  hm2<-hm %>%
    gather(key="Var_name",value="Var_value",-c(period,dt))%>%
    mutate("Var_lag"=Var_name)%>%
    separate("Var_name", c("Var", "Lag"), sep = "_") 
    #mutate(Lag =  substrRight(Var_name,1)) %>%
    #mutate("Var" = strsplit(Var_name,split="_")[[1]])
  #pal <- wes_palette("Zissou1", 100, type = "continuous")
  #b <- c(-10,-5, 0, 5,10)
  fig3<-hm2 %>%
    ggplot( aes( x=dt, y=Lag,fill=Var_value)) +
    geom_tile()+
    facet_wrap(~ Var, nrow=7, strip.position = "right", scales="free")+
    theme(panel.spacing.y = unit(0, "lines"))+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks = element_blank())+
     scale_fill_gradientn(limits = c(-maxlim,maxlim),
                         colours=c("red4", "white", "darkolivegreen"))+#,
                         #breaks=b, labels=format(b))+
    # scale_fill_gradient2(low="red4", 
     #                    mid="white", 
      #                   high="darkolivegreen", #colors in the scale
       #                  midpoint=0,
        #                 na.value="white")+
    #scale_fill_gradientn(colours = pal)+
  labs(x="",y="",title=paste("VN",inflname))+
    geom_vline(xintercept = cps, linetype="dashed", 
               color = "black", size=1.2)+
    theme_bw()
  plot(fig3)
  
  
  ######################
  ### Plots together
  ######################
  #layout<-matrix(c(1,2,3),ncol=1)
  layout<-rbind(c(1,1,1,1,1,1,1,1,NA),
                c(2,2,2,2,2,2,2,2,NA),
                c(3,3,3,3,3,3,3,3,3),
                c(3,3,3,3,3,3,3,3,3))
  file.fig = paste0("analysis/model_parameters/",folder,"/fig3in1/",inflname,".png")
  png(file.fig,width=600,height=600)  
  grid.arrange(fig1,fig2,fig3,layout_matrix=layout) 
  #plot(fig)
  dev.off()
  #plot(fig)

}
filename = paste0("analysis/model_parameters/",folder,"/P_VN.Rds")
saveRDS(mP,file=filename)



