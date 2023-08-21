##Tea model selection

library(mgcv)
library(parallel)
library(MuMIn)
library(dplyr)

##LOAD Tea DATA from India
load("india_yield_climate_df.Rdata")

df<-india_yield_climate_df


##drop high yield and weird temperature record
df<-subset(df, Yield..Kgs.acre.<8000)
df<-subset(df, wet_meantmin<21)

##set up random factors

df$region<-as.factor(ifelse(df$Longitude > 76.8, "East", "West"))
df$elevation_grouping<-as.factor(ifelse(df$Elevation >4000, "High", "Low"))

##Set up a global model (this includes all justified predictors). It will be overfitted and have collinearity issues, but we 
##will deal with through model selection


global_tea<-gam(Yield..Kgs.acre. ~ 
                  
                   variety +
                    s(Age,k=5) +
                  
                    s(Elevation,k=5) + 
                    
                  
                  s(dry_sumrain,k=5) +
                  s(dry_meantmax,k=5) +
                  s(dry_meantmin,k=5) +
                  s(dry_meanvpd,k=5) +
                  s(dry_sumsoil,k=5) +
                  
                  
                  
                  s(wet_sumrain,k=5) +
                  s(wet_meantmax,k=5) +
                  s(wet_meantmin,k=5) +
                  s(wet_meanvpd,k=5) +
                  s(wet_sumsoil,k=5) +
                    
               
                
                   + s(region, bs="re"),
                
                
                  #family="poisson",
                  #family=nb(link="log"),
                  #family = poisson(link = "log"),
                  #family=tw(link = "log"),
                  #family=gaussian(link="identity"),
                  family=Gamma(link="log"),
                  gamma=1, data=df,na.action = "na.fail", method="REML")



##Set up a matrix do remove strongly correlated variables. Usign 0.7 at moment, but we can tweak.


dftrial_corq<-df[,c(5,18:27)]

colnames(dftrial_corq)<-paste("s(",colnames(dftrial_corq),", k = 5)",sep="")

#r > 0.7
smat <- abs(cor(dftrial_corq)) <= .7
smat[!lower.tri(smat)] <- NA




#####Dredge in parallel
#see here if can't get complete global model to fit https://stackoverflow.com/questions/35363447/mumin-dredge-when-global-mixed-effects-model-is-rank-deficient
#prepare cluster for pdredge
stopCluster(clust)
clust <- makeCluster(detectCores()-4) ##change the number of cores depending on how many your computer uses. Don't use them all!

#export data and required packages to clusters
clusterEvalQ(clust, library(mgcv))
clusterExport(clust, "df")
clusterExport(clust, "smat")


##
tea_dredge_test<- pdredge(global_tea, cluster=clust, trace=2,subset=smat,rank="AIC", 
                                        fixed=c("s(region)")) #we want only models with region, but it is misbehaving at moment..



##Best model (i.e. the one with the lowest AIC)



best_tea<-gam(Yield..Kgs.acre. ~ 
                   s(Age,k=5) +
                  # 
                  variety +
                  #s(Elevation, k=5) + 
                  # 
                  # 
                  # s(dry_sumrain,k=5) +
                #  s(dry_meantmax,k=5, by=elevation_grouping) +
                  # s(dry_meantmin,k=5) +
                  # s(dry_meanvpd,k=5) +
                  # s(dry_sumsoil,k=5) +
                  # 
                  # 
                  # 
                  # s(wet_sumrain,k=5) +
                  # s(wet_meantmax,k=5) +
                 #  s(wet_meantmin,k=5),
                 # s(wet_meanvpd,k=5),
                   s(wet_sumsoil,k=5),
                  # 
   
                  
              #   s(region, bs="re"),
                
                
                #family="poisson",
                #family=nb(link="log"),
                #family = poisson(link = "log"),
                #family=tw(link = "log"),
                family=Gamma(link="log"),
                gamma=1, data=df,na.action = "na.fail", method="REML")


summary(best_tea)
plot(best_tea,pages=1)
gam.check(best_tea)
