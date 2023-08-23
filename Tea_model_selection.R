#Load Packages:==========
if (!require("mgcv")) install.packages(get("mgcv"))
if (!require("parallel")) install.packages(get("parallel"))
if (!require("MuMIn")) install.packages(get("MuMIn"))
if (!require("dplyr")) install.packages(get("dplyr"))

library(mgcv)
library(parallel)
library(MuMIn)
library(dplyr)

#Add these packages to draw pretty stats output tables:
if (!require("sjPlot")) install.packages(get("sjPlot"))
if (!require("sjstats")) install.packages(get("sjstats"))
if (!require("sjmisc")) install.packages(get("sjmisc"))

library(sjPlot)
library(sjstats)
library(sjmisc)

##Load Data and run Tea model selection process========
##LOAD Tea DATA from India
load("india_yield_climate_df.Rdata")
df<-india_yield_climate_df

##drop high yield and weird temperature record
df<-subset(df, Yield..Kgs.acre.<8000)
df<-subset(df, wet_meantmin<21)

##set up random factors
df$region<-as.factor(ifelse(df$Longitude > 76.8, "East", "West"))
df$elevation_grouping<-as.factor(ifelse(df$Elevation >4000, "High", "Low"))

##Set up a global model (this includes all justified predictors).
#It will be overfitted and have collinearity issues, but we 
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



##Set up a matrix to remove strongly correlated variables. Assign 0.7 at moment (but we can tweak that).
dftrial_corq<-df[,c(5,18:27)]
colnames(dftrial_corq)<-paste("s(",colnames(dftrial_corq),", k = 5)",sep="")

#r > 0.7
smat <- abs(cor(dftrial_corq)) <= .7
smat[!lower.tri(smat)] <- NA




#####Dredge in parallel
#see here if can't get complete global model to fit https://stackoverflow.com/questions/35363447/mumin-dredge-when-global-mixed-effects-model-is-rank-deficient
#prepare cluster for dredge
stopCluster(clust)
clust <- makeCluster(detectCores()-4) ##change the number of cores depending on how many your computer uses. Don't use them all!

#export data and required packages to clusters
clusterEvalQ(clust, library(mgcv))
clusterExport(clust, "df")
clusterExport(clust, "smat")


##
tea_dredge_test<- pdredge(global_tea, cluster=clust, trace=2,subset=smat,rank="AIC", 
                                        fixed=c("s(region)")) #we want only models with region, but it is misbehaving at moment..


##Best model (i.e. the one with the lowest AIC):
best_tea<-gam(Yield..Kgs.acre. ~ 
                   s(Age,k=5) +
                  # 
                  variety3 +
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
                 #   +wet_sumsoil +
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


#Compare tea models with and without elevation:==========
best_tea1<-gam(Yield..Kgs.acre. ~ 
                  s(Age,k=5) +
                #variety3   +
                 #s(Elevation, k=5)+ 
                 s(wet_sumsoil,k=5),
                family=Gamma(link="log"),
                gamma=1, data=df,na.action = "na.fail", method="REML")

summary(best_tea1)

#best_tea_elev model:
best_tea_elev<-gam(Yield..Kgs.acre. ~ 
                  s(Elevation, k=5),
                  gamma=1, data=df,na.action = "na.fail", method="REML")

summary(best_tea_elev)


#best_tea_elev1 model:
best_tea_elev1<-gam(Yield..Kgs.acre. ~ 
                    #s(Age,k=5) +
                    #variety3+
                    Elevation+
                    s(Elevation,k=5),
                family=Gamma(link="log"),
                gamma=1, data=df,na.action = "na.fail", method="REML")

summary(best_tea_elev1)

AIC(best_tea,best_tea1,best_tea_elev,best_tea_elev1)
#                   df      AIC
#best_tea       9.044292 3013.939
#best_tea1      7.844792 3014.518
#best_tea_elev  4.803704 3045.167
#best_tea_elev1 7.125455 3001.200



#The best model TABLE===========
best_tea_elev1<-gam(Yield..Kgs.acre. ~ 
                    #s(Age,k=5) +
                   
                    #Elevation+
                    s(Elevation,k=6) + variety3,
                family=Gamma(link="log"),
                gamma=1, data=df,na.action = "na.fail", method="REML")

tab_model(best_tea_elev1, show.stat = T, digits=3, show.se = F )

#CLEAN Tea Variety:=============
Best_Tea <- df %>% group_by(variety) %>% summarise(TeaYield = mean(Yield..Kgs.acre.)) %>% arrange(desc(TeaYield))
Best_Tea #Arranged in descending by their means:
#variety   TeaYield
#1 Chinese      8000 
#2 kottathur    5600 
#3 UPASI        4242.
#4 VP           4111.
#5 chinese      3944.
#6 NOT KNOWN    3925 
#7 clone        3870.
#8 S61          3614.
#9 V.P CLONE    3200 


table(df$variety) #Frequencies of tea variety data:
#  chinese   Chinese     clone kottathur NOT KNOWN       S61     UPASI V.P CLONE        VP 
#        9         1        44         1         8        49        26        10        36 

#Fix the variety names:
df$variety2 <- ifelse(df$variety=="chinese","Chinese", df$variety) #merge 2 chinese varieties together
df$variety2 <- ifelse(df$variety2=="kottathur" |df$variety2=="NOT KNOWN","Unknown", df$variety2)  #kottathur is a village in Tamil Ladu district. Move it NOT KNOWN type of variety.
df$variety2 <- ifelse(df$variety2=="V.P CLONE" | df$variety2=="clone","VP", df$variety2) #"V.P CLONE", "clone" and "VP" look like one variety
unique(df$variety2)# "S61"     "VP"      "Chinese" "UPASI"   "Unknown"
table(df$variety2)
#Chinese     S61 Unknown   UPASI      VP 
#     10      49       9      26      90 

#The best model after we remove Unknown tea variety===========
df_known <- filter(df, variety2 !="Unknown")
dim(df_known)# 175  32

df_known$variety2 <- factor(df_known$variety2, levels = c( "S61","Chinese", "UPASI","VP"))
best_tea_elev1_known<-gam(Yield..Kgs.acre. ~ 
                    #s(Age,k=5) +
                   
                    #Elevation+
                    s(Elevation,k=6) + variety2,
                family=Gamma(link="log"),
                gamma=1, data=df_known,na.action = "na.fail", method="REML")

summary(best_tea_elev1_known)
tab_model(best_tea_elev1_known, show.stat = T, digits=3, show.se = F )

AIC(best_tea_elev1_known, best_tea_elev1)
#best_tea_elev1_known 2845.101 = Better FIT after removing the unknonw tea variety
#best_tea_elev1       3001.200


#PLOT The best model variables===========
df_known$variety2 <- factor(df_known$variety2, levels = c("Chinese", "S61","UPASI", "VP"))


ggplot(df_known, aes(x = variety2, y = Elevation*0.3048)) + #Elevation*0.3048 # = converting feet to meters
  geom_violin(fill = "lightgreen", alpha = 0.8) +
  geom_jitter(aes(color=variety2, size = Yield..Kgs.acre.), width = 0.2, alpha = 0.5) +
  scale_size_continuous(range = c(2, 8)) +
  guides(colour = "none")+
  labs( title = "",
    x = "Tea Variety",
    y = "Elevation (m)",
    size = "Yield (kg/acre): ") +
    #theme_classic() +
    theme_minimal() +
  
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = c(0.15, 0.31),
      legend.background = element_blank(),,
       legend.box = "horizontal",  # Display the legend as a box
       legend.box.background = element_rect(color = "black"))  # Add a frame around the legend


#ggsave(tea_plot, bg = "white", width = 12, height = 7, file = "TeaVarieties_Yield_Elevation_m.png") 
  


#Soil and Elevation correlation+================
summary(lm(wet_sumsoil~Elevation, data =df))
#           Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 3228.0974   113.0670  28.550  < 2e-16 ***
#Elevation      0.1306     0.0213   6.133 5.24e-09 ***
