#LOAD DATA and PACKAGES/LIBRARIES:=========
install.packages("mgcv")
install.packages("maps")
install.packages("sf")
install.packages("ggrepel")
install.packages("tidyverse")
#library(dplyr) #contained withing "tidyverse" package
#library(ggplot2)#contained withing "tidyverse" package
library(mgcv)
library(tidyverse)
library(maps)
library(sf) 
library(ggrepel)

#Load support functions that Zuur wrote
library(lattice)  #For fancy multipanel graphs
source("HighstatLibV6.R")


#LOAD Tea DATA from India
load("india_yield_climate_df.Rdata")

#Explore Tea DATA:========
##quick test gam to make sure things are working
df<-india_yield_climate_df
summary(df$Yield..Kgs.acre.)
df$district_re<-as.factor(df$District)

#Structure/Matadata:
str(df)
#data.frame':	184 obs. of  28 variables:
#$ Village                       : chr  "Kunjapanai" "Kunjapanai" "Kunjapanai" "Kunjapanai" ...
#$ District                      : chr  "The Nilgiris"
#$ Latitude                      : num  11.4 11.4 11.4 11.4 11.4 ...
#$ Longitude                     : num  76.9 76.9 76.9 76.9 76.9 ...
#$ Elevation                     : int  1058 1048 985 1060 991 1235 1483 7088 7183 7034 ...
#$ Acres                         : num  30 1 1 2 1 50 365 0.5 1 1 ...
#$ Planting.year                 : int  1995 2014 2005 1996 2000 2005 2000 1990 1995 1985 ...
#$ Age                           : int  27 8 17 26 22 17 22 32 27 37 ...
#$ variety                       : chr  "S61" "S61" "S61" "S61" ...
#$ Row.spacing..meter.           : chr  "0.9 m" "0.9 m" "0.9 m" "0.9 m" ...
#$ Plant.to.plant.spacing..meter.: chr  "0.9 m" "0.9 m" "0.9 m" "0.9 m" ...
#$ Population.acre               : int  3700 3700 3700 3700 3700 3700 3700 5500 4500 5500 ...
#$ Rainfed..irrigated            : chr  "Rainfed" "Rainfed" "Rainfed" "Rainfed" ...
#$ Irrigation.frequency          : chr  "Nil" "Nil" "Nil" "Nil" ...
#$ N.fert                        : chr  "Urea" "Urea" NA NA ...
#$ Yield..Kgs.acre.              : int  2800 3000 2500 2800 

#Questions to Jarrod:==============
#How do we define dry versus wet?
#Why 2 districts? Nilgiris  & The Nilgiris
plot(table(df$District)) #Are these categorical covariates balanced? why in GAM if the seame ditrict?
df$District1<- as.factor(as.character(("Nilgiris"))) #renamed district to one levele


#Some varieties need joining,  or? clone with other clone, ch with Chinese
plot(table(df$variety)) #Are these categorical covariates balanced? GOOD Q could be on Yield per Variety before after climate change (how shall we rank 82 years)

plot(table(df$Age),xlab = "Tea age") #Are these categorical covariates balanced?
df$Age2 <- as.factor(ifelse(df$Age < 41, "young", "old" ))
plot(table(df$Age2),xlab = "Tea age") 

#ID as in study ID? plotID, samplingTime_ID? When all these records were collected? Before or After Yield data

 #$ ID                            : chr  "76.93167_11.36357" ????????????????????????????????/
 #$ dry_sumrain                   : num  1058 1058 1058 1058 ????????????????????????????????/
 #$ dry_meantmax                  : num  31.4 31.4 31.4 31.4 ????????????????????????????????/
 #$ dry_meantmin                  : num  20.3 20.3 20.3 20.3 ????????????????????????????????/
 #$ dry_meanvpd                   : num  1.35 1.35 1.35 1.35 ????????????????????????????????/
 #$ dry_sumsoil                   : num  568 568 568 568 568 ????????????????????????????????/
 #$ wet_sumrain                   : num  3313 3313 3313 3313 ????????????????????????????????/
 #$ wet_meantmax                  : num  28.6 28.6 28.6 28.6 2????????????????????????????????/
 #$ wet_meantmin                  : num  20.1 20.1 20.1 20.1 ????????????????????????????????/
 #$ wet_meanvpd                   : num  0.781 0.781 0.781 0.????????????????????????????????/.
 #$ wet_sumsoil                   : num  1461 1461 1461 1461 ????????????????????????????????/
 #$ district_re                   : Factor w/ 2 levels "Nilgiris ","The Nilgiris" ??????????? one level?



#A Outliers=================
#Is there an outlier in the spatial sampling positions?
xyplot(Latitude ~ Longitude, 
       aspect = "iso", 
       data = df,
       col = 1,
       pch = 16)

#REMOVE SPATIAL OUTLIER:
df2 <- df[-which.min(df$Longitude), ] #Remove that one spatial outlier, lowest Longitute
df2$Elevation2 <- ifelse(df2$Elevation < 3715.5, "Low","High") #Rank Elevation
unique(df2$Elevation2 )
range(df$Elevation)#985 7431
7431/2 #3715.5 in the middle


yield_Location <-   ggplot(df2,aes(x= Longitude, y= Latitude)) +
  geom_point(aes(shape=Elevation2,color = Age2, size = Yield..Kgs.acre.))+
   scale_shape_manual(values=c(17,16))+
   scale_color_manual(values=c("#CC6666", "#66CC99"))+
  
  ggtitle("Tea study")+
             
  theme_classic() +
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "right",
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        strip.background =  element_rect(fill = "white"))


yield_Location #ExploreTEA_YieldElevationCoordinatesAge_FIGURE


#ExploreTEA_Collinearity_FIGURE:===============
MyVar <- c("Yield..Kgs.acre.", "Latitude", "Longitude", "Elevation")
pairs(df[,MyVar], 
      lower.panel = panel.cor) #ExploreTEA_Collinearity_LatLonElevation_FIGURE

MyVar <- c("Yield..Kgs.acre.","dry_meanvpd", "dry_sumrain", "wet_sumsoil", "wet_sumrain")
pairs(df[,MyVar], 
      lower.panel = panel.cor) #ExploreTEA_Collinearity_YieldRainSoil_FIGURE

summary(lm(Yield..Kgs.acre.~dry_sumrain*dry_meanvpd, data = df2))#NO EFFECT
summary(lm(Yield..Kgs.acre.~dry_sumrain, data = df2))#NO EFFECT
summary(lm(Yield..Kgs.acre.~wet_meantmax*dry_sumrain, data = df2))#NO EFFECT


par(mfrow = c(1, 2))
boxplot(Yield..Kgs.acre. ~ Age2, data = df)
boxplot(Yield..Kgs.acre. ~ Elevation2, data = df) #ExploreTEA_BOXPLOT_AgeElevation
summary(lm(Yield..Kgs.acre.~Elevation2*Age2, data = df2))#NO EFFECT



#ExploreTEA_PLOT_Varieties=========
#Spatial/temporal aspects of sampling:
xyplot( Latitude ~ Longitude | factor(variety), ##ExploreTEA_PLOT_Varieties
       aspect = "iso", 
       data = df,
       col = 1,
       pch = 16)



##ExploreTEA_PLOT_YieldSummerRainElevation======
coplot(Yield..Kgs.acre. ~ dry_sumrain| factor(Elevation2), data=df2)

xyplot(Yield..Kgs.acre. ~ dry_sumrain | factor(Elevation2), #ExploreTEA_PLOT_YieldSummerRainElevation
   data = df2[df2$Age<30,], 
   strip = function(bg = 'white', ...) 
   strip.default(bg = 'white', ...),
   scales = list(alternating = T, 
                x = list(relation = "free"),
                y = list(relation = "same")),
   panel=function(x,y){
    panel.grid(h=-1, v= 2)
    panel.points(x, y, col = "red")
    panel.loess(x,y,col=2,lwd=2) #Add smoother
    panel.abline(lm(y~x))        #Add regression line
    })


#All Data lm:
summary(lm(Elevation~dry_sumrain, data = df))# YES Effect
summary(lm(Yield..Kgs.acre.~Elevation, data = df))#YES effect

#Young Data where Age<50
summary(lm(Elevation~dry_sumrain,      data = df2[df2$Age<50, ]))#
summary(lm(Yield..Kgs.acre.~Elevation, data = df2[df2$Age<50, ]))#



#ExploreTEA_PLOT_YieldSummerRainElevation=========
xyplot( Yield..Kgs.acre. ~ dry_sumrain| factor(Age2), #ExploreTEA_PLOT_YieldSummerRainElevation
   data = df2 ,
   strip = function(bg = 'white', ...) 
   strip.default(bg = 'white', ...),
   scales = list(alternating = T, 
                x = list(relation = "free"),
                y = list(relation = "same")),
   panel=function(x,y){
    panel.grid(h=-1, v= 2)
    panel.points(x, y, col = "red")
    panel.loess(x,y,col=2,lwd=2) #Add smoother
    panel.abline(lm(y~x))        #Add regression line
    })




#Elevation * Age effect on Yield:======
range(df$Age) #8 82

xyplot(Yield..Kgs.acre. ~ Elevation | factor(Age2),
   data = df2, 
   strip = function(bg = 'white', ...) 
   strip.default(bg = 'white', ...),
   scales = list(alternating = T, 
                x = list(relation = "free"),
                y = list(relation = "same")),
   panel=function(x,y){
    panel.grid(h=-1, v= 2)
    panel.points(x, y, col = "red")
    panel.loess(x,y,col=2,lwd=2) #Add smoother
    panel.abline(lm(y~x))        #Add regression line
    })

summary(lm(Yield..Kgs.acre.~Elevation2*Age2, data = df2))#
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     4169.6      139.1  29.975   <2e-16 ***
#Elevation2Low   -259.9      145.0  -1.792   0.0748 .  
#Age2young       -215.0      159.7  -1.347   0.1798    




#Transformations==========
#Why we don't like transformations,It can remove interactions!





