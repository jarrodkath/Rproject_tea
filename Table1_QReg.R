#LOAD PACKAGES and DATA:======
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(quantreg)) install.packages('quantreg')
if (!require(sjPlot)) install.packages('sjPlot')
if (!require(sjstats)) install.packages('sjstats')
if (!require(sjmisc)) install.packages('sjmisc')

library(ggplot2)
library(quantreg)
library(sjPlot)
library(sjstats)
library(sjmisc)

df<-read.csv("df.csv")
dim(df)# 184  30

#Exploratory plot=======
ggplot(data=df) + geom_point(aes(x=Elevation_update, y=Yield..Kgs.acre.))# 3 distinct bands of elevation andyield outliers at 8000


# Remove two apparent outliers (8000 kg yield) - need to check this again though
df2<-subset(df, Yield..Kgs.acre.<7999)
dim(df2)

#remove any duplicated records (same yield values within villages)
df3 <- df2 %>% 
      group_by(Village) %>% 
  do (data.frame( . [!duplicated(.$Yield..Kgs.acre.), ])) #remove any duplicated records.

plot(df3$Elevation_update, df3$Yield..Kgs.acre.)
dim(df) - dim(df3)   # 114    records were removed from the original df.
dim(df3)


#Look at all quantile regressions===========
quant_seq<-seq(from=0.05,to=0.95,by=0.05)#Create quantiles to run against data
quant_seq#[1]0.05 0.10 0.15 0.20 0.25 0.30 0.35 0.40 0.45 0.50 0.55 0.60 0.65 0.70 0.75 0.80 0.85 0.90 0.95

#Run linear function (no knots) for each quantile of df3 data:
f <- rq(Yield..Kgs.acre.~ Elevation_update, data=df3, tau=quant_seq)#Run quantiles regression through each of the quantile
summ <- summary(f, se = "boot", R=1000)
summ
#tau: [1] 0.95 - the last and only quantile that shows P<0.05
#                 Value      Std. Error t value    Pr(>|t|)  
#(Intercept)      1877.95858 1432.29017    1.31116    0.19421
#Elevation_update    2.21893    0.82337    2.69495    0.00886



#0.95 Quntalie Regression STATS Table:==============
f_q <- rq((Yield..Kgs.acre.)~ Elevation_update, data=df3, tau=0.95)
summary(f_q)#Coef for Elevation_update  = 2.21893  
summary(f_q, se = "boot", R=1000)

#Produce a table for MS:
tab_model(f_q, show.stat=T, show.se=T, show.ci = F, show.r2 = F)

#Prediction of Yield:=======
Xp <- predict(f,df3)
Yield_Prediction<-data.frame(Xp, df3$Elevation_update, df3$Yield..Kgs.acre.)
plot(Yield_Prediction$df3.Elevation_update, Yield_Prediction$tau..0.95)



##PLOT=========
ggplot(data=Yield_Prediction) + geom_point(aes( x=df3.Elevation_update, y=df3.Yield..Kgs.acre.,col=df3$Village),size=4) +
  geom_line(aes(x=df3.Elevation_update, y=tau..0.95),size=1.2) +
  geom_line(aes(x=df3.Elevation_update, y=tau..0.80)) +
  geom_line(aes(x=df3.Elevation_update, y=tau..0.70)) +
  geom_line(aes(x=df3.Elevation_update, y=tau..0.60)) +
  geom_line(aes(x=df3.Elevation_update, y=tau..0.50)) +
  geom_line(aes(x=df3.Elevation_update, y=tau..0.30)) +
  geom_line(aes(x=df3.Elevation_update, y=tau..0.20)) +
  geom_line(aes(x=df3.Elevation_update, y=tau..0.10,))+
  labs( x = "Elevation (m)", y = "Yield (kg/acre)", col = "Village: ")+
    theme_bw()+
    theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16,),
        legend.position = "top",               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=6),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))+
        
  guides(size="none")

ggsave( bg = "white", width = 12, height = 7, file = "FigS2_QuantileRegresssion_Yield_Elevation_m.png") 


