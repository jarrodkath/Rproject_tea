#LOAD PACKAGES and DATA:======
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(quantreg)) install.packages('quantreg')
if (!require(dplyr)) install.packages('dplyr')
if (!require(splines)) install.packages('splines')
if (!require(raster)) install.packages('raster')
if (!require(RColorBrewer)) install.packages('RColorBrewer')

library(ggplot2)
library(quantreg)
library(dplyr)
library(splines)
library(raster)
library(RColorBrewer)


# Install and load the  rasterVis package if not already installed
if (!requireNamespace("rasterVis", quietly = TRUE)) {
  install.packages("rasterVis")
}
library(rasterVis)



##LOAD DATA:
dir()
df<-read.csv("df.csv")

#CLEAN DATA of Duplicates:
df1<-subset(df, Elevation_update>500) #apparent outliers
df2<-subset(df1, Yield..Kgs.acre.<7999)#Remove two apparent outliers (8000 kg yield) - need to check this again though

df3<-df2 %>% group_by(Village) %>% do (data.frame( .[!duplicated(.$Yield..Kgs.acre.), ]))
plot(df3$Elevation_update, df3$Yield..Kgs.acre.)
dim(df3)#70 30

##Run Quantile regression===========
quant_seq<-seq(from=0.1,to=0.95,by=0.05)
quant_seq#[1] 0.10 0.15 0.20 0.25 0.30 0.35 0.40 0.45 0.50 0.55 0.60 0.65 0.70 0.75 0.80 0.85 0.90 0.95

#Linear function (no knots) only fits best
f <- rq(Yield..Kgs.acre.~ Elevation_update, data=df3, tau=quant_seq)
summary(f)

AIC(f) # The 0.95 quantile is the most predictive (High Tea Potential "Lid")
summ <- summary(f, se = "boot", R=1000)
summ

##tau: [1] 0.95   Value      Std. Error   t value    Pr(>|t|)  
#(Intercept)      1877.95858 1432.29017    1.31116    0.19421
#Elevation_update    2.21893    0.82337    2.69495    0.00886

Xp <- predict(f,df3)#Predict yield for all quantiles
Xp

Yield_Prediction<-data.frame(Xp, df3$Elevation_update, df3$Yield..Kgs.acre., df3$Village)
plot(Yield_Prediction$df3.Elevation_update, Yield_Prediction$tau..0.9)

#PLOT Q-REGRESSION LINES =======
ggplot(data=Yield_Prediction) + geom_point(aes( x=df3.Elevation_update, y=df3.Yield..Kgs.acre.,col=df3$Village),size=4) +
  geom_line(aes(x=df3.Elevation_update, y=tau..0.90),size=1.2) +
  geom_line(aes(x=df3.Elevation_update, y=tau..0.80)) +
  geom_line(aes(x=df3.Elevation_update, y=tau..0.70)) +
  geom_line(aes(x=df3.Elevation_update, y=tau..0.60)) +
  geom_line(aes(x=df3.Elevation_update, y=tau..0.50)) +
  geom_line(aes(x=df3.Elevation_update, y=tau..0.30)) +
  geom_line(aes(x=df3.Elevation_update, y=tau..0.20)) +
  geom_line(aes(x=df3.Elevation_update, y=tau..0.10,))+
  labs( x = "Elevation (m)", y = "Yield (kg/acre)", col = "Village: ")+
    theme_bw()+
    theme(axis.text.x = element_text(size = 10,angle=90),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16,),
        legend.position = "top",               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=6),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))+
        
  guides(size="none")

#ggsave( bg = "white", width = 12, height = 7, file = "QuantileRegresssion_Yield_Elevation_m.png") 

#Predict from quantile reg to raster=======
r1<-raster( "N11E076.hgt")
r2<-raster("N11E077.hgt" )
m <- merge(r1,r2)
plot(m)

##Change resolution for testing
res(m)
mlow<- aggregate(m, fact=40)
res(mlow)
plot(mlow)
summary(mlow)

##
f_q <- rq((Yield..Kgs.acre.)~ Elevation_update, data=df3, tau=0.95)
summary(f_q)#Coef for Elevation_update  = 2.21893  

base_rstack<-stack(mlow)
names(base_rstack)<-c("Elevation_update")

rpred_dem<-predict(base_rstack, f_q, type="response")
summary(rpred_dem)
str(rpred_dem)
length(rpred_dem@data@values)#16471
plot(rpred_dem)


#MAP PREDICTIONS ======
mlow2<-mlow # reclass so within bounds of dataset below
mlow2[] <- ifelse(mlow[]<965,NA, 
               ifelse(mlow[]>2500,NA,mlow[])) ##restrict to higher elevation areas


base_rstack<-stack(mlow2)
plot(base_rstack)
names(base_rstack)<-c("Elevation_update")

rpred_dem<-predict(base_rstack, f_q, type="response")
summary(rpred_dem)#Max.     7381.938
str(rpred_dem)
length(rpred_dem@data@values)#16471
plot(rpred_dem)
hist(rpred_dem@data@values)



#Climate change mapping scenarios===========
#For every 100-meter increase in elevation, the average temperature decreases by 0.7°C - googled - need to find paper reference
#0.7/100 = 0.07 degree C for every metre 
#how many metres is 0.1 deg c 14.28571 metres for every 0.1 deg C
##assuming currently at 1.2 deg C (need to check), then to get 2 deg C add (8*14.28571=114.2857m)  and 3 C (18*14.28571=257.1428m) and 4 C (28*14.28571=399.9999m)
#So if have conversion from temp to metres can just add to dem to create scenarios...?

summary(f_q) #Elevation_update     2.21893 
coef(f_q)[2] # 2.218935 
Yield_Loss_0.1C <-  2.21893   * 14.28571 #14.28571 metres for every 0.1 deg 
Yield_Loss_0.1C # 31.69899kg/acre

CurrentTemperature<- 1.2
Yield_Loss_2C <- (2 - CurrentTemperature)/0.1 * Yield_Loss_0.1C
Yield_Loss_2C # 253.5919 kg/acre

Yield_Loss_3C <- (3 - CurrentTemperature)/0.1 * Yield_Loss_0.1C
Yield_Loss_3C #  570.5818 kg/acre

Yield_Loss_4C <- (4 - CurrentTemperature)/0.1 * Yield_Loss_0.1C
Yield_Loss_4C #  887.5717 kg/acre


#LEVELPLOT the Yield (Green & Red Divergence)==========
divergent_color_scheme <- colorRampPalette(c("red", "white", "green"))# Create a levelplot with a divergent color scheme
levelplot( rpred_dem, col.regions = divergent_color_scheme(100), xlab="Current Temperature 1.2C")


#HIGH YIELD POTENTIAL LOSS =======
non_na_values <- rpred_dem@data@values[!is.na(rpred_dem@data@values)] # Get the non-NA values from the rpred_dem raster's data values
Total_Cells <- length(non_na_values)#2494,  the length of non-NA values


#Get 0.95 quantile of yield data for setting our High Yield Potential threshold:
Yield_095 <- quantile(df3$Yield..Kgs.acre., probs = c(.25, .90, .95))
Yield_095
# 25%  90%  95% 
# 3000 5000 6000 

Loss_2C <- 6000 + Yield_Loss_2C #at that elevation today we will have 6000 kg/acre under 2C
Loss_3C <- 6000 + Yield_Loss_3C ##at that elevation we will have 6000 kg/acre under 3C
Loss_4C <- 6000 + Yield_Loss_4C #at that elevation we will have 6000 kg/acre under $C

#6000 Yield today at Base 
below_6000_binary <- rpred_dem > 6000 ## Create a binary raster where values below 6000 are 1, and others are 0
area_below_6000 <- cellStats(below_6000_binary, sum)# Calculate the area below 6000 using cellStats

#Yield at Loss_2C
below_2C_binary <- rpred_dem > Loss_2C ## Create a binary raster where values under 2C
area_below_2C<- cellStats(below_2C_binary, sum)# Calculate the area below 

#Yield at Loss_3C
below_3C_binary <- rpred_dem > Loss_3C ## Create a binary raster where values under 3C
area_below_3C<- cellStats(below_3C_binary, sum)# Calculate the area below 

#Yield at Loss_4C
below_4C_binary <- rpred_dem > Loss_4C ## Create a binary raster where values under 4C
area_below_4C<- cellStats(below_4C_binary, sum)# Calculate the area below 


df_Yield_Loss<- data.frame(Condition = c("0C", "2C", "3C", "4C"),
                           Yield_Loss = c(area_below_6000,area_below_2C,area_below_3C, area_below_4C),
                                 Unit = "Cells")                # Create a data frame
                           


df_Yield_Loss$Percent_Loss <- df_Yield_Loss$Yield_Loss / Total_Cells *100 #Compute % of High Yield

#BARPLOT HIGH TEA POTENTIAL LOSS===========
ggplot(df_Yield_Loss, aes(x = Condition, y = Percent_Loss, fill = Percent_Loss)) +
  geom_bar(stat = "identity") +
  labs(subtitle = "High yield potential under future climate",
       title = "Tea industry in India",
       y = "%", x="") +
   scale_y_continuous(limits = c(0,30))+
   scale_fill_gradient(high = "green", low = "red")+
    theme_minimal() +
    theme(axis.text.x = element_text(size = 12,color="black"),
        axis.text.y = element_text(size = 12,color="black"),
        axis.title.y = element_text(size = 20,color="black"),
        axis.title.x = element_text(size = 20,color="black"),
        legend.position = "none",               #c(0.2, 0.5)
        legend.text = element_text(size = 16),
        strip.text=element_text(size=6),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))

#ggsave( bg = "white", width = 12, height = 7, file = "High_Yield_Potential_Loss2.png") 
