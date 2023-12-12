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

##LOAD DATA:
dir()
df<-read.csv("df.csv")

##1.Exploratory plot=======
ggplot(data=df) + geom_point(aes(x=Elevation_update, y=Yield..Kgs.acre.))# 3 bands of elevation. Some clustered in lines. These lines need to be dealt with


##2. Remove two apparent outliers (8000 kg yield) - need to check this again though
df2<-subset(df, Yield..Kgs.acre.<7999)


##try remove duplicate values
df2_uniq<-df2[!duplicated(df2$Yield..Kgs.acre.),]
plot(df2_uniq$Elevation_update, df2_uniq$Yield..Kgs.acre.)


#Compare plots with all yields to no outlier yield:
par(mfrow = c(1, 1))

df3<-df2 %>% group_by(Village) %>% do (data.frame( . [!duplicated(.$Yield..Kgs.acre.), ]))
plot(df3$Elevation_update, df3$Yield..Kgs.acre.)
dim(df3)#70 30

##3. Quantile regression===========
quant_seq<-seq(from=0.1,to=0.95,by=0.05)
quant_seq#[1] 0.10 0.15 0.20 0.25 0.30 0.35 0.40 0.45 0.50 0.55 0.60 0.65 0.70 0.75 0.80 0.85 0.90 0.95

#Linear function (no knots) only fits best
f <- rq(Yield..Kgs.acre.~ Elevation_update, data=df3, tau=quant_seq)
summary(f)

AIC(f)

summ <- summary(f, se = "boot", R=1000)
summ
#tau: [1] 0.95
#Coefficients:       Value      Std. Error t value    Pr(>|t|)  
#(Intercept)      2270.61106 1181.64980    1.92156    0.05885
#Elevation_update    1.93986    0.68942    2.81377    0.00640

Xp <- predict(f,df3)
Xp

Yield_Prediction<-data.frame(Xp, df3$Elevation_update, df3$Yield..Kgs.acre., df3$Village)
plot(Yield_Prediction$df3.Elevation_update, Yield_Prediction$tau..0.9)

##

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

ggsave( bg = "white", width = 12, height = 7, file = "QuantileRegresssion_Yield_Elevation_m.png") 



##With unique data only
f2<-nlrq(Yield..Kgs.acre. ~ SSlogis(Elevation_update, Asym, mid, scal), data=df3, tau=0.90, trace=TRUE)
summary(f2)
summ2 <- summary(f2, se = "boot", R=100)
summ2


#Predict from quantile reg to raster=======
dir()

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

#QR STATS Table:==============
library(sjPlot)
library(sjstats)
library(sjmisc)
tab_model(f_q, show.stat=T, show.se=T, show.ci = F, show.r2 = F)



base_rstack<-stack(mlow)

names(base_rstack)<-c("Elevation_update")

rpred_dem<-predict(base_rstack, f_q, type="response")
summary(rpred_dem)
str(rpred_dem)
length(rpred_dem@data@values)
plot(rpred_dem)







####Proposed figure for paper
#1. Study area - done already
#2. Descriptive statistics of yield, elevation and maybe aspect
#3. Table of Quantreg results (Model parameters and results for elevation , aspect and elevation + aspect with AIC)
#4. Quant reg plot from 'best model'
#5. Prediction map under current (1.2 C) scenario
#6. Prediction map under 2,3 and 4 degree global warming scenarios
#7. Bar charts summarizing area change in 'top yielding' areas


#2. Descriptive stats
ggplot(data=df2) + geom_boxplot(aes(x=Elevation_update, y=Yield..Kgs.acre.))

#3. Table of quantreg results
quant_seq<-seq(from=0.1,to=0.9,by=0.1)
f <- rq((Yield..Kgs.acre.)~ Elevation_update, data=df3, tau=quant_seq) ##run for df=1,df=2,df=3 and df=4 AND for aspect and age
summary(f)

AIC(f)
summ <- summary(f, se = "boot", R=100)
summ

#4.Quantreg best model======
#focus on 0.9 quantile as want to look at limiting factor:
f_q <- rq(Yield..Kgs.acre.~ Elevation_update, data=df3, tau=0.95)
summary(f_q)

Xp <- predict(f,df3)


jd2<-data.frame(Xp, df3$Elevation_update, df3$Yield..Kgs.acre., df3$Village)
plot(jd2$df3.Elevation_update, jd2$tau..0.9)

ggplot(data=jd2) + geom_point(aes(x=df3.Elevation_update, y=df3.Yield..Kgs.acre., col=df3.Village)) +
  geom_line(aes(x=df3.Elevation_update, y=tau..0.90), col="red", size = 1.5) +
  geom_line(aes(x=df3.Elevation_update, y=tau..0.80, col="red")) +
  geom_line(aes(x=df3.Elevation_update, y=tau..0.70, col="red")) +
  geom_line(aes(x=df3.Elevation_update, y=tau..0.60, col="red")) +
  geom_line(aes(x=df3.Elevation_update, y=tau..0.50, col="red")) +
  geom_line(aes(x=df3.Elevation_update, y=tau..0.30, col="red")) +
  geom_line(aes(x=df3.Elevation_update, y=tau..0.20, col="red")) +
  geom_line(aes(x=df3.Elevation_update, y=tau..0.10, col="red")) +
  theme_classic() + theme(legend.position = "none") +
  xlab("Elevation (m)") + ylab("Yield (kg/acres)")


#MAP PREDICTIONS ======
#See if can predict from quantile reg to raster
dir()

r1<-raster( "N11E076.hgt")
r2<-raster("N11E077.hgt" )

m <- merge(r1,r2)
plot(m)

##Change resolution for testing and then can increase later when want higher resolution
res(m)
mlow<- aggregate(m, fact=40)
res(mlow)
plot(mlow)
summary(mlow)

##f_q <- rq((Yield..Kgs.acre.)~ ns(Elevation_update, df=4), data=df2, tau=0.9)#OLD WAY
f_q <- rq(Yield..Kgs.acre.~ Elevation_update, data=df3, tau=0.95) #NEW WAY

##
##restrict to higher elevation areas
# reclass so within bounds of dataset

mlow2<-mlow

mlow2[] <- ifelse(mlow[]<965,NA, 
               ifelse(mlow[]>2500,NA,mlow[]))


base_rstack<-stack(mlow2)
plot(base_rstack)
View(base_rstack)

##

names(base_rstack)<-c("Elevation_update")

rpred_dem<-predict(base_rstack, f_q, type="response")
summary(rpred_dem)#Max.     7381.938
str(rpred_dem)
length(rpred_dem@data@values)
plot(rpred_dem)
hist(rpred_dem@data@values)



##5. Climate change mapping scenarios===========

#For every 100-meter increase in elevation, the average temperature decreases by 0.7°C - googled - need to find paper reference
#0.7/100 = 0.07 degree C for every metre 
#how many metres is 0.1 deg c 14.28571 metres for every 0.1 deg C
##assuming currently at 1.2 deg C (need to check), then to get 2 deg C add (8*14.28571=114.2857m)  and 3 C (18*14.28571=257.1428m) and 4 C (28*14.28571=399.9999m)

#So if have conversion from temp to metres can just add to dem to create scenarios...?
f_q<- rq(Yield..Kgs.acre.~ Elevation_update, data=df3, tau=0.95)

rstack_2c<-stack(mlow_2C)
names(rstack_2c)<-c("Elevation_update")

rpred_2Cdem<-predict(rstack_2c, f_q, type="response")
plot(rpred_2Cdem)







# Create a sequence of values for Elevation_update
elevation_values <- seq(from = 2500, by = -14.28571, length.out = 100)  # You can adjust the length as needed
df3_dummy <- data.frame(Elevation_update = elevation_values)# Create a data frame with the Elevation_update column
f_q<- rq(Yield..Kgs.acre.~ Elevation_update, data=df3, tau=0.95)
summary(f_q)

predict(f_q, df3_dummy)


Yield_Loss_0.1C <-  2.21893   * 14.28571 #14.28571 metres for every 0.1 deg 
Yield_Loss_0.1C # 31.69899kg/acre
CurrentTemperature<- 1.2
Yield_Loss_2C <- (2 - CurrentTemperature)/0.1 * Yield_Loss_0.1C
Yield_Loss_2C # 253.5919 kg/acre

Yield_Loss_3C <- (3 - CurrentTemperature)/0.1 * Yield_Loss_0.1C
Yield_Loss_3C #  570.5818 kg/acre

Yield_Loss_4C <- (4 - CurrentTemperature)/0.1 * Yield_Loss_0.1C
Yield_Loss_4C #  887.5717 kg/acre






# Install and load the raster and rasterVis packages if not already installed
if (!requireNamespace("raster", quietly = TRUE)) {
  install.packages("raster")
}

if (!requireNamespace("rasterVis", quietly = TRUE)) {
  install.packages("rasterVis")
}

library(raster)
library(rasterVis)

# Your existing code for mlow and rpred_dem
# ...

# Create a levelplot with a divergent color scheme
plot(rpred_dem)
names(rpred_2Cdem)

divergent_color_scheme <- colorRampPalette(c("red", "white", "green"))
levelplot( rpred_dem, col.regions = divergent_color_scheme(100), xlab="Current Temperature 1.2C")

# Get the non-NA values from the rpred_dem raster's data values
non_na_values <- rpred_dem@data@values[!is.na(rpred_dem@data@values)]
# Get the length of non-NA values
Total_Cells <- length(non_na_values)#2494


#Get 0.95 quantile of yield data for our High Yield Potential threshold:
Yield_095 <- quantile(df3$Yield..Kgs.acre., probs = c(.25, .90, .95))
Yield_095
# 25%  90%  95% 
# 3000 5000 6000 

Loss_2C <- 6000 -Yield_Loss_2C #5746.408
Loss_3C <- 6000 -Yield_Loss_3C #5429.418
Loss_4C <- 6000 -Yield_Loss_4C #5112.428 

#6000 Yield today at Base 
levelplot(rpred_dem, col.regions = divergent_color_scheme(100),)# Create the levelplot with a divergent color scheme
below_6000_binary <- rpred_dem < 6000 ## Create a binary raster where values below 6000 are 1, and others are 0
area_below_6000 <- cellStats(below_6000_binary, sum)# Calculate the area below 6000 using cellStats
cat("Area below 6000:", area_below_6000, "cells") # Print the area below 6000
#Area below 6000: 1884 cells

#Yield at Loss_2C
levelplot(rpred_dem, col.regions = divergent_color_scheme(100), xlab="Current Temperature 1.2C")# Create the levelplot with a divergent color scheme
below_2C_binary <- rpred_dem < Loss_2C ## Create a binary raster where values under 2C
area_below_2C<- cellStats(below_2C_binary, sum)# Calculate the area below 
cat("Area below 2C:", area_below_2C, "cells") # Print the area below
#Area below 2C: 1748 cells

#Yield at Loss_3C
levelplot(rpred_dem, col.regions = divergent_color_scheme(100), xlab="Current Temperature 1.2C")# Create the levelplot with a divergent color scheme
below_3C_binary <- rpred_dem < Loss_3C ## Create a binary raster where values under 3C
area_below_3C<- cellStats(below_3C_binary, sum)# Calculate the area below 
cat("Area below 3C:", area_below_3C, "cells") # Print the area below
#Area below 3C: 1615  cells

#Yield at Loss_4C
levelplot(rpred_dem, col.regions = divergent_color_scheme(100), xlab="Current Temperature 1.2C")# Create the levelplot with a divergent color scheme
below_4C_binary <- rpred_dem < Loss_4C ## Create a binary raster where values under 4C
area_below_4C<- cellStats(below_4C_binary, sum)# Calculate the area below 
cat("Area below 4C:", area_below_4C, "cells") # Print the area below
#Area below 4C: 1475  cells


loss_values <- seq(from = 2500, by = -14.28571, length.out = 100)  # You can adjust the length as needed
df_Yield_Loss<- data.frame( Condition = c("0C", "2C", "3C", "4C"),
                           Yield_Loss = c( 1884,1748,1615, 1475),
                                 Unit = "Cells",
                           Percent_Loss = df_Yield_Loss$Yield_Loss / Total_Cells *100)# Create a data frame


df_Yield_Loss

library(ggplot2)

# Create the data frame
df_Yield_Loss <- data.frame(
  Condition = c("0C", "2C", "3C", "4C"),
  Yield_Loss = c(1884, 1748, 1615, 1475),
  Unit = "Cells",
  Percent_Loss = c(6.3, 5.9, 5.4, 4.9)  # I've added Percent_Loss values based on your example
)

# Define a gradient color scale from green to red
color_scale <- scale_fill_gradient(high = "green", low = "red")

# Create the bar plot using ggplot2
ggplot(df_Yield_Loss, aes(x = Condition, y = Percent_Loss, fill = Percent_Loss)) +
  geom_bar(stat = "identity") +
  labs(subtitle = "High yield potential under future climate",
       title = "Tea industry in India",
       y = "%", x="") +
   scale_y_continuous(limits = c(0,80))+
    theme_minimal() +
    theme(axis.text.x = element_text(size = 12,color="black"),
        axis.text.y = element_text(size = 12,color="black"),
        axis.title.y = element_text(size = 20,color="black"),
        axis.title.x = element_text(size = 20,color="black"),
        legend.position = "none",               #c(0.2, 0.5)
        legend.text = element_text(size = 16),
        strip.text=element_text(size=6),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))+

  color_scale  # Apply the color scale

# Print the plot
print(p)


# Create the levelplot with a divergent color scheme
levelplot(rpred_2Cdem, col.regions = divergent_color_scheme(100))
# Create a binary raster where values below 6000 are 1, and others are 0
below_6000_binary_2C <- rpred_2Cdem < 6000
# Calculate the area below 6000 using cellStats
area_below_6000_2C <- cellStats(below_6000_binary, sum)
# Print the area below 6000
cat("Area below 6000:", area_below_6000_2C, "cells")
#Area below 6000:  2007 cells





##
mlow_3C<-mlow2-257.1428
plot(mlow_3C)

rstack_3c<-stack(mlow_3C)
names(rstack_3c)<-c("Elevation_update")

rpred_3Cdem<-predict(rstack_3c, f_q, type="response")

##
mlow_4C<-mlow2-399.99
plot(mlow_4C)

rstack_4c<-stack(mlow_4C)
names(rstack_4c)<-c("Elevation_update")


rpred_4Cdem<-predict(rstack_4c, f_q, type="response")
###

plot(rpred_2Cdem)
plot(rpred_3Cdem)
plot(rpred_4Cdem)

####Library ggplot2
#do a raster using ggplot2
#https://stackoverflow.com/questions/33227182/how-to-set-use-ggplot2-to-map-a-raster

library(ggplot2)
library(raster)
library(rasterVis)
library(rgdal)
library(sf)
library(grid)
library(scales)
library(viridis)  # better colors for everyone
library(ggthemes) # theme_map()

##current


spdf <- as(rpred_dem, "SpatialPixelsDataFrame")
spdf_df <- as.data.frame(spdf)
colnames(spdf_df) <- c("value", "x", "y")


base<-ggplot() +  
  geom_tile(data=spdf_df, aes(x=x, y=y, fill=value), alpha=0.8) + 
  #scale_fill_continuous(limits=c(0, 6000)) +
  # geom_polygon(data=OR, aes(x=long, y=lat, group=group), 
  #              fill=NA, color="grey50", size=0.25) +
  scale_fill_viridis(limits=c(0, 12500)) +
  coord_equal() +
  theme_map() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm"))

base

####

spdf2c <- as(rpred_2Cdem, "SpatialPixelsDataFrame")
spdf2c_df <- as.data.frame(spdf2c)
colnames(spdf2c_df) <- c("value", "x", "y")


deg2<-ggplot() +  
  geom_tile(data=spdf2c_df, aes(x=x, y=y, fill=value), alpha=0.8) + 
  #scale_fill_continuous(limits=c(0, 6000)) +
  # geom_polygon(data=OR, aes(x=long, y=lat, group=group), 
  #              fill=NA, color="grey50", size=0.25) +
  scale_fill_viridis(limits=c(0, 12500)) +
  coord_equal() +
  theme_map() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm"))

diff2<-spdf2c_df
diff2$diff<-spdf2c_df$value-spdf_df$value


deg2_diff<-ggplot() +  
  geom_tile(data=diff2, aes(x=x, y=y, fill=diff), alpha=0.8) + 
  #scale_fill_continuous(limits=c(0, 6000)) +
  # geom_polygon(data=OR, aes(x=long, y=lat, group=group), 
  #              fill=NA, color="grey50", size=0.25) +
  scale_fill_viridis() +
  coord_equal() +
  theme_map() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm"))


###

spdf3c <- as(rpred_3Cdem, "SpatialPixelsDataFrame")
spdf3c_df <- as.data.frame(spdf3c)
colnames(spdf3c_df) <- c("value", "x", "y")


deg3<-ggplot() +  
  geom_tile(data=spdf3c_df, aes(x=x, y=y, fill=value), alpha=0.8) + 
  #scale_fill_continuous(limits=c(0, 6000)) +
  # geom_polygon(data=OR, aes(x=long, y=lat, group=group), 
  #              fill=NA, color="grey50", size=0.25) +
  scale_fill_viridis(limits=c(0, 12500)) +
  coord_equal() +
  theme_map() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm"))


###
spdf4c <- as(rpred_4Cdem, "SpatialPixelsDataFrame")
spdf4c_df <- as.data.frame(spdf4c)
colnames(spdf4c_df) <- c("value", "x", "y")


deg4<-ggplot() +  
  geom_tile(data=spdf4c_df, aes(x=x, y=y, fill=value), alpha=0.8) + 
  #scale_fill_continuous(limits=c(0, 6000)) +
  # geom_polygon(data=OR, aes(x=long, y=lat, group=group), 
  #              fill=NA, color="grey50", size=0.25) +
  scale_fill_viridis(limits=c(0, 12500)) +
  coord_equal() +
  theme_map() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm"))


deg4
deg3
deg2
