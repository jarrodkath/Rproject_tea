#LOAD PACKAGES and DATA:======
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(quantreg)) install.packages('quantreg')
if (!require(dplyr)) install.packages('dplyr')
if (!require(splines)) install.packages('splines')
if (!require(raster)) install.packages('raster')
if (!require(RColorBrewer)) install.packages('RColorBrewer')
if (!require(ggpmisc)) install.packages('ggpmisc')
if (!require(ggpubr)) install.packages('ggpubr')

library(ggplot2)
library(quantreg)
library(dplyr)
library(splines)
library(raster)
library(RColorBrewer)
library(ggpmisc)
library (ggpubr) # ggarrange function for merging multiple plots

# Install and load the  rasterVis package if not already installed
if (!requireNamespace("rasterVis", quietly = TRUE)) {
  install.packages("rasterVis")
}
library(rasterVis)



##LOAD DATA:
df<-read.csv("df.csv")

#CLEAN DATA off Duplicates:
#df1<-subset(df, Elevation_update>500) #apparent outliers but decided to keep as many like this point are present
df2<-subset(df, Yield..Kgs.acre.<7999)#Remove two apparent outliers (8000 kg yield) - need to check this again though
df3<-df2 %>% group_by(Village) %>% do (data.frame( .[!duplicated(.$Yield..Kgs.acre.), ]))
plot(df3$Elevation_update, df3$Yield..Kgs.acre.)
dim(df3)#70 30


##Run Quantile regression===========
f_q <- rq((Yield..Kgs.acre.)~ Elevation_update, data=df3, tau=0.95)
summary(f_q)#Coef for Elevation_update  = 2.21893  
f_q_yield_coef <- as.numeric(coef(f_q)[2]) 
f_q_yield_coef #1.939864


#Predict from quantile reg to raster=======
r1<-raster( "N11E076.hgt")
r2<-raster("N11E077.hgt" )
m <- merge(r1,r2)
plot(m)

##Change resolution for testing from:
res(m) #0.0002777778 ~30m by 30m, 1 CELL = 0.09 ha
mlow<- aggregate(m, fact=40) #To:
res(mlow)#0.01111111 
0.01111111/0.0002777778 #1200 by 1200m = new coarser resolution, 1 CELL = 144 ha

plot(mlow)
summary(mlow)

#Rename layer in mlow to "Elevation_update":
base_rstack<-stack(mlow)
names(base_rstack)<-c("Elevation_update")

#Predict tea yield based on "Elevation_update":
rpred_dem<-predict(base_rstack, f_q, type="response")
summary(rpred_dem)
str(rpred_dem)
length(rpred_dem@data@values)#16471
plot(rpred_dem)


#MAP PREDICTIONS ======
mlow2<-mlow # re-class so within bounds of dataset below
mlow2[] <- ifelse(mlow[]<965,NA, 
           ifelse(mlow[]>2500,NA,mlow[])) ##restrict to higher elevation areas


base_rstack<-stack(mlow2)
plot(base_rstack)
names(base_rstack)<-c("Elevation_update")

rpred_dem<-predict(base_rstack, f_q, type="response")
length(rpred_dem@data@values)#16471
plot(rpred_dem)
hist(rpred_dem@data@values) #Predicted Yield values



#Climate change mapping scenarios===========
#0.7/100 = 0.07 degree C cooler for every metre or  0.1C = 14.28571 vertical metres up
##assuming currently at 1.2 deg C (need to check), then to get 2 deg C add (8*14.28571=114.2857m)  and 3 C (18*14.28571=257.1428m) and 4 C (28*14.28571=399.9999m)
#So if have conversion from temp to metres can just add to dem to create scenarios...?


#Run Quantile regression
f_q <- rq((Yield..Kgs.acre.)~ Elevation_update, data=df3, tau=0.95)#Our predictive Quantile regression
summary(f_q)
f_q_yield_coef <- as.numeric(coef(f_q)[2]) #Coef for Elevation_update
f_q_yield_coef #1.939864

#Compute Yield loss for every 0.1C warming
Yield_Loss_0.1C <- f_q_yield_coef  * 14.28571 #14.28571 metres for every 0.1 deg 
Yield_Loss_0.1C # 27.71234 kg/acre

CurrentTemperature<- 1.2
Yield_Loss_2C <- (2 - CurrentTemperature)/0.1 * Yield_Loss_0.1C
Yield_Loss_2C # kg/acre

Yield_Loss_3C <- (3 - CurrentTemperature)/0.1 * Yield_Loss_0.1C
Yield_Loss_3C #  kg/acre

Yield_Loss_4C <- (4 - CurrentTemperature)/0.1 * Yield_Loss_0.1C
Yield_Loss_4C #   kg/acre



#HIGH YIELD POTENTIAL LOSS =======
non_na_values <- rpred_dem@data@values [!is.na(rpred_dem@data@values)] # Get the non-NA values from the rpred_dem raster's data values
Total_Cells <- length(non_na_values)#2494,  the length of non-NA values, cells with yield values in them.


#Get 0.95 quantile of yield data for setting our High Yield Potential threshold:
Yield_095 <- quantile(df3$Yield..Kgs.acre., probs = c(.25, .90, .95))
Yield_095
# 25%  90%  95% 
# 3000 5000 4000 
max(df3$Yield..Kgs.acre.) #7000


#6000 kg/ha BARPLOT HIGH TEA POTENTIAL LOSS===========
Loss_2C <- 6000 + Yield_Loss_2C #at that elevation today we will have 6000 kg/acre under 2C
Loss_3C <- 6000 + Yield_Loss_3C ##at that elevation we will have 6000 kg/acre under 3C
Loss_4C <- 6000 + Yield_Loss_4C #at that elevation we will have 6000 kg/acre under 4C


#ON THE MAP:
#6000 Yield today at Baseline Temperature: 
below_6000_binary <- rpred_dem > 6000 ## Create a binary raster where values below 6000 are 1, and others are 0
area_below_6000 <- cellStats (below_6000_binary, sum)# Calculate the area below 6000 using cellStats

#Yield at Loss_2C
below_2C_binary <- rpred_dem > Loss_2C ## Create a binary raster where values under 2C
area_below_2C<- cellStats(below_2C_binary, sum)# Calculate the area below 

#Yield at Loss_3C
below_3C_binary <- rpred_dem > Loss_3C ## Create a binary raster where yield values are under 3C
area_below_3C<- cellStats(below_3C_binary, sum)# Calculate the area below 

#Yield at Loss_4C
below_4C_binary <- rpred_dem > Loss_4C ## Create a binary raster where yield values are under 4C
area_below_4C<- cellStats(below_4C_binary, sum)# Calculate the area below 


df_Yield_Loss_6k<- data.frame(Condition = c("1.2C", "2C", "3C", "4C"),
                           Yield_Loss = c(area_below_6000,area_below_2C,area_below_3C, area_below_4C),
                                 Unit = "Cells")                # Create a data frame
                           
df_Yield_Loss_6k$Percent_Loss <- df_Yield_Loss_6k$Yield_Loss / Total_Cells *100 #Compute % of High Yield


#PLOT:
k6 <- ggplot(df_Yield_Loss_6k, aes(x = Condition, y = Percent_Loss, fill = Percent_Loss)) +
  geom_bar(stat = "identity") +
  labs(title = "High tea yield  (6000 kg/acre)", #subtitle = "Yield potential loss under future climate",
       y = "%", x="") +
   scale_y_continuous(limits = c(0,100))+
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

k6
#ggsave( bg = "white", width = 12, height = 7, file = "High_Yield_Potential_Loss2.png") 







#5000 kg/ha BARPLOT HIGH TEA POTENTIAL LOSS===========
#Yield Loss for 5000 kg/ha areas:
Loss_2C <- 5000 + Yield_Loss_2C #at that elevation today we will have 5000 kg/acre under 2C
Loss_3C <- 5000 + Yield_Loss_3C ##at that elevation we will have 5000 kg/acre under 3C
Loss_4C <- 5000 + Yield_Loss_4C #at that elevation we will have 5000 kg/acre under 4C

#5000 Yield today at Base 
below_5000_binary <- rpred_dem > 5000 ## Create a binary raster where values below 5000 are 1, and others are 0
area_below_5000 <- cellStats (below_5000_binary, sum)# Calculate the area below 5000 using cellStats

#Yield at Loss_2C
below_2C_binary <- rpred_dem > Loss_2C ## Create a binary raster where values under 2C
area_below_2C<- cellStats(below_2C_binary, sum)# Calculate the area below 

#Yield at Loss_3C
below_3C_binary <- rpred_dem > Loss_3C ## Create a binary raster where yield values are under 3C
area_below_3C<- cellStats(below_3C_binary, sum)# Calculate the area below 

#Yield at Loss_4C
below_4C_binary <- rpred_dem > Loss_4C ## Create a binary raster where yield values are under 4C
area_below_4C<- cellStats(below_4C_binary, sum)# Calculate the area below 

df_Yield_Loss_5k<- data.frame(Condition = c("1.2C", "2C", "3C", "4C"),
                           Yield_Loss = c(area_below_5000,area_below_2C,area_below_3C, area_below_4C),
                                 Unit = "Cells")                # Create a data frame
                           
df_Yield_Loss_5k$Percent_Loss <- df_Yield_Loss_5k$Yield_Loss / Total_Cells *100 #Compute % of High Yield
df_Yield_Loss_5k

k5 <- ggplot(df_Yield_Loss_5k, aes(x = Condition, y = Percent_Loss, fill = Percent_Loss)) +
  geom_bar(stat = "identity") +
  labs( title = "High tea yield (5000 kg/acre)", #subtitle = "Yield potential loss under future climate",
       y = "%", x="") +
   scale_y_continuous(limits = c(0,100))+
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

k5



#7000 kg/ha BARPLOT HIGH TEA POTENTIAL LOSS===========
Loss_2C <- 7000 + Yield_Loss_2C #at that elevation today we will have 7000 kg/acre under 2C
Loss_3C <- 7000 + Yield_Loss_3C ##at that elevation we will have 7000 kg/acre under 3C
Loss_4C <- 7000 + Yield_Loss_4C #at that elevation we will have 7000 kg/acre under 4C


#ON THE MAP:
#7000 Yield today at Baseline Temperature: 
below_7000_binary <- rpred_dem > 7000 ## Create a binary raster where values below 7000 are 1, and others are 0
area_below_7000 <- cellStats (below_7000_binary, sum)# Calculate the area below 7000 using cellStats

#Yield at Loss_2C
below_2C_binary <- rpred_dem > Loss_2C ## Create a binary raster where values under 2C
area_below_2C<- cellStats(below_2C_binary, sum)# Calculate the area below 

#Yield at Loss_3C
below_3C_binary <- rpred_dem > Loss_3C ## Create a binary raster where yield values are under 3C
area_below_3C<- cellStats(below_3C_binary, sum)# Calculate the area below 

#Yield at Loss_4C
below_4C_binary <- rpred_dem > Loss_4C ## Create a binary raster where yield values are under 4C
area_below_4C<- cellStats(below_4C_binary, sum)# Calculate the area below 


df_Yield_Loss_7k<- data.frame(Condition = c("1.2C", "2C", "3C", "4C"),
                           Yield_Loss = c(area_below_7000,area_below_2C,area_below_3C, area_below_4C),
                                 Unit = "Cells")                # Create a data frame
                           
df_Yield_Loss_7k$Percent_Loss <- df_Yield_Loss_7k$Yield_Loss / Total_Cells *100 #Compute % of High Yield
df_Yield_Loss_7k

k7 <- ggplot(df_Yield_Loss_7k, aes(x = Condition, y = Percent_Loss, fill = Percent_Loss)) +
  geom_bar(stat = "identity") +
  labs(  title = "High tea yield  (7000 kg/acre)", #subtitle = "Yield potential loss under future climate",
       y = "%", x="") +
   scale_y_continuous(limits = c(0,50))+
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

k7


#4000 kg/ha BARPLOT HIGH TEA POTENTIAL LOSS===========
Loss_2C <- 4000 + Yield_Loss_2C #at that elevation today we will have 4000 kg/acre under 2C
Loss_3C <- 4000 + Yield_Loss_3C ##at that elevation we will have 4000 kg/acre under 3C
Loss_4C <- 4000 + Yield_Loss_4C #at that elevation we will have 4000 kg/acre under 4C


#ON THE MAP:
#4000 Yield today at Baseline Temperature: 
below_4000_binary <- rpred_dem > 4000 ## Create a binary raster where values below 4000 are 1, and others are 0
area_below_4000 <- cellStats (below_4000_binary, sum)# Calculate the area below 4000 using cellStats

#Yield at Loss_2C
below_2C_binary <- rpred_dem > Loss_2C ## Create a binary raster where values under 2C
area_below_2C<- cellStats(below_2C_binary, sum)# Calculate the area below 

#Yield at Loss_3C
below_3C_binary <- rpred_dem > Loss_3C ## Create a binary raster where yield values are under 3C
area_below_3C<- cellStats(below_3C_binary, sum)# Calculate the area below 

#Yield at Loss_4C
below_4C_binary <- rpred_dem > Loss_4C ## Create a binary raster where yield values are under 4C
area_below_4C<- cellStats(below_4C_binary, sum)# Calculate the area below 


df_Yield_Loss_4k<- data.frame(Condition = c("1.2C", "2C", "3C", "4C"),
                           Yield_Loss = c(area_below_4000,area_below_2C,area_below_3C, area_below_4C),
                                 Unit = "Cells")                # Create a data frame
                           
df_Yield_Loss_4k$Percent_Loss <- df_Yield_Loss_4k$Yield_Loss / Total_Cells *100 #Compute % of High Yield
df_Yield_Loss_4k

k4 <- ggplot(df_Yield_Loss_4k, aes(x = Condition, y = Percent_Loss, fill = Percent_Loss)) +
  geom_bar(stat = "identity") +
  labs(   title = "High tea yield (4000 kg/acre)", #subtitle = "Potential loss under future climate",
       y = "%", x="") +
   scale_y_continuous(limits = c(0,100))+
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

k4


#Combined k-PLOTS====
p_plots_horizon  <- ggarrange(k4,k5,k6, ncol=3) 
p_plots_horizon
ggsave(p_plots_horizon, filename = "Fig04_BARPLOT_TeaYieldLoss2.jpg", width = 7250, height = 5437, units = "px", dpi = 600)





#Prediction Map under 1.2C ==========
#LEVELPLOT the Yield (Green & Red Divergence):
divergent_color_scheme <- colorRampPalette(c("red", "white", "green"))# Create a levelplot with a divergent color scheme
levelplot( rpred_dem, col.regions = divergent_color_scheme(100), xlab="Tea yield (kg/acre) \n under current temperature of 1.2C")

