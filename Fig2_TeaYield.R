#Load Packages:==========
if (!require("ggpmisc")) install.packages(get("ggpmisc"))
if (!require("tidyverse")) install.packages(get("tidyverse"))

library(ggpmisc)
library(tidyverse)


#LOAD DATA:====
##LOAD Tea DATA from India #load("india_yield_climate_df.Rdata") - does not contained updated elevation. Load df.csv instead
#df<-india_yield_climate_df

df<- read.csv("df.csv")

median_elevation <- median(df$Elevation_update)
median_elevation # 2036

df$elevation_bands<-as.factor(ifelse(df$Elevation_update > median_elevation, "High", "Low"))


#Fix the variety names (variety into variety2):
df$variety2 <- ifelse(df$variety=="chinese","Chinese", df$variety) #merge 2 Chinese varieties together
df$variety2 <- ifelse(df$variety2=="kottathur" |df$variety2=="NOT KNOWN","Unknown", df$variety2)  #kottathur is a village in Tamil Ladu district. Move it "Unknown" type of variety.
df$variety2 <- ifelse(df$variety2=="V.P CLONE" | df$variety2=="clone","VP", df$variety2) #"V.P CLONE", "clone" and "VP" look like one variety

table(df$variety2)
#Chinese     S61 Unknown   UPASI      VP 
#     10      49       9      26      90 



#PLOT Tea Yield by variety===========
df_known <- filter(df, variety2 !="Unknown") #remove "Unknown" tea variety

AV_Yield <- df %>% group_by(variety2) %>% summarise(AV = mean(Yield..Kgs.acre. , na.rm=T)) %>% arrange(desc(AV))
AV_Yield
#1 Chinese  4350 
#2 UPASI    4242.
#3 Unknown  4111.
#4 VP       3892.
#5 S61      3614.

df_known$variety2 <- factor(df_known$variety2, levels = c("Chinese","UPASI", "VP", "S61")) #arrange varieties by their by AV_YIELD

df_known2 <- subset(df_known, Yield..Kgs.acre.<7999)
dim(df_known2) #172  32

#remove any duplicated records (same yield values within villages)
df_known3<- df_known2 %>% 
      group_by(Village) %>% 
  do (data.frame( . [!duplicated(.$Yield..Kgs.acre.), ])) #remove any duplicated records.

dim(df_known3)#66 32 unknown tea varieties were excluded.

ggplot(df_known3, aes(x = variety2, y = Elevation*0.3048, fill=variety2)) + #Elevation*0.3048 # = converting feet to meters
  geom_violin( alpha = 0.8) +
  geom_jitter(aes( size = Yield..Kgs.acre. , color = Village), width = 0.2, alpha = 0.5) +
  scale_size_continuous(range = c(2, 8)) +
  scale_fill_manual(values = c('#238b45','#74c476','#bae4b3','#edf8e9'))+ #Levels of green related to AV_Yield:
  guides(fill = "none")+
  
  labs( title = "",
    x = "Tea Variety",
    y = "Elevation (m)",
    fill = "Tea variety:",
    size = "Yield (kg/acre): ",
    col = "Village: ") +
  
    theme_minimal() +
  
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.y = element_text(size = 26),
        axis.title.x = element_text(size = 26),
       
        legend.position = "right", #c(0.15, 0.31),
        #legend.background = element_blank(),
        #legend.box = "vertical",  # Display the legend as a box
        #legend.box.background = element_rect(color = "black"),  # Add a frame around the legend
        legend.text = element_text(size = 10))

ggsave( bg = "white", width = 13, height = 7, file = "TeaVarieties_66Records_Yield_Elevation_17Villages.jpg") 



#All yield data plot for Appendix===============
ggplot(df_known, aes(x = variety2, y = Elevation*0.3048, fill=variety2)) + #Elevation*0.3048 # = converting feet to meters
  geom_violin( alpha = 0.8) +
  geom_jitter(aes( size = Yield..Kgs.acre. , color = Village), width = 0.2, alpha = 0.5) +
  scale_size_continuous(range = c(2, 8)) +
  scale_fill_manual(values = c('#238b45','#74c476','#bae4b3','#edf8e9'))+ #Levels of green related to AV_Yield:
  guides(fill = "none")+
  
  labs( title = "",
    x = "Tea Variety",
    y = "Elevation (m)",
    fill = "Tea variety:",
    size = "Yield (kg/acre): ",
    col = "Village: ") +
  
    theme_minimal() +
  
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        axis.title.y = element_text(size = 26),
        axis.title.x = element_text(size = 26),
        legend.position = "right", #c(0.15, 0.31),
      legend.background = element_blank(),
       legend.box = "vertical",  # Display the legend as a box
       legend.box.background = element_rect(color = "black"))  # Add a frame around the legend


ggsave( bg = "white", width = 13, height = 7, file = "FigS1_TeaVarietiesGreen_Yield_Elevation_m_17Villages.jpg") 


#Descriptive stats of yield + elevation ==============
median_elevation <- median(df$Elevation_update)
median_elevation # 2036
df$elevation_bands<-as.factor(ifelse(df$Elevation_update > median_elevation, "High", "Low")) #Split elevation into 2 even bands

#Fix the variety names:
df$variety2 <- ifelse(df$variety=="chinese","Chinese", df$variety) #merge 2 chinese varieties together
df$variety2 <- ifelse(df$variety2=="kottathur" |df$variety2=="NOT KNOWN","Unknown", df$variety2)  #kottathur is a village in Tamil Ladu district. Move it "Unknown" type of variety.
df$variety2 <- ifelse(df$variety2=="V.P CLONE" | df$variety2=="clone","VP", df$variety2) #"V.P CLONE", "clone" and "VP" look like one variety
unique(df$variety2)# "S61"     "VP"      "Chinese" "UPASI"   "Unknown"
table(df$variety2)


AV_Yield2 <- df %>% filter( variety2 !="Unknown") %>%  #remove "Unknown" tea variety
  
                   group_by(variety2, elevation_bands) %>%
  
                   summarise(AV = mean(Yield..Kgs.acre. , na.rm=T),
                   SD =  sd(Yield..Kgs.acre. , na.rm=T) ,
                   N = n(),
                   SE = SD/sqrt(N)) %>%
  
                  arrange(elevation_bands)

AV_Yield2
# variety2 elevation_bands  AV    SD       N   SE
#1 Chinese  High            4714. 1524.     7  576.
#2 S61      High            3776   599.    25  120.
#3 UPASI    High            7000  1414.     2 1000 
#4 VP       High            4022   819.    50  116.

#5 Chinese  Low             3500   500      3  289.
#6 S61      Low             3446.  619.    24  126.
#7 UPASI    Low             4012. 1255.    24  256.
#8 VP       Low             3730   698.    40  110.

write.csv(AV_Yield2,"DescriptiveStatsYieldElevation.csv", row.names = F ) #Save the AV_Yield2 Table as spreadsheet

summary (df$Elevation_update)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#376    1648    2036    1901    2088    2224 

