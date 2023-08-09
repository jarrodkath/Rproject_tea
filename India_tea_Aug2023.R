library(usethis)
use_git()

setwd("/Users/kathj/Documents/DELL 28th July 2022/Attachments/Projects/ASIRF/Datasets 2023")

load("india_yield_climate_df.Rdata")


##quick test gam to make sure things are working
df<-india_yield_climate_df

library(mgcv)


summary(df$Yield..Kgs.acre.)

df$district_re<-as.factor(df$District)

test_gam<-gam(Yield..Kgs.acre. ~ 
                s(dry_meanvpd,k=5) + 
                s(dry_sumrain,k=5) + 
                s(wet_sumsoil,k=5)  + 
                s(Elevation) +
                s(district_re, bs="re"),method = "REML", #+ s(year_re, bs="re"), 
              family="gaussian", gamma=1 ,na.action = "na.fail", data=df)

summary(test_gam)
plot(test_gam, pages=1)

##############################
##Lets add a study area map
#############################

library(dplyr)
library(maps)
library(sf) 
library(ggplot2)
library(ggrepel)

##First get a map of india

world_shp <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

india_shp<- world_shp[world_shp$ID=="India",]
plot(india_shp)


##Subset out the study district

setwd("/Users/kathj/Documents/DELL 28th July 2022/Attachments/Projects/IKI/Coffee/Indian_robusta/India_Districts_ADM2_GADM")
dir()

ind_dist <- read_sf(dsn = ".", layer = "India_Districts_ADM2_GADM")

targ_dist<-c("Nilgiris")

study_dist<-filter(ind_dist, NAME_2 %in% targ_dist)



study_area<-ggplot() + ##Looking at this there appears to be a spatial outlier at ~76.4E that we should probably drop.
  
  geom_sf(data = study_dist, fill="white") + 
  geom_point(data=df, aes(x=Longitude, y=Latitude)) + 
  #geom_text_repel(data=dlabel, aes(x=dlon, y=dlat, label=d), size=12) +
  theme_classic() + 
  theme(legend.position = "none") +
  theme(text = element_text(size=28)) +
  ylab("Latitude")  + xlab("Longitude")


india_study_area<-ggplot() +
  
  geom_sf(data = india_shp, fill="lightgrey") + #geom_point(data=rob_df_join, aes(x=lon, y=lat), col="black") +
  
  geom_sf(data = study_dist, fill="red", col="red") + 
  
  theme_classic() +
  
  theme(line = element_blank(),
        text = element_blank(),
        title = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) 

##inset the map
india_rob_fig1_v2<-study_area + annotation_custom(
  ggplotGrob(india_study_area), 
  xmin = 76.2, xmax = 76.4, ymin = 11.2, ymax = 11.4) +
  
  annotate("segment", 
           x = 76.275, xend = 76.5, y = 11.245, yend=11.35, 
           colour = "gray20", size=0.7, arrow = arrow(length = unit(2, "mm"))) 


###################
