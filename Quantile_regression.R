
##

library(ggplot2)
library(quantreg)


##Quantile regression

setwd("/Users/kathj/Documents/DELL 28th July 2022/Attachments/R files 2/R files/Rproject_tea")
dir()

df<-read.csv("df.csv")

##1.Exploratory plot

ggplot(data=df) + geom_point(aes(x=Elevation_update, y=Yield..Kgs.acre.))


ggplot(data=df) + geom_point(aes(x=aspect, y=Yield..Kgs.acre.))


##2. Remove two apparent outliers - need to check this again though

df1<-subset(df, Elevation_update>500)
df2<-subset(df1, Yield..Kgs.acre.<7999)


##3. Qunatile regression

quant_seq<-seq(from=0.1,to=0.9,by=0.1)
quant_seq2<-seq(from=0.25,to=0.75,by=0.25)

library(splines)

f <- rq((Yield..Kgs.acre.)~ ns(Age, df=4), data=df2, tau=quant_seq)
summary(f)

AIC(f)

summ <- summary(f, se = "boot", R=100)
summ

Xp <- predict(f,df2)
Xp

jd2<-data.frame(Xp, df2$Elevation_update, df2$Yield..Kgs.acre.)
plot(jd2$df2.Elevation_update, jd2$tau..0.9)

##

library(ggplot2)

ggplot(data=jd2) + geom_point(aes(x=df2.Elevation_update, y=df2.Yield..Kgs.acre.)) +
  geom_line(aes(x=df2.Elevation_update, y=tau..0.9, col="red")) +
  geom_line(aes(x=df2.Elevation_update, y=tau..0.8, col="red")) +
  geom_line(aes(x=df2.Elevation_update, y=tau..0.7, col="red")) +
  geom_line(aes(x=df2.Elevation_update, y=tau..0.6, col="red")) +
  geom_line(aes(x=df2.Elevation_update, y=tau..0.5, col="red")) +
  geom_line(aes(x=df2.Elevation_update, y=tau..0.3, col="red")) +
  geom_line(aes(x=df2.Elevation_update, y=tau..0.2, col="red")) +
  geom_line(aes(x=df2.Elevation_update, y=tau..0.1, col="red"))



##

f2<-nlrq(Yield..Kgs.acre. ~ SSlogis(Elevation_update, Asym, mid, scal), data=df2, tau=0.9, trace=TRUE)
summary(f2)


summ2 <- summary(f2, se = "boot", R=100)
summ2




#See if can predict from quantile reg to raster

library(raster)

setwd("/Users/kathj/Documents/DELL 28th July 2022/Attachments/R files 2/R files/Rproject_tea")
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
f_q <- rq((Yield..Kgs.acre.)~ ns(Age, df=4), data=df2, tau=0.9)


base_rstack<-stack(mlow)

names(base_rstack)<-c("Elevation_update")

rpred_dem<-predict(base_rstack, f_q, type="response")
summary(rpred_dem)
str(rpred_dem)
length(rpred_dem@data@values)
plot(rpred_dem)

####Propsed figure for paper
#1. Study area - done already
#2. Descriptive statistics of yield, elevation and maybe aspect
#3. Table of Quantreg results (Model parameters and results for elevation , aspect and elevation + aspect with AIC)
#4. Quant reg plot from 'best model'
#5. Prediction map under current (1.2 C) scenario
#6. Prediction map under 2,3 and 4 degree global warming scenarios
#7. Bar charts summarizing area change in 'top yielding' areas



#2. Descriptive stats

library(ggplot2)
library(RColorBrewer)

ggplot(data=df2) + geom_boxplot(aes(x=Age, y=Yield..Kgs.acre.))



#3. Table of quantreg results

quant_seq<-seq(from=0.1,to=0.9,by=0.1)
f <- rq((Yield..Kgs.acre.)~ ns(Elevation_update, df=4), data=df2, tau=quant_seq) ##run for df=1,df=2,df=3 and df=4 AND for aspect and age
summary(f)

AIC(f)

summ <- summary(f, se = "boot", R=100)
summ


#4.Quantreg best model (assume it is elevation and df=4) result plot - focus on 0.9 quantile as want to look at limiting factor



f_q <- rq((Yield..Kgs.acre.)~ ns(Elevation_update, df=4), data=df2, tau=0.9)


Xp <- predict(f,df2)


jd2<-data.frame(Xp, df2$Elevation_update, df2$Yield..Kgs.acre.)
plot(jd2$df2.Elevation_update, jd2$tau..0.9)

##

library(ggplot2)

ggplot(data=jd2) + geom_point(aes(x=df2.Elevation_update, y=df2.Yield..Kgs.acre.)) +
  geom_line(aes(x=df2.Elevation_update, y=tau..0.9, col="red")) +
  geom_line(aes(x=df2.Elevation_update, y=tau..0.8, col="red")) +
  geom_line(aes(x=df2.Elevation_update, y=tau..0.7, col="red")) +
  geom_line(aes(x=df2.Elevation_update, y=tau..0.6, col="red")) +
  geom_line(aes(x=df2.Elevation_update, y=tau..0.5, col="red")) +
  geom_line(aes(x=df2.Elevation_update, y=tau..0.3, col="red")) +
  geom_line(aes(x=df2.Elevation_update, y=tau..0.2, col="red")) +
  geom_line(aes(x=df2.Elevation_update, y=tau..0.1, col="red")) +
  theme_classic() + theme(legend.position = "none") +
  xlab("Elevation (m)") + ylab("Yield (kg/acres)")



#See if can predict from quantile reg to raster

library(raster)

setwd("/Users/kathj/Documents/DELL 28th July 2022/Attachments/R files 2/R files/Rproject_tea")
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

##
f_q <- rq((Yield..Kgs.acre.)~ ns(Elevation_update, df=4), data=df2, tau=0.9)

##
##restrict to higher elevation areas
# reclass so within bounds of dataset

mlow2<-mlow

mlow2[] <- ifelse(mlow[]<965,NA, 
               ifelse(mlow[]>2500,NA,mlow[]))


base_rstack<-stack(mlow2)


##

names(base_rstack)<-c("Elevation_update")

rpred_dem<-predict(base_rstack, f_q, type="response")
summary(rpred_dem)
str(rpred_dem)
length(rpred_dem@data@values)
plot(rpred_dem)
hist(rpred_dem@data@values)

##5. Climate change mapping scenarios

#For every 100-meter increase in elevation, the average temperature decreases by 0.7°C - googled - need to find paper reference
#0.7/100 = 0.07 degree C for every metre 
#how many metres is 0.1 deg c 14.28571 metres for every 0.1 deg C
##assuming currently at 1.2 deg C (need to check), then to get 2 deg C add (8*14.28571=114.2857m)  and 3 C (18*14.28571=257.1428m) and 4 C (28*14.28571=399.9999m)

#So if have conversin from temp to metres can just add to dem to create scenarios...?

mlow_2C<-mlow2-114.2857
plot(mlow_2C)

rstack_2c<-stack(mlow_2C)
names(rstack_2c)<-c("Elevation_update")

rpred_2Cdem<-predict(rstack_2c, f_q, type="response")

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





