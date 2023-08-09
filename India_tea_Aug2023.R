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
