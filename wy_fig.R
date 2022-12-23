library("vioplot")
library(beanplot)

#travel time vs. water year

# data
model_dat_complete<-read.csv("results/SD/model_dat.csv")

boxplot(travel_time~Year, data= model_dat_complete)

Year<-c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)
WaterYear<-c("Dry", "Critically Dry", "Dry", "Below Normal", "Wet", "Below Normal", "Dry", "Critically Dry", "Critically Dry", "Below Normal", "Wet")

W_Y <- data.frame(Year, WaterYear)
dat_tt_yr <- merge(model_dat_complete, W_Y, by="Year")

dat_tt_yr$WaterYear = factor(dat_tt_yr$WaterYear, c("Critically Dry", "Dry", "Below Normal", "Wet"))

boxplot(travel_time~WaterYear, data= dat_tt_yr,
        ylab = "Travel Time (days)", xlab = "Sacramento Valley Water Year Index")

summary(aov(travel_time~WaterYear, data= dat_tt_yr))

# sD vs. water year
summary_dat <-dat_tt_yr %>%
  group_by(Year) %>%
  summarise(tt_sd=sd(travel_time, na.rm=TRUE))

summary_dat_yr <- merge(summary_dat, W_Y, by="Year")
summary_dat_yr$WaterYear = factor(summary_dat_yr$WaterYear, c("Critically Dry", "Dry", "Below Normal", "Wet"))

tiff(filename="SDvs.WaterYear_modeldat.tif", units="in", bg="white", height=9, width =12, res=300)

par(mfrow=c(3,1), mar=c(4,4,0.5,0.5))

boxplot(tt_sd ~ WaterYear, data = summary_dat_yr,
        ylab = "SD in Travel Time (days)", xlab = "Sacramento Valley Water Year Index")

vioplot(tt_sd ~ WaterYear, data = summary_dat_yr,
        ylab = "SD in Travel Time (days)", xlab = "Sacramento Valley Water Year Index")

beanplot(tt_sd ~ WaterYear, data = summary_dat_yr,
         ylab = "SD in Travel Time (days)", xlab = "Sacramento Valley Water Year Index")


dev.off()

summary(aov(tt_sd~WaterYear, data= summary_dat_yr))

mod<-aov(tt_sd~WaterYear, data=summary_dat_yr)
mod<-aov(travel_time~WaterYear, data=dat_tt_yr)
summary(mod)
TukeyHSD(mod)
