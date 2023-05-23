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

boxplot(travel_time~WaterYear, data= dat_tt_yr, boxwex = 0.5,
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

boxplot(tt_sd ~ WaterYear, data = summary_dat_yr, boxwex = 0.5,
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

# final version

tiff(filename="SDvs.WaterYear_Route_revised.tif", units="in", bg="white", height=9, width =12, res=300)

par(mfrow=c(1,2), mar=c(4,4,0.5,0.5))

boxplot(tt_sd ~ WaterYear, data = summary_dat_yr, boxwex = 0.5, ylim = c(0,20),
        ylab = "SD in Travel Time (days)", xlab = "Sacramento Valley Water Year Index")

legend("topright", "A", bty="n")
# add cyril's plot (from cyrils_fig.R)

plot(jitter(dat_fin$num_route), dat_fin$sd, ylim = c(0,20),
     col=as.character(dat_fin$colors),
     bg = as.character(dat_fin$colors),
     pch=dat_fin$shapes,
     xlab="Number of Routes",
     ylab="SD in Travel Time (days)",
     cex=1.5,
     lwd=2)
legend("topleft", c("Upper River",
                    "Middle River",
                    "Tidal Delta",
                    "2007","2008",
                    "2009","2010","2011","2012",
                    "2013","2014","2015",
                    "2016","2017"),
       pch=c(16,16,16,8,0,2,1,10,5,4,9,6,3,7), cex = 0.75, col=c("#f58231", "#911eb4", "#42d4f4",
                                                              "black","black","black","black","black","black",
                                                              "black","black","black","black","black"),
                                                              bty="o", ncol=2)
legend("topright", "B", bty="n")

dev.off()

# for supplemental, Will's suggestion
# make release and estuary entry numeric
data_doy <- dat_tt_yr %>%
  mutate(month_end = lubridate::month(end),
         year_end  = lubridate::year(end),
         rdoy_end  = lubridate::yday(end) + 92,
         week_end  = lubridate::week(end),
         water_year_end = ifelse(month_end > 9, year_end + 1, year_end),
         dowy_end = ifelse(rdoy_end > 366, rdoy_end - 366, rdoy_end),
         month_rel = lubridate::month(rel),
         year_rel  = lubridate::year(rel),
         rdoy_rel  = lubridate::yday(rel) + 92,
         week_rel  = lubridate::week(rel),
         water_year_rel = ifelse(month_rel > 9, year_rel + 1, year_rel),
         dowy_rel = ifelse(rdoy_rel > 366, rdoy_rel - 366, rdoy_rel))

summary_dat <-data_doy %>%
  group_by(Year) %>%
  summarise(ee_sd=sd(dowy_end, na.rm=TRUE))

summary_dat_yr <- merge(summary_dat, W_Y, by="Year")
summary_dat_yr$WaterYear = factor(summary_dat_yr$WaterYear, c("Critically Dry", "Dry", "Below Normal", "Wet"))

# add cyril's plot (from cyrils_fig.R)
route_summary <- data_na %>%
  group_by(Year, Release_Group_SAIL) %>%
  summarize(num_route = length(unique(Route)), sd = sd(dowy_end),
            mean = mean(dowy_end))

# define colors
colors <- c("#f58231", "#911eb4", "#42d4f4")
Release_Group_SAIL<- unique(route_summary$Release_Group_SAIL)
dat_col<-data.frame(colors, Release_Group_SAIL)
dat_fin<-merge(route_summary, dat_col, by="Release_Group_SAIL")

# Define shapes
shapes = c(0,1,2,3,4,5,6,7,8,9,10)
Year <- unique(dat_fin$Year)
dat_pch<-data.frame(shapes,Year)
dat_fin<-merge(dat_fin, dat_pch, by="Year")


tiff(filename="EE_WaterYear_Route.tif", units="in", bg="white", height=9, width =12, res=300)

par(mfrow=c(1,2), mar=c(4,4,0.5,0.5))

boxplot(ee_sd ~ WaterYear, data = summary_dat_yr, boxwex = 0.5, ylim = c(0,30),
        ylab = "SD in First Date Detected in Estuary", xlab = "Sacramento Valley Water Year Index")

legend("topright", "A", bty="n")

plot(jitter(dat_fin$num_route), dat_fin$sd,
     col=as.character(dat_fin$colors),
     bg = as.character(dat_fin$colors),
     pch=dat_fin$shapes,
     xlab="Number of Routes",
     ylab="SD in First Date Detected in Estuary",
     cex=1.5,
     lwd=2)
legend("bottomright", c("Upper River",
                    "Middle River",
                    "Tidal Delta",
                    "2007","2008",
                    "2009","2010","2011","2012",
                    "2013","2014","2015",
                    "2016","2017"),
       pch=c(16,16,16,8,0,2,1,10,5,4,9,6,3,7), cex = 0.75, col=c("#f58231", "#911eb4", "#42d4f4",
                                                              "black","black","black","black","black","black",
                                                              "black","black","black","black","black"),
                                                              bty="o", ncol=2)

legend("topright", "B", bty="n")

dev.off()
