# library
library(contentid)
library(scales)
library(dplyr)

#data
stage_daily <- read.csv("results/SD/stage_daily_mean.csv")
head(stage_daily)
str(stage_daily)

stage_daily$date <- as.Date(stage_daily$date)

plot(stage_daily$date, stage_daily$mean)

# flow is best now
library(devtools)
devtools::install_github("goertler/inundation")

library(inundation)
inun <- calc_inundation()

head(inun)
str(inun)

plot(inun$date, inun$sac)

data_URL = "https://portal.edirepository.org/nis/dataviewer?packageid=edi.1178.1&entityid=5055c89851653f175078378a6e8ba6eb"
integrated_data_id <- contentid::store(data_URL)
integrated_temp <- read.csv(contentid::retrieve(integrated_data_id))
str(integrated_temp)
integrated_temp$date <- as.Date(integrated_temp$date)

temp_daily <- subset(integrated_temp, region == "river_downstream")

head(temp_daily)
str(temp_daily)

plot(temp_daily$date, temp_daily$mean)

# plot

temp_daily_time <- subset(temp_daily, date>=as.Date("2007-01-01") & date<=as.Date("2017-12-29"))
inun_time <- subset(inun, date>=as.Date("2007-01-01") & date<=as.Date("2017-12-29"))

# color by quantiles
quantile(temp_daily_time$mean)
quantile(stage_daily$mean)
quantile(inun_time$sac)

temp_daily_time$color <- ifelse(temp_daily_time$mean <= 11.991667, "#ffc20a",
                           ifelse(temp_daily_time$mean <= 16.731250 & temp_daily_time$mean > 11.991667, "#e66100",
                                  ifelse(temp_daily_time$mean <= 20.888542 & temp_daily_time$mean > 16.731250, "#DC3220", "red")))

stage_daily$color <- ifelse(stage_daily$mean <= 4.265883, "#abeb88",
                           ifelse(stage_daily$mean <= 4.586667 & stage_daily$mean > 4.265883, "#67d294",
                                  ifelse(stage_daily$mean <= 4.892187 & stage_daily$mean > 4.586667, "#17b79c", "#009a9c")))

inun$color <- ifelse(inun$sac <= 9727.5, "#abeb88",
                            ifelse(inun$sac <= 13900.0 & inun$sac > 9727.5, "#67d294",
                                   ifelse(inun$sac <= 19300.0 & inun$sac > 13900.0, "#17b79c", "#009a9c")))



# change colors to black and grey based on when fish are in system (12/15/22)
# data
model_dat_complete<-read.csv("results/SD/model_dat.csv")

model_dat_complete$rel <- as.Date(model_dat_complete$rel)
model_dat_complete$end <- as.Date(model_dat_complete$end)

model_dat_complete %>%
  group_by(Year) %>%
  summarize(min = min(rel, na.rm = TRUE), max = max(end, na.rm = TRUE))

temp_daily_time$color <- ifelse(temp_daily_time$date >= '2007-01-31' & temp_daily_time$date <= '2007-02-27', "black",
                                ifelse(temp_daily_time$date >= '2007-12-07' & temp_daily_time$date <= '2008-03-05', "black",
                                       ifelse(temp_daily_time$date >= '2008-12-13' & temp_daily_time$date <= '2009-03-18', "black",
                                              ifelse(temp_daily_time$date >= '2009-12-15' & temp_daily_time$date <= '2010-04-12', "black",
                                                     ifelse(temp_daily_time$date >= '2010-12-17' & temp_daily_time$date <= '2011-03-24', "black",
                                                            ifelse(temp_daily_time$date >= '2012-03-30' & temp_daily_time$date <= '2012-04-14', "black",
                                                                   ifelse(temp_daily_time$date >= '2013-02-07' & temp_daily_time$date <= '2013-05-12', "black",
                                                                          ifelse(temp_daily_time$date >= '2014-02-10' & temp_daily_time$date <= '2014-03-20', "black",
                                                                                 ifelse(temp_daily_time$date >= '2015-02-04' & temp_daily_time$date <= '2015-04-23', "black",
                                                                                        ifelse(temp_daily_time$date >= '2016-02-17' & temp_daily_time$date <= '2016-05-19', "black",
                                                                                               ifelse(temp_daily_time$date >= '2017-02-02' & temp_daily_time$date <= '2017-06-18', "black","gray")))))))))))

temp_daily_time$pch <- ifelse(temp_daily_time$date >= '2007-01-31' & temp_daily_time$date <= '2007-02-27', 16,
                                ifelse(temp_daily_time$date >= '2007-12-07' & temp_daily_time$date <= '2008-03-05', 16,
                                       ifelse(temp_daily_time$date >= '2008-12-13' & temp_daily_time$date <= '2009-03-18', 16,
                                              ifelse(temp_daily_time$date >= '2009-12-15' & temp_daily_time$date <= '2010-04-12', 16,
                                                     ifelse(temp_daily_time$date >= '2010-12-17' & temp_daily_time$date <= '2011-03-24', 16,
                                                            ifelse(temp_daily_time$date >= '2012-03-30' & temp_daily_time$date <= '2012-04-14', 16,
                                                                   ifelse(temp_daily_time$date >= '2013-02-07' & temp_daily_time$date <= '2013-05-12', 16,
                                                                          ifelse(temp_daily_time$date >= '2014-02-10' & temp_daily_time$date <= '2014-03-20', 16,
                                                                                 ifelse(temp_daily_time$date >= '2015-02-04' & temp_daily_time$date <= '2015-04-23', 16,
                                                                                        ifelse(temp_daily_time$date >= '2016-02-17' & temp_daily_time$date <= '2016-05-19', 16,
                                                                                               ifelse(temp_daily_time$date >= '2017-02-02' & temp_daily_time$date <= '2017-06-18', 16,1)))))))))))

inun_time$color <- ifelse(inun_time$date >= '2007-01-31' & inun_time$date <= '2007-02-27', "black",
                                ifelse(inun_time$date >= '2007-12-07' & inun_time$date <= '2008-03-05', "black",
                                       ifelse(inun_time$date >= '2008-12-13' & inun_time$date <= '2009-03-18', "black",
                                              ifelse(inun_time$date >= '2009-12-15' & inun_time$date <= '2010-04-12', "black",
                                                     ifelse(inun_time$date >= '2010-12-17' & inun_time$date <= '2011-03-24', "black",
                                                            ifelse(inun_time$date >= '2012-03-30' & inun_time$date <= '2012-04-14', "black",
                                                                   ifelse(inun_time$date >= '2013-02-07' & inun_time$date <= '2013-05-12', "black",
                                                                          ifelse(inun_time$date >= '2014-02-10' & inun_time$date <= '2014-03-20', "black",
                                                                                 ifelse(inun_time$date >= '2015-02-04' & inun_time$date <= '2015-04-23', "black",
                                                                                        ifelse(inun_time$date >= '2016-02-17' & inun_time$date <= '2016-05-19', "black",
                                                                                               ifelse(inun_time$date >= '2017-02-02' & inun_time$date <= '2017-06-18', "black","gray")))))))))))

inun_time$pch <- ifelse(inun_time$date >= '2007-01-31' & inun_time$date <= '2007-02-27', 17,
                          ifelse(inun_time$date >= '2007-12-07' & inun_time$date <= '2008-03-05', 17,
                                 ifelse(inun_time$date >= '2008-12-13' & inun_time$date <= '2009-03-18', 17,
                                        ifelse(inun_time$date >= '2009-12-15' & inun_time$date <= '2010-04-12', 17,
                                               ifelse(inun_time$date >= '2010-12-17' & inun_time$date <= '2011-03-24', 17,
                                                      ifelse(inun_time$date >= '2012-03-30' & inun_time$date <= '2012-04-14', 17,
                                                             ifelse(inun_time$date >= '2013-02-07' & inun_time$date <= '2013-05-12', 17,
                                                                    ifelse(inun_time$date >= '2014-02-10' & inun_time$date <= '2014-03-20', 17,
                                                                           ifelse(inun_time$date >= '2015-02-04' & inun_time$date <= '2015-04-23', 17,
                                                                                  ifelse(inun_time$date >= '2016-02-17' & inun_time$date <= '2016-05-19', 17,
                                                                                         ifelse(inun_time$date >= '2017-02-02' & inun_time$date <= '2017-06-18', 17, 2)))))))))))

tiff(filename = "covars_3.tiff", width = 11, height = 8, units = "in", pointsize = 12, bg = "white", res = 350)

par(mfrow=c(2,1), mar=c(4,4,0.5,0.5))

plot(inun_time$date, inun_time$sac,  pch = inun_time$pch, col = inun_time$color, ylab = "Daily mean Sacramento River Outflow", xlab = "Time", cex = 1.25)

plot(temp_daily_time$date, temp_daily_time$mean, ylim = c(2,25), pch = temp_daily_time$pch, col = temp_daily_time$color, ylab = "Daily mean Water Temperature", xlab = "Time", cex = 1.25)

dev.off()

# for discussion

timing <- subset(temp_daily_time, mean >11 & mean <14)

summary_time <- timing %>%
  group_by(year) %>%
  summarize(min = min(date, na.rm = TRUE), max = max(date, na.rm = TRUE))

tt <- model_dat_complete %>%
  group_by(Year, Route, Release_Group_SAIL, Group) %>%
  summarize(min = min(travel_time, na.rm = TRUE), max = max(travel_time, na.rm = TRUE), mean = mean(travel_time, na.rm = TRUE))

trans <- model_dat_complete %>%
  group_by(Year, Route, Release_Group_SAIL, Group) %>%
  summarize(min = min(Transport_distance, na.rm = TRUE), max = max(Transport_distance, na.rm = TRUE), mean = mean(Transport_distance, na.rm = TRUE))
