# library
library(contentid)

#data
stage_daily <- read.csv("results/SD/stage_daily_mean.csv")
head(stage_daily)
str(stage_daily)

stage_daily$date <- as.Date(stage_daily$date)

plot(stage_daily$date, stage_daily$mean)

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
library(scales)

temp_daily_time <- subset(temp_daily, date>=as.Date("2007-01-01") & date<=as.Date("2017-12-29"))

quantile(temp_daily_time$mean)
quantile(stage_daily$mean)

temp_daily_time$color <- ifelse(temp_daily_time$mean <= 11.991667, "#ffc20a",
                           ifelse(temp_daily_time$mean <= 16.731250 & temp_daily_time$mean > 11.991667, "#e66100",
                                  ifelse(temp_daily_time$mean <= 20.888542 & temp_daily_time$mean > 16.731250, "#DC3220", "red")))

stage_daily$color <- ifelse(stage_daily$mean <= 4.265883, "#abeb88",
                           ifelse(stage_daily$mean <= 4.586667 & stage_daily$mean > 4.265883, "#67d294",
                                  ifelse(stage_daily$mean <= 4.892187 & stage_daily$mean > 4.586667, "#17b79c", "#009a9c")))

png(filename = "covars.png", width = 11, height = 8, units = "in", pointsize = 12, bg = "white", res = 350)

plot(stage_daily$date, stage_daily$mean, ylim = c(2,25), pch = 17, col = alpha(stage_daily$color, 0.4), ylab = "", xlab = "")

legend("topleft", c("Water Temperature", "Stage"), pch = c(16,17), bty = "n")

par(new = TRUE)

plot(temp_daily_time$date, temp_daily_time$mean, ylim = c(2,25), pch = 16, col = alpha(temp_daily_time$color, 0.4), ylab = "Daily mean Stage and Water Temperature", xlab = "Time")

dev.off()

