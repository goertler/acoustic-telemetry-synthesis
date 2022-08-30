source("R/01_setup.R")
library(reshape2)
library(dplyr)

# estimate variation in travel time (sd)

# load data from results (only one file this time)
ybus_dpd <- read.csv("results/YBUS/2016_ybus_dpd_refactor.csv") # latest data 6/28/22
head(ybus_dpd)

# make matrix
ybus_dpd$date <- as.Date(ybus_dpd$date_time)
str(ybus_dpd)
length(unique(ybus_dpd$date))

#summary_ybus_dpd <- ybus_dpd %>%
#  group_by(FishID, date) %>%
#  summarise(dist = sum(prop_dist))

summary_ybus_dpd <- ybus_dpd %>%
  group_by(FishID) %>%
  arrange(date, .by_group = TRUE) %>%
  complete(date = seq.Date(min(date), max(date), by="days"), fill = list(prop_dist = 0))

ybus_dpd_matrix <- dcast(summary_ybus_dpd, FishID~date, fun = sum, fill = NA_real_)
head(ybus_dpd_matrix)

#ybus_dpd_matrix[ybus_dpd_matrix == 0] <- NA # should double check that this isn't breaking up time series (if zero between > 0 values)
min(ybus_dpd_matrix[,-1], na.rm = TRUE)

# SD
spread_ybus <- transform(ybus_dpd_matrix, SD=apply(ybus_dpd_matrix[c(2:ncol(ybus_dpd_matrix))],1, sd, na.rm = TRUE))
rel <- names(spread_ybus[-1])[max.col(!is.na(spread_ybus[-1]), "first")]
end <- names(spread_ybus[-ncol(spread_ybus)])[max.col(!is.na(spread_ybus[-ncol(spread_ybus)]), "last")]

sd_dat <- data.frame(spread_ybus[,c(1,44)], rel, end)
head(sd_dat)

# fix date
sd_dat$rel <- gsub("X","", sd_dat$rel)
sd_dat$end <- gsub("X","", sd_dat$end)
sd_dat$rel <-  as.Date(gsub('\\.', '-', sd_dat$rel))
sd_dat$end <-  as.Date(gsub('\\.', '-', sd_dat$end))

str(sd_dat)
sd_dat$Year <- "2016"
length(unique(sd_dat$FishID))#662

write.csv(sd_dat, "results/SD/YBUS.csv")
