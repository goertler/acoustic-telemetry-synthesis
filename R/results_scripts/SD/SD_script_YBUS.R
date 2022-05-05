source("R/01_setup.R")

# estimate variation in travel time (sd)

# load data from results (only one file this time)
ybus_dpd <- read.csv("results/YBUS/2016_ybus_dpd.csv")
head(ybus_dpd)

spread_ybus <- transform(ybus_dpd, SD=apply(ybus_dpd[c(2:ncol(ybus_dpd))],1, sd, na.rm = TRUE))
rel <- names(spread_ybus[-1])[max.col(!is.na(spread_ybus[-1]), "first")]
end <- names(spread_ybus[-ncol(spread_ybus)])[max.col(!is.na(spread_ybus[-ncol(spread_ybus)]), "last")]

sd_dat <- data.frame(spread_ybus[,c(1,72)], rel, end)
head(sd_dat)

# fix date
sd_dat$rel <- gsub("X","", sd_dat$rel)
sd_dat$end <- gsub("X","", sd_dat$end)
sd_dat$rel <-  as.Date(gsub('\\.', '-', sd_dat$rel))
sd_dat$end <-  as.Date(gsub('\\.', '-', sd_dat$end))

str(sd_dat)
sd_dat$Year <- "2016"
length(unique(sd_dat$FishID))

write.csv(sd_dat, "results/SD/YBUS.csv")


