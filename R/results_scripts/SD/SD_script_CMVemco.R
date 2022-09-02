source("R/01_setup.R")
library(reshape2)
library(dplyr)
library(tidyr)

# data
cmvemco_dpd <- read.csv("results/CMVemco/cmvemco_dpd_refactor.csv")
length(unique(cmvemco_dpd$FishID))#296
cmvemco_dpd$date <- as.Date(cmvemco_dpd$date_time)
str(cmvemco_dpd)
length(unique(cmvemco_dpd$date)) #419

key <- read.csv("data/common_data/FishID_key.csv")
# 2007 2008 2009 2010 2011
vemco_07 <- subset(key, TagType == "Vemco" & Year == 2007)
vemco_08 <- subset(key, TagType == "Vemco" & Year == 2008)
vemco_09 <- subset(key, TagType == "Vemco" & Year == 2009)
vemco_10 <- subset(key, TagType == "Vemco" & Year == 2010)
vemco_11 <- subset(key, TagType == "Vemco" & Year == 2011)

# subset
vemco_07_dpd <- merge(cmvemco_dpd, vemco_07[,c(2,10)], by = "FishID")
length(unique(vemco_07_dpd$FishID))#5

vemco_08_dpd <- merge(cmvemco_dpd, vemco_08[,c(2,10)], by = "FishID")
length(unique(vemco_08_dpd$FishID))#81

vemco_09_dpd <- merge(cmvemco_dpd, vemco_09[,c(2,10)], by = "FishID")
length(unique(vemco_09_dpd$FishID))#58

vemco_10_dpd <- merge(cmvemco_dpd, vemco_10[,c(2,10)], by = "FishID")
length(unique(vemco_10_dpd$FishID))#48

vemco_11_dpd <- merge(cmvemco_dpd, vemco_11[,c(2,10)], by = "FishID")
length(unique(vemco_11_dpd$FishID))#104

# order
vemco_07_summary <- vemco_07_dpd[,c(1,4,5)] %>%
  group_by(FishID) %>%
  arrange(date, .by_group = TRUE) %>%
  complete(date = seq.Date(min(date), max(date), by="days"), fill = list(prop_dist = 0))

vemco_08_summary <- vemco_08_dpd[,c(1,4,5)] %>%
  group_by(FishID) %>%
  arrange(date, .by_group = TRUE) %>%
  complete(date = seq.Date(min(date), max(date), by="days"), fill = list(prop_dist = 0))

vemco_09_summary <- vemco_09_dpd[,c(1,4,5)] %>%
  group_by(FishID) %>%
  arrange(date, .by_group = TRUE) %>%
  complete(date = seq.Date(min(date), max(date), by="days"), fill = list(prop_dist = 0))

vemco_10_summary <- vemco_10_dpd[,c(1,4,5)] %>%
  group_by(FishID) %>%
  arrange(date, .by_group = TRUE) %>%
  complete(date = seq.Date(min(date), max(date), by="days"), fill = list(prop_dist = 0))

vemco_11_summary <- vemco_11_dpd[,c(1,4,5)] %>%
  group_by(FishID) %>%
  arrange(date, .by_group = TRUE) %>%
  complete(date = seq.Date(min(date), max(date), by="days"), fill = list(prop_dist = 0))

# make matrix
vemco_07_matrix <- dcast(vemco_07_summary, FishID~date, fun = sum, fill = NA_real_)
vemco_08_matrix <- dcast(vemco_08_summary, FishID~date, fun = sum, fill = NA_real_)
vemco_09_matrix <- dcast(vemco_09_summary, FishID~date, fun = sum, fill = NA_real_)
vemco_10_matrix <- dcast(vemco_10_summary, FishID~date, fun = sum, fill = NA_real_)
vemco_11_matrix <- dcast(vemco_11_summary, FishID~date, fun = sum, fill = NA_real_)

#SD
spread_vemco_07 <- transform(vemco_07_matrix, SD=apply(vemco_07_matrix[c(2:ncol(vemco_07_matrix))],1, sd, na.rm = TRUE))

rel <- names(vemco_07_matrix[-1])[max.col(!is.na(vemco_07_matrix[-1]), "first")]
end <- names(vemco_07_matrix[-ncol(vemco_07_matrix)])[max.col(!is.na(vemco_07_matrix[-ncol(vemco_07_matrix)]), "last")]
sd_vemco_07 <- data.frame(spread_vemco_07[,c(1,31)], rel, end)

head(sd_vemco_07)
str(sd_vemco_07)

spread_vemco_08 <- transform(vemco_08_matrix, SD=apply(vemco_08_matrix[c(2:ncol(vemco_08_matrix))],1, sd, na.rm = TRUE))

rel <- names(vemco_08_matrix[-1])[max.col(!is.na(vemco_08_matrix[-1]), "first")]
end <- names(vemco_08_matrix[-ncol(vemco_08_matrix)])[max.col(!is.na(vemco_08_matrix[-ncol(vemco_08_matrix)]), "last")]
sd_vemco_08 <- data.frame(spread_vemco_08[,c(1,93)], rel, end)

head(sd_vemco_08)
str(sd_vemco_08)

spread_vemco_09 <- transform(vemco_09_matrix, SD=apply(vemco_09_matrix[c(2:ncol(vemco_09_matrix))],1, sd, na.rm = TRUE))

rel <- names(vemco_09_matrix[-1])[max.col(!is.na(vemco_09_matrix[-1]), "first")]
end <- names(vemco_09_matrix[-ncol(vemco_09_matrix)])[max.col(!is.na(vemco_09_matrix[-ncol(vemco_09_matrix)]), "last")]
sd_vemco_09 <- data.frame(spread_vemco_09[,c(1,99)], rel, end)

head(sd_vemco_09)
str(sd_vemco_09)

spread_vemco_10 <- transform(vemco_10_matrix, SD=apply(vemco_10_matrix[c(2:ncol(vemco_10_matrix))],1, sd, na.rm = TRUE))

rel <- names(vemco_10_matrix[-1])[max.col(!is.na(vemco_10_matrix[-1]), "first")]
end <- names(vemco_10_matrix[-ncol(vemco_10_matrix)])[max.col(!is.na(vemco_10_matrix[-ncol(vemco_10_matrix)]), "last")]
sd_vemco_10 <- data.frame(spread_vemco_10[,c(1,122)], rel, end)

head(sd_vemco_10)
str(sd_vemco_10)

spread_vemco_11 <- transform(vemco_11_matrix, SD=apply(vemco_11_matrix[c(2:ncol(vemco_11_matrix))],1, sd, na.rm = TRUE))

rel <- names(vemco_11_matrix[-1])[max.col(!is.na(vemco_11_matrix[-1]), "first")]
end <- names(vemco_11_matrix[-ncol(vemco_11_matrix)])[max.col(!is.na(vemco_11_matrix[-ncol(vemco_11_matrix)]), "last")]
sd_vemco_11 <- data.frame(spread_vemco_11[,c(1,101)], rel, end)

head(sd_vemco_11)
str(sd_vemco_11)

write.csv(sd_vemco_07, "results/SD/CMvemco_07.csv")
write.csv(sd_vemco_08, "results/SD/CMvemco_08.csv")
write.csv(sd_vemco_09, "results/SD/CMvemco_09.csv")
write.csv(sd_vemco_10, "results/SD/CMvemco_10.csv")
write.csv(sd_vemco_11, "results/SD/CMvemco_11.csv")
