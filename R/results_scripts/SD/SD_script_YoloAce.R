source("R/01_setup.R")
library(reshape2)
library(dplyr)
library(tidyr)

# estimate variation in travel time (sd)

# make it reproducible

# load data from results
yoloace_dpd <- read.csv("results/YoloAce/yoloace_dpd_refactored.csv") # 7/29/22

#file_names <- list.files(path = "results/YoloAce",recursive = TRUE)
# need to move into folder to run the rest of the code
#setwd("C:/Users/pgoertler/Desktop/MJ's Repo/merged/acoustic-telemetry-synthesis/results/YoloAce")

#for(i in file_names){
#  file <- read.csv(i)
#  }

#file <- lapply(file_names,read.csv)
#names(file) <- substr(file_names, 1, 4)
#str(file)

#list.names <- substr(file_names, 1, 4)
#output <- vector("list", length(list.names))
#names(output) <- list.names

#dat_goods <- data.frame(FishID = NA, SD = NA, Year = NA, rel = NA, end = NA)

# loop
#for (i in substr(file_names, 1, 4)){
#  temp_df <- data.frame(file[[i]])
#  temp_df2 <- transform(temp_df, SD=apply(temp_df[c(2:ncol(temp_df))],1, sd, na.rm = TRUE))
#  temp_df3 <- temp_df2[, c("FishID", "SD")]
#  temp_df3$Year <- i
#  temp_df3$Col_first <- names(temp_df2[-1])[max.col(!is.na(temp_df2[-1]), "first")]
#  temp_df3$rel <- gsub("X","", temp_df3$Col_first)
#  temp_df3$Col_last <- names(temp_df2[-ncol(temp_df2)])[max.col(!is.na(temp_df2[-ncol(temp_df2)]), "last")]
#  temp_df3$end <- gsub("X","", temp_df3$Col_last)
#  dat_goods <- rbind(dat_goods, temp_df3[,-c(4,6)])
#}


#dat_goods <- dat_goods[-1,]
#head(dat_goods)
#str(dat_goods)
#unique(dat_goods$Year)

# make matrix
yoloace_dpd$date <- as.Date(yoloace_dpd$date_time)
str(yoloace_dpd)
length(unique(yoloace_dpd$date))

# bring in (real) FishID (currently is actually tagID)
# it is an issue bc R thinks tagID is an integer
key <- read.csv("data/common_data/FishID_key.csv")
key_mj_12 <- subset(key, TagType == "Vemco" & Year == 2012)
key_mj_13 <- subset(key, TagType == "Vemco" & Year == 2013)
# also separated years

colnames(yoloace_dpd)[2] <- "TagID"
yoloace_dpd_12 <- merge(yoloace_dpd, key_mj_12[,c(2,10)], by = "TagID")
unique(yoloace_dpd_12[,c(1,6)])

yoloace_dpd_13 <- merge(yoloace_dpd, key_mj_13[,c(2,10)], by = "TagID")
unique(yoloace_dpd_13[,c(1,6)])

yoloace_12_summary <- yoloace_dpd_12 %>%
  group_by(FishID) %>%
  arrange(date, .by_group = TRUE) %>%
  complete(date = seq.Date(min(date), max(date), by="days"), fill = list(prop_dist = 0))

yoloace_13_summary <- yoloace_dpd_13 %>%
  group_by(FishID) %>%
  arrange(date, .by_group = TRUE) %>%
  complete(date = seq.Date(min(date), max(date), by="days"), fill = list(prop_dist = 0))

head(summary_yoloace_dpd_13)

yoloace_dpd_matrix_13 <- dcast(test_summary, FishID~date, fun = sum, fill = NA_real_)
head(yoloace_dpd_matrix_13)

#test.yoloace_dpd_matrix_13 <- yoloace_dpd_matrix_13

#for(i in 2:nrow(test.yoloace_dpd_matrix_13)){
#  for(j in 2:ncol(test.yoloace_dpd_matrix_13)){
    #browser()
#    if(test.yoloace_dpd_matrix_13[i, j] == 0 & test.yoloace_dpd_matrix_13[i, j+1] == 0 | test.yoloace_dpd_matrix_13[i, j] == 0 & test.yoloace_dpd_matrix_13[i, j-1] == 0)
#      test.yoloace_dpd_matrix_13[i, j] <- NA
    #browser()
    #else
    #  test.yoloace_dpd_matrix_13[i, j] <- 0
#  }
#}

#head(test.yoloace_dpd_matrix_13)

test.yoloace_dpd_matrix_13


yoloace_dpd_matrix_13 <- dcast(summary_yoloace_dpd_13, FishID~date, fun.aggregate = function(x) if(length(x) == 0) NA_real_ else sum(x, na.rm = TRUE))
head(yoloace_dpd_matrix_13)

yoloace_dpd_matrix_13 <- dcast(summary_yoloace_dpd_13, FishID~date, value.var = "date", fun.aggregate = function(x) lubridate::as_date(x), fill = 0)

min(yoloace_dpd_matrix_12[,-1], na.rm = TRUE)
yoloace_dpd_matrix_12[yoloace_dpd_matrix_12 == 0] <- NA

min(yoloace_dpd_matrix_13[,-1], na.rm = TRUE)
yoloace_dpd_matrix_13[yoloace_dpd_matrix_13 == 0] <- NA

# need to investigate NA vs 0 issue...

MJ_63 <- subset(yoloace_dpd, TagID == "5870") # 2013-03-16 is missing
MJ_45 <- subset(summary_yoloace_dpd_13, FishID == "45.MJ") # 2013-03-13 is missing

test <- MJ_63 %>%
  complete(date, by = 'day')

# fix date
#dat_goods$rel <-  as.Date(gsub('\\.', '-', dat_goods$rel))
#dat_goods$end <-  as.Date(gsub('\\.', '-', dat_goods$end))

# back out
#setwd("C:/Users/pgoertler/Desktop/MJ's Repo/merged/acoustic-telemetry-synthesis")

# need to get fishID
#FishID_key <- read_csv("data/common_data/FishID_key.csv")
#colnames(dat_goods)[1] <- "TagID"

#dat_goods_ID <- merge(dat_goods, FishID_key[,c(2,10)], by = "TagID", all.x = TRUE)
#head(dat_goods_ID)

#write.csv(dat_goods_ID[,-1], "results/SD/YoloAce.csv")
