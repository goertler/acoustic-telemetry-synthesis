# add SAC dayflow
# add inundation data
library(dplyr)
library(devtools)
devtools::install_github("goertler/inundation")

library(inundation)
inun <- calc_inundation()

head(inun)
str(inun)
length(is.na(inun$inund_days == TRUE))

sd_meta <- read.csv("results/SD/model_dat.csv")
head(sd_meta)
str(sd_meta)
sd_meta$rel <- as.Date(sd_meta$rel)
sd_meta$end <- as.Date(sd_meta$end)
length(unique(sd_meta$FishID))

keeper_dat <- data.frame(FishID = NA, date_min = as.Date("1900-01-01"),  date_max = as.Date("1900-01-01"), sac_mean = NA, sac_sd = NA, max_inun = NA, sum_inun = NA)

for(i in unique(sd_meta$FishID)){
  temp_dat <- subset(sd_meta, FishID == i)
  inun_df <- subset(inun, date >= min(temp_dat$rel) & date <= max(temp_dat$end))
  new_dat <- data.frame(FishID = i, date_min = min(temp_dat$rel),  date_max = max(temp_dat$end), sac_mean = mean(inun_df$sac), sac_sd = sd(inun_df$sac), max_inun = max(inun_df$inund_days), sum_inun = sum(inun_df$inundation))
  keeper_dat <- rbind(keeper_dat, new_dat)
}

head(keeper_dat)
keeper_dat <- keeper_dat[-1,]

model_df <- merge(keeper_dat[,-c(2:3)], sd_meta, by = "FishID", all = TRUE)
head(model_df)

sapply(X = model_df, FUN = function(x) sum(is.na(x))) #just the 8 JSATS
check_sd <- model_df[is.na(model_df$SD),]

write.csv(model_df[,-6], "results/SD/model_dat.csv")

# add PDO
pdo <- read.csv("data/enviro_data/PDO.csv")

month_dat <- model_df[,c(1,15,25,26)] %>%
  mutate(month_end = lubridate::month(end),
         year_end  = lubridate::year(end),
         month_rel = lubridate::month(rel),
         year_rel  = lubridate::year(rel))

min(month_dat$month_rel, na.rm = TRUE)
max(month_dat$month_end, na.rm = TRUE)

month_dat$combo <- paste(month_dat$month_rel, "_", month_dat$year_rel, "-", month_dat$month_end, "_", month_dat$year_end)

length(unique(month_dat$combo)) #52

# one month
combo_1 <- c("2 _ 2016 - 2 _ 2016","3 _ 2016 - 3 _ 2016", "3 _ 2013 - 3 _ 2013", "5 _ 2016 - 5 _ 2016", "5 _ 2017 - 5 _ 2017", "4 _ 2013 - 4 _ 2013", "4 _ 2017 - 4 _ 2017", "4 _ 2016 - 4 _ 2016",   "4 _ 2015 - 4 _ 2015", "2 _ 2007 - 2 _ 2007", "12 _ 2007 - 12 _ 2007", "1 _ 2008 - 1 _ 2008","12 _ 2008 - 12 _ 2008", "1 _ 2009 - 1 _ 2009", "12 _ 2009 - 12 _ 2009", "1 _ 2010 - 1 _ 2010", "12 _ 2010 - 12 _ 2010", "1 _ 2011 - 1 _ 2011", "6 _ 2017 - 6 _ 2017", "5 _ 2013 - 5 _ 2013",  "2 _ 2014 - 2 _ 2014", "2 _ 2015 - 2 _ 2015", "2 _ 2017 - 2 _ 2017" ) #23

combo_1_dat = month_dat[(month_dat$combo %in% combo_1), ] #1128 (of 1814)
combo_1_dat <- merge(combo_1_dat, pdo, by = "combo")

# two months
combo_2 <- c("3 _ 2012 - 4 _ 2012",
               "2 _ 2016 - 3 _ 2016",
               "3 _ 2016 - 4 _ 2016",
               "5 _ 2017 - 6 _ 2017",
               "4 _ 2013 - 5 _ 2013",
               "4 _ 2017 - 5 _ 2017",
               "1 _ 2007 - 2 _ 2007",
               "12 _ 2007 - 1 _ 2008",
               "1 _ 2008 - 2 _ 2008",
               "12 _ 2008 - 1 _ 2009",
               "1 _ 2009 - 2 _ 2009",
               "12 _ 2009 - 1 _ 2010",
               "1 _ 2010 - 2 _ 2010",
               "12 _ 2010 - 1 _ 2011",
               "1 _ 2011 - 2 _ 2011",
               "4 _ 2016 - 5 _ 2016",
               "2 _ 2013 - 3 _ 2013",
               "2 _ 2014 - 3 _ 2014",
               "2 _ 2015 - 3 _ 2015",
               "2 _ 2017 - 3 _ 2017")

combo_2_dat = month_dat[(month_dat$combo %in% combo_2), ] #630

combo_2_dat$label <- paste(combo_2_dat$month_rel, "_", combo_2_dat$year_rel)
combo_2_dat <- merge(combo_2_dat, pdo[,1:2], by = "label")
colnames(combo_2_dat)[11] <- "rel_pdo"
combo_2_dat <- combo_2_dat[,-1]

combo_2_dat$label <- paste(combo_2_dat$month_end, "_", combo_2_dat$year_end)
combo_2_dat <- merge(combo_2_dat, pdo[,1:2], by = "label")
colnames(combo_2_dat)[12] <- "end_pdo"

combo_2_dat$PDO <- ((combo_2_dat$rel_pdo+combo_2_dat$end_pdo)/2)

# more (should be 56 remaining)
combo_3 <- c("12 _ 2007 - 2 _ 2008",
               "1 _ 2008 - 3 _ 2008",
               "12 _ 2008 - 2 _ 2009",
               "1 _ 2009 - 3 _ 2009",
               "12 _ 2009 - 2 _ 2010",
               "1 _ 2010 - 3 _ 2010",
               "1 _ 2010 - 4 _ 2010",
               "1 _ 2011 - 3 _ 2011",
               "2 _ 2017 - 4 _ 2017")

combo_3_dat = month_dat[(month_dat$combo %in% combo_3), ] #48 (8 NAs from JSATS)

# wasn't sure how else to do this
combo_3_pdo <- read.csv("data/enviro_data/combo_3_pdo.csv")
combo_3_dat <- merge(combo_3_dat, combo_3_pdo, by = "combo")


pdo_all <- rbind(combo_1_dat[,c(2,3,10)], combo_2_dat[,c(2,3,13)], combo_3_dat[,c(2,3,14)])

model_df_pdo <- merge(sd_meta[,-1], pdo_all[,c(1,3)], by = "FishID", all.x = TRUE)

write.csv(model_df_pdo, "results/SD/model_dat.csv")

# add transport distance fish size and release temperature
# already organized for JSTAS, MJ and CM (but need to deal with YBUS)
setwd("C:/Users/pgoertler/Documents/fishtrackr")
dat.mj <- read.csv("chiptt_MJ.csv")
dat.mj$FishID = paste(dat.mj$X, "MJ", sep = ".")
head(dat.mj)

dat.mj <- dat.mj[,-c(1,11)]
dat.mj$Transport_distance <- ifelse(dat.mj$Release_Group_SAIL == "Middle Sacramento River", 252.667,
                                    ifelse(dat.mj$Release_Group_SAIL == "Tidal Delta", 230.136, "NA" ))

colnames(dat.mj)[12] <- "River_temp"

setwd("C:/Users/pgoertler/OneDrive - deltacouncil/AT Diversity ms/DFA/study-detections")
dat.cm <- read.csv("travel.time_CM.Vemco_v3.csv", check.names = FALSE)
head(dat.cm)

colnames(dat.cm)[15] <- "FL"
colnames(dat.cm)[18] <- "Transport_distance"
colnames(dat.cm)[19] <- "River_temp"

dat.jsats <- read.csv("travel.time.jsats.FIN_v2.csv")
head(dat.jsats)

dat.jsats$TagID <- "NA"
colnames(dat.jsats)[9] <- "FL"

setwd("C:/Users/pgoertler/Documents/YBUS")
ybus_fl_wt <- read.csv("ds1066_table5.csv")
ybus_temp_transp <- read.csv("Yolo water temp summary_9 Aug 2021.csv")

ybus <- subset(model_dat, Year == 2016 & TagType == "Vemco")
Tag.ID <- ybus$TagID
ybus_fl_wt_subset = ybus_fl_wt[(ybus_fl_wt$Tag.ID %in% Tag.ID), ]
length(unique(ybus_fl_wt_subset$Tag.ID)) #662
ybus_fl_wt_subset$check <- duplicated(ybus_fl_wt_subset$Tag.ID) # 39196 is duplicated

subset(ybus_fl_wt, Tag.ID == 39196) # two weights and two lengths, but very close

ybus_fl_wt_subset <- subset(ybus_fl_wt_subset, check == FALSE)

unique(ybus_temp_transp$Release.location)
unique(ybus_fl_wt_subset$Release.location)

ybus_temp_transp$Release_location <- ifelse(ybus_temp_transp$Release.location == "Tisdale", "TISDALE",
                                            ifelse(ybus_temp_transp$Release.location == "Tisdale ", "TISDALE",
                                                   ifelse(ybus_temp_transp$Release.location == "Tule Canal at I-5", "TULE CANAL at I-5",
                                                          ifelse(ybus_temp_transp$Release.location =="Yolo", "YOLO BYPASS",
                                                                 ifelse(ybus_temp_transp$Release.location == "Verona", "VERONA", NA)))))

ybus_temp_transp$Tag.date <- as.Date(ybus_temp_transp$rel_date)
colnames(ybus_fl_wt_subset)[6] <- "Release_location"
ybus_fl_wt_subset$Tag.date <- as.Date(ybus_fl_wt_subset$Tag.date)

unique(ybus_fl_wt_subset[,c(4,6)])
unique(ybus_temp_transp[,c(9,10)])

unique(ybus_fl_wt_subset$Release.ID)
unique(ybus_temp_transp$Subrelease)

ybus_fl_wt_temp_transp <- merge(ybus_fl_wt_subset, ybus_temp_transp, by.x = "Release.ID", by.y = "Subrelease", all.x = TRUE)

colnames(ybus_fl_wt_temp_transp)[2] <- "TagID"
colnames(ybus_fl_wt_temp_transp)[20] <- "Transport_distance"
colnames(ybus_fl_wt_temp_transp)[19] <- "River_temp"

ybus_fishID <- unique(ybus[,c(2,11)])
ybus_fl_wt_temp_transp <- merge(ybus_fl_wt_temp_transp,ybus_fishID, by = "TagID")

missing_meta <- rbind(dat.mj[,c(1,10:12,14,15)], dat.cm[,c(2,13,14,15,18,19)], dat.jsats[,c(2,9:12,16)], ybus_fl_wt_temp_transp[,c(1,9,10,19,20,23)])

#half_done <- merge(model_df_pdo, missing_meta, by = "FishID", all.x = TRUE)
#half_done$compare <- (half_done$TagID.x == half_done$TagID.y)# check that MJ's fishIDs didnt change across versions

#check_ybus <- subset(half_done, is.na(Weight)==TRUE) #yup, only missing ybus
#296.16 in MJ data
# Coleman fish hatchery to release loc (Liedtke and Hurst (2017))

model_dat_complete <- merge(model_dat, missing_meta, by = "FishID", all.x = TRUE)
model_dat_complete <- model_dat_complete[,-c(2,24)]
colnames(model_dat_complete)[10] <- "TagID"

write.csv(model_dat_complete, "results/SD/model_dat.csv")
