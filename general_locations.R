# final location data
library(dplyr)

# clean_yolo_ace_dets.R
d <- readRDS("data_clean/YoloAce/yoloace_dfa_detects.rds")
# clean_ybus_data.R
ybus2 <- readRDS("data_clean/YBUS/ybus_clean.rds")
# clean_JSATS_dets.R
ans3 <- readRDS("data_clean/JSATS/jsats_detects2013-2017.rds")
# clean_CMVemco_dets.R
v2 <- readRDS("data_clean/CMVemco/v2.rds")

length(unique(d$GEN)) #35
length(unique(ybus2$GEN)) #30
length(unique(ans3$GEN)) # 100
length(unique(v2$GEN)) #119

vemco <- c(unique(v2$GEN), unique(d$GEN), unique(ybus2$GEN))
length(unique(vemco))

DateTime_PST

v2$year <- format(as.Date(v2$DateTime_PST, format="%d/%m/%Y"),"%Y")
d$year <- format(as.Date(d$DateTime_PST, format="%d/%m/%Y"),"%Y")
ybus2$year <- format(as.Date(ybus2$DateTime_PST, format="%d/%m/%Y"),"%Y")


ans3$year <- format(as.Date(ans3$DateTime_PST, format="%d/%m/%Y"),"%Y")

location <- rbind(unique(v2[,c(11,13)]), unique(ans3[,c(4,10)]), unique(ybus2[,c(3,5)]), unique(d[,c(11,13)]))
location$tag_type <- c(rep("Vemco", length = 373), rep("JSATS", length = 272), rep("Vemco", length = 77))
location$study <-  c(rep("CM", length = 373), rep("JSATS", length = 272), rep("YBUS", length = 30), rep("MJ", length = 47))

# 1320 in shiny app currently (needs to be 722)

# data in 2021 shiny
rec<- read.csv("R/shiny_app/data/rec4shiny_2021.csv")
rel<- read.csv("R/shiny_app/data/rel4shiny_2021.csv")

length(setdiff(rec$GEN, location$gen)) #240
length(setdiff(location$gen, rec$GEN)) #0 (ybus all NAs in GEN for rec)
check <- unique(v2[,c(7,8,11,13)])

# I made all the vemco distance matrices, so I have the lat/lon... JSATS is the one to check first
JSATS_loc <- subset(rec, Type == "JSATS")
length(setdiff(ans3$GEN, JSATS_loc$GEN)) #22
length(setdiff(JSATS_loc$GEN, ans3$GEN)) #15

JSATS_dects <- unique(ans3[,c(4,10)])
colnames(JSATS_dects)[2] <- "Year"
JSATS_loc$Year <- as.character(JSATS_loc$Year)

# fix names
JSATS_loc[JSATS_loc$GEN == "I80-50_Br", "GEN"] <- "I80_Br"
JSATS_loc[JSATS_loc$GEN == "BeniciaW", "GEN"] <- "Benicia"
JSATS_loc[JSATS_loc$GEN == "BlwGeorg_1", "GEN"] <- "BlwGeorgiana"
JSATS_loc[JSATS_loc$GEN == "GeorgSl_1", "GEN"] <- "Georgiana_Slough"
JSATS_loc[JSATS_loc$GEN == "RBDD1", "GEN"] <- "RedBluffDivDam"
JSATS_loc[JSATS_loc$GEN == "RBDD2", "GEN"] <- "RedBluffDivDam"
JSATS_loc[JSATS_loc$GEN == "FreeportDiv", "GEN"] <- "Freeport"

JSATS_dataset <- JSATS_loc %>% right_join(JSATS_dects, by=c("GEN","Year")) # fine - have all lat/lon for those used in distance matrix (by GEN and year)
length(unique(JSATS_dataset$GEN)) #91
length(setdiff(ans3$GEN, JSATS_dataset$GEN)) #17

# separate out release sites
rel_jsats <- subset(rel, Type == "JSAT")
length(unique(rel_jsats$Release.Location)) #12

#"FR_Boyds_Rel"    == "FR Boyds Release"
#"FR_Gridley_Rel"    == "Gridley_Rel"
#"Sac_Ist_Bridge_Rel"
#"SutterBypass_Weir2_RST_Rel" == "SutterBypass Weir2 RST"
#"AR_Sunrise_Ramp_Rel"   == "AR_Sunrise_Ramp"
#"RBDD_Rel"

# missing locations - had to get back into raw github data
# Riverbank_Marina_Rel 38.60350 -121.5191
# Bonnyview_Rel 40.53758 -122.0321

JSATS_dataset$LAT <- ifelse(JSATS_dataset$GEN == "Blw_ClearCr", 40.49729, JSATS_dataset$LAT)
JSATS_dataset$LAT <- ifelse(JSATS_dataset$GEN == "Blw_Cypress", 40.56015, JSATS_dataset$LAT)
JSATS_dataset$LAT <- ifelse(JSATS_dataset$GEN == "BoydsPump", 39.05180, JSATS_dataset$LAT)
JSATS_dataset$LAT <- ifelse(JSATS_dataset$GEN == "FR_Goose", 39.40632, JSATS_dataset$LAT)
JSATS_dataset$LAT <- ifelse(JSATS_dataset$GEN == "FR_Verona", 38.81675, JSATS_dataset$LAT)
JSATS_dataset$LAT <- ifelse(JSATS_dataset$GEN == "Hwy 44 Bridge", 40.58691, JSATS_dataset$LAT)
# Blw_ClearCr 40.49729 -122.3573
# Blw_Cypress 40.56015 -122.3742
# BoydsPump 39.05180 -121.6104
# FR_Goose 39.40632 -121.6164
# FR_Verona 38.81675 -121.6385
# Hwy 44 Bridge  40.58691 -122.3689
JSATS_dataset$LON <- ifelse(JSATS_dataset$GEN == "Blw_ClearCr", -122.3573, JSATS_dataset$LON)
JSATS_dataset$LON <- ifelse(JSATS_dataset$GEN == "Blw_Cypress", -122.3742, JSATS_dataset$LON)
JSATS_dataset$LON <- ifelse(JSATS_dataset$GEN == "BoydsPump", -121.6104, JSATS_dataset$LON)
JSATS_dataset$LON <- ifelse(JSATS_dataset$GEN == "FR_Goose", -121.6164, JSATS_dataset$LON)
JSATS_dataset$LON <- ifelse(JSATS_dataset$GEN == "FR_Verona", -121.6385, JSATS_dataset$LON)
JSATS_dataset$LON <- ifelse(JSATS_dataset$GEN == "Hwy 44 Bridge", -122.3689, JSATS_dataset$LON)

# missing year info, but data exists
JSATS_dataset$LAT <- ifelse(JSATS_dataset$GEN == "Abv_WoodsonBr", 39.91819, JSATS_dataset$LAT)
JSATS_dataset$LAT <- ifelse(JSATS_dataset$GEN == "BC_Beach", 38.88226, JSATS_dataset$LAT)
JSATS_dataset$LAT <- ifelse(JSATS_dataset$GEN == "Benicia", 38.04494, JSATS_dataset$LAT)
JSATS_dataset$LAT <- ifelse(JSATS_dataset$GEN == "Blw_IrvineFinch", 39.73233, JSATS_dataset$LAT)
JSATS_dataset$LAT <- ifelse(JSATS_dataset$GEN == "Blw_Paynes_Ck", 40.25460, JSATS_dataset$LAT)
JSATS_dataset$LAT <- ifelse(JSATS_dataset$GEN == "Blw_Salt", 40.14556, JSATS_dataset$LAT)
JSATS_dataset$LAT <- ifelse(JSATS_dataset$GEN == "Freeport", 38.46467, JSATS_dataset$LAT)
JSATS_dataset$LAT <- ifelse(JSATS_dataset$GEN == "KnightsLandingBr", 38.80260, JSATS_dataset$LAT)
JSATS_dataset$LAT <- ifelse(JSATS_dataset$GEN == "MillCk2", 40.04606, JSATS_dataset$LAT)
JSATS_dataset$LAT <- ifelse(JSATS_dataset$GEN == "ShanghaiBend", 39.09114, JSATS_dataset$LAT)
JSATS_dataset$LAT <- ifelse(JSATS_dataset$GEN == "StarBend", 39.00988, JSATS_dataset$LAT)
JSATS_dataset$LAT <- ifelse(JSATS_dataset$GEN == "TowerBridge", 38.58046, JSATS_dataset$LAT)

JSATS_dataset$LON <- ifelse(JSATS_dataset$GEN == "Abv_WoodsonBr", -122.0861, JSATS_dataset$LON)
JSATS_dataset$LON <- ifelse(JSATS_dataset$GEN == "BC_Beach", -121.6140, JSATS_dataset$LON)
JSATS_dataset$LON <- ifelse(JSATS_dataset$GEN == "Benicia", -122.1269, JSATS_dataset$LON)
JSATS_dataset$LON <- ifelse(JSATS_dataset$GEN == "Blw_IrvineFinch", -121.9729, JSATS_dataset$LON)
JSATS_dataset$LON <- ifelse(JSATS_dataset$GEN == "Blw_Paynes_Ck", -122.1709, JSATS_dataset$LON)
JSATS_dataset$LON <- ifelse(JSATS_dataset$GEN == "Blw_Salt", -122.1510, JSATS_dataset$LON)
JSATS_dataset$LON <- ifelse(JSATS_dataset$GEN == "Freeport", -121.5050, JSATS_dataset$LON)
JSATS_dataset$LON <- ifelse(JSATS_dataset$GEN == "KnightsLandingBr", -121.7200, JSATS_dataset$LON)
JSATS_dataset$LON <- ifelse(JSATS_dataset$GEN == "MillCk2", -122.0944, JSATS_dataset$LON)
JSATS_dataset$LON <- ifelse(JSATS_dataset$GEN == "ShanghaiBend", -121.5955, JSATS_dataset$LON)
JSATS_dataset$LON <- ifelse(JSATS_dataset$GEN == "StarBend", -121.5938, JSATS_dataset$LON)
JSATS_dataset$LON <- ifelse(JSATS_dataset$GEN == "TowerBridge", -121.5087, JSATS_dataset$LON)

JSATS_dataset$Type <- "JSATS"
JSATS_dataset <- JSATS_dataset[,-c(1:2)]
check_jsat <- JSATS_dataset[rowSums(is.na(JSATS_dataset)) > 0,] # all release sites
JSATS_dataset$study <- "JSATS"

# ybus la/lon by name instead of receiver number
ybus_loc <- read.csv("R/shiny_app/data/Lat_lon_YBUS.csv")
dim(ybus_loc)

length(setdiff(ybus2$GEN,ybus_loc$Location)) #0
ybus_gen <- unique(ybus2$GEN)
ybus_loc_4shiny <- ybus_loc[ybus_loc$Location %in% ybus_gen, ]
colnames(ybus_loc_4shiny)[2] <- "GEN"
ybus_loc_4shiny$Year <- 2016
ybus_loc_4shiny$study <- "YBUS"
ybus_loc_4shiny$Type <- "Vemco"
colnames(ybus_loc_4shiny)[3] <- "LAT"
colnames(ybus_loc_4shiny)[4] <- "LON"
ybus_loc_4shiny <- ybus_loc_4shiny[,-1]

# MJ's locations
mj_loc_12 <- subset(rec, Year == 2012 & Type == "Vemco")
mj_loc_13 <- subset(rec, Year == 2013 & Type == "Vemco")
mj_loc <- rbind(mj_loc_12, mj_loc_13)
setdiff(unique(d$GEN), unique(mj_loc$GEN)) # "Yolo_Release"       "Sacramento_Release"
setdiff(unique(mj_loc$GEN), unique(d$GEN)) #  "MS15"

mj_detects <- unique(d[,c(7,11)])
mj_dataset <- mj_loc %>% right_join(mj_detects, by=c("GEN","Year")) # fine - have all lat/lon for

# locations missing for 2013, but data is there for 2012
mj_dataset$LAT <- ifelse(mj_dataset$GEN == "SacMouth", 38.17392, mj_dataset$LAT)
mj_dataset$LAT <- ifelse(mj_dataset$GEN == "Steamboat", 38.28498, mj_dataset$LAT)
mj_dataset$LAT <- ifelse(mj_dataset$GEN == "Sutter", 38.32860, mj_dataset$LAT)
mj_dataset$LAT <- ifelse(mj_dataset$GEN == "Georgiana", 38.23725, mj_dataset$LAT)

mj_dataset$LON <- ifelse(mj_dataset$GEN == "SacMouth", -121.6483, mj_dataset$LON)
mj_dataset$LON <- ifelse(mj_dataset$GEN == "Steamboat", -121.5868, mj_dataset$LON)
mj_dataset$LON <- ifelse(mj_dataset$GEN == "Sutter", -121.5778, mj_dataset$LON)
mj_dataset$LON <- ifelse(mj_dataset$GEN == "Georgiana", -121.5176, mj_dataset$LON)

mj_loc_4shiny <- mj_dataset[,-c(1:2)]
mj_loc_4shiny$Type <- "Vemco"
mj_loc_4shiny <- na.omit(mj_loc_4shiny)

length(unique(mj_loc_4shiny$GEN)) #33 is correct, because 2 are release locations
mj_loc_4shiny$study <- "YoloAce"

# CM's data
cm_dects <- unique(v2[,c(11,13)])
cm_loc <- subset(rec, Year <= 2011 & Type == "Vemco")

setdiff(unique(v2$GEN), unique(cm_loc$GEN)) #6
# "BtlCkAbatPnd"       "SacJellyRamp"       "SacIrvineFinchRamp" "SacButteCityRamp"  are release locations
# "Richmond Bridge"    "MRGC"  are the same and west of Benicia
setdiff(unique(cm_loc$GEN), unique(v2$GEN))
colnames(cm_dects)[2] <- "Year"
cm_loc$Year <- as.character(cm_loc$Year)
cm_dataset <- cm_loc %>% right_join(cm_dects, by=c("GEN","Year"))
cm_loc_4shiny <- na.omit(cm_dataset)
cm_loc_4shiny <- cm_loc_4shiny[,-c(1:2)]
cm_loc_4shiny$study <- "CM"

rec4shiny_2023 <- rbind(mj_loc_4shiny, ybus_loc_4shiny, cm_loc_4shiny, JSATS_dataset)

write.csv(rec4shiny_2023, "rec4shiny_2023.csv")
