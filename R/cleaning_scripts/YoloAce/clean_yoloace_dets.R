# Clean Yolo and ACE detections, 2012-2013
# M. Johnston
# Thu Feb 24 21:32:12 2022 ------------------------------
source("R/utils.R")
# data origins: from Myfanwy's 2018 publication
#-------------------------------------------------------#

y = readRDS("data_clean/YoloAce/yolo2012-2013_dets.rds")
y$Year = lubridate::year(y$DateTimeUTC)
y$RelLoc = "Yolo"
y = dplyr::rename(y, Rkm = RKM)
ace = readRDS("data_clean/YoloAce/ace2012-2013_dets.rds")
ace$Year = lubridate::year(ace$DateTimeUTC)
ace$RelLoc = "Sac"
  
reldet= rbind(y, ace)
reldet$TagGroup = paste0(reldet$RelLoc,reldet$Year)
  
chp = reldet[reldet$Station == "Chipps", ] # fish detected @chipps

tapply(chp$TagID, chp$TagGroup, len) # should be 30 fish in 2012, 33 in 2013

d = reldet
d$DateTime_PST = lubridate::with_tz(d$DateTimeUTC, 
                                       tz = "Etc/GMT+8")

d$GEN = d$Station
d$GEN[d$GEN %in% c("BCE", "BCW")] <- "BC_joint"
d$GEN[d$GEN %in% c("RM88_R", "RM88_L")] <- "RM88"
d$GEN[d$GEN %in% c("RM71_L", "RM71_R")] <- "RM71"
d[d$GEN == "I80_1", "GEN"] <- "I_80_1"
d[d$GEN == "BaseSac", "GEN"] <- "SacMouth"
d[d$GEN %in% c("BC2_joint", "BC_joint2", "BC_joint3"), "GEN"] <- "MS16"
d[d$GEN == "TopofSutter", "GEN"] <- "Sutter"
d[d$GEN == "BlwGeorg", "GEN"] <- "MS14"

# differentiate release stations

## Sac
d$GEN[d$GEN == "Release" &
        d$TagGroup %in% c("Sac2012", "Sac2013")] <- "Sacramento_Release"

## Yolo
d$GEN[d$GEN == "Release" & d$TagGroup %in% c("Yolo2012",
                                             "Yolo2013")] <- "Yolo_Release"

# load distance matrix 
dm = read.csv("data/distance_matrices/Distance_Matrix_MJ_corr_mean.csv")
dmnames = tidyr::separate(dm, Name_corr, into = c("Nm1", "Nm2"), sep = "-")
dmnames = unique(dmnames$Nm2)

stopifnot(len(d$GEN) == sum(unique(d$GEN) %in% unique(dmnames))) # all recs should be in dmnames
stopifnot(length(setdiff(unique(d$GEN), unique(dmnames))) == 0) # double-check
setdiff(unique(dmnames), unique(d$GEN)) # MS15 is in the distance matrix but not in d; that's okay

d$FishID = d$TagID # for the dpd function

saveRDS(d, "data_clean/YoloAce/yoloace_dfa_detects.rds")
