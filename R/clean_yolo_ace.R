# Clean Yolo and ACE detections, 2013
# M. Johnston
# Fri May 21 12:04:52 2021 ------------------------------
source("R/utils.R")
# data origins: from Myfanwy's 2018 publication; for convenience, pulled from the fishtrackr package:
#jt = fishtrackr::alljuvtt
#saveRDS(jt, "data/detection_data/YoloACE_2012-2013MEJ.rds")
# sac13 data is from JuvSalmon_Manuscript/munge/No_fish_at_Chipps.R
#-------------------------------------------------------#
library(dplyr)

reldet = readRDS("data/detection_data/YoloACE_2012-2013MEJ.rds")
str(reldet)
sort(unique(reldet$Station))
chp = reldet[reldet$Station == "Chipps", ]
tapply(chp$TagID, chp$TagGroup, len)

reldet %>% 
  filter(TagID %in% chp$TagID) -> d

d$DateTime_PST = lubridate::with_tz(d$DateTimeUTC, 
                                       tz = "Etc/GMT+8")

d$GEN = d$Station
d[which(d$GEN == "I80_1"), "GEN"] <- "I_80_1"
d[which(d$GEN == "BaseSac"), "GEN"] <- "SacMouth"
d[which(d$GEN %in% c("BC2_joint", "BC_joint2", "BC_joint3")), "GEN"] <- "MS16"
d[which(d$GEN == "TopofSutter"), "GEN"] <- "Sutter"
d[which(d$GEN == "BlwGeorg"), "GEN"] <- "MS14"

# differentiate release stations

## Sac
d$GEN[d$GEN == "Release" & d$TagGroup %in% c("Sacramento River 2012",
"Sacramento River 2013")] <- "Sacramento_Release"

## Yolo
d$GEN[d$GEN == "Release" & d$TagGroup %in% c("Yolo Bypass 2012",
                                             "Yolo Bypass 2013")] <- "Yolo_Release"

# load distance matrix (using DCC closed only)
dm = read.csv("data/distance_matrices/Distance_Matrix_MJ_corr_mean.csv")
str(dm)
dmnames = tidyr::separate(dm, Name_corr, into = c("Nm1", "Nm2"), sep = "-")
dmnames = unique(dmnames$Nm2)

len(d$GEN)
sum(unique(d$GEN) %in% unique(dmnames))
setdiff(unique(d$GEN), unique(dmnames))
setdiff(unique(dmnames), unique(d$GEN))
sort(unique(dmnames))


saveRDS(d, "data_clean/yoloace_dfa_detects.rds")

