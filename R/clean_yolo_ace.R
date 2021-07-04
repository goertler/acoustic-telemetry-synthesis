# Clean Yolo and ACE detections, 2013
# M. Johnston
# Sat Jul  3 20:26:27 2021 ------------------------------
source("R/utils.R")
# data origins: from Myfanwy's 2018 publication
#-------------------------------------------------------#
retidy_data = FALSE # set to true if you want to re-make

if(retidy_data | !file.exists("data_clean/yoloace_dfa_detects.rds")){

  reldet = readRDS("data/detection_data/YoloACE_2012-2013MEJ.rds") # JuvSalmon_Manuscript/data/allfishtraveltime_starterdf.rds

chp = reldet[reldet$Station == "Chipps", ] # fish detected @chipps

tapply(chp$TagID, chp$TagGroup, len) # should be 30 fish in 2012, 33 in 2013

d = subset(reldet, TagID %in% chp$TagID)

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
dmnames = tidyr::separate(dm, Name_corr, into = c("Nm1", "Nm2"), sep = "-")
dmnames = unique(dmnames$Nm2)

stopifnot(len(d$GEN) == sum(unique(d$GEN) %in% unique(dmnames))) # all recs should be in dmnames
stopifnot(length(setdiff(unique(d$GEN), unique(dmnames))) == 0) # double-check
setdiff(unique(dmnames), unique(d$GEN)) # MS15 is in the distance matrix but not in d; that's okay

d$FishID = d$TagID # for the dpd function

saveRDS(d, "data_clean/yoloace_dfa_detects.rds")
}
