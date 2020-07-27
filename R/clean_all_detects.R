#-------------------------------------------------------#
# Fix GEN locations in all_detects
# M. Johnston - from T. Grimes & C. Michel's code; has not been altered
# DFA analysis
# Fri Jul 24 14:01:50 2020 ------------------------------

# this script gets sourced in line 30 of the DFA_dataorg_distmatrix_MEJ.R script

# calls: R/clean_tagging_metadata

## Load detections
temp = list.files(path = "data/detection_data/study-detections-JSATS", full.names = T, pattern="*.csv") 

## Bind all detections into one mother dataframe
all_detects <- do.call("rbind", lapply(temp, 
                                       read.csv, 
                                       stringsAsFactors = FALSE))

all_detects$DetectDate <- as.POSIXct(all_detects$DateTime_PST, 
                                       tz = "Etc/GMT+8", 
                                      format = "%m/%d/%Y %H:%M:%OS")

# load and clean tagging metadata
source("R/clean_tagging_metadata.R") # creates tagging_meta object

## Merge in pertinent data for grouping by runs/years
all_detects <-
  merge(all_detects, tagging_meta[, c("FishID", 
                                      "StudyID", 
                                      "Fish_Type", 
                                      "Rel_loc", 
                                      "Rel_rkm")], 
        by = "FishID")

# Get rid of unused columns
all_detects  = all_detects[ , c("FishID",
                                "TagID",
                                "DateTime_PST",
                                "StudyID",
                                "RecSN",
                                "GEN",
                                "RKM",
                                "DetectDate",
                                "Fish_Type",
                                "Rel_loc",
                                "Rel_rkm"
                                )]


## Remove detections at some key locations which seem to have a lot of false detects
all_detects <-
  all_detects[!all_detects$GEN %in% c(
    "Battle_Conf",
    "ButteBr",
    "AbvColusaBrRT",
    "BattleCk2",
    "BattleCk3",
    "Sac_Ist_Bridge_Rel",
    "MillCk2_Rel",
    "FR_Vance_Rel",
    "MinerNorth"
  ), ] # added Sac_Ist_Bridge_Rel,MillCk2_Rel,and FR_Vance_Rel because they are not in the distance matrix

## Remove detections for certain fish, seems to have bad data (maybe 2 tags in system with that tag code? Or just false detects)
all_detects <- all_detects[!all_detects$FishID %in% c(
  "CFC2013-080",
  "CFR2016-079"
  ), ]

## Now fix some GEN loc names that don't exactly match in detection files vs dist matrices
all_detects[which(all_detects$GEN == "BattleCk_RST"), "GEN"] <-
  "BattleCk_RST_Rel"
all_detects[which(all_detects$GEN == "FR Gridley Release"), "GEN"] <-
  "Gridley_Rel"
all_detects[which(all_detects$GEN == "RBDD Release"), "GEN"] <-
  "RedBluffDivDam"
## Fixing more GEN loc names that don't match
all_detects[which(all_detects$GEN == "MillCk_RST"), "GEN"] <-
  "MillCk_RST_Rel"
all_detects[which(all_detects$GEN == "I80-50_Br"), "GEN"] <-
  "I80_Br"
all_detects[which(all_detects$GEN == "BeniciaW"), "GEN"] <-
  "Benicia"
all_detects[which(all_detects$GEN == "BlwGeorg_1"), "GEN"] <-
  "BlwGeorgiana"
all_detects[which(all_detects$GEN == "SutterBypass_Weir2_RST_Rel"), "GEN"] <-
  "SutterBypass Weir2 RST"
all_detects[which(all_detects$GEN == "GeorgSl_1"), "GEN"] <-
  "Georgiana_Slough"
all_detects[which(all_detects$GEN == "FR_Gridley_Rel"), "GEN"] <-
  "Gridley_Rel"
all_detects[which(all_detects$GEN == "RBDD_Rel"), "GEN"] <-
  "RedBluffDivDam"
all_detects[which(all_detects$GEN == "RBDD1"), "GEN"] <-
  "RedBluffDivDam"
all_detects[which(all_detects$GEN == "RBDD2"), "GEN"] <-
  "RedBluffDivDam"
all_detects[which(all_detects$GEN == "FR_Boyds_Rel"), "GEN"] <-
  "FR Boyds Release"
all_detects[which(all_detects$GEN == "Tower_Bridge_Rel"), "GEN"] <-
  "TowerBridge"
all_detects[which(all_detects$GEN == "AR_Sunrise_Ramp_Rel"), "GEN"] <-
  "AR_Sunrise_Ramp"
all_detects[which(all_detects$GEN == "DeerCk_RST_Rel"), "GEN"] <-
  "DeerCk_RST"
all_detects[which(all_detects$GEN == "FreeportDiv"), "GEN"] <-
  "Freeport"
## combine certain locations where receivers are too close together
all_detects[which(all_detects$GEN == "ChippsE"), "GEN"] <- "ChippsW"

## remove mokbase since it is one site we don't have distance matrix for
all_detects <- all_detects[which(all_detects$GEN != "MokBase"),]

# Prepare for calculating distances by detection order
all_detects <-
  all_detects[order(all_detects$FishID, 
                    all_detects$DetectDate), ] 

# prepare lagged columns
all_detects$prev_FishID <- NA
all_detects$prev_GEN <- NA
all_detects$prev_DetectDate <- NA


#########################################################
# test lag shift
tt = subset(all_detects, FishID %in% c("ARF2016-114", "SB2017-167", "WR2017-555"))
tt[, c("prev_FishID", "prev_GEN", "prev_DetectDate")] <- # populate this with...
  as.data.frame(
      data.table::shift(
           x = tt[, c("FishID", "GEN", "DetectDate")], # this, lagged by 1
           n = 1,
          fill = NA,
          type = "lag"
                        )
                )

tt = tt[ , c(1, 6, 8, 12:14)]

# this should give us only the first detections of each fish
ttfd <-
  tt[which(
    tt$FishID != tt$prev_FishID |
      tt$GEN != tt$prev_GEN
  ),]   #  first detections or first detections at a new receiver; by fish, each row should have a different receiver than the last one


# make movement column; this is needed in order to merge with the distance matrix later.
ttfd$movement <-
  paste(ttfd$prev_GEN, "-", 
        ttfd$GEN, sep = " ")

library(dplyr)
route <- route %>%
  select(FishID, Route) %>%
  distinct()

## Joining the routing determination to the detection data
## and removing fish that have no routing determination

first_detects_routes <-
  left_join(ttfd, route, by = "FishID") %>%
  filter(!is.na(Route))

fdr <-
  merge(
    first_detects_routes,
    dist_DCC_Yolo_Tisd_closed[, c("Name", "Total_Length_m")], # only merging the total length
    by.x  = "movement",
    by.y = "Name",
    all.x = TRUE # we're keeping all rows from x, not y
  )

#/end test lag shift

#########################################################
### shifts the three columns down by 1; the first row will be an NA
all_detects[, c("prev_FishID", "prev_GEN", "prev_DetectDate")] <- # populate this with...
  as.data.frame(
      data.table::shift(
           x = all_detects[, c("FishID", "GEN", "DetectDate")], # this, lagged by 1
           n = 1,
          fill = NA,
          type = "lag"
                        )
                )
