#-------------------------------------------------------#
# Fix GEN locations in all_detects
# M. Johnston - from T. Grimes & C. Michel's code; has not been altered
# DFA analysis
# Fri Jul 24 14:01:50 2020 ------------------------------

# this script gets sourced in line 30 of the DFA_dataorg_distmatrix_MEJ.R script
# calls: R/clean_tagging_metadata
source("R/utils.R")
#-------------------------------------------------------#
if(FALSE){
# OLD METHOD: build from raw detection files
  ## Load detections
temp = list.files(path = "data/detection_data/study-detections-JSATS", 
                  full.names = TRUE, 
                  pattern="*.csv") 

## Bind all detections into one mother dataframe
all_detects <- do.call("rbind", lapply(temp, 
                                       read.csv, 
                                       stringsAsFactors = FALSE))

# fix date formats
all_detects$DetectDate <- as.POSIXct(all_detects$DateTime_PST, 
                                       tz = "Etc/GMT+8", 
                                      format = "%m/%d/%Y %H:%M:%OS")
}
#-------------------------------------------------------#

# NEW METHOD: use Pascale's pre-made .csv (not uploaded to github b/c 550MB)
all_detects = data.table::fread("data/detection_data/all_JSATS.csv")
all_detects = as.data.frame(all_detects)

all_detects$DetectDate = as.Date(as.POSIXct(all_detects$DateTime_PST,
                                            tz = "Etc/GMT+8",
                                            format = "%m/%d/%Y %H:%M:%OS"))


all_detects$DateTime_PST = as.POSIXct(all_detects$DateTime_PST, 
                                       tz = "Etc/GMT+8", 
                                      format = "%m/%d/%Y %H:%M:%OS")

csn(all_detects)

# load and clean tagging metadata
source("R/clean_tagging_metadata.R") # creates tagging_meta object

## Merge in pertinent data for grouping by runs/years
all_detects <-
  merge(all_detects, tagging_meta[, c("FishID", 
                                      "StudyID", 
                                      "Fish_Type", 
                                      "Rel_loc", 
                                      "Rel_rkm")], 
        by = "FishID",
        all.x = TRUE)

# Get rid of unused columns
all_detects  = all_detects[ , c("FishID",
                                "DateTime_PST",
                                "StudyID",
                                "GEN",
                                "RKM",
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
all_detects[which(all_detects$GEN == "ChippsE"), "GEN"] <- "ChippsW" # this is how it is in the dist matrix
all_detects[which(all_detects$GEN == "GoldenGateE"), "GEN"] <- "GoldenGateW"

## remove mokbase since it is one site we don't have distance matrix for
all_detects <- all_detects[which(all_detects$GEN != "MokBase"),]

all_detects$Year = lubridate::year(all_detects$DateTime_PST)
len(all_detects$FishID[all_detects$GEN == "Benicia" & all_detects$Year == 2013])

#-------------------------------------------------------#
# QAQC
#-------------------------------------------------------#

# check for duplicated tagIDs across studies:
stopifnot(anyDuplicated(tagging_meta$FishID) == 0)

# check for simultaneous detections within fish and locations
i = duplicated(all_detects) # 5848 duplicate rows
i2 = duplicated(all_detects, fromLast = TRUE)

dups = all_detects[i,]
dups2 = all_detects[i2, ]

alldups = rbind(dups, dups2)
alldups = alldups[order(alldups$FishID, alldups$DateTime_PST), ]

# remove duplicate detections:
ans = all_detects[!i,]

# TagIDs with suspicious detections (detected before released)
chk = c("CFC2012-007", "CFC2012-018", "CFC2012-019", "CFC2012-020", 
"CFC2012-022", "CFC2012-023", "Delta2012-001", "Delta2012-002", 
"Delta2012-003", "Delta2012-021", "Delta2012-031", "Delta2012-034", 
"Delta2012-039", "Delta2012-052", "Delta2012-054", "Delta2012-080", 
"Delta2012-087", "Delta2012-090", "Delta2012-095", "Delta2012-097", 
"Delta2012-098", "Delta2012-108")

sum(ans$FishID %in% chk) # none of these are kept for the DFA analysis, but we'll remove the false dets just in case:

ans2 <-
  merge(ans, tagging_meta[, c("FishID", "Rel_datetime")], 
        by = "FishID",
        all.x = TRUE)

library(dplyr)

ans2 %>% 
  group_by(FishID) %>% 
  filter(DateTime_PST < Rel_datetime) %>% 
  ungroup() -> too_early

too_early
length(chk)
sum(chk %in% unique(too_early$FishID))

too_early = data.frame(too_early)

ans3 = anti_join(ans2, too_early)
stopifnot(nrow(ans2) - nrow(too_early) == nrow(ans3))


#-------------------------------------------------------#
# Subset down to the years and fish we need for the DFA:

ids = read.csv("data/tagging_data/dat4Von.csv", stringsAsFactors = FALSE)

ans3 = ans3[ans3$Year %in% c(2013, 2016, 2017), ]

sum(unique(ans3$FishID) %in% ids$FishID[ids$TagType == "JSATS"])
sum(ans3$FishID %in% chk) # 0


# write.csv(data.frame(FishID = DFAids), "results/JSATS_FishIDs_for_DFA_analysis.csv", 
#           row.names = FALSE) # for Pascale

# find tracks where fish go backwards
ans3 = ans3[order(ans3$FishID, ans3$DateTime_PST), ]

fp = tagtales::tag_tales(ans3, ans3$FishID, ans3$GEN, "DateTime_PST")

plot_track(fp, fp$FishID[7])
saveRDS(fp, "data_clean/jsats_dfa_detects_fishpaths.rds")
saveRDS(ans3, "data_clean/jsats_dfa_detects.rds")


#########################################################


