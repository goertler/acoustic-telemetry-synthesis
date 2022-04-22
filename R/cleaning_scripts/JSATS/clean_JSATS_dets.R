#-------------------------------------------------------#
# Fix GEN locations in all_detects
# M. Johnston - re-factored from T. Grimes & C. Michel's code
# DFA analysis
# Thu Mar 3 21:13:30 2022 ------------------------------

source("R/utils.R")
library(dplyr)
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
all_detects = as.data.frame(data.table::fread("data/JSATS/all_JSATS.csv"))

all_detects$DetectDate = as.Date(as.POSIXct(all_detects$DateTime_PST,
                                            tz = "Etc/GMT+8",
                                            format = "%m/%d/%Y %H:%M:%OS"))


all_detects$DateTime_PST = as.POSIXct(all_detects$DateTime_PST, 
                                       tz = "Etc/GMT+8", 
                                      format = "%m/%d/%Y %H:%M:%OS")

#-------------------------------------------------------#
# Subset down to the years and fish we need for the DFA:
all_detects = all_detects[lubridate::year(all_detects$DetectDate) != 2012, ] # we want 2013:2017

## combine certain locations where receivers are too close together
exits = unique(all_detects$FishID[all_detects$GEN %in% c("ChippsE", "ChippsW", "Benicia")]) # only use the fish detected at Chipps/Benicia # 575 fish for DFA analysis

all_detects = all_detects[all_detects$FishID %in% exits, ]

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

# load and clean tagging metadata
#-------------------------------------------------------#

tagging_meta <-
  read.csv("data/JSATS/Tagging_Fish_Metadata.txt",
           stringsAsFactors = F)

tagging_meta$Rel_datetime <-
  as.POSIXct(tagging_meta$Rel_datetime, 
             tz = "Etc/GMT+8", 
             format = "%m/%d/%Y %H:%M:%OS")

stopifnot(length(unique(tagging_meta$FishID)) == nrow(tagging_meta))     # test that each row represents a unique fishID


## Change fish_type "Chinook" to "RBDD Chinook" to be more informative
# original code:
tagging_meta[tagging_meta$Fish_Type == "Chinook" & tagging_meta$StudyID == "RBDD-2017", "Fish_Type"] <- "RBDD Chinook"

## Change Fish_Type "Fall run Chinook" for StudyID "ColemanFall_2016" to more informative "CNFH Fall Chinook"
tagging_meta[tagging_meta$Fish_Type == "Fall run Coleman", "Fish_Type"] <- "CNFH Fall Chinook"

# make sure all analysis fish are in the metadata:
stopifnot(setdiff(all_detects$FishID, tagging_meta$FishID) == 0)

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


## Now fix some GEN loc names that don't exactly match in detection files vs dist matrices
all_detects[all_detects$GEN == "BattleCk_RST", "GEN"] <-
  "BattleCk_RST_Rel"
all_detects[all_detects$GEN == "FR Gridley Release", "GEN"] <-
  "Gridley_Rel"
all_detects[all_detects$GEN == "RBDD Release", "GEN"] <-
  "RedBluffDivDam"
## Fixing more GEN loc names that don't match
all_detects[all_detects$GEN == "MillCk_RST", "GEN"] <-
  "MillCk_RST_Rel"
all_detects[all_detects$GEN == "I80-50_Br", "GEN"] <-
  "I80_Br"
all_detects[all_detects$GEN == "BeniciaW", "GEN"] <-
  "Benicia"
all_detects[all_detects$GEN == "BlwGeorg_1", "GEN"] <-
  "BlwGeorgiana"
all_detects[all_detects$GEN == "SutterBypass_Weir2_RST_Rel", "GEN"] <-
  "SutterBypass Weir2 RST"
all_detects[all_detects$GEN == "GeorgSl_1", "GEN"] <-
  "Georgiana_Slough"
all_detects[all_detects$GEN == "FR_Gridley_Rel", "GEN"] <-
  "Gridley_Rel"
all_detects[all_detects$GEN == "RBDD_Rel", "GEN"] <-
  "RedBluffDivDam"
all_detects[all_detects$GEN == "RBDD1", "GEN"] <-
  "RedBluffDivDam"
all_detects[all_detects$GEN == "RBDD2", "GEN"] <-
  "RedBluffDivDam"
all_detects[all_detects$GEN == "FR_Boyds_Rel", "GEN"] <-
  "FR Boyds Release"
all_detects[all_detects$GEN == "Tower_Bridge_Rel", "GEN"] <-
  "TowerBridge"
all_detects[all_detects$GEN == "AR_Sunrise_Ramp_Rel", "GEN"] <-
  "AR_Sunrise_Ramp"
all_detects[all_detects$GEN == "DeerCk_RST_Rel", "GEN"] <-
  "DeerCk_RST"
all_detects[all_detects$GEN == "FreeportDiv", "GEN"] <-
  "Freeport"

## combine certain locations where receivers are too close together
all_detects[all_detects$GEN == "ChippsE", "GEN"] <- "ChippsW" # this is how it is in the dist matrix
all_detects[all_detects$GEN == "GoldenGateE", "GEN"] <- "GoldenGateW"

## remove mokbase since it is one site we don't have distance matrix for
all_detects <- all_detects[all_detects$GEN != "MokBase",]

#-------------------------------------------------------#
# QAQC
#-------------------------------------------------------#

# check for duplicated tagIDs across studies:
stopifnot(anyDuplicated(tagging_meta$FishID) == 0)

# check for simultaneous detections within fish and locations
i = duplicated(all_detects) 
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

sum(unique(ans$FishID) %in% chk) # none of these are kept for the DFA analysis, but we'll remove the false dets just in case:

ans2 <-
  merge(ans, tagging_meta[, c("FishID", "Rel_datetime")], 
        by = "FishID",
        all.x = TRUE)


ans2 %>% 
  group_by(FishID) %>% 
  arrange(DateTime_PST) %>% 
  filter(DateTime_PST < Rel_datetime) %>% 
  ungroup() -> too_early

length(chk) # 22 fish
sum(chk %in% unique(too_early$FishID)) #16 of them detected prior to tagging

too_early = data.frame(too_early)

# get rid of all the fish that had detections before tagging
ans3 = filter(ans2, !(FishID %in% unique(too_early$FishID)))
len(ans2$FishID) - len(ans3$FishID)

ans3 = ans3[order(ans3$FishID, ans3$DateTime_PST), ]



saveRDS(ans3, "data_clean/JSATS/jsats_detects2013-2017.rds")



# all following code  is to check backwards movements - it is not used to make any of the analysis products
#-------------------------------------------------------#
if(FALSE) {

  # Build/load fishpaths of all JSATs detections:
  #-------------------------------------------------------#
  
  if(!require("tagtales")) devtools::install_github("Myfanwy/tagtales")
  
  fp = tagtales::tag_tales(ans3, ans3$FishID, ans3$GEN, "DateTime_PST")
  
  #-------------------------------------------------------#

# Only check backwards moving fish on the DFA Fish
  fpp = fp[fp$FishID %in% exits,]
  
  test_split = split(fpp, fpp$FishID)
  
  test_split = test_split[sapply(test_split, nrow) > 0]
  bck1 = lapply(test_split, FUN = backwards_onefish)
  
  # fish paths for those fish that go backwards
  bck2 = data.frame(FishID = names(bck1), nrev = as.integer(bck1))
  bck2 = bck2[bck2$nrev > 0 ,]
  
  backtracks = fpp[fpp$FishID %in% bck2$FishID,]
  bsplit = split(backtracks, backtracks$FishID)
  
  get_stns = lapply(
    bsplit,
    FUN = function(x)
      y = x[["GEN"]]
  ) # get station path from each fish
  
  #-------------------------------------------------------#
  loc.rkm <- unique(fpp[, c(4, 5, 7, 8)])
  # break at Freeport OR 153.140
  
  # ID when fish move into tidal
  back.move.fish <-
    data.frame(
      FishID = NA,
      min.time = strptime(NA, format = "%m/%d/%Y %H:%M:%OS", tz = "Etc/GMT+8")
    )
  
  
  for (i in unique(fpp$FishID)) {
    temp.dat <- fpp[fpp$FishID == i, ]
    
    tidal <- subset(temp.dat, RKM <= 153.140)
    
    min.time = min(tidal$DateTime_PST)
    
    temp.results <-
      data.frame(
        FishID = i,
        min.time = as.POSIXct(min.time,
                              
                              tz = "Etc/GMT+8",
                              
                              format = "%m/%d/%Y %H:%M:%OS")
      )
    
    
    
    back.move.fish <- rbind(back.move.fish, temp.results)
    
  }
  
  
  # then ask if RKM is greater than 153 after min.time (TRUE/FALSE)
  
  back.test <- merge(fpp, back.move.fish, by = "FishID")
  
  back.test2 <-
    data.frame(
      FishID = NA,
      DateTime_PST = strptime(NA, format = "%m/%d/%Y %H:%M:%OS", tz = "Etc/GMT+8"),
      StudyID  = NA,
      GEN = NA,
      RKM = NA,
      Fish_Type = NA,
      Rel_loc = NA,
      Rel_rkm = NA,
      Year = NA,
      Rel_datetime = strptime(NA, format = "%m/%d/%Y %H:%M:%OS", tz = "Etc/GMT+8"),
      arrival = strptime(NA, format = "%m/%d/%Y %H:%M:%OS", tz = "Etc/GMT+8"),
      departure = strptime(NA, format = "%m/%d/%Y %H:%M:%OS", tz = "Etc/GMT+8"),
      min.time = strptime(NA, format = "%m/%d/%Y %H:%M:%OS", tz = "Etc/GMT+8")
    )
  
  for (i in unique(back.test$FishID)) {
    temp.dat <- back.test[back.test$FishID == i, ]
    
    test2 <- subset(temp.dat, DateTime_PST > min.time)
    
    back.test2 <- rbind(back.test2, test2)
    
  }
  
  
  max(back.test2$RKM, na.rm = TRUE) # maximum rkm = 153.14; FPT
  length(unique(back.test2$FishID)) # still 692 fish
  
}


