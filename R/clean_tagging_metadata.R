#-------------------------------------------------------#
# M. Johnston
# Clean and prep tagging metadata - JSATS and YBUS
# Fri Jul 24 09:33:14 2020 ------------------------------
## Load and clean tag metadata

tagging_meta <-
  read.csv("data/tagging_data/Tagging_Fish_Metadata.txt",
           stringsAsFactors = F)

tagging_meta$Rel_datetime <-
  as.POSIXct(tagging_meta$Rel_datetime, 
             tz = "Etc/GMT+8", 
             format = "%m/%d/%Y %H:%M:%S")

stopifnot(length(unique(tagging_meta$FishID)) == nrow(tagging_meta))     # test that each row represents a unique fishID

#### The following few lines of code massage things that could probably be rectified in the JSATS database ####
## Change fish_type "Chinook" to "RBDD Chinook" to be more informative

# original code:
tagging_meta[tagging_meta$Fish_Type == "Chinook" & tagging_meta$StudyID == "RBDD-2017", "Fish_Type"] <- "RBDD Chinook"


## Change Fish_Type "Fall run Chinook" for StudyID "ColemanFall_2016" to more informative "CNFH Fall Chinook"
tagging_meta[tagging_meta$Fish_Type == "Fall run Coleman", "Fish_Type"] <- "CNFH Fall Chinook"

