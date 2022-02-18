## Make location exclusions csv for CJVemco analysis
## based on code sent by Pascale on 2022-02-02
# M. Johnston
# Thu Feb 17 21:18:36 2022 ------------------------------
library(lubridate)

## Load detections of interest
v = as.data.frame(data.table::fread("data/detection_data/Query3.csv")) # uploaded to Sharepoint by Pascale; see README
v$DateTime_PST = force_tz(mdy_hms(v$DetectDate), tzone = "Etc/GMT+8")
v$Date_Released = force_tz(mdy_hms(v$Date_Released), tzone = "Etc/GMT+8") # Release detection
v$DetectDate = force_tz(mdy_hms(v$DetectDate), tzone = "Etc/GMT+8") # re-format
stopifnot(identical(v$DateTime_PST, v$DetectDate))

# Locations to exclude:
loc <- unique(v[,4])  # should be "General_Location"

loc <- data.frame(loc = loc, exclud = "NA")

exclud <- c(
  rep("N", 28),
  "Y",
  rep("N", 3),
  "Y",
  "Y",
  "N",
  "N",
  "Y",
  rep("N", 4),
  "Y",
  "Y",
  rep("N", 7),
  "Y",
  rep("N", 13),
  "Y",
  rep("N", 7),
  "Y",
  "N",
  "N",
  "Y",
  "Y",
  rep("N", 8),
  "Y",
  "Y",
  "Y",
  "N",
  "Y",
  rep("N", 6),
  "Y",
  rep("N", 24),
  "Y",
  "Y",
  "N",
  "N",
  "Y",
  "Y",
  "Y",
  "N",
  "Y",
  "N",
  "N",
  "Y",
  "Y",
  "Y",
  "Y",
  rep("N", 15),
  "Y",
  "N",
  "Y",
  rep("N", 7),
  "Y",
  "N",
  "N",
  "N"
)

loc$exclud = exclud
colnames(loc) <- c("General_Location", "exclude")

chk = read.csv("data/exclude_loc.csv") # from Pascale on 2/2/22
all.equal(chk[, c(2:3)], loc)

write.csv(loc, "data_clean/exclude_loc_clean.csv", row.names = FALSE)