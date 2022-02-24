# M. Johnston
# Clean CJVemco_distance matrix
# Fri Feb 11 09:26:16 2022 ------------------------------

source("R/utils.R")

## In the clean_all_detects.R script:
 # - only need fish (JSATS) from: 2013, 2016, and 2017
 # - only need fish (all groups) that reach either Ben or Chipps recs
 # - End Recs: "BeniciaW"  "ChippsW"
 # - Rename some receiver locations to agree with the names in the distance matrices

# load distance matrix (emailed by CJM on 2021-09-)
mat <- read.csv("data/distance_matrices/Vemco_dist_matrix_DCC-Yolo-Tisdale_closed.txt",
                         sep =  ",",
                         header = TRUE)

# add in battle creek movement that's missing from matrix
battle = data.frame(ObjectID = range(mat$ObjectID)[2] + 1,
                    Name = as.character("BtlCkAbatPnd - BattleCk10"),
                    OriginID = NA,
                    DestinationID = NA,
                    DestinationRank = NA,
                    Total_Length = as.numeric(6893.039)) # from Pascale, 2/17

mrgc = data.frame(ObjectID = range(mat$ObjectID)[2] + 2,
                    Name = as.character("Richmond Bridge - MRGC"),
                    OriginID = NA,
                    DestinationID = NA,
                    DestinationRank = NA,
                    Total_Length = as.numeric(407.85)) # from Pascale, 2/17

rich = data.frame(ObjectID = range(mat$ObjectID)[2] + 3,
                    Name = as.character("MRGC - Richmond Bridge"),
                    OriginID = NA,
                    DestinationID = NA,
                    DestinationRank = NA,
                    Total_Length = as.numeric(407.85)) # from Pascale, 2/17

mat = rbind(mat, battle, mrgc, rich)
# chk - this should be there:
mat[mat$Name == "SR_OrdBend - SR_I-80/50Br", ]
mat[mat$Name == "BtlCkAbatPnd - BattleCk10", ]
mat[mat$Name == "Richmond Bridge - MRGC", ]
mat[mat$Name == "MRGC - Richmond Bridge", ]

mat$Total_Length_m = mat$Total_Length # rename so that the dpd_allfish fxn works

# matrix data checks
summary(mat$Total_Length)
stopifnot(sum(rowSums(is.na(mat))) == 9) # there should be exactly 9 NAs (the ones introduced above)

saveRDS(mat, "data_clean/CMVemco/CM_vemco_distance_matrix_DCC-Yolo-Tisdale_closed_clean.rds")
