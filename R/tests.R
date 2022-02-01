# tests
source("R/utils.R")

# load test data frames:
v2 = readRDS("data_clean/v2.rds") # made in make_CJVemco_DFA_matrix_MEJ.R

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
                    Total_Length = as.numeric(6702.75))

mat = rbind(mat, battle)
# chk - this should be there:
mat[mat$Name == "SR_OrdBend - SR_I-80/50Br", ]
mat[mat$Name == "BtlCkAbatPnd - BattleCk10", ]
mat$Total_Length_m = mat$Total_Length # rename so that the dpd_allfish fxn works


# first last onefish: expects a data frame with 1 tagid,  but will happily accept one with more than 1 tagids and then will return everything
# returns a subset with only the first and/or last detection; not in order
# if the first and last are the same (i.e, one detection) it returns a single row.

# if
test_fl_onefish(v2) # 
v2[v2$DateTime_PST == min(v2$DateTime_PST), ]

v3 = subset(v2, FishID == unique(v2$FishID)[1])

test_fl_onefish(v3)

v4 = subset(v3, DateTime_PST == min(DateTime_PST))
v4 == test_fl_onefish(v4)


# tests for calc_dist_per_day:
# fish with multiple movements occurring on the same day

# fish gets released, detected again 3 days later:
# first movement: NA - release site: timediff = 1, distance = 0
# second movement: release site -> station 2: timediff = 3, distance = x
# expect 0, x/3 for total distance traveled in those three rows

dpd_allfish(v3, distance_matrix = mat)


# moeved from dpd calcs - turn into formal test
if(FALSE){
stopifnot(all.equal(sum(fin$Distance_m, na.rm = TRUE) , 
                    sum(tt3$Total_Length_m, na.rm = TRUE),
          tolerance = 0.1)
 )
}