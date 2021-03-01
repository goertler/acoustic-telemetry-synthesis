# Tests

source("R/make_DFA_matrix_MEJ.R") # fin$distance_m and sum(ff$total_dist) need to be closer
library(ggplot2)

ggplot(test, aes(x = Date, y=Distance_m)) +
  geom_point(aes(group = FishID, color = FishID)) +
  geom_path(aes(group = FishID, color = FishID)) +
  theme_minimal()


ggplot(jsats[jsats$FishID == test$FishID[1], ],
       aes(x = DateTime_PST, y = RKM)) +
  geom_point() +
  geom_path(aes(group = FishID))

#-------------------------------------------------------#
# Check 1 fish line by line
#-------------------------------------------------------#

tt = jsats[jsats$FishID == "CFC2017-127", 
           c("FishID", "DateTime_PST", "GEN", "Rel_rkm")]

tt = tt[order(tt$DateTime_PST), ]

tt$visitID = data.table::rleidv(tt, "GEN") # add rle for station visits

tt2 = do.call(rbind, by(tt, tt$visitID, test_fl_onefish)) # split by station visits, apply test_fl_onefish 

tt3 = tt2[!duplicated(tt2$visitID, fromLast = TRUE), ] # keeps departure at each station; not sure about this step yet; but I *think* it might make sure that movements denote the day on which they arrive at the second location

# make movements
tt3$movement = paste(dplyr::lag(tt3$GEN), tt3$GEN, sep = " - ")

# pull movements from the matrix
tt3 =
  merge(
    tt3,
    dm_closed[, c("Name", "Total_Length_m")],
    by.x  = "movement",
    by.y = "Name",
    all.x = TRUE
  )

tt3 = tt3[order(tt3$FishID, tt3$DateTime_PST), ]
tt3$Date = as.Date(tt3$DateTime_PST)

# I can get it to a data frame-like structure with this, but it strips the column names:
ff =  tapply(tt3[ , "Total_Length_m"], 
                       tt3[ , c("FishID", "Date")], 
                       sum, 
                       na.rm = TRUE,
                       simplify = TRUE)

ff = as.data.frame(cbind(t(ff), dimnames(ff)[[1]]))
ff$Date = as.Date(row.names(ff))
colnames(ff) = c("tot_distance", "FishID", "Date")

# calculate vector of time differences & add as column
ff$timediff = abs(as.numeric(difftime(dplyr::lag(ff$Date), # time 1
                                     ff$Date, # time 2
                                     units = "days")))

# create vector of dates for the total time period
dates = seq.Date(from = ff$Date[1], # from 1st date
                 to = ff$Date[length(ff$Date)], # to final date
                by = "days"
                  ) 

ff$timediff[is.na(ff$timediff)] <- 1 # replace lag NA with 1

ff$tot_distance = round(as.numeric(ff$tot_distance), 2)

ff$dist_day = ff$tot_distance/ff$timediff

dists = rep(ff$dist_day, ff$timediff)

stopifnot(length(dists) == length(dates))

fin = data.frame(FishID = unique(ff$FishID),
           Date = dates, 
           Distance_m = dists)

stopifnot(all.equal(sum(fin$Distance_m, na.rm = TRUE) , sum(ff$tot_distance, na.rm = TRUE)))








#-------------------------------------------------------#
# know it works within itself - now need to check a fish "by hand" and test
#-------------------------------------------------------#
if(FALSE){
  
source("R/make_DFA_matrix_MEJ.R")

f1 = jsats[jsats$FishID == "WR2017-484", ] # fish with the fewest movements
 
tt = f1[order(f1$DateTime_PST), ]

tt$visitID = data.table::rleidv(tt, "GEN") # add rle for station visits

tt2 = do.call(rbind, by(tt, tt$visitID, test_fl_onefish)) # split by station visits, apply test_fl_onefish 

tt3 = tt2[!duplicated(tt2$visitID, fromLast = TRUE), ] # keeps departure at each station; not sure about this step yet; but I *think* it might make sure that movements denote the day on which they arrive at the second location

# make movements
tt3$movement = paste(dplyr::lag(tt3$GEN), tt3$GEN, sep = " - ")

# pull movements from the matrix
tt3 =
  merge(
    tt3,
    dm_closed[, c("Name", "Total_Length_m")],
    by.x  = "movement",
    by.y = "Name",
    all.x = TRUE
  )

tt3 = tt3[order(tt3$FishID, tt3$DateTime_PST), ]
tt3$Date = as.Date(tt3$DateTime_PST)

write.csv(tt3, "tests/movements_WR2017-484.csv")

f2 = bigtest[bigtest$FishID == unique(tt3$FishID), ]
all.equal(sum(f2$Distance_m, na.rm = TRUE), sum(tt3$Total_Length_m, na.rm = TRUE))
}
