#--------------------------------------------#
# M. Johnston 
# Utility/convenience functions for analysis 
# Fri Jan 14 10:24:36 2022 ------------------------------

# Analysis-specific cleaning functions

# this function adds a date column and a water year column, and subsets down to the years you want.

add_date_wy_and_subset <- function(df, datetime_col, wyears) {
  df$Date = lubridate::as_date(df[[datetime_col]])
  df$wateryr = lubridate::year(df[[datetime_col]])
  df2 = subset(df, wateryr %in% wyears)
}

# quickly 'vet' a dataframe by previewing rows at the head, middle, and tail:
#-------------------------------------------------------#
vet <- function(d, n = 4L) {
 if(class(d) != 'data.frame') stop('vet() can only vet dataframes')
 left <- as.integer(nrow(d) / 2 - n / 2)
 torso = d[seq_len(n) + left - 1L,]
rbind(head(d, n), torso, tail(d, n))
}


# typing shortcuts
#--------------------------------------------#
len <- function(x){length(unique(x))}
csn <- function(x){colSums(is.na(x))}
rsn <- function(x){rowSums(is.na(x))}

library(ggplot2)

plot_track <- function(df, ID, idcol = "FishID") {
  df$FishID = df[[idcol]]
  df = df[order(df$DateTime_PST), ]
  ggplot(subset(df, FishID == ID), aes(x = DateTime_PST, y = reorder(GEN, RKM))) +
    geom_point() +
    geom_path(aes(group = FishID)) +
    theme_minimal() +
    scale_x_datetime(date_labels = "%b-%d") +
    labs(x = "DateTime", y = "GEN", title = paste(ID))
}


#-------------------------------------------------------#
# refactor first_last

# keep first and last detection at a receiver.
# takes a data frame of detections of a single receiver and a single tag
# returns a subset of the data frame where the date time column is either the first OR the last detection
# if these are the same, only returns one row; otherwise, will return 2 rows
test_fl_onefish <- function (x, 
                             dtc2 = "DateTime_PST") 
{
    newdf = x[ x[[dtc2]] == max(x[[dtc2]]) | x[[dtc2]] == min(x[[dtc2]]) ,  ]
    return(newdf)
}

## function for all fish: assumes input will be a single data frame; need to split large data frame by FishID
#df = jsats; fish = "ARF2017-211" ; distance_matrix = dm_closed# testing


dpd_allfish = function(detdf, distance_matrix) {

  tmp1 = assign_station_visits(detdf)
  tmp2 = make_movements(tmp1, distance_matrix = distance_matrix)
  tmp3 = calc_dist_per_day(tmp2)
  pad_days(tmp3)

}

# get movements missing from matrix
# Mon Jan 17 13:15:25 2022 ------------------------------
if(FALSE){ f1 = split(v2, v2$FishID)
f1 = f1[sapply(f1, nrow) > 0]
tmp1 = lapply(f1, assign_station_visits)
str(tmp1[1])

tmp2 = lapply(tmp1, make_movements, distance_matrix = mat)
str(tmp2[1])

ans = do.call(rbind, tmp2)
head(ans)
missing_movements = setdiff(ans$movement, mat$Name)
write.csv(missing_movements, "results/temporary/movements_mising_from_dist_matrix_2022-01-17.csv")
}

#-------------------------------------------------------#
# 1. assign station visits
#-------------------------------------------------------#
# this function takes a data frame of detections for a single fish, orders it by the date time column, 
# and creates a new column called VisitID that labels each row with a sequential ID by station; for example,
# if a fish has one release detection, three detections at the station 1, and 8 detections at station 2, and then another detection at station 1, the VisitID column would read: 1 2 2 2 3 3 3 3 3 3 3 3 4
# then it splits the detection data frame on the VisitID column, so that we have one data frame for each visit it.  Then it applies the get test_first_last function to each visitID data frame
# and binds all the resulting 1-2 row data frames back into a single data frame and returns it.

assign_station_visits = function(df) { # add check to see if all movements are in there
  
tt = df[ , c("FishID", "DateTime_PST", "GEN")]
tt = tt[order(tt$DateTime_PST), ]
tt$visitID = data.table::rleidv(tt, "GEN") # add rle for station visits

tt2 = do.call(rbind, by(tt, tt$visitID, test_fl_onefish)) # split by station visits, apply test_fl_onefish 

return(tt2)
}

#-------------------------------------------------------#
# make/check movements
#-------------------------------------------------------#
# function expects a single data frame with one fish in it and all its station visits (first and last detection at each)

make_movements = function(tt2, distance_matrix) {
  
tt3 = tt2[!duplicated(tt2$visitID, fromLast = TRUE), ] # keeps departure at each station; not sure about this step yet; ensures that track goes from departure to departure across all the stations, which incorporates all the residence time in between receivers

# make movements
tt3$movement = paste(dplyr::lag(tt3$GEN), tt3$GEN, sep = " - ")
# browser()
# if(!(length(setdiff(tt3$movement, distance_matrix$Name)) == 0)) {
# 
#   stop("Movements present in data are missing from distance matrix") # stop if there are movements in the data that aren't in the distance matrix
# 
#   } else {
# pull movements from the matrix

tt3 =
  merge(
    tt3,
    distance_matrix[, c("Name", "Total_Length_m")],
    by.x  = "movement",
    by.y = "Name",
    all.x = TRUE
  )

tt3 = tt3[order(tt3$FishID, tt3$DateTime_PST), ]
tt3$Date = as.Date(tt3$DateTime_PST, tz = "Etc/GMT+8") # # this needs a tz, otherwise some departure dates turn over to the next day
return(tt3)

  }
#}

#-------------------------------------------------------#

#-------------------------------------------------------#
# calculate distance per day
#-------------------------------------------------------#
calc_dist_per_day = function(tt3) {

# browser()
ff =  aggregate(Total_Length_m ~ FishID + Date, data = tt3, FUN = sum, na.rm = TRUE)
# calculate vector of time differences & add as column
ff$timediff = c(1, as.numeric(diff(ff$Date))) # add 1 day for the NA of first movement

#ff$timediff[is.na(ff$timediff)] <- 1 # replace lag NA with 1

ff$tot_distance = round(as.numeric(ff$Total_Length_m), 2)

ff$dist_day = ff$tot_distance/ff$timediff

return(ff)

}
#-------------------------------------------------------#

#-------------------------------------------------------#
# Pad days for each track to make one continuous date vector from first detection to last detection;
# return final data frame of fishID, dates, and distance traveled per date
#-------------------------------------------------------#
pad_days = function(ff) {
  
  # create vector of dates for the total time period represented by a movement
dates = seq.Date(from = ff$Date[1], # from 1st date
                 to = ff$Date[length(ff$Date)], # to final date
                by = "days"
                  ) 
  
dists = rep(ff$dist_day, ff$timediff)

stopifnot(length(dists) == length(dates))

fin = data.frame(FishID = unique(ff$FishID),
           Date = dates, 
           Distance_m = dists)

stopifnot(
  all.equal(sum(fin$Distance_m, na.rm = TRUE) , 
            sum(ff$tot_distance, na.rm = TRUE),
         tolerance = 0.1 )
  )

return(fin)
#-------------------------------------------------------#
}

#-------------------------------------------------------#
# Function to make final data frame

make_matrix = function(dt) {
  
    chk = seq.Date(from = min(dt$Date), to = max(dt$Date), by = "day")
    date_diff = setdiff(chk, dt$Date)
    
    if(length(date_diff)) {
      dt = rbind(dt, data.frame(FishID = NA,
                                Date = as.Date(date_diff, origin = "1970-01-01"),
                                Distance_m = NA)) }
    
    dt = as.data.frame(tidyr::pivot_wider(dt, names_from = Date, values_from = Distance_m))
    
    dt[!is.na(dt$FishID) , c("FishID", as.character(chk))] # order the data frame by the correct date sequence

  }

# find fish that go backwards
#riverkilometer increases after having previously decreased
backwards_onefish = function(fp_df_onefish){
  
rkms_increase = lag(round(fp_df_onefish$RKM, 1)) < round(fp_df_onefish$RKM, 1)

ans = sum(rkms_increase, na.rm = TRUE)

}

