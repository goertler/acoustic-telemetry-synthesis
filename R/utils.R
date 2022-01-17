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

# keep first and last detection at each receiver
test_fl_onefish <- function (x, 
                             dtc2 = "DateTime_PST") 
{
    newdf = x[ x[[dtc2]] == min(x[[dtc2]]) | x[[dtc2]] == max(x[[dtc2]]) ,  ]
    return(newdf)
}

## function for all fish: assumes input will be a single data frame; need to split large data frame by FishID
#df = jsats; fish = "ARF2017-211" ; distance_matrix = dm_closed# testing
# test: one fish
df = v2[v2$FishID == "LFC0687", ]
distance_matrix = mat


dpd_allfish = function(detdf, distance_matrix) {
  f1 = split(detdf, detdf$FishID)
  f1 = f1[sapply(f1, nrow) > 0] # only keep obs with > 1 det
  tmp1 = lapply(f1, assign_station_visits)
  tmp2 = lapply(tmp1, make_movements, distance_matrix = distance_matrix)
  #tmp3 = lapply(tmp2, calc_dist_per_day)
  #tmp4 = lapply(tmp3, pad_days)
  do.call(rbind, tmp2)
}

# get movements missing from matrix
# Mon Jan 17 13:15:25 2022 ------------------------------
f1 = split(v2, v2$FishID)
f1 = f1[sapply(f1, nrow) > 0]
tmp1 = lapply(f1, assign_station_visits)
str(tmp1[1])

tmp2 = lapply(tmp1, make_movements, distance_matrix = mat)
str(tmp2[1])

ans = do.call(rbind, tmp2)
head(ans)
missing_movements = setdiff(ans$movement, mat$Name)
write.csv(missing_movements, "results/temporary/movements_mising_from_dist_matrix_2022-01-17.csv")

#-------------------------------------------------------#
# assign station visits
#-------------------------------------------------------#
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
make_movements = function(tt2, distance_matrix) {
  
tt3 = tt2[!duplicated(tt2$visitID, fromLast = TRUE), ] # keeps departure at each station; not sure about this step yet; ensures that track goes from departure to departure across all the stations, which incorporates all the residence time in between receivers

# make movements
tt3$movement = paste(dplyr::lag(tt3$GEN), tt3$GEN, sep = " - ")

# if(!(length(setdiff(tt3$movement, distance_matrix$Name)) == 0)) {
#   
#   stop("Movements present in data are missing from distance matrix") # stop if there are movements in the data that aren't in the distance matrix
# 
#   } else {
# # pull movements from the matrix
# tt3 =
#   merge(
#     tt3,
#     distance_matrix[, c("Name", "Total_Length_m")],
#     by.x  = "movement",
#     by.y = "Name",
#     all.x = TRUE
#   )

tt3 = tt3[order(tt3$FishID, tt3$DateTime_PST), ]
tt3$Date = as.Date(tt3$DateTime_PST) # , tz = "Etc/GMT+8") should be added in but I've left it out to make sure yolo/ace are consistent with other results
return(tt3)

  }

#-------------------------------------------------------#

#-------------------------------------------------------#
# calculate distance per day
#-------------------------------------------------------#
calc_dist_per_day = function(tt3) {

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

return(ff)

}
#-------------------------------------------------------#

#-------------------------------------------------------#
# Pad days for each track to make continuous
#-------------------------------------------------------#
pad_days = function(ff) {
  
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

stopifnot(all.equal(sum(fin$Distance_m, na.rm = TRUE) , 
                    sum(tt3$Total_Length_m, na.rm = TRUE),
          tolerance = 0.1)
 )

return(fin)
#-------------------------------------------------------#
}

# find fish that go backwards
#riverkilometer increases after having previously decreased
backwards_onefish = function(fp_df_onefish){
  
rkms_increase = lag(round(fp_df_onefish$RKM, 1)) < round(fp_df_onefish$RKM, 1)

ans = sum(rkms_increase, na.rm = TRUE)

}

