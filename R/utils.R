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


#----------------------------------------------------#
# refactor dpd with telemetry::div_dist()
# Tue Jun 28 10:37:34 2022 ------------------------------
#----------------------------------------------------#
# div_dist setup functions:
add_lag_col = function(df, order_by = "DateTime_PST", col_to_lag, lagged_col_name, ...) {
  
  df = df[order(df[[order_by]]), ]
  df[[lagged_col_name]] = c(df[[col_to_lag]][-1], NA)
  return(df)
  
}

# make the movements:
make_movements = function(df, col_to_lead, lagged_col_name, sep = " - ") {
  
  df[[lagged_col_name]] = paste(df[[col_to_lead]], dplyr::lead(df[[col_to_lead]]), sep = sep)
  return(df)
}

# remove the NAs and merge with the distance matrix function:
rm_nas_and_merge = function(df, dist_mat, na_col = "next_arrival") {
  
  tt3 = df[!is.na(df[[na_col]]), ] # remove last row, as the arrival at last receiver is now on the second-to-last row
  
  tt3 = merge(
    tt3,
    dist_mat[, c("Name", "Total_Length_m")],
    by.x  = "movement",
    by.y = "Name",
    all.x = TRUE
  )
  
  tt3 = tt3[order(tt3$FishID, tt3$DateTime_PST), ]
  tt3$Date = as.Date(tt3$DateTime_PST, tz = "Etc/GMT+8") # # this needs a tz, otherwise some departure dates turn over to the next day
  return(tt3)
  
}

# remove movements w/ dist = 0m

rm_zero_dists = function(x) x[x$Total_Length_m != 0, ] # does not take care of NAs - just propagates them


# mapply the div_dist function:
hs = function(df) {
  
  do.call(rbind, mapply(div_dist, 
                        start = df$DateTime_PST, 
                        end = df$next_arrival, 
                        distance = df$Total_Length_m, 
                        time_units = "hour",
                        SIMPLIFY = FALSE))
  
}

# add movement column so we can compare with the distance matrix
movement_col = function(detdf, distance_matrix) {
  
  tmp1 = add_lag_col(detdf, 
                     order_by = "DateTime_PST",
                     col_to_lag = "DateTime_PST",
                     lagged_col_name = "next_arrival")
  
  make_movements(tmp1, 
                        col_to_lead = 'GEN', 
                        lagged_col_name = 'movement')
  

}


dpd_allfish = function(detdf, distance_matrix) {
  
  tmp1 = add_lag_col(detdf, 
                     order_by = "DateTime_PST",
                     col_to_lag = "DateTime_PST",
                     lagged_col_name = "next_arrival")
  
  tmp2 = make_movements(tmp1, 
                        col_to_lead = 'GEN', 
                        lagged_col_name = 'movement')
  
  tmp3 = rm_nas_and_merge(tmp2, dist_mat = distance_matrix, na_col = "next_arrival")
  
  rm_zero_dists(tmp3)
  
}


# find fish that go backwards
#riverkilometer increases after having previously decreased
backwards_onefish = function(fp_df_onefish){
  
rkms_increase = lag(round(fp_df_onefish$RKM, 1)) < round(fp_df_onefish$RKM, 1)

ans = sum(rkms_increase, na.rm = TRUE)

}

