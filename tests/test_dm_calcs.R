# Tests

d = readRDS("data_clean/YBUS/ybus_clean.rds")
x = subset(d, FishID == "1.YBUS")


dm_ybus = read.csv("data/distance_matrices/Distance_Matrix_YBUS_corr_07_21.csv")
dm_ybus = dm_ybus[ , c("Name_corr", "Total_Length")]
colnames(dm_ybus) = c("Name", "Total_Length_m")
dm_ybus$Name = gsub("-", " - ", dm_ybus$Name)

add_lag_col = function(df, order_by = "DateTime_PST", col_to_lag, lagged_col_name, ...) {
  
  df = df[order(df[[order_by]]), ]
  df[[lagged_col_name]] = data.table::shift(df[[col_to_lag]], ...)
  return(df)
  
}

make_movements = function(df, col_to_lead, lagged_col_name, sep = " - ") {
  
  df[[lagged_col_name]] = paste(df[[col_to_lead]], dplyr::lead(df[[col_to_lead]]), sep = sep)
  return(df)
}

rm_nas_and_merge = function(df, dist_mat, na_col = "first_arrs") {
  
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

# test on one
chk2 = add_lag_col(x, order_by = 'DateTime_PST', 
                   col_to_lag = 'DateTime_PST' ,  
                   lagged_col_name = 'first_arrs', 
                   type = 'lead')

head(chk2)
chk3 = make_movements(chk2, 'GEN', 'movement')
chk3
chk4 = rm_nas_and_merge(chk3, dist_mat = dm_ybus)
chk4

chk5 = do.call(rbind, mapply(div_dist, 
                             start = chk4$departure, 
                             end = chk4$first_arrs, 
                             distance = chk4$Total_Length_m, 
                             time_units = "hour",
                             SIMPLIFY = FALSE))

chk5
all.equal(sum(chk5$prop_dist), sum(chk4$Total_Length_m), tolerance = 0.001) # might want to fix eventually; can end up way off with additive small differences.


# all fish
f1 = split(f1, f1$FishID)

f2 = lapply(f1, add_lag_col, 'arrival', 'first_arrs', type = 'lead')

f3 = lapply(f2, make_movements, 'GEN', 'movement')

tt3 = lapply(f3, rm_nas_and_merge, dist_mat = dm_ybus, na_col = "first_arrs")

hs = function(df) {
  
  do.call(rbind, mapply(div_dist, 
                        start = df$departure, 
                        end = df$first_arrs, 
                        distance = df$Total_Length_m, 
                        time_units = "hour",
                        SIMPLIFY = FALSE))
  
}

ans3 = lapply(tt3, hs)
