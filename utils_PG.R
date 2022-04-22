#--------------------------------------------#
# M. Johnston 
# Utility/convenience functions for analysis 
# Thursday 2020-07-23 14:40:19 ----------------# 

#edits Pascale 07-22-2021 
#-------------------------------------------------------#

# refactor first_last

# keep first and last detection at each receiver
test_fl_onefish <- function (x, 
                             dtc2 = "DetectDate", 
                             tagc = "FishID", 
                             stnc2 = "Name_corr") 
{
  x = x[order(x[[dtc2]]), ]
  newdf = x[x[[dtc2]] == min(x[[dtc2]]) | x[[dtc2]] == max(x[[dtc2]]) ,  ]
  
  return(newdf)
  
}

## function for all fish: assumes input will be a single data frame; need to split large data frame by FishID
#df = jsats; fish = "ARF2017-211" ; distance_matrix = dm_closed# testing

dpd_allfish = function(detdf, dm) {
  f1 = split(detdf, detdf$FishID)
  f1 = f1[sapply(f1, nrow) > 0] # only keep obs with > 1 det
  tmp = lapply(f1, calc_distance_per_day, distance_matrix = dm)
  do.call(rbind, tmp)
}

calc_distance_per_day = function(df, distance_matrix = dm) {
  
  tt = df[ , c("FishID", "DetectDate", "Name_corr")]
  tt = tt[order(tt$DetectDate), ]
  
  tt$visitID = data.table::rleidv(tt, "Name_corr") # add rle for station visits
  
  tt2 = do.call(rbind, by(tt, tt$visitID, test_fl_onefish)) # split by station visits, apply test_fl_onefish 
  
  tt3 = tt2[!duplicated(tt2$visitID, fromLast = TRUE), ] # keeps departure at each station; not sure about this step yet; but I *think* it might make sure that movements denote the day on which they arrive at the second location
  
  # make movements
  tt3$movement = paste(dplyr::lag(tt3$Name_corr), tt3$Name_corr, sep = "-")
  
  # pull movements from the matrix
  tt3 =
    merge(
      tt3,
      distance_matrix[, c("Name_corr", "Total_Length")],
      by.x  = "movement",
      by.y = "Name_corr",
      all.x = TRUE
    )
  
  tt3 = tt3[order(tt3$FishID, tt3$DetectDate), ]
  tt3$Date = as.Date(tt3$DetectDate)
  
  # I can get it to a data frame-like structure with this, but it strips the column names:
  ff =  tapply(tt3[ , "Total_Length"], 
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
  
  stopifnot(
    all.equal(sum(fin$Distance_m, na.rm = TRUE) , 
              sum(ff$tot_distance, na.rm = TRUE),
              tolerance = 0.1 )
  )
  
  stopifnot(all.equal(sum(fin$Distance_m, na.rm = TRUE) , 
                      sum(tt3$Total_Length, na.rm = TRUE),
                      tolerance = 0.1)
  )
  
  return(fin)
  
}

