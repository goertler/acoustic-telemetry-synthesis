#--------------------------------------------#
# M. Johnston 
# Utility/convenience functions for analysis 
# Thursday 2020-07-23 14:40:19 ----------------# 


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

# from the rethinking package
coerce_index <- function( ... ) {
    L <- list(...)
    if ( is.list(L[[1]]) && length(L)==1 ) L <- L[[1]]
    if ( length(L)==1 ) {
        # first try to coerce straight to integer as test for any NAs
        x <- as.integer(L[[1]])
        if ( any(is.na(x)) ) 
            # brute method
            x <- as.integer(as.factor(as.character(L[[1]])))
        return( x )
    } else {
        # multiple inputs
        vnames <- match.call()
        vnames <- as.character(vnames)[2:(length(L)+1)]
        # generate levels that include all labels from all inputs
        M <- L
        for ( i in 1:length(L) ) M[[i]] <- as.character(L[[i]])
        Mall <- M[[1]]
        for ( i in 2:length(L) ) Mall <- c( Mall , M[[i]] )
        Mall <- unique(Mall)
        new_levels <- levels(as.factor(Mall))
        for ( i in 1:length(L) ) {
            M[[i]] <- factor(M[[i]],levels=new_levels)
            M[[i]] <- as.integer(M[[i]])
        }
        names(M) <- paste( vnames , "_idx" , sep="" )
        return(M)
    } 
}

library(ggplot2)

plot_track <- function(df, ID) {
  ss = subset(df, FishID == ID)
  ggplot(ss, aes(x = DateTime_PST, y = GEN)) +
    geom_point() +
    theme_minimal() +
    labs(x = "DateTime", y = "GEN", title = paste(ID))
}


#-------------------------------------------------------#

# refactor first_last

# keep first and last detection at each receiver
test_fl_onefish <- function (x, 
                             dtc2 = "DateTime_PST", 
                             tagc = "FishID", 
                             stnc2 = "GEN") 
{
    x = x[order(x[[dtc2]]), ]
    newdf = x[x[[dtc2]] == min(x[[dtc2]]) | x[[dtc2]] == max(x[[dtc2]]) ,  ]
    
    return(newdf)
    
}

## function for all fish: assumes input will be a single data frame; need to split large data frame by FishID
#df = jsats; fish = "ARF2017-211" ; distance_matrix = dm_closed# testing

dpd_allfish = function(detdf) {
  f1 = split(detdf, detdf$FishID)
  f1 = f1[sapply(f1, nrow) > 0] # only keep obs with > 1 det
  tmp = lapply(f1, calc_distance_per_day)
  do.call(rbind, tmp)
}

calc_distance_per_day = function(df, distance_matrix = dm_closed) {
  
tt = df[ , c("FishID", "DateTime_PST", "GEN", "Rel_rkm")]
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
    distance_matrix[, c("Name", "Total_Length_m")],
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

}



