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

