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


test_fl_onefish <- function (x, 
                             dtc2 = "DateTime_PST", 
                             tagc = "FishID", 
                             stnc2 = "GEN") 
{
    x = x[order(x[[dtc2]]), ]
    newdf = x[x[[dtc2]] == min(x[[dtc2]]) | x[[dtc2]] == max(x[[dtc2]]) ,  ]
    
    return(newdf)
    
}




