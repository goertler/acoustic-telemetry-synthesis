source("R/01_setup.R")

# estimate variation in travel time (sd)
# look at results data -- missing 4 years
FishID_key <- read_csv("data/common_data/FishID_key.csv")
FishID_key %>%
  group_by(Year, TagType) %>%
  summarise(total = length(unique(FishID))) #793 (2013-2017)
# fixed with adding "BeniciaW" to line 46 in clean_JSATS_dets.R
# now #843, need to investigate

# make it reproducible

# load data from results
file_names <- list.files(path = "results/JSATS",recursive = TRUE)
# need to move into folder to run the rest of the code
setwd("C:/Users/pgoertler/Desktop/MJ's Repo/merged/acoustic-telemetry-synthesis/results/JSATS")

for(i in file_names){
  file <- read.csv(i)
  }

#years <- substr(file_names, 1, 4)

file <- lapply(file_names,read.csv)
names(file) <- substr(file_names, 1, 4)
str(file)

list.names <- substr(file_names, 1, 4)
output <- vector("list", length(list.names))
names(output) <- list.names

dat_goods <- data.frame(FishID = NA, SD = NA, Year = NA, rel = NA, end = NA)

# loop
for (i in substr(file_names, 1, 4)){
  temp_df <- data.frame(file[[i]])
  temp_df2 <- transform(temp_df, SD=apply(temp_df[c(2:ncol(temp_df))],1, sd, na.rm = TRUE))
  temp_df3 <- temp_df2[, c("FishID", "SD")]
  temp_df3$Year <- i
  temp_df3$Col_first <- names(temp_df2[-1])[max.col(!is.na(temp_df2[-1]), "first")]
  temp_df3$rel <- gsub("X","", temp_df3$Col_first)
  temp_df3$Col_last <- names(temp_df2[-ncol(temp_df2)])[max.col(!is.na(temp_df2[-ncol(temp_df2)]), "last")]
  temp_df3$end <- gsub("X","", temp_df3$Col_last)
  dat_goods <- rbind(dat_goods, temp_df3[,-c(4,6)])
}


dat_goods <- dat_goods[-1,]
head(dat_goods)
str(dat_goods)
unique(dat_goods$Year)

# fix date
dat_goods$rel <-  as.Date(gsub('\\.', '-', dat_goods$rel))
dat_goods$end <-  as.Date(gsub('\\.', '-', dat_goods$end))

# back out
setwd("C:/Users/pgoertler/Desktop/MJ's Repo/merged/acoustic-telemetry-synthesis")
write.csv(dat_goods, "results/SD/JSATS.csv")

# look into additional 50 fish
setdiff(dat_goods$FishID, FishID_key$FishID)

check <- subset(all_detects, FishID == "WR2017-066")
unique(check$GEN)
# these are fish that have no travel time because of Benicia combination rule
