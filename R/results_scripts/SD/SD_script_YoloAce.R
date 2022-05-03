source("R/01_setup.R")

# estimate variation in travel time (sd)

# make it reproducible

# load data from results
file_names <- list.files(path = "results/YoloAce",recursive = TRUE)
# need to move into folder to run the rest of the code
setwd("C:/Users/pgoertler/Desktop/MJ's Repo/merged/acoustic-telemetry-synthesis/results/YoloAce")

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

# need to get fishID
FishID_key <- read_csv("data/common_data/FishID_key.csv")
colnames(dat_goods)[1] <- "TagID"

dat_goods_ID <- merge(dat_goods, FishID_key[,c(2,10)], by = "TagID", all.x = TRUE)
head(dat_goods_ID)

write.csv(dat_goods_ID[,-1], "results/SD/YoloAce.csv")
