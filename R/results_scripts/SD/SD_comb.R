# load data

ya_sd <- read.csv("results/SD/YoloAce.csv")
ybus_sd <- read.csv("results/SD/YBUS.csv")
jsats_sd <- read.csv("results/SD/JSATS.csv")
cm_sd <- read.csv("results/SD/CMVemco.csv")

head(ya_sd)
head(ybus_sd)
head(jsats_sd)
head(cm_sd)

sd_all <- rbind(ya_sd[,-1], ybus_sd[,-1], jsats_sd[,-1], cm_sd[,-1]) # extra 50 are from Decker/Antioch decision

FishID_key <- read_csv("data/common_data/FishID_key.csv")

sd_meta <- merge(FishID_key, sd_all, by = "FishID", all = TRUE)
head(sd_meta)

sd_meta$check_year <- duplicated(sd_meta[,c(6,13)])
sd_check <- subset(sd_meta, check_year == FALSE)

sum(is.na(sd_meta) == TRUE)
summary(sd_meta) # looks okay

write.csv(sd_meta[,-c(2,6,16)], "results/SD/SD_dat.csv")


