# load data

stage <- read.csv("data/enviro_data/RVB_Stage.csv")
stage$Date <- as.Date(stage$Date)
stage <- subset(stage, DATA_FLAG != "N")
head(stage)
str(stage)
sum(is.na(stage$VALUE)) #1.2%
plot(stage$Date, stage$VALUE) # a few outlier looking values in 2009
stage_cehck <- subset(stage, Date < as.Date("2010-01-01"))
plot(stage_cehck$Date, stage_cehck$VALUE)
# get rid of negative data and then use daily mean
stage_clean <- subset(stage, VALUE>=0)

#temp <- read.csv("data/enviro_data/RVB-TEMP.csv")
#temp$Date <- as.Date(temp$Date)
#temp$RVB.TEMP <- as.numeric(temp$RVB.TEMP)
#plot(temp$Date, temp$RVB.TEMP)# weird values
#temp_clean <- subset(temp, RVB.TEMP<=100 & RVB.TEMP>=0)

#min(temp_clean$Date, na.rm = TRUE) #2007-05-31, need to start at or before 2007-01-31
temp_fill <- read.csv("data/enviro_data/RIV_25.csv")
temp_fill$Date <- as.Date(temp_fill$OBS.DATE)
head(temp_fill)
str(temp_fill)
temp_fill$RIV.TEMP <- as.numeric(temp_fill$RIV.TEMP)
plot(temp_fill$Date, temp_fill$RIV.TEMP)# weird values
temp_fill_clean <- subset(temp_fill, RIV.TEMP<=40 & RIV.TEMP>=0)
min(temp_fill_clean$Date, na.rm = TRUE) # easier to substitute

fish_dat <- read.csv("results/SD/SD_dat.csv")
head(fish_dat)
str(fish_dat)
fish_dat$rel <- as.Date(fish_dat$rel)
fish_dat$end <- as.Date(fish_dat$end)
length(unique(fish_dat$FishID))

Keeper_dat <- data.frame(FishID = NA, Date_min = as.Date("1900-01-01"),  Date_max = as.Date("1900-01-01"), Temp_mean = NA, Temp_sd = NA, Stage_mean = NA, Stage_sd = NA)

for(i in unique(fish_dat$FishID)){
  temp_dat <- subset(fish_dat, FishID == i)
  Temp_df <- subset(temp_fill_clean, Date >= min(temp_dat$rel) & Date <= max(temp_dat$end))
  Stage_df <- subset(stage_clean, Date >= min(temp_dat$rel) & Date <= max(temp_dat$end))
  new_dat <- data.frame(FishID = i, Date_min = min(temp_dat$rel),  Date_max = max(temp_dat$end), Temp_mean = mean(Temp_df$RIV.TEMP, na.rm=TRUE), Temp_sd = sd(Temp_df$RIV.TEMP, na.rm=TRUE), Stage_mean = mean(Stage_df$VALUE, na.rm=TRUE), Stage_sd = sd(Stage_df$VALUE, na.rm=TRUE))
  Keeper_dat <- rbind(Keeper_dat, new_dat)
}

head(Keeper_dat)
Keeper_dat <- Keeper_dat[-1,]

model_df <- merge(Keeper_dat[,-c(2:3)], fish_dat, by = "FishID", all = TRUE)
head(model_df)
colnames(model_df)[16] <- "Year"

sapply(X = model_df, FUN = function(x) sum(is.na(x))) #just the 50 JSATS
check_sd <- model_df[is.na(model_df$SD),] #6, all YBUS

write.csv(model_df[,-6], "results/SD/model_dat.csv")
