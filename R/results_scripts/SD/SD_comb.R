# library
library(lattice)
library(dplyr)
library(car)

# load data

mj_12_sd <- read.csv("results/SD/YoloAce_12.csv")
mj_13_sd <- read.csv("results/SD/YoloAce_13.csv")
ybus_sd <- read.csv("results/SD/YBUS.csv")
jsats_13_sd <- read.csv("results/SD/JSATS_13.csv")
jsats_14_sd <- read.csv("results/SD/JSATS_14.csv")
jsats_15_sd <- read.csv("results/SD/JSATS_15.csv")
jsats_16_sd <- read.csv("results/SD/JSATS_16.csv")
jsats_17_sd <- read.csv("results/SD/JSATS_17.csv")
cm_07_sd <- read.csv("results/SD/CMvemco_07.csv")
cm_08_sd <- read.csv("results/SD/CMvemco_08.csv")
cm_09_sd <- read.csv("results/SD/CMvemco_09.csv")
cm_10_sd <- read.csv("results/SD/CMvemco_10.csv")
cm_11_sd <- read.csv("results/SD/CMvemco_11.csv")

head(mj_12_sd)
head(mj_13_sd)
head(ybus_sd)
head(jsats_13_sd)
head(jsats_14_sd)
head(jsats_15_sd)
head(jsats_16_sd)
head(jsats_17_sd)
head(cm_07_sd)
head(cm_08_sd)
head(cm_09_sd)
head(cm_10_sd)
head(cm_11_sd)

# wasn't consistent with year

sd_all <- rbind(mj_12_sd[,-c(1,6)], mj_13_sd[,-c(1,6)], ybus_sd[,-c(1,6)], jsats_13_sd[,-1], jsats_14_sd[,-1], jsats_15_sd[,-1], jsats_16_sd[,-1], jsats_17_sd[,-1], cm_07_sd[,-1], cm_08_sd[,-1], cm_09_sd[,-1], cm_10_sd[,-1], cm_11_sd[,-1]) # 1806

FishID_key <- read.csv("data/common_data/FishID_key.csv")

sd_meta <- merge(FishID_key, sd_all, by = "FishID", all = TRUE)
head(sd_meta) #1814

sd_meta$check_year <- duplicated(sd_meta)
sd_check <- subset(sd_meta, check_year == FALSE)

sum(is.na(sd_meta) == TRUE) # 24
summary(is.na(sd_meta) == TRUE) #8 fish
check <- subset(sd_meta, is.na(SD) == TRUE) # JSAT fish from 2016 & 2017
summary(sd_meta) # looks okay

# diagnostics

#outliers
boxplot(sd_meta$SD)
dotchart(sd_meta$SD) # alright, but not the best

#homogeneity of variance
bwplot(SD ~ Release_Group_SAIL | Route, data = sd_meta)
bwplot(SD ~ TagType | tt.grp, data = sd_meta)
bwplot(SD ~ Group | origin, data = sd_meta)
sd_meta$Year <- as.factor(sd_meta$Year)
bwplot(SD ~ Year | TagType, data = sd_meta)
bwplot(SD ~ Year | tt.grp, data = sd_meta)

max(sd_meta$SD, na.rm = TRUE)
subset(sd_meta, SD >= 88377) # 57.MJ

# add day of water year
sd_meta$rel <- as.Date(sd_meta$rel)

sd_meta_doy <- sd_meta %>%
  mutate(month = lubridate::month(rel),
         year  = lubridate::year(rel),
         rdoy  = lubridate::yday(rel) + 92,
         week  = lubridate::week(rel),
         water_year = ifelse(month > 9, year + 1, year),
         dowy = ifelse(rdoy > 366, rdoy - 366, rdoy))

dotplot(as.matrix(sd_meta_doy[,c(4,12,20)]), groups = FALSE,
        strip = strip.custom(bg = 'white',
                             par.strip.text = list(cex = 0.8)),
        scales = list(x = list(relation = "free"),
                      y = list(relation = "free"),
                      draw = FALSE),
        col = 1, cex  = 0.5, pch = 16)

#distribution
hist(sd_meta$SD)
hist(log(sd_meta$SD)) # log normal
histogram( ~ SD | Release_Group_SAIL, data = sd_meta, breaks = 100)
histogram( ~ SD | Route, data = sd_meta, breaks = 100)
histogram( ~ SD | TagType, data = sd_meta, breaks = 100)
histogram( ~ SD | tt.grp, data = sd_meta, breaks = 100)
histogram( ~ SD | Group, data = sd_meta, breaks = 100)
histogram( ~ SD | Year, data = sd_meta, breaks = 100)

#zeros
sd_meta_rmnas <- sd_meta[!is.na(sd_meta$SD),]
plot(table(round(sd_meta_rmnas$SD * sd_meta_rmnas$SD)),
     type = "h")

#relationships
# also want to look at end
sd_meta_doy <- sd_meta_doy[,-c(15:19)] %>%
  mutate(month = lubridate::month(end),
         year  = lubridate::year(end),
         rdoy  = lubridate::yday(end) + 92,
         week  = lubridate::week(end),
         water_year = ifelse(month > 9, year + 1, year),
         dowy_end = ifelse(rdoy > 366, rdoy - 366, rdoy))
colnames(sd_meta_doy)[15] <- "dowy_rel"

scatterplotMatrix(~SD + travel_time + dowy_rel + dowy_end | TagType, data=sd_meta_doy)
scatterplotMatrix(~SD + travel_time + dowy_rel + dowy_end | tt.grp, data=sd_meta_doy)
scatterplotMatrix(~SD + travel_time + dowy_rel + dowy_end | Release_Group_SAIL, data=sd_meta_doy)
scatterplotMatrix(~SD + travel_time + dowy_rel + dowy_end | Year, data=sd_meta_doy)
scatterplotMatrix(~SD + travel_time + dowy_rel + dowy_end | Group, data=sd_meta_doy)
scatterplotMatrix(~SD + travel_time + dowy_rel + dowy_end | origin, data=sd_meta_doy)
scatterplotMatrix(~SD + travel_time + dowy_rel + dowy_end | Route, data=sd_meta_doy)

#interactions
coplot(dowy_rel ~  dowy_end | Year, data = sd_meta_doy,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(dowy_rel ~  dowy_end | TagType, data = sd_meta_doy,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(dowy_rel ~  dowy_end | tt.grp, data = sd_meta_doy,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(dowy_rel ~  travel_time | Release_Group_SAIL, data = sd_meta_doy,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(dowy_rel ~  travel_time | origin, data = sd_meta_doy,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(dowy_rel ~  travel_time | Group, data = sd_meta_doy,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

# need to remove unique values
sd_meta_doy <- subset(sd_meta_doy, Route != "Yolo, SacRSlough, CenDel")

coplot(dowy_end ~  travel_time | Route, data = sd_meta_doy,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })
