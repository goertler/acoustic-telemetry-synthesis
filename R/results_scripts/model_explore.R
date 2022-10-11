library(car)
library(dplyr)
library(lattice)

# data

data <- read.csv("results/SD/model_dat.csv")
head(data)
str(data)

# a few follow up diagnostic plots now that we have enviro data
coplot(temp_mean ~  stage_mean | Year, data = data,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(temp_mean ~  stage_mean | Release_Group_SAIL, data = data,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

coplot(temp_sd ~  stage_sd | Year, data = data,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })

scatterplotMatrix(~SD + travel_time + temp_mean + stage_mean + temp_sd + stage_sd | Route, data=data)
scatterplotMatrix(~SD + travel_time + temp_mean + stage_mean + temp_sd + stage_sd | Year, data=data)
scatterplotMatrix(~SD + travel_time + temp_mean + stage_mean + temp_sd + stage_sd | Release_Group_SAIL, data=data)

# make dates into day of water year to examine VIF

data_doy <- data %>%
  mutate(month_end = lubridate::month(end),
         year_end  = lubridate::year(end),
         rdoy_end  = lubridate::yday(end) + 92,
         week_end  = lubridate::week(end),
         water_year_end = ifelse(month_end > 9, year_end + 1, year_end),
         dowy_end = ifelse(rdoy_end > 366, rdoy_end - 366, rdoy_end),
         month_rel = lubridate::month(rel),
         year_rel  = lubridate::year(rel),
         rdoy_rel  = lubridate::yday(rel) + 92,
         week_rel  = lubridate::week(rel),
         water_year_rel = ifelse(month_rel > 9, year_rel + 1, year_rel),
         dowy_rel = ifelse(rdoy_rel > 366, rdoy_rel - 366, rdoy_rel))

cor(data_doy[,c(3:6,8,16,24,30)], use = "complete.obs") # vif is failing because high cor with rel and estuary entry dates (0.9169428), temp_mean and dowy_rel are next hightest (0.8916105)

# side note - stage_sd and travel_time are high (0.8577677)
Route <- unique(data_doy$Route)
color <-  c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")

palette <- data.frame(Route, color)
data_doy<- merge(data_doy, palette, by = "Route")

plot(data_doy$travel_time, data_doy$stage_sd, col = data_doy$color)

plot(data_doy$travel_time, data_doy$SD, col = data_doy$color)

# Group & origin cannot be in the same model

cor(data_doy[,c(3:6,8,16)], use = "complete.obs")

monster_model <- lm(SD ~ temp_mean + stage_mean + Year + temp_sd + stage_sd + travel_time + Release_Group_SAIL + Route + origin + TagType + tt.grp + dowy_end + dowy_rel, data = data_doy)

vif(monster_model)
# dowy_rel - 110.159179 GVIF (or 10.495674 GVIF to the power of 1/2df)

next_model <- lm(SD ~ temp_mean + stage_mean + Year + temp_sd + stage_sd + travel_time + Release_Group_SAIL + Route + origin + TagType + tt.grp + dowy_end, data = data_doy)

vif(next_model)
# Release_Group_SAIL 10.232275 GVIF (or 1.788517 GVIF to the power of 1/2df)

smaller_model <- lm(SD ~ temp_mean + stage_mean + Year + temp_sd + stage_sd + travel_time + Route + origin + TagType + tt.grp + dowy_end, data = data_doy)

vif(smaller_model)

histogram( ~ SD | Release_Group_SAIL, data = data_doy, breaks = 100)
bwplot(SD ~ Release_Group_SAIL, data = data_doy)

smaller2_model <- lm(SD ~ Release_Group_SAIL + stage_mean + Year + temp_sd + stage_sd + travel_time + Route + origin + TagType + tt.grp + dowy_end, data = data_doy)

vif(smaller2_model)

# hypotheses: (1) the variation in travel by day is a measure of diversity and resilience in ocean-ward migration of juvenile chinook, this variation is driven by (a) demographics, (b) environment and (c) tagging practices, (2) variation in route and (3) travel time are also indicators of diversity

# model building
bwplot(SD ~ Release_Group_SAIL, data = data_doy)
data_doy$Year <- as.character(data_doy$Year)
bwplot(SD ~ Year, data = data_doy)
bwplot(SD ~ Group, data = data_doy)
bwplot(SD ~ origin, data = data_doy)
bwplot(SD ~ Route, data = data_doy)
bwplot(SD ~ TagType, data = data_doy)
bwplot(SD ~ tt.grp, data = data_doy)

bwplot(temp_mean ~ Route, data = data_doy)
bwplot(temp_sd ~ Route, data = data_doy)
bwplot(stage_mean ~ Route, data = data_doy)
bwplot(stage_sd ~ Route, data = data_doy)
bwplot(travel_time ~ Route, data = data_doy)
bwplot(dowy_end ~ Route | Group, data = data_doy)
bwplot(dowy_end ~ Route | Release_Group_SAIL, data = data_doy)

bwplot(travel_time ~ Release_Group_SAIL, data = data_doy)
bwplot(travel_time ~ Year, data = data_doy)
bwplot(travel_time ~ Group, data = data_doy)
bwplot(travel_time ~ origin, data = data_doy)
bwplot(travel_time ~ TagType, data = data_doy)
bwplot(travel_time ~ tt.grp, data = data_doy)

unique(data_doy[,c(11:12)])

# autocorrelation
data_na <- na.omit(data)
acf(data_na$SD)
acf(data_na$travel_time)
## not good, but also might not be relevant (data is not organized by time or space, but individual characteristics)

# try a few gams
library(mgcv)

data_na$Release_Group_SAIL <- as.factor(data_na$Release_Group_SAIL)

model <- gam(log(SD) ~ s(log(travel_time), k=5, m=2) +
      s(log(travel_time), Release_Group_SAIL, k=3, bs="fs", m=2),
    data=data_na, method="REML")



# travel time

model_tt_stage <- gam(log(travel_time) ~ s(log(stage_sd ), k=5, m=2) +
               s(log(stage_sd ), Release_Group_SAIL, k=3, bs="fs", m=2),
             data=data_na, method="REML") #Deviance explained = 76.5%

model_tt_both <- gam(log(travel_time) ~ s(log(stage_sd ), k=5, m=2) +
                  s(log(stage_sd ), Release_Group_SAIL, k=3, bs="fs", m=2) +
                  s(log(temp_sd ), k=5, m=2) +
                  s(log(temp_sd ), Release_Group_SAIL, k=3, bs="fs", m=2),
                data=data_na, method="REML") #Deviance explained = 81.9%

model_tt_0_stage <- gam(log(travel_time) ~ s(log(stage_sd ), k=5, m=2),
                data=data_na, method="REML")

model_tt_temp <- gam(log(travel_time) ~ s(log(temp_sd ), k=5, m=2) +
                    s(log(temp_sd ), Release_Group_SAIL, k=3, bs="fs", m=2),
                  data=data_na, method="REML")

model_tt_0_temp <- gam(log(travel_time) ~ s(log(temp_sd ), k=5, m=2),
                  data=data_na, method="REML")

AIC(model_tt_stage, model_tt_both, model_tt_0_stage, model_tt_temp, model_tt_0_temp)

summary(model_tt_both)
gam.check(model_tt_both)

boxplot(stage_sd ~ Year, data = data_doy)
boxplot(temp_sd ~ Year, data = data_doy)

hist(data_doy$stage_sd, breaks = 100)
hist(data_doy$temp_sd)
hist(data_doy$stage_mean, breaks = 100)
hist(data_doy$temp_mean, breaks = 100)

plot(data_doy$stage_sd, data_doy$travel_time)

# look into cyclical spline for mean_temp
# look at stage data

# summary table

table <- data_na %>%
  group_by(Release_Group_SAIL, Year, Group, origin) %>%
  summarise(n = sum(!is.na(unique(FishID))))

write.csv(table, "table_1.csv")


