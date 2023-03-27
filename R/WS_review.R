# route diversity by water year

# library
library(dplyr)
library(vegan)

# data
model_dat_complete<-read.csv("results/SD/model_dat.csv")
head(model_dat_complete)

# add water year type
Year<-c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)
WaterYear<-c("Dry", "Critically Dry", "Dry", "Below Normal", "Wet", "Below Normal", "Dry", "Critically Dry", "Critically Dry", "Below Normal", "Wet")

W_Y <- data.frame(Year, WaterYear)
dat_tt_yr <- merge(model_dat_complete, W_Y, by="Year")

dat_tt_yr$WaterYear = factor(dat_tt_yr$WaterYear, c("Critically Dry", "Dry", "Below Normal", "Wet"))

# make release and estuary entry numeric
data_doy <- dat_tt_yr %>%
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

data_na <- na.omit(data_doy)

# travel time
ws_dat <-dat_tt_yr %>%
  group_by(WaterYear) %>%
  summarise(route_diversity=length(unique(Route)))

Route_diversity_tt <- dat_tt_yr %>%
  group_by(Route) %>%
  summarize(count = n(),
            Shannon.indx = diversity(travel_time, index = "shannon"),
            mean = mean(travel_time, na.rm=TRUE),
            SD = sd(travel_time, na.rm=TRUE))%>%
  mutate(CV=SD/mean)

# estuary entry

Route_diversity_ee <- data_na %>%
  group_by(Route) %>%
  summarize(count = n(),
            Shannon.indx = diversity(dowy_end, index = "shannon"),
            mean = mean(dowy_end, na.rm=TRUE),
            SD = sd(dowy_end, na.rm=TRUE))%>%
  mutate(CV=SD/mean)

WaterYear_diversity_ee <- data_na %>%
  group_by(WaterYear) %>%
  summarize(count = n(),
            Shannon.indx = diversity(dowy_end, index = "shannon"),
            mean = mean(dowy_end, na.rm=TRUE),
            SD = sd(dowy_end, na.rm=TRUE))%>%
  mutate(CV=SD/mean)

Year_diversity_ee <- data_na %>%
  group_by(Year) %>%
  summarize(count = n(),
            Shannon.indx = diversity(dowy_end, index = "shannon"),
            mean = mean(dowy_end, na.rm=TRUE),
            SD = sd(dowy_end, na.rm=TRUE))%>%
  mutate(CV=SD/mean)

Year_diversity_ee <- merge(Year_diversity_ee, W_Y, by="Year")

Year_diversity_ee$WaterYear = factor(Year_diversity_ee$WaterYear, c("Critically Dry", "Dry", "Below Normal", "Wet"))

plot(data_na$Route, data_na$dowy_end)
plot(data_na$WaterYear, data_na$dowy_end)
plot(data_na$WaterYear, data_na$dowy_end)
plot(Year_diversity_ee$WaterYear, Year_diversity_ee$SD)

# evenness

year_dat <-dat_tt_yr %>%
  group_by(Year, Route) %>%
  summarise(route_diversity=length(unique(FishID)))

year_dat <- merge(year_dat, W_Y, by="Year")

W_Year_diversity <- year_dat %>%
  group_by(WaterYear) %>%
  summarize(count = n(),
            Shannon.indx = diversity(route_diversity, index = "shannon"),
            mean = mean(route_diversity, na.rm=TRUE),
            SD = sd(route_diversity, na.rm=TRUE))%>%
  mutate(CV=SD/mean)

W_Year_diversity <- year_dat %>%
  group_by(WaterYear, Route) %>%
  summarize(count = n(),
            Shannon.indx = diversity(route_diversity, index = "shannon"),
            mean = mean(route_diversity, na.rm=TRUE),
            SD = sd(route_diversity, na.rm=TRUE))%>%
  mutate(CV=SD/mean)

wy_dat <-dat_tt_yr %>%
  group_by(Year) %>%
  summarise(route_diversity=length(unique(Route)))

wyear_dat <- merge(wy_dat, W_Y, by="Year")

ws_dat %>%
  group_by(WaterYear) %>%
  summarize(count = n(),
            Shannon.indx = diversity(route_diversity, index = "shannon"),
            mean = mean(route_diversity, na.rm=TRUE),
            SD = sd(route_diversity, na.rm=TRUE))%>%
  mutate(CV=SD/mean)

water_year_dat <-dat_tt_yr %>%
  group_by(WaterYear, Route) %>%
  summarise(route_diversity=length(unique(FishID)))

water_year_dat %>%
  group_by(WaterYear) %>%
  summarize(count = n(),
            Shannon.indx = diversity(route_diversity, index = "shannon"),
            mean = mean(route_diversity, na.rm=TRUE),
            SD = sd(route_diversity, na.rm=TRUE))%>%
  mutate(CV=SD/mean)

# distribution
mean(data_na$travel_time)
sd(data_na$travel_time)

# check on sd stage
stage_dat <- read.csv("results/SD/stage_daily_mean.csv")
stage_dat$date <- as.Date(stage_dat$date)
plot(stage_dat$date, stage_dat$sd)

# run models with estuary entry
plot(data_na$travel_time, data_na$dowy_end)

bwplot(dowy_end ~ Release_Group_SAIL, data = data_doy)

coplot(SD ~  dowy_end| Release_Group_SAIL, data = data_na,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })


modGI_null <- gam(dowy_end ~ Release_Group_SAIL,
                  data=data_na, method="REML", family="gaussian")

modGI_temp_mean <- gam(dowy_end ~ Release_Group_SAIL +
                         te(temp_mean, bs="cc", k=12, m=2) +
                         te(temp_mean, by=Release_Group_SAIL, bs= "cc",
                            k=12, m=1),
                       data=data_na, method="REML", family="gaussian")

modGI_temp_sd <- gam(dowy_end ~ Release_Group_SAIL +
                       te(temp_sd, bs="tp", k=12, m=2) +
                       te(temp_sd, by=Release_Group_SAIL, bs= "tp",
                          k=12, m=1),
                     data=data_na, method="REML", family="gaussian")

modGI_stage_sd <- gam(dowy_end ~ Release_Group_SAIL +
                        te(stage_sd, bs="tp", k=10, m=2) +
                        te(stage_sd, by=Release_Group_SAIL, bs="tp",
                           k=10, m=1),
                      data=data_na, method="REML", family="gaussian")

modGI_stage_mean <- gam(dowy_end ~ Release_Group_SAIL +
                          te(stage_mean, bs="tp", k=10, m=2) +
                          te(stage_mean, by=Release_Group_SAIL, bs="tp",
                             k=10, m=1),
                        data=data_na, method="REML", family="gaussian")

modGI_yr <- gam(dowy_end ~ Release_Group_SAIL + Year,
                data=data_na, method="REML", family="gaussian")

modGI_gr <- gam(dowy_end ~ Release_Group_SAIL + Group,
                data=data_na, method="REML", family="gaussian")

modGI_or <- gam(dowy_end ~ Release_Group_SAIL + origin,
                data=data_na, method="REML", family="gaussian")

modGI_r <- gam(dowy_end ~ Release_Group_SAIL + Route,
               data=data_na, method="REML", family="gaussian")

modGI_tag <- gam(dowy_end ~ Release_Group_SAIL + TagType,
                 data=data_na, method="REML", family="gaussian")

modGI_tt <- gam(dowy_end ~ Release_Group_SAIL + tt.grp,
                data=data_na, method="REML", family="gaussian")

modGI_rel <- gam(dowy_end ~ Release_Group_SAIL +
                   te(dowy_rel, bs="cc", k=12, m=2) +
                   te(dowy_rel, by=Release_Group_SAIL, bs= "cc",
                      k=12, m=1),
                 data=data_na, method="REML", family="gaussian")

modGI_SD <- gam(dowy_end ~ Release_Group_SAIL +
                   te(SD, bs="cc", k=12, m=2) +
                   te(SD, by=Release_Group_SAIL, bs= "cc",
                      k=12, m=1),
                 data=data_na, method="REML", family="gaussian")

modGI_sac_mean  <- gam(dowy_end ~ Release_Group_SAIL +
                         te(sac_mean, bs="tp", k=12, m=2) +
                         te(sac_mean, by=Release_Group_SAIL, bs= "tp",
                            k=12, m=1),
                       data=data_na, method="REML", family="gaussian")

modGI_sac_sd  <- gam(dowy_end ~ Release_Group_SAIL +
                       te(sac_sd, bs="tp", k=12, m=2) +
                       te(sac_sd, by=Release_Group_SAIL, bs= "tp",
                          k=12, m=1),
                     data=data_na, method="REML", family="gaussian")

modGI_max_inun  <- gam(dowy_end ~ Release_Group_SAIL +
                         te(max_inun, bs="tp", k=12, m=2) +
                         te(max_inun, by=Release_Group_SAIL, bs= "tp",
                            k=12, m=1),
                       data=data_na, method="REML", family="gaussian")

modGI_sum_inun  <- gam(dowy_end ~ Release_Group_SAIL +
                         te(sum_inun, bs="tp", k=12, m=2) +
                         te(sum_inun, by=Release_Group_SAIL, bs= "tp",
                            k=12, m=1),
                       data=data_na, method="REML", family="gaussian")

modGI_PDO  <- gam(dowy_end ~ Release_Group_SAIL +
                    te(PDO, bs="tp", k=12, m=2) +
                    te(PDO, by=Release_Group_SAIL, bs= "tp",
                       k=12, m=1),
                  data=data_na, method="REML", family="gaussian")

modGI_Weight  <- gam(dowy_end ~ Release_Group_SAIL +
                       te(Weight, bs="tp", k=12, m=2) +
                       te(Weight, by=Release_Group_SAIL, bs= "tp",
                          k=12, m=1),
                     data=data_na, method="REML", family="gaussian")

modGI_FL  <- gam(dowy_end ~ Release_Group_SAIL +
                   te(FL, bs="tp", k=12, m=2) +
                   te(FL, by=Release_Group_SAIL, bs= "tp",
                      k=12, m=1),
                 data=data_na, method="REML", family="gaussian")

modGI_River_temp <- gam(dowy_end ~ Release_Group_SAIL +
                          te(River_temp, bs="cc", k=12, m=2) +
                          te(River_temp, by=Release_Group_SAIL, bs= "cc",
                             k=12, m=1),
                        data=data_na, method="REML", family="gaussian")

modGI_Transport_distance <- gam(dowy_end ~ Release_Group_SAIL +
                                  te(Transport_distance, bs="tp", k=12, m=2) +
                                  te(Transport_distance, by=Release_Group_SAIL, bs= "tp",
                                     k=12, m=1),
                                data=data_na, method="REML", family="gaussian")

AIC(modGI_null, modGI_temp_mean, modGI_temp_sd, modGI_stage_sd, modGI_stage_mean, modGI_yr, modGI_gr, modGI_or, modGI_r, modGI_tag, modGI_tt, modGI_rel, modGI_SD, modGI_River_temp, modGI_FL, modGI_Weight, modGI_PDO, modGI_sum_inun, modGI_max_inun, modGI_sac_sd, modGI_sac_mean) # all release related variable
# release date, 87.7%
# genetic group, 84.9%
# release temperature, 85.1%
# PDO, 83.9%
# year, 75%

# route by water year
library(vegan)

Year_route <- data_na %>%
  group_by(Year, Route) %>%
  summarise(count = n())

WaterYear_route <- merge(Year_route, W_Y, by="Year")

WaterYear_route$WaterYear = factor(WaterYear_route$WaterYear, c("Critically Dry", "Dry", "Below Normal", "Wet"))

shannon_route <- WaterYear_route %>%
  group_by(WaterYear, Route) %>%
  summarise(Shannon.indx = diversity(count, index = "shannon"))

plot(shannon_route$Route, shannon_route$Shannon.indx)
plot(shannon_route$WaterYear, shannon_route$Shannon.indx)

library(ggplot2)
p <- ggplot(shannon_route, aes(Route, Shannon.indx))

p + geom_jitter(aes(colour = WaterYear))


