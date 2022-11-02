# library
library(mgcv)
library(dplyr)
library(lattice)

# data
model_dat_complete<-read.csv("results/SD/model_dat.csv")

model_dat_complete$rel <- as.Date(model_dat_complete$rel)
model_dat_complete$end <- as.Date(model_dat_complete$end)

data_doy <- model_dat_complete %>%
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
data_na$Release_Group_SAIL <- as.factor(data_na$Release_Group_SAIL)
data_na$Year <- as.factor(data_na$Year)
data_na$Group <- as.factor(data_na$Group)
data_na$origin <- as.factor(data_na$origin)
data_na$Route <- as.factor(data_na$Route)
data_na$TagType <- as.factor(data_na$TagType)
data_na$tt.grp <- as.factor(data_na$tt.grp)
data_na$FL <- as.numeric(data_na$FL)
data_na$sum_inun <- as.numeric(data_na$sum_inun)

# look at covars (n = 37) relationship simply
cor_results <- cor(data_na[,c(3:10,12,20,23:27,33,39)], use = "complete.obs")

# already know from model_explore.R that Group & origin cannot be in the same model

monster_model <- lm(SD ~ temp_mean + stage_mean + Year + temp_sd + stage_sd + travel_time + Release_Group_SAIL + Route + origin + TagType + tt.grp + dowy_end + dowy_rel + FL + River_temp + Transport_distance + sac_mean + sac_sd + max_inun + sum_inun + PDO + Weight, data = data_na)

vif(monster_model) # max_inun 12.100949

model_2 <- lm(SD ~ temp_mean + stage_mean + Year + temp_sd + stage_sd + travel_time + Release_Group_SAIL + Route + origin + TagType + tt.grp + dowy_end + dowy_rel + FL + River_temp + Transport_distance + sac_mean + sac_sd + sum_inun + PDO + Weight, data = data_na)

vif(model_2) # dowy_rel 12.085545

model_3 <- lm(SD ~ temp_mean + stage_mean + Year + temp_sd + stage_sd + travel_time + Release_Group_SAIL + Route + origin + TagType + tt.grp + dowy_end + FL + River_temp + Transport_distance + sac_mean + sac_sd + sum_inun + PDO + Weight, data = data_na)

vif(model_3) # PDO 11.017446

model_4 <- lm(SD ~ temp_mean + stage_mean + Year + temp_sd + stage_sd + travel_time + Release_Group_SAIL + Route + origin + TagType + tt.grp + dowy_end + FL + River_temp + Transport_distance + sac_mean + sac_sd + sum_inun + Weight, data = data_na)

vif(model_4) # FL 9.023635

model_5 <- lm(SD ~ temp_mean + stage_mean + Year + temp_sd + stage_sd + travel_time + Release_Group_SAIL + Route + origin + TagType + tt.grp + dowy_end + River_temp + Transport_distance + sac_mean + sac_sd + sum_inun + Weight, data = data_na)

vif(model_5) # stage_mean  5.319059

model_6 <- lm(SD ~ temp_mean + Year + temp_sd + stage_sd + travel_time + Release_Group_SAIL + Route + origin + TagType + tt.grp + dowy_end + River_temp + Transport_distance + sac_mean + sac_sd + sum_inun + Weight, data = data_na)

vif(model_6) # sum_inun 4.807004

model_7 <- lm(SD ~ temp_mean + Year + temp_sd + stage_sd + travel_time + Release_Group_SAIL + Route + origin + TagType + tt.grp + dowy_end + River_temp + Transport_distance + sac_mean + sac_sd + Weight, data = data_na)

vif(model_7) # TagType 4.387102

model_8 <- lm(SD ~ temp_mean + Year + temp_sd + stage_sd + travel_time + Release_Group_SAIL + Route + origin + tt.grp + dowy_end + River_temp + Transport_distance + sac_mean + sac_sd + Weight, data = data_na)

vif(model_8) # dowy_end 4.124227

model_9 <- lm(SD ~ temp_mean + Year + temp_sd + stage_sd + travel_time + Release_Group_SAIL + Route + origin + tt.grp + River_temp + Transport_distance + sac_mean + sac_sd + Weight, data = data_na)

vif(model_9) # Transport_distance 3.537392

# check on new covars
bwplot(sac_mean ~ Release_Group_SAIL, data = data_doy)
bwplot(sac_sd ~ Release_Group_SAIL, data = data_doy)
bwplot(max_inun ~ Release_Group_SAIL, data = data_doy)
bwplot(sum_inun ~ Release_Group_SAIL, data = data_doy)
bwplot(PDO ~ Release_Group_SAIL, data = data_doy)
bwplot(Weight ~ Release_Group_SAIL, data = data_doy)
bwplot(FL ~ Release_Group_SAIL, data = data_doy)
bwplot(River_temp ~ Release_Group_SAIL, data = data_doy)
bwplot(Transport_distance ~ Release_Group_SAIL, data = data_doy)

coplot(SD ~  River_temp| Release_Group_SAIL, data = data_na,
       panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y) })


# testing covars one at a time (only updated SD models with new covars)
modGI_null <- gam(SD ~ Release_Group_SAIL,
                  data=data_na, method="REML", family="gaussian")

modGI_temp_mean <- gam(SD ~ Release_Group_SAIL +
                         te(temp_mean, bs="cc", k=12, m=2) +
                         te(temp_mean, by=Release_Group_SAIL, bs= "cc",
                            k=12, m=1),
                       data=data_na, method="REML", family="gaussian")

modGI_temp_sd <- gam(SD ~ Release_Group_SAIL +
                       te(temp_sd, bs="tp", k=12, m=2) +
                       te(temp_sd, by=Release_Group_SAIL, bs= "tp",
                          k=12, m=1),
                     data=data_na, method="REML", family="gaussian")

modGI_stage_sd <- gam(SD ~ Release_Group_SAIL +
                        te(stage_sd, bs="tp", k=10, m=2) +
                        te(stage_sd, by=Release_Group_SAIL, bs="tp",
                           k=10, m=1),
                      data=data_na, method="REML", family="gaussian")

modGI_stage_mean <- gam(SD ~ Release_Group_SAIL +
                          te(stage_mean, bs="tp", k=10, m=2) +
                          te(stage_mean, by=Release_Group_SAIL, bs="tp",
                             k=10, m=1),
                        data=data_na, method="REML", family="gaussian")

modGI_yr <- gam(SD ~ Release_Group_SAIL + Year,
                data=data_na, method="REML", family="gaussian")

modGI_gr <- gam(SD ~ Release_Group_SAIL + Group,
                data=data_na, method="REML", family="gaussian")

modGI_or <- gam(SD ~ Release_Group_SAIL + origin,
                data=data_na, method="REML", family="gaussian")

modGI_r <- gam(SD ~ Release_Group_SAIL + Route,
               data=data_na, method="REML", family="gaussian")

modGI_tag <- gam(SD ~ Release_Group_SAIL + TagType,
                 data=data_na, method="REML", family="gaussian")

modGI_tt <- gam(SD ~ Release_Group_SAIL + tt.grp,
                data=data_na, method="REML", family="gaussian")

modGI_rel <- gam(SD ~ Release_Group_SAIL +
                   te(dowy_rel, bs="cc", k=12, m=2) +
                   te(dowy_rel, by=Release_Group_SAIL, bs= "cc",
                      k=12, m=1),
                 data=data_na, method="REML", family="gaussian")

modGI_end <- gam(SD ~ Release_Group_SAIL +
                   te(dowy_end, bs="cc", k=12, m=2) +
                   te(dowy_end, by=Release_Group_SAIL, bs= "cc",
                      k=12, m=1),
                 data=data_na, method="REML", family="gaussian")

modGI_sac_mean  <- gam(SD ~ Release_Group_SAIL +
                         te(sac_mean, bs="tp", k=12, m=2) +
                         te(sac_mean, by=Release_Group_SAIL, bs= "tp",
                            k=12, m=1),
                       data=data_na, method="REML", family="gaussian")

modGI_sac_sd  <- gam(SD ~ Release_Group_SAIL +
                       te(sac_sd, bs="tp", k=12, m=2) +
                       te(sac_sd, by=Release_Group_SAIL, bs= "tp",
                          k=12, m=1),
                     data=data_na, method="REML", family="gaussian")

modGI_max_inun  <- gam(SD ~ Release_Group_SAIL +
                         te(max_inun, bs="tp", k=12, m=2) +
                         te(max_inun, by=Release_Group_SAIL, bs= "tp",
                            k=12, m=1),
                       data=data_na, method="REML", family="gaussian")

modGI_sum_inun  <- gam(SD ~ Release_Group_SAIL +
                         te(sum_inun, bs="tp", k=12, m=2) +
                         te(sum_inun, by=Release_Group_SAIL, bs= "tp",
                            k=12, m=1),
                       data=data_na, method="REML", family="gaussian")

modGI_PDO  <- gam(SD ~ Release_Group_SAIL +
                    te(PDO, bs="tp", k=12, m=2) +
                    te(PDO, by=Release_Group_SAIL, bs= "tp",
                       k=12, m=1),
                  data=data_na, method="REML", family="gaussian")

modGI_Weight  <- gam(SD ~ Release_Group_SAIL +
                       te(Weight, bs="tp", k=12, m=2) +
                       te(Weight, by=Release_Group_SAIL, bs= "tp",
                          k=12, m=1),
                     data=data_na, method="REML", family="gaussian")

modGI_FL  <- gam(SD ~ Release_Group_SAIL +
                   te(FL, bs="tp", k=12, m=2) +
                   te(FL, by=Release_Group_SAIL, bs= "tp",
                      k=12, m=1),
                 data=data_na, method="REML", family="gaussian")

modGI_River_temp <- gam(SD ~ Release_Group_SAIL +
                          te(River_temp, bs="cc", k=12, m=2) +
                          te(River_temp, by=Release_Group_SAIL, bs= "cc",
                             k=12, m=1),
                        data=data_na, method="REML", family="gaussian")

modGI_Transport_distance <- gam(SD ~ Release_Group_SAIL +
                                  te(Transport_distance, bs="tp", k=12, m=2) +
                                  te(Transport_distance, by=Release_Group_SAIL, bs= "tp",
                                     k=12, m=1),
                                data=data_na, method="REML", family="gaussian")

AIC(modGI_null, modGI_temp_mean, modGI_temp_sd, modGI_stage_sd, modGI_stage_mean, modGI_yr, modGI_gr, modGI_or, modGI_r, modGI_tag, modGI_tt,
    modGI_rel, modGI_end, modGI_River_temp, modGI_FL, modGI_Weight, modGI_PDO, modGI_sum_inun, modGI_max_inun, modGI_sac_sd, modGI_sac_mean) # mean sac for the win

summary(modGI_sac_mean)# Deviance explained = 46.6%

# add second term to mean stage model
# sac mean
modGI_sac_mean_rel  <- gam(SD ~ Release_Group_SAIL +
                         te(sac_mean, dowy_rel, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                         te(sac_mean, dowy_rel, by=Release_Group_SAIL, bs= c("tp", "cc"),
                            k=c(10, 10), m=1),
                       data=data_na, method="REML", family="gaussian")

modGI_sac_mean_end  <- gam(SD ~ Release_Group_SAIL +
                             te(sac_mean, dowy_end, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                             te(sac_mean, dowy_end, by=Release_Group_SAIL, bs= c("tp", "cc"),
                                k=c(10, 10), m=1),
                           data=data_na, method="REML", family="gaussian")

modGI_sac_mean_yr <- gam(SD ~ Release_Group_SAIL + Year +
                      te(sac_mean, bs="tp", k=12, m=2) +
                      te(sac_mean, by=Release_Group_SAIL, bs= "tp",
                         k=12, m=1),
                    data=data_na, method="REML", family="gaussian")

modGI_sac_mean_gr <- gam(SD ~ Release_Group_SAIL + Group +
                      te(sac_mean, bs="tp", k=12, m=2) +
                      te(sac_mean, by=Release_Group_SAIL, bs= "tp",
                         k=12, m=1),
                    data=data_na, method="REML", family="gaussian")

modGI_sac_mean_or <- gam(SD ~ Release_Group_SAIL + origin +
                      te(sac_mean, bs="tp", k=12, m=2) +
                      te(sac_mean, by=Release_Group_SAIL, bs= "tp",
                         k=12, m=1),
                    data=data_na, method="REML", family="gaussian")

modGI_sac_mean_r <- gam(SD ~ Release_Group_SAIL + Route +
                     te(sac_mean, bs="tp", k=12, m=2) +
                     te(sac_mean, by=Release_Group_SAIL, bs= "tp",
                        k=12, m=1),
                   data=data_na, method="REML", family="gaussian")

modGI_sac_mean_tag <- gam(SD ~ Release_Group_SAIL + TagType +
                       te(sac_mean, bs="tp", k=12, m=2) +
                       te(sac_mean, by=Release_Group_SAIL, bs= "tp",
                          k=12, m=1),
                     data=data_na, method="REML", family="gaussian")

modGI_sac_mean_tt <- gam(SD ~ Release_Group_SAIL + tt.grp +
                      te(sac_mean, bs="tp", k=12, m=2) +
                      te(sac_mean, by=Release_Group_SAIL, bs= "tp",
                         k=12, m=1),
                    data=data_na, method="REML", family="gaussian")


modGI_sac_mean_stage_sd <- gam(SD ~ Release_Group_SAIL +
                            te(stage_sd, sac_mean, bs=c("tp", "tp"), k=c(10, 10), m=2) +
                            te(stage_sd, sac_mean, by=Release_Group_SAIL, bs= c("tp", "tp"),
                               k=c(10, 10), m=1),
                          data=data_na, method="REML", family="gaussian")

modGI_sac_mean_stage_mean <- gam(SD ~ Release_Group_SAIL +
                              te(stage_mean, sac_mean, bs=c("tp", "tp"), k=c(10, 10), m=2) +
                              te(stage_mean, sac_mean, by=Release_Group_SAIL, bs= c("tp", "tp"),
                                 k=c(10, 10), m=1),
                            data=data_na, method="REML", family="gaussian")

modGI_sac_mean_temp_sd <- gam(SD ~ Release_Group_SAIL +
                           te(temp_sd, sac_mean, bs=c("tp", "tp"), k=c(10, 10), m=2) +
                           te(temp_sd, sac_mean, by=Release_Group_SAIL, bs= c("tp", "tp"),
                              k=c(10, 10), m=1),
                         data=data_na, method="REML", family="gaussian")

modGI_sac_mean_temp_mean <- gam(SD ~ Release_Group_SAIL +
                           te(temp_mean, sac_mean, bs=c("cc", "tp"), k=c(10, 10), m=2) +
                           te(temp_mean, sac_mean, by=Release_Group_SAIL, bs= c("cc", "tp"),
                              k=c(10, 10), m=1),
                         data=data_na, method="REML", family="gaussian")


modGI_sac_mean_sac_sd <- gam(SD ~ Release_Group_SAIL +
                                te(sac_sd, sac_mean, bs=c("tp", "tp"), k=c(10, 10), m=2) +
                                te(sac_sd, sac_mean, by=Release_Group_SAIL, bs= c("tp", "tp"),
                                   k=c(10, 10), m=1),
                              data=data_na, method="REML", family="gaussian")
modGI_sac_mean_max_inun <- gam(SD ~ Release_Group_SAIL +
                                te(max_inun, sac_mean, bs=c("tp", "tp"), k=c(10, 10), m=2) +
                                te(max_inun, sac_mean, by=Release_Group_SAIL, bs= c("tp", "tp"),
                                   k=c(10, 10), m=1),
                              data=data_na, method="REML", family="gaussian")
modGI_sac_mean_sum_inun <- gam(SD ~ Release_Group_SAIL +
                                te(sum_inun, sac_mean, bs=c("tp", "tp"), k=c(10, 10), m=2) +
                                te(sum_inun, sac_mean, by=Release_Group_SAIL, bs= c("tp", "tp"),
                                   k=c(10, 10), m=1),
                              data=data_na, method="REML", family="gaussian")
modGI_sac_mean_PDO <- gam(SD ~ Release_Group_SAIL +
                                te(PDO, sac_mean, bs=c("tp", "tp"), k=c(10, 10), m=2) +
                                te(PDO, sac_mean, by=Release_Group_SAIL, bs= c("tp", "tp"),
                                   k=c(10, 10), m=1),
                              data=data_na, method="REML", family="gaussian")
modGI_sac_mean_Weight <- gam(SD ~ Release_Group_SAIL +
                                te(Weight, sac_mean, bs=c("tp", "tp"), k=c(10, 10), m=2) +
                                te(Weight, sac_mean, by=Release_Group_SAIL, bs= c("tp", "tp"),
                                   k=c(10, 10), m=1),
                              data=data_na, method="REML", family="gaussian")
modGI_sac_mean_FL <- gam(SD ~ Release_Group_SAIL +
                                te(FL, sac_mean, bs=c("tp", "tp"), k=c(10, 10), m=2) +
                                te(FL, sac_mean, by=Release_Group_SAIL, bs= c("tp", "tp"),
                                   k=c(10, 10), m=1),
                              data=data_na, method="REML", family="gaussian")
modGI_sac_mean_River_temp <- gam(SD ~ Release_Group_SAIL +
                                te(River_temp, sac_mean, bs=c("tp", "tp"), k=c(10, 10), m=2) +
                                te(River_temp, sac_mean, by=Release_Group_SAIL, bs= c("tp", "tp"),
                                   k=c(10, 10), m=1),
                              data=data_na, method="REML", family="gaussian")
modGI_sac_mean_Transport_distance <- gam(SD ~ Release_Group_SAIL +
                                te(Transport_distance, sac_mean, bs=c("tp", "tp"), k=c(10, 10), m=2) +
                                te(Transport_distance, sac_mean, by=Release_Group_SAIL, bs= c("tp", "tp"),
                                   k=c(10, 10), m=1),
                              data=data_na, method="REML", family="gaussian")

# rel

modGI_rel_stage_sd <- gam(SD ~ Release_Group_SAIL +
                            te(stage_sd, dowy_rel, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                            te(stage_sd, dowy_rel, by=Release_Group_SAIL, bs= c("tp", "cc"),
                               k=c(10, 10), m=1),
                          data=data_na, method="REML", family="gaussian")

modGI_rel_stage_mean <- gam(SD ~ Release_Group_SAIL +
                              te(stage_mean, dowy_rel, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                              te(stage_mean, dowy_rel, by=Release_Group_SAIL, bs= c("tp", "cc"),
                                 k=c(10, 10), m=1),
                            data=data_na, method="REML", family="gaussian")

modGI_rel_temp_sd <- gam(SD ~ Release_Group_SAIL +
                           te(temp_sd, dowy_rel, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                           te(temp_sd, dowy_rel, by=Release_Group_SAIL, bs= c("tp", "cc"),
                              k=c(10, 10), m=1),
                         data=data_na, method="REML", family="gaussian")

modGI_rel_end <- gam(SD ~ Release_Group_SAIL +
                       te(dowy_end, dowy_rel, bs=c("cc", "cc"), k=c(10, 10), m=2) +
                       te(dowy_end, dowy_rel, by=Release_Group_SAIL, bs= c("cc", "cc"),
                          k=c(10, 10), m=1),
                     data=data_na, method="REML", family="gaussian")

modGI_rel_yr <- gam(SD ~ Release_Group_SAIL + Year +
                      te(dowy_rel, bs="cc", k=12, m=2) +
                      te(dowy_rel, by=Release_Group_SAIL, bs= "cc",
                         k=12, m=1),
                    data=data_na, method="REML", family="gaussian")

modGI_rel_gr <- gam(SD ~ Release_Group_SAIL + Group +
                      te(dowy_rel, bs="cc", k=12, m=2) +
                      te(dowy_rel, by=Release_Group_SAIL, bs= "cc",
                         k=12, m=1),
                    data=data_na, method="REML", family="gaussian")

modGI_rel_or <- gam(SD ~ Release_Group_SAIL + origin +
                      te(dowy_rel, bs="cc", k=12, m=2) +
                      te(dowy_rel, by=Release_Group_SAIL, bs= "cc",
                         k=12, m=1),
                    data=data_na, method="REML", family="gaussian")

modGI_rel_r <- gam(SD ~ Release_Group_SAIL + Route +
                     te(dowy_rel, bs="cc", k=12, m=2) +
                     te(dowy_rel, by=Release_Group_SAIL, bs= "cc",
                        k=12, m=1),
                   data=data_na, method="REML", family="gaussian")

modGI_rel_tag <- gam(SD ~ Release_Group_SAIL + TagType +
                       te(dowy_rel, bs="cc", k=12, m=2) +
                       te(dowy_rel, by=Release_Group_SAIL, bs= "cc",
                          k=12, m=1),
                     data=data_na, method="REML", family="gaussian")

modGI_rel_tt <- gam(SD ~ Release_Group_SAIL + tt.grp +
                      te(dowy_rel, bs="cc", k=12, m=2) +
                      te(dowy_rel, by=Release_Group_SAIL, bs= "cc",
                         k=12, m=1),
                    data=data_na, method="REML", family="gaussian")


modGI_rel_FL <- gam(SD ~ Release_Group_SAIL +
                            te(FL, dowy_rel, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                            te(FL, dowy_rel, by=Release_Group_SAIL, bs= c("tp", "cc"),
                               k=c(10, 10), m=1),
                          data=data_na, method="REML", family="gaussian")

modGI_rel_Weight <- gam(SD ~ Release_Group_SAIL +
                            te(Weight, dowy_rel, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                            te(Weight, dowy_rel, by=Release_Group_SAIL, bs= c("tp", "cc"),
                               k=c(10, 10), m=1),
                          data=data_na, method="REML", family="gaussian")

modGI_rel_PDO <- gam(SD ~ Release_Group_SAIL +
                            te(PDO, dowy_rel, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                            te(PDO, dowy_rel, by=Release_Group_SAIL, bs= c("tp", "cc"),
                               k=c(10, 10), m=1),
                          data=data_na, method="REML", family="gaussian")

modGI_rel_sum_inun <- gam(SD ~ Release_Group_SAIL +
                       te(sum_inun, dowy_rel, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                       te(sum_inun, dowy_rel, by=Release_Group_SAIL, bs= c("tp", "cc"),
                          k=c(10, 10), m=1),
                     data=data_na, method="REML", family="gaussian")

modGI_rel_max_inun <- gam(SD ~ Release_Group_SAIL +
                       te(max_inun, dowy_rel, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                       te(max_inun, dowy_rel, by=Release_Group_SAIL, bs= c("tp", "cc"),
                          k=c(10, 10), m=1),
                     data=data_na, method="REML", family="gaussian")

modGI_rel_sac_sd <- gam(SD ~ Release_Group_SAIL +
                            te(sac_sd, dowy_rel, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                            te(sac_sd, dowy_rel, by=Release_Group_SAIL, bs= c("tp", "cc"),
                               k=c(10, 10), m=1),
                          data=data_na, method="REML", family="gaussian")

modGI_rel_temp <- gam(SD ~ Release_Group_SAIL +
                       te(River_temp, dowy_rel, bs=c("cc", "cc"), k=c(10, 10), m=2) +
                       te(River_temp, dowy_rel, by=Release_Group_SAIL, bs= c("cc", "cc"),
                          k=c(10, 10), m=1),
                     data=data_na, method="REML", family="gaussian")

AIC(modGI_sac_mean_River_temp, modGI_sac_mean_FL, modGI_sac_mean_Weight, modGI_sac_mean_PDO, modGI_sac_mean_sum_inun, modGI_sac_mean_max_inun, modGI_sac_mean_sac_sd, modGI_sac_mean_temp_mean, modGI_sac_mean_temp_sd, modGI_sac_mean_stage_mean, modGI_sac_mean_stage_sd, modGI_sac_mean_tt, modGI_sac_mean_tag, modGI_sac_mean_r, modGI_sac_mean_or, modGI_sac_mean_gr, modGI_sac_mean_yr, modGI_sac_mean_end, modGI_sac_mean_rel, modGI_sac_mean, modGI_rel, modGI_rel_yr, modGI_rel_gr, modGI_rel_or, modGI_rel_r, modGI_rel_tag, modGI_rel_tt, modGI_rel_temp_sd, modGI_rel_end, modGI_rel_stage_sd, modGI_rel_stage_mean, modGI_rel_FL, modGI_rel_Weight, modGI_rel_PDO, modGI_rel_sum_inun, modGI_rel_max_inun, modGI_rel_sac_sd, modGI_rel_temp)

summary (modGI_sac_mean_temp_mean) # Deviance explained = 70.3%
summary(modGI_sac_mean_rel) # Deviance explained = 69.2%
summary(modGI_rel_stage_mean) # Deviance explained =   67%

modGI_temp_stage_mean <- gam(log(SD) ~ Release_Group_SAIL +
                            te(stage_mean, temp_mean, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                            te(stage_mean, temp_mean, by=Release_Group_SAIL, bs= c("tp", "cc"),
                               k=c(10, 10), m=1),
                          data=data_na, method="REML", family="gaussian")
AIC(modGI_temp_stage_mean) # 946.5078
summary(modGI_temp_stage_mean) # Deviance explained =   62%

#
modGI_01 <- gam(SD ~ Release_Group_SAIL +
                                  te(temp_mean, sac_mean, temp_sd, bs=c("cc", "tp", "tp"), k=c(10, 10, 10), m=2) +
                                  te(temp_mean, sac_mean, temp_sd, by=Release_Group_SAIL, bs= c("cc", "tp", "tp"),
                                     k=c(10, 10), m=1),
                                data=data_na, method="REML", family="gaussian")

modGI_02 <- gam(SD ~ Release_Group_SAIL +
                  te(temp_mean, sac_mean, sac_sd, bs=c("cc", "tp", "tp"), k=c(10, 10, 10), m=2) +
                  te(temp_mean, sac_mean, sac_sd, by=Release_Group_SAIL, bs= c("cc", "tp", "tp"),
                     k=c(10, 10), m=1),
                data=data_na, method="REML", family="gaussian")

modGI_03 <- gam(SD ~ Release_Group_SAIL +
                  te(temp_mean, sac_mean, stage_sd, bs=c("cc", "tp", "tp"), k=c(10, 10, 10), m=2) +
                  te(temp_mean, sac_mean, stage_sd, by=Release_Group_SAIL, bs= c("cc", "tp", "tp"),
                     k=c(10, 10), m=1),
                data=data_na, method="REML", family="gaussian")

modGI_04 <- gam(SD ~ Release_Group_SAIL +
                  te(temp_mean, sac_mean, max_inun, bs=c("cc", "tp", "tp"), k=c(10, 10, 10), m=2) +
                  te(temp_mean, sac_mean, max_inun, by=Release_Group_SAIL, bs= c("cc", "tp", "tp"),
                     k=c(10, 10), m=1),
                data=data_na, method="REML", family="gaussian")

modGI_05 <- gam(SD ~ Release_Group_SAIL +
                  te(temp_mean, sac_mean, sum_inun, bs=c("cc", "tp", "tp"), k=c(10, 10, 10), m=2) +
                  te(temp_mean, sac_mean, sum_inun, by=Release_Group_SAIL, bs= c("cc", "tp", "tp"),
                     k=c(10, 10), m=1),
                data=data_na, method="REML", family="gaussian")

modGI_06 <- gam(SD ~ Release_Group_SAIL +
                  te(temp_mean, sac_mean, PDO, bs=c("cc", "tp", "tp"), k=c(10, 10, 10), m=2) +
                  te(temp_mean, sac_mean, PDO, by=Release_Group_SAIL, bs= c("cc", "tp", "tp"),
                     k=c(10, 10), m=1),
                data=data_na, method="REML", family="gaussian")

modGI_07 <- gam(SD ~ Release_Group_SAIL +
                  te(temp_mean, sac_mean, FL, bs=c("cc", "tp", "tp"), k=c(10, 10, 10), m=2) +
                  te(temp_mean, sac_mean, FL, by=Release_Group_SAIL, bs= c("cc", "tp", "tp"),
                     k=c(10, 10), m=1),
                data=data_na, method="REML", family="gaussian")

modGI_08 <- gam(SD ~ Release_Group_SAIL +
                  te(temp_mean, sac_mean, Weight, bs=c("cc", "tp", "tp"), k=c(10, 10, 10), m=2) +
                  te(temp_mean, sac_mean, Weight, by=Release_Group_SAIL, bs= c("cc", "tp", "tp"),
                     k=c(10, 10), m=1),
                data=data_na, method="REML", family="gaussian")

AIC(modGI_sac_mean_temp_mean, modGI_sac_mean_rel, modGI_rel_stage_mean, modGI_01, modGI_02, modGI_03, modGI_04, modGI_05, modGI_06, modGI_07, modGI_08)

summary(modGI_02) #Deviance explained = 71.2%

# visualize modGI_sac_mean_temp_mean
library(gratia)
library(patchwork)
library(ggplot2)

vis.gam(modGI_sac_mean_temp_mean)
gam.vcomp(modGI_sac_mean_temp_mean)
draw(modGI_sac_mean_temp_mean)

p1 <- draw(modGI_sac_mean_temp_mean, select = "Release_Group_SAILUpper Sacramento River", partial_match = TRUE, residuals = TRUE, n_contour = 5,
           continuous_fill = scale_fill_distiller(palette = "Spectral", type = "div"))
p2 <- draw(modGI_sac_mean_temp_mean, select = "Release_Group_SAILMiddle Sacramento River", partial_match = TRUE, residuals = TRUE, n_contour = 5,
           continuous_fill = scale_fill_distiller(palette = "Spectral", type = "div"))
p3 <- draw(modGI_sac_mean_temp_mean, select = "Release_Group_SAILTidal Delta", partial_match = TRUE, residuals = TRUE, n_contour = 5,
           continuous_fill = scale_fill_distiller(palette = "Spectral", type = "div"))

p1 + p2 + p3 + plot_layout(ncol = 3)

# custom
# evaluate the smooths at values of their covariates
sm <- smooth_estimates(modGI_sac_mean_temp_mean) %>%
  add_confint()

# add the partial residuals to the data used to fit the model
eg1 <- data_na %>% add_partial_residuals(modGI_sac_mean_temp_mean)

names(eg1)


p_Tidal_Delta <- sm %>%
  filter(smooth == "Tidal Delta") %>%
  ggplot() +
  geom_rug(aes(x = sac_mean),
           data = eg1,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = sac_mean),
              alpha = 0.2) +
  geom_point(aes(x = sac_mean, y = `te(temp_mean,sac_mean):Release_Group_SAILTidal Delta`),
             data = eg1, cex = 1.5, colour = "steelblue3") +
  geom_line(aes(x = sac_mean, y = est), lwd = 1.2) +
  labs(y = "Partial effect", title = "Tidal Delta")

# add colour aesthetic when we plot the partial residuals

td_sm <- subset(sm, Release_Group_SAIL == "Tidal Delta")

td_eg <- subset(eg1, Release_Group_SAIL == "Tidal Delta")

plt_Tidal_Delta <-  ggplot() +
  geom_rug(aes(x = sac_mean),
           data = td_sm,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = sac_mean),
              alpha = 0.2, data = td_sm) +
  geom_line(aes(x = sac_mean, y = est), lwd = 1.2, data = td_sm) +
  labs(y = "Partial effect", title = "Tidal Delta") +
  xlab("Sacramento River outflow") +
  theme_bw()

colnames(td_eg)[42] <- "est"

plt_Tidal_Delta +
  geom_point(aes(x = sac_mean, y = est,
                 colour = Year), # <-- map fac to colour aesthetic
             data = td_eg, cex = 1.5)

# plotting the smooth with partial residuals coloured according to one of the other covariates

plt_Tidal_Delta +
  geom_point(aes(x = sac_mean, y = est,
                 colour = Year, size = temp_mean), # <-- map fac to colour aesthetic
             data = td_eg, alpha = 0.3)

# upper
up_sm <- subset(sm, Release_Group_SAIL == "Upper Sacramento River")

up_eg <- subset(eg1, Release_Group_SAIL == "Upper Sacramento River")

colnames(up_eg)[42] <- "est"

plt_Upper_Sacramento <-  ggplot() +
  geom_rug(aes(x = sac_mean),
           data = up_sm,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = sac_mean),
              alpha = 0.2, data = up_sm) +
  geom_line(aes(x = sac_mean, y = est), lwd = 1.2, data = up_sm) +
  labs(y = "Partial effect", title = "Upper Sacramento River") +
  xlab("Sacramento River outflow") +
  theme_bw()

plt_Upper_Sacramento +
  geom_point(aes(x = sac_mean, y = est,
                 colour = temp_mean, size = temp_mean),
             data = up_eg, alpha = 0.3) +
  scale_colour_viridis_c(option = "plasma") +
  labs(colour = "Temperature (C)") +
  guides(size = FALSE) +
  theme_bw()

# middle

msr_sm <- subset(sm, Release_Group_SAIL == "Middle Sacramento River")

msr_eg <- subset(eg1, Release_Group_SAIL == "Middle Sacramento River")

colnames(msr_eg)[42] <- "est"

plt_Middle_Sacramento <-  ggplot() +
  geom_rug(aes(x = sac_mean),
           data = msr_sm,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = sac_mean),
              alpha = 0.2, data = msr_sm) +
  geom_line(aes(x = sac_mean, y = est), lwd = 1.2, data = msr_sm) +
  labs(y = "Partial effect", title = "Middle Sacramento River") +
  xlab("Sacramento River outflow") +
  theme_bw()

plt_Middle_Sacramento +
  geom_point(aes(x = sac_mean, y = est,
                 colour = temp_mean, size = temp_mean),
             data = msr_eg, alpha = 0.3) +
  scale_colour_viridis_c(option = "plasma") +
  labs(colour = "Temperature (C)") +
  guides(size = FALSE) +
  theme_bw()


# custom layout

plot_1 <- draw(modGI_sac_mean_temp_mean, select = 1, residuals = TRUE, n_contour = 5,
           continuous_fill = scale_fill_distiller(palette = "Spectral", type = "div")) &
  theme_bw() &
  labs(title = "Single Common Smoother", x = "Temperature (C)", y = "Sacramento River outflow")

plot_2 <- plt_Upper_Sacramento +
  geom_point(aes(x = sac_mean, y = est,
                 colour = temp_mean, size = temp_mean),
             data = up_eg, alpha = 0.3) +
  scale_colour_viridis_c(option = "plasma") +
  labs(colour = "Temperature (C)") +
  guides(size = FALSE) +
  theme_bw()

plot_3 <- plt_Middle_Sacramento +
  geom_point(aes(x = sac_mean, y = est,
                 colour = temp_mean, size = temp_mean),
             data = msr_eg, alpha = 0.3) +
  scale_colour_viridis_c(option = "plasma") +
  labs(colour = "Temperature (C)") +
  guides(size = FALSE) +
  theme_bw()

plot_4 <- plt_Tidal_Delta +
  geom_point(aes(x = sac_mean, y = est,
                 colour = temp_mean, size = temp_mean),
             data = td_eg, alpha = 0.3) +
  scale_colour_viridis_c(option = "plasma") +
  labs(colour = "Temperature (C)") +
  guides(size = FALSE) +
  theme_bw()

png("hgam_results_river.png", bg = "transparent", width = 11, height = 20, units = "in", pointsize = 12, res = 350)

plot_1 + plot_2 + plot_3 + plot_4 + plot_layout(nrow = 4)

dev.off()

# temp instead
plt_Upper_Sacramento <-  ggplot() +
  geom_rug(aes(x = temp_mean),
           data = up_sm,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = temp_mean),
              alpha = 0.2, data = up_sm) +
  geom_line(aes(x = temp_mean, y = est), lwd = 1.2, data = up_sm) +
  labs(y = "Partial effect", title = "Upper Sacramento River") +
  xlab("Temperature (C)") +
  theme_bw()

plt_Middle_Sacramento <-  ggplot() +
  geom_rug(aes(x = temp_mean),
           data = msr_sm,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = temp_mean),
              alpha = 0.2, data = msr_sm) +
  geom_line(aes(x = temp_mean, y = est), lwd = 1.2, data = msr_sm) +
  labs(y = "Partial effect", title = "Middle Sacramento River") +
  xlab("Temperature (C)") +
  theme_bw()

plt_Tidal_Delta <-  ggplot() +
  geom_rug(aes(x = temp_mean),
           data = td_sm,
           sides = "b", length = grid::unit(0.02, "npc")) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = temp_mean),
              alpha = 0.2, data = td_sm) +
  geom_line(aes(x = temp_mean, y = est), lwd = 1.2, data = td_sm) +
  labs(y = "Partial effect", title = "Tidal Delta") +
  xlab("Temperature (C)") +
  theme_bw()

#
plot_1 <- draw(modGI_sac_mean_temp_mean, select = 1, residuals = TRUE, n_contour = 5,
               continuous_fill = scale_fill_distiller(palette = "Spectral", type = "div")) &
  theme_bw() &
  labs(title = "Single Common Smoother", x = "Temperature (C)", y = "Sacramento River outflow")

plot_2 <- plt_Upper_Sacramento +
  geom_point(aes(x = temp_mean, y = est,
                 colour = sac_mean, size = sac_mean),
             data = up_eg, alpha = 0.3) +
  scale_colour_viridis_c(option = "plasma") +
  labs(colour = "Sacramento River outflow") +
  guides(size = FALSE) +
  theme_bw()

plot_3 <- plt_Middle_Sacramento +
  geom_point(aes(x = temp_mean, y = est,
                 colour = sac_mean, size = sac_mean),
             data = msr_eg, alpha = 0.3) +
  scale_colour_viridis_c(option = "plasma") +
  labs(colour = "Sacramento River outflow") +
  guides(size = FALSE) +
  theme_bw()

plot_4 <- plt_Tidal_Delta +
  geom_point(aes(x = temp_mean, y = est,
                 colour = sac_mean, size = sac_mean),
             data = td_eg, alpha = 0.3) +
  scale_colour_viridis_c(option = "plasma") +
  labs(colour = "Sacramento River outflow") +
  guides(size = FALSE) +
  theme_bw()

png("hgam_results_temp.png", bg = "transparent", width = 11, height = 20, units = "in", pointsize = 12, res = 350)

plot_1 + plot_2 + plot_3 + plot_4 + plot_layout(nrow = 4)

dev.off()

# alternative

png("hgam_results_alternative.png", bg = "transparent", width = 8, height = 8, units = "in", pointsize = 12, res = 350)

draw(modGI_sac_mean_temp_mean)

dev.off()
