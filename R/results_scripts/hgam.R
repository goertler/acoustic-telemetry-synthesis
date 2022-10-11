# testing different hierarchical gam structures

# library
library(mgcv)

# data from model_explore.R
data_na <- na.omit(data_doy)
data_na$Release_Group_SAIL <- as.factor(data_na$Release_Group_SAIL)
data_na$Year <- as.factor(data_na$Year)
data_na$Group <- as.factor(data_na$Group)
data_na$origin <- as.factor(data_na$origin)
data_na$Route <- as.factor(data_na$Route)
data_na$TagType <- as.factor(data_na$TagType)
data_na$tt.grp <- as.factor(data_na$tt.grp)

### A single common smoother for all observations; only has a Global smoother

modG <- gam(log(travel_time) ~ s(log(stage_sd), k=10, bs="tp") +
                  s(Release_Group_SAIL, k=3, bs="re"),
                data=data_na, method="REML", family="gaussian")

modG <- gam(log(SD) ~ s(log(stage_sd), k=10, bs="tp") +
              s(Release_Group_SAIL, k=3, bs="re"),
            data=data_na, method="REML", family="gaussian")

# random effect of release group
# common structure with single smooth term for each variable
# s() terms to include one-dimensional or isotropic multidimensional smoothers
# here we use a TPRS smoother (bs="tp")

modG_cyclic <- gam(log(travel_time) ~ te(temp_mean, stage_sd, bs=c("cc", "tp"), k=c(10, 10)), data=data_na, method="REML", family="gaussian")

modG_cyclic <- gam(log(SD) ~ te(temp_mean, stage_sd, bs=c("cc", "tp"), k=c(10, 10)), data=data_na, method="REML", family="gaussian")

# tensor product of stage and temperature, using a TPRS for the marginal stage effect, and a cyclic CRS for the marginal temp effect to account for the cyclic nature
# when interested in interaction between continuous variables


### A global smoother plus group-level smoothers that have the same wiggliness(for Global penalty).

modGS <- gam(log(travel_time) ~ s(log(stage_sd ), k=5, m=2) +
                        s(log(stage_sd ), Release_Group_SAIL, k=3, bs="fs", m=2),
                      data=data_na, method="REML")

modGS <- gam(log(SD) ~ s(log(stage_sd ), k=5, m=2) +
               s(log(stage_sd ), Release_Group_SAIL, k=3, bs="fs", m=2),
             data=data_na, method="REML")

# factor-smoother interaction or "fs" (This smoother creates a copy of each set of basis functions for each level of the grouping variable, but only estimates one smoothing parameter for all groups)

modGS_cyclic <- gam(log(travel_time) ~ te(temp_mean, stage_sd, bs=c("cc", "tp"),
                             k=c(10, 10), m=2) +
                    t2(temp_mean, stage_sd, Release_Group_SAIL, bs=c("cc", "tp", "re"),
                       k=c(10, 10, 6), m=2, full=TRUE),
                  data=data_na, method="REML", family="gaussian")

modGS_cyclic <- gam(log(SD) ~ te(temp_mean, stage_sd, bs=c("cc", "tp"),
                                          k=c(10, 10), m=2) +
                      t2(temp_mean, stage_sd, Release_Group_SAIL, bs=c("cc", "tp", "re"),
                         k=c(10, 10, 6), m=2, full=TRUE),
                    data=data_na, method="REML", family="gaussian")

#The factor-smoother interaction-based approach above does not work for higher-dimensional tensor product smoothers. Instead, the group-specific term is specified with a tensor product of the continuous smoothers and a random effect for the grouping parameter

### A single common smoother plus group-level smoothers with differing wiggliness (very similar to model GS, but we now allow each group-specific smoother to have its own smoothing parameter and hence its own level of wiggliness)- Global smoother with individual effects that have Individual penalties
# the only information shared between groups is through the global smoother, the common error term, and through the random effect for group-level intercepts
# explicitly include a random effect for the intercept (the bs="re" term)
# specify m=1 instead of m=2 for the group-level smoothers, which means the marginal TPRS basis for this term will penalize the squared first derivative of the function, rather than the second derivative. --> reduces colinearity between the global smoother and the group-specific terms which occasionally leads to high uncertainty around the global smoother

modGI <- gam(log(travel_time) ~ s(log(stage_sd), k=5, m=2, bs="tp") +
                   s(log(stage_sd), by=Release_Group_SAIL, k=5, m=1, bs="tp") +
                   s(Release_Group_SAIL, bs="re", k=3),
                 data=data_na, method="REML")

modGI <- gam(log(SD) ~ s(log(stage_sd), k=5, m=2, bs="tp") +
               s(log(stage_sd), by=Release_Group_SAIL, k=5, m=1, bs="tp") +
               s(Release_Group_SAIL, bs="re", k=3),
             data=data_na, method="REML")


modGI_cyclic <- gam(log(travel_time) ~ Release_Group_SAIL +
                    te(temp_mean, stage_sd, bs=c("cc", "tp"), k=c(10, 10), m=2) +
                    te(temp_mean, stage_sd, by=Release_Group_SAIL, bs= c("cc", "tp"),
                       k=c(10, 10), m=1),
                  data=data_na, method="REML", family="gaussian")

modGI_cyclic <- gam(log(SD) ~ Release_Group_SAIL +
                      te(temp_mean, stage_sd, bs=c("cc", "tp"), k=c(10, 10), m=2) +
                      te(temp_mean, stage_sd, by=Release_Group_SAIL, bs= c("cc", "tp"),
                         k=c(10, 10), m=1),
                    data=data_na, method="REML", family="gaussian")

### Group-specific smoothers without a global smoother, but with all smoothers having the same wiggliness
# allowing each group to be differently shaped without restriction (true for next too)
# group-level smooth terms do not share or deviate from a common form (true for next too)
# This model assumes all groups have the same smoothness, but that the individual shapes of the smooth terms are not related

modS <- gam(log(travel_time) ~ s(log(stage_sd), Release_Group_SAIL, k=5, bs="fs", m=2),
                data=data_na, method="REML")

modS <- gam(log(SD) ~ s(log(stage_sd), Release_Group_SAIL, k=5, bs="fs", m=2),
            data=data_na, method="REML")

modS_cyclic <- gam(log(travel_time) ~ t2(temp_mean, stage_sd, Release_Group_SAIL, bs=c("cc", "tp", "re"), k=c(10, 10, 3), m=2, full=TRUE), data=data_na, method="REML", family="gaussian")

modS_cyclic <- gam(log(SD) ~ t2(temp_mean, stage_sd, Release_Group_SAIL, bs=c("cc", "tp", "re"), k=c(10, 10, 3), m=2, full=TRUE), data=data_na, method="REML", family="gaussian")

### Group-specific smoothers with different wiggliness

modI <- gam(log(travel_time) ~ s(log(stage_sd), by=Release_Group_SAIL, k=5, bs="tp", m=2) + s(Release_Group_SAIL, bs="re", k=3),
                data=data_na, method="REML")

modI <- gam(log(SD) ~ s(log(stage_sd), by=Release_Group_SAIL, k=5, bs="tp", m=2) + s(Release_Group_SAIL, bs="re", k=3),
            data=data_na, method="REML")

modI_cyclic <- gam(log(travel_time) ~ Release_Group_SAIL + te(temp_mean, stage_sd, by=Release_Group_SAIL, bs=c("cc", "tp"), k=c(10, 10), m=2),
                 data=data_na, method="REML", family="gaussian")

modI_cyclic <- gam(log(SD) ~ Release_Group_SAIL + te(temp_mean, stage_sd, by=Release_Group_SAIL, bs=c("cc", "tp"), k=c(10, 10), m=2),
                   data=data_na, method="REML", family="gaussian")

AIC(modG, modGI, modGS, modI, modS)
AIC(modG_cyclic, modGI_cyclic, modGS_cyclic, modI_cyclic, modS_cyclic)
gam.check(modGI_cyclic)# best for travel_time and SD

# testing covars one at a time
modGI_null <- gam(log(travel_time) ~ Release_Group_SAIL,
                    data=data_na, method="REML", family="gaussian")

modGI_temp_mean <- gam(log(travel_time) ~ Release_Group_SAIL +
                      te(temp_mean, bs="cc", k=12, m=2) +
                      te(temp_mean, by=Release_Group_SAIL, bs= "cc",
                         k=12, m=1),
                    data=data_na, method="REML", family="gaussian")

modGI_temp_sd <- gam(log(travel_time) ~ Release_Group_SAIL +
                    te(temp_sd, bs="tp", k=12, m=2) +
                    te(temp_sd, by=Release_Group_SAIL, bs= "tp",
                       k=12, m=1),
                  data=data_na, method="REML", family="gaussian")

modGI_stage_sd <- gam(log(travel_time) ~ Release_Group_SAIL +
                      te(stage_sd, bs="tp", k=10, m=2) +
                      te(stage_sd, by=Release_Group_SAIL, bs="tp",
                         k=10, m=1),
                    data=data_na, method="REML", family="gaussian")

modGI_stage_mean <- gam(log(travel_time) ~ Release_Group_SAIL +
                        te(stage_mean, bs="tp", k=10, m=2) +
                        te(stage_mean, by=Release_Group_SAIL, bs="tp",
                           k=10, m=1),
                      data=data_na, method="REML", family="gaussian")

modGI_yr <- gam(log(travel_time) ~ Release_Group_SAIL + Year,
                  data=data_na, method="REML", family="gaussian")

modGI_gr <- gam(log(travel_time) ~ Release_Group_SAIL + Group,
                  data=data_na, method="REML", family="gaussian")

modGI_or <- gam(log(travel_time) ~ Release_Group_SAIL + origin,
                  data=data_na, method="REML", family="gaussian")

modGI_r <- gam(log(travel_time) ~ Release_Group_SAIL + Route,
                  data=data_na, method="REML", family="gaussian")

modGI_tag <- gam(log(travel_time) ~ Release_Group_SAIL + TagType,
                  data=data_na, method="REML", family="gaussian")

modGI_tt <- gam(log(travel_time) ~ Release_Group_SAIL + tt.grp,
                  data=data_na, method="REML", family="gaussian")

modGI_rel <- gam(log(travel_time) ~ Release_Group_SAIL +
                         te(dowy_rel, bs="cc", k=12, m=2) +
                         te(dowy_rel, by=Release_Group_SAIL, bs= "cc",
                            k=12, m=1),
                       data=data_na, method="REML", family="gaussian")

modGI_end <- gam(log(travel_time) ~ Release_Group_SAIL +
                         te(dowy_end, bs="cc", k=12, m=2) +
                         te(dowy_end, by=Release_Group_SAIL, bs= "cc",
                            k=12, m=1),
                       data=data_na, method="REML", family="gaussian")

# testing covars one at a time
modGI_null <- gam(log(SD) ~ Release_Group_SAIL,
                  data=data_na, method="REML", family="gaussian")

modGI_temp_mean <- gam(log(SD) ~ Release_Group_SAIL +
                         te(temp_mean, bs="cc", k=12, m=2) +
                         te(temp_mean, by=Release_Group_SAIL, bs= "cc",
                            k=12, m=1),
                       data=data_na, method="REML", family="gaussian")

modGI_temp_sd <- gam(log(SD) ~ Release_Group_SAIL +
                       te(temp_sd, bs="tp", k=12, m=2) +
                       te(temp_sd, by=Release_Group_SAIL, bs= "tp",
                          k=12, m=1),
                     data=data_na, method="REML", family="gaussian")

modGI_stage_sd <- gam(log(SD) ~ Release_Group_SAIL +
                        te(stage_sd, bs="tp", k=10, m=2) +
                        te(stage_sd, by=Release_Group_SAIL, bs="tp",
                           k=10, m=1),
                      data=data_na, method="REML", family="gaussian")

modGI_stage_mean <- gam(log(SD) ~ Release_Group_SAIL +
                          te(stage_mean, bs="tp", k=10, m=2) +
                          te(stage_mean, by=Release_Group_SAIL, bs="tp",
                             k=10, m=1),
                        data=data_na, method="REML", family="gaussian")

modGI_yr <- gam(log(SD) ~ Release_Group_SAIL + Year,
                data=data_na, method="REML", family="gaussian")

modGI_gr <- gam(log(SD) ~ Release_Group_SAIL + Group,
                data=data_na, method="REML", family="gaussian")

modGI_or <- gam(log(SD) ~ Release_Group_SAIL + origin,
                data=data_na, method="REML", family="gaussian")

modGI_r <- gam(log(SD) ~ Release_Group_SAIL + Route,
               data=data_na, method="REML", family="gaussian")

modGI_tag <- gam(log(SD) ~ Release_Group_SAIL + TagType,
                 data=data_na, method="REML", family="gaussian")

modGI_tt <- gam(log(SD) ~ Release_Group_SAIL + tt.grp,
                data=data_na, method="REML", family="gaussian")

modGI_rel <- gam(log(SD) ~ Release_Group_SAIL +
                   te(dowy_rel, bs="cc", k=12, m=2) +
                   te(dowy_rel, by=Release_Group_SAIL, bs= "cc",
                      k=12, m=1),
                 data=data_na, method="REML", family="gaussian")

modGI_end <- gam(log(SD) ~ Release_Group_SAIL +
                   te(dowy_end, bs="cc", k=12, m=2) +
                   te(dowy_end, by=Release_Group_SAIL, bs= "cc",
                      k=12, m=1),
                 data=data_na, method="REML", family="gaussian")


AIC(modGI_null, modGI_temp_mean, modGI_temp_sd, modGI_stage_sd, modGI_stage_mean, modGI_yr, modGI_gr, modGI_or, modGI_r, modGI_tag, modGI_tt,
    modGI_rel, modGI_end) # mean stage for the win for travel time, release date is best for SD

# add second term to mean stage model

modGI_sm_temp_mean <- gam(log(travel_time) ~ Release_Group_SAIL +
                      te(stage_mean, temp_mean, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                      te(stage_mean, temp_mean, by=Release_Group_SAIL, bs= c("tp", "cc"),
                         k=c(10, 10), m=1),
                    data=data_na, method="REML", family="gaussian")

modGI_sm_temp_sd <- gam(log(travel_time) ~ Release_Group_SAIL +
                      te(stage_mean, temp_sd, bs=c("tp", "tp"), k=c(10, 10), m=2) +
                      te(stage_mean, temp_sd, by=Release_Group_SAIL, bs= c("tp", "tp"),
                         k=c(10, 10), m=1),
                    data=data_na, method="REML", family="gaussian")

modGI_sm_rel <- gam(log(travel_time) ~ Release_Group_SAIL +
                          te(stage_mean, dowy_rel, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                          te(stage_mean, dowy_rel, by=Release_Group_SAIL, bs= c("tp", "cc"),
                             k=c(10, 10), m=1),
                        data=data_na, method="REML", family="gaussian")

modGI_sm_end <- gam(log(travel_time) ~ Release_Group_SAIL +
                          te(stage_mean, dowy_end, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                          te(stage_mean, dowy_end, by=Release_Group_SAIL, bs= c("tp", "cc"),
                             k=c(10, 10), m=1),
                        data=data_na, method="REML", family="gaussian")

modGI_sm_yr <- gam(log(travel_time) ~ Release_Group_SAIL + Year +
                          te(stage_mean, bs="tp", k=10, m=2) +
                          te(stage_mean, by=Release_Group_SAIL, bs="tp",
                             k=10, m=1),
                        data=data_na, method="REML", family="gaussian")

modGI_sm_gr <- gam(log(travel_time) ~ Release_Group_SAIL + Group +
                     te(stage_mean, bs="tp", k=10, m=2) +
                     te(stage_mean, by=Release_Group_SAIL, bs="tp",
                        k=10, m=1),
                   data=data_na, method="REML", family="gaussian")

modGI_sm_or <- gam(log(travel_time) ~ Release_Group_SAIL + origin +
                     te(stage_mean, bs="tp", k=10, m=2) +
                     te(stage_mean, by=Release_Group_SAIL, bs="tp",
                        k=10, m=1),
                   data=data_na, method="REML", family="gaussian")

modGI_sm_r <- gam(log(travel_time) ~ Release_Group_SAIL + Route +
                     te(stage_mean, bs="tp", k=10, m=2) +
                     te(stage_mean, by=Release_Group_SAIL, bs="tp",
                        k=10, m=1),
                   data=data_na, method="REML", family="gaussian")

modGI_sm_tag <- gam(log(travel_time) ~ Release_Group_SAIL + TagType +
                     te(stage_mean, bs="tp", k=10, m=2) +
                     te(stage_mean, by=Release_Group_SAIL, bs="tp",
                        k=10, m=1),
                   data=data_na, method="REML", family="gaussian")

modGI_sm_tt <- gam(log(travel_time) ~ Release_Group_SAIL + tt.grp +
                     te(stage_mean, bs="tp", k=10, m=2) +
                     te(stage_mean, by=Release_Group_SAIL, bs="tp",
                        k=10, m=1),
                   data=data_na, method="REML", family="gaussian")

AIC(modGI_stage_mean, modGI_sm_yr, modGI_sm_gr, modGI_sm_or, modGI_sm_r, modGI_sm_tag, modGI_sm_tt, modGI_sm_temp_mean, modGI_sm_temp_sd, modGI_sm_rel, modGI_sm_end)

summary(modGI_sm_temp_mean)#94.4%
summary(modGI_sm_rel)#93.9%
summary(modGI_sm_temp_sd)#93.9%

# add second term to mean stage model

modGI_rel_stage_sd <- gam(log(SD) ~ Release_Group_SAIL +
                   te(stage_sd, dowy_rel, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                   te(stage_sd, dowy_rel, by=Release_Group_SAIL, bs= c("tp", "cc"),
                      k=c(10, 10), m=1),
                 data=data_na, method="REML", family="gaussian")

modGI_rel_stage_mean <- gam(log(SD) ~ Release_Group_SAIL +
                      te(stage_mean, dowy_rel, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                      te(stage_mean, dowy_rel, by=Release_Group_SAIL, bs= c("tp", "cc"),
                         k=c(10, 10), m=1),
                    data=data_na, method="REML", family="gaussian")

modGI_rel_temp_sd <- gam(log(SD) ~ Release_Group_SAIL +
                          te(temp_sd, dowy_rel, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                          te(temp_sd, dowy_rel, by=Release_Group_SAIL, bs= c("tp", "cc"),
                             k=c(10, 10), m=1),
                        data=data_na, method="REML", family="gaussian")

modGI_rel_end <- gam(log(SD) ~ Release_Group_SAIL +
                      te(dowy_end, dowy_rel, bs=c("cc", "cc"), k=c(10, 10), m=2) +
                      te(dowy_end, dowy_rel, by=Release_Group_SAIL, bs= c("cc", "cc"),
                         k=c(10, 10), m=1),
                    data=data_na, method="REML", family="gaussian")

modGI_rel_yr <- gam(log(SD) ~ Release_Group_SAIL + Year +
                   te(dowy_rel, bs="cc", k=12, m=2) +
                   te(dowy_rel, by=Release_Group_SAIL, bs= "cc",
                      k=12, m=1),
                 data=data_na, method="REML", family="gaussian")

modGI_rel_gr <- gam(log(SD) ~ Release_Group_SAIL + Group +
                      te(dowy_rel, bs="cc", k=12, m=2) +
                      te(dowy_rel, by=Release_Group_SAIL, bs= "cc",
                         k=12, m=1),
                    data=data_na, method="REML", family="gaussian")

modGI_rel_or <- gam(log(SD) ~ Release_Group_SAIL + origin +
                      te(dowy_rel, bs="cc", k=12, m=2) +
                      te(dowy_rel, by=Release_Group_SAIL, bs= "cc",
                         k=12, m=1),
                    data=data_na, method="REML", family="gaussian")

modGI_rel_r <- gam(log(SD) ~ Release_Group_SAIL + Route +
                      te(dowy_rel, bs="cc", k=12, m=2) +
                      te(dowy_rel, by=Release_Group_SAIL, bs= "cc",
                         k=12, m=1),
                    data=data_na, method="REML", family="gaussian")

modGI_rel_tag <- gam(log(SD) ~ Release_Group_SAIL + TagType +
                     te(dowy_rel, bs="cc", k=12, m=2) +
                     te(dowy_rel, by=Release_Group_SAIL, bs= "cc",
                        k=12, m=1),
                   data=data_na, method="REML", family="gaussian")

modGI_rel_tt <- gam(log(SD) ~ Release_Group_SAIL + tt.grp +
                     te(dowy_rel, bs="cc", k=12, m=2) +
                     te(dowy_rel, by=Release_Group_SAIL, bs= "cc",
                        k=12, m=1),
                   data=data_na, method="REML", family="gaussian")

AIC(modGI_rel, modGI_rel_yr, modGI_rel_gr, modGI_rel_or, modGI_rel_r, modGI_rel_tag, modGI_rel_tt, modGI_rel_temp_sd, modGI_rel_end, modGI_rel_stage_sd, modGI_rel_stage_mean)

summary(modGI_rel) #46%
summary(modGI_rel_stage_mean) #62.5%

modGI_test <- gam(log(SD) ~ Release_Group_SAIL +
                              te(stage_mean, temp_mean, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                              te(stage_mean, temp_mean, by=Release_Group_SAIL, bs= c("tp", "cc"),
                                 k=c(10, 10), m=1),
                            data=data_na, method="REML", family="gaussian")

summary(modGI_test) #62% -- better AIC, but not much gained in deviance explained

# add one more

modGI_sm_temp_mean_yr <- gam(log(travel_time) ~ Release_Group_SAIL + Year +
                            te(stage_mean, temp_mean, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                            te(stage_mean, temp_mean, by=Release_Group_SAIL, bs= c("tp", "cc"),
                               k=c(10, 10), m=1),
                          data=data_na, method="REML", family="gaussian")

modGI_sm_temp_mean_gr <- gam(log(travel_time) ~ Release_Group_SAIL + Group +
                               te(stage_mean, temp_mean, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                               te(stage_mean, temp_mean, by=Release_Group_SAIL, bs= c("tp", "cc"),
                                  k=c(10, 10), m=1),
                             data=data_na, method="REML", family="gaussian")

modGI_sm_temp_mean_or <- gam(log(travel_time) ~ Release_Group_SAIL + origin +
                               te(stage_mean, temp_mean, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                               te(stage_mean, temp_mean, by=Release_Group_SAIL, bs= c("tp", "cc"),
                                  k=c(10, 10), m=1),
                             data=data_na, method="REML", family="gaussian")

modGI_sm_temp_mean_r <- gam(log(travel_time) ~ Release_Group_SAIL + Route +
                               te(stage_mean, temp_mean, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                               te(stage_mean, temp_mean, by=Release_Group_SAIL, bs= c("tp", "cc"),
                                  k=c(10, 10), m=1),
                             data=data_na, method="REML", family="gaussian")

modGI_sm_temp_mean_tag <- gam(log(travel_time) ~ Release_Group_SAIL + TagType +
                               te(stage_mean, temp_mean, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                               te(stage_mean, temp_mean, by=Release_Group_SAIL, bs= c("tp", "cc"),
                                  k=c(10, 10), m=1),
                             data=data_na, method="REML", family="gaussian")

modGI_sm_temp_mean_tt <- gam(log(travel_time) ~ Release_Group_SAIL + tt.grp +
                               te(stage_mean, temp_mean, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                               te(stage_mean, temp_mean, by=Release_Group_SAIL, bs= c("tp", "cc"),
                                  k=c(10, 10), m=1),
                             data=data_na, method="REML", family="gaussian")


AIC(modGI_sm_temp_mean_yr, modGI_sm_temp_mean_gr, modGI_sm_temp_mean_or, modGI_sm_temp_mean_r, modGI_sm_temp_mean_tag, modGI_sm_temp_mean_tt, modGI_sm_temp_mean)

summary(modGI_sm_temp_mean_yr) #95.1%
summary(modGI_sm_temp_mean_gr) #94.7%

modGI_sm_temp_mean_yr_gr <- gam(log(travel_time) ~ Release_Group_SAIL + Year + Group +
                               te(stage_mean, temp_mean, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                               te(stage_mean, temp_mean, by=Release_Group_SAIL, bs= c("tp", "cc"),
                                  k=c(10, 10), m=1),
                             data=data_na, method="REML", family="gaussian")

AIC(modGI_sm_temp_mean_yr_gr)
summary(modGI_sm_temp_mean_yr_gr) #95.2%

# add one more

modGI_rel_stage_mean_yr <- gam(log(SD) ~ Release_Group_SAIL + Year +
                              te(stage_mean, dowy_rel, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                              te(stage_mean, dowy_rel, by=Release_Group_SAIL, bs= c("tp", "cc"),
                                 k=c(10, 10), m=1),
                            data=data_na, method="REML", family="gaussian")

modGI_rel_stage_mean_gr <- gam(log(SD) ~ Release_Group_SAIL + Group +
                              te(stage_mean, dowy_rel, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                              te(stage_mean, dowy_rel, by=Release_Group_SAIL, bs= c("tp", "cc"),
                                 k=c(10, 10), m=1),
                            data=data_na, method="REML", family="gaussian")

modGI_rel_stage_mean_or <- gam(log(SD) ~ Release_Group_SAIL + origin +
                              te(stage_mean, dowy_rel, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                              te(stage_mean, dowy_rel, by=Release_Group_SAIL, bs= c("tp", "cc"),
                                 k=c(10, 10), m=1),
                            data=data_na, method="REML", family="gaussian")

modGI_rel_stage_mean_r <- gam(log(SD) ~ Release_Group_SAIL + Route +
                              te(stage_mean, dowy_rel, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                              te(stage_mean, dowy_rel, by=Release_Group_SAIL, bs= c("tp", "cc"),
                                 k=c(10, 10), m=1),
                            data=data_na, method="REML", family="gaussian")

modGI_rel_stage_mean_tag <- gam(log(SD) ~ Release_Group_SAIL + TagType +
                              te(stage_mean, dowy_rel, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                              te(stage_mean, dowy_rel, by=Release_Group_SAIL, bs= c("tp", "cc"),
                                 k=c(10, 10), m=1),
                            data=data_na, method="REML", family="gaussian")

modGI_rel_stage_mean_tt <- gam(log(SD) ~ Release_Group_SAIL + tt.grp +
                              te(stage_mean, dowy_rel, bs=c("tp", "cc"), k=c(10, 10), m=2) +
                              te(stage_mean, dowy_rel, by=Release_Group_SAIL, bs= c("tp", "cc"),
                                 k=c(10, 10), m=1),
                            data=data_na, method="REML", family="gaussian")

AIC(modGI_rel_stage_mean_yr, modGI_rel_stage_mean_gr, modGI_rel_stage_mean_or, modGI_rel_stage_mean_r, modGI_rel_stage_mean_tag, modGI_rel_stage_mean_tt)

summary(modGI_rel_stage_mean_or) #62.5% (AIC is better, 881.1497 vs 885.3569, but no change to R2)

# visualize modGI_sm_temp_mean
library(gratia)

vis.gam(modGI_sm_temp_mean)
gam.vcomp(modGI_sm_temp_mean)
draw(modGI_sm_temp_mean)

vis.gam(modGI_rel_stage_mean)
gam.vcomp(modGI_rel_stage_mean)
draw(modGI_rel_stage_mean)

vis.gam(modGI_test)
draw(modGI_test)
