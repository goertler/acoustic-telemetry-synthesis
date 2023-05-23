# Cyril's plot from 12/15 meeting
library(dplyr)
model_dat_complete<-read.csv("results/SD/model_dat.csv")

head(model_dat_complete)
unique(model_dat_complete[,c(14,17)])

model_dat_complete$Year <- as.factor(model_dat_complete$Year)

route_summary <- model_dat_complete %>%
  group_by(Year, Release_Group_SAIL) %>%
  summarize(num_route = length(unique(Route)), sd = sd(travel_time),
            mean = mean(travel_time))

route_summary <- data_na %>%
  group_by(Year, Release_Group_SAIL) %>%
  summarize(num_route = length(unique(Route)), sd = sd(dowy_end),
            mean = mean(dowy_end))

# define colors
colors <- c("#f58231", "#911eb4", "#42d4f4")
Release_Group_SAIL<- unique(route_summary$Release_Group_SAIL)
dat_col<-data.frame(colors, Release_Group_SAIL)
dat_fin<-merge(route_summary, dat_col, by="Release_Group_SAIL")

# Define shapes
shapes = c(0,1,2,3,4,5,6,7,8,9,10)
Year <- unique(dat_fin$Year)
dat_pch<-data.frame(shapes,Year)
dat_fin<-merge(dat_fin, dat_pch, by="Year")


plot(jitter(dat_fin$num_route), dat_fin$sd,
     col=as.character(dat_fin$colors),
     bg = as.character(dat_fin$colors),
     pch=dat_fin$shapes,
     xlab="Number of Routes",
     ylab="SD in travel time",
     cex=1.5,
     lwd=2)
legend("topleft", c("Upper River",
                        "Middle River",
                        "Tidal Delta",
                        "2007","2008",
                        "2009","2010","2011","2012",
                        "2013","2014","2015",
                        "2016","2017"),
       pch=c(16,16,16,8,0,2,1,10,5,4,9,6,3,7), col=c("#f58231", "#911eb4", "#42d4f4",
                                                     "black","black","black","black","black","black",
                                                     "black","black","black","black","black"),
       bty="o", ncol=2)

# option 2
route_summary = na.omit(route_summary)
num_route = route_summary$num_route
mean = route_summary$mean
sd = route_summary$sd
plot(num_route, mean, ylim = range(c(mean-sd, mean+sd)), pch = 19)
arrows(num_route, mean-sd, num_route, mean+sd, length =0.05, angle = 90, code = 3)

# option 3

sd_summary <- model_dat_complete %>%
  group_by(Year, Release_Group_SAIL, Route) %>%
  summarize(sd = sd(travel_time))

#4

ggplot(sd_summary, aes(Year, sd, colour = factor(Route))) +
  geom_jitter(aes(shape = factor(Release_Group_SAIL)), size = 4) +
  theme_bw()

# 5

ggplot(model_dat_complete, aes(Year, travel_time, colour = factor(Route))) +
  geom_jitter(aes(shape = factor(Release_Group_SAIL)), size = 4) +
  theme_bw()

# 6

ggplot(model_dat_complete, aes(Year, SD, colour = factor(Route))) +
  geom_jitter(aes(shape = factor(Release_Group_SAIL)), size = 4) +
  theme_bw()
