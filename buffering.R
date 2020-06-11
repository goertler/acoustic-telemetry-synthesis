# boxplot for manuscript
chip.dat<-read.csv("masterCV_chip.csv")

library(ggplot2)

neworder <- c("Upper Sacramento River","Middle Sacramento River","Tidal Delta")
library(plyr)  ## or dplyr (transform -> mutate)
chip.dat_v2 <- arrange(transform(chip.dat,
                                 Release_Group_SAIL=factor(Release_Group_SAIL,levels=neworder)),Release_Group_SAIL)

chip.dat_v2$wateryear<-as.factor(as.character(chip.dat_v2$wateryear))

library(viridis)

plot4ted<-ggplot(chip.dat_v2, aes(Route, travel_time)) +
  geom_boxplot(colour = "coral2") +
  geom_point(aes(col=wateryear, shape=Run)) + 
  ylim(0, 100)+
  facet_wrap(~ Release_Group_SAIL)+ theme_minimal()+ 
  labs(y = "Travel Time", color = "Water Year\n")

png(filename = "boxplot_CV_chip.png", width = 12, height = 8, units = "in", pointsize = 12, bg = "white", res = 350)

plot4ted + 
  geom_jitter(aes(col=wateryear, shape=Run)) 

dev.off()

  #scale_colour_gradient(low = "white", high = "black")
  #scale_fill_viridis(option="magma")
  #scale_colour_brewer(palette = "PRGn")

# same with date of arrival
hist(chip.dat$Salmon.Season)

plot4date<-ggplot(chip.dat_v2, aes(Route, Salmon.Season)) +
  geom_boxplot(colour = "coral2") +
  geom_point(aes(col=wateryear, shape=Run)) + 
  facet_wrap(~ Release_Group_SAIL)+ theme_minimal()+ 
  labs(y = "Day of Arrival to Tidal Delta", color = "Water Year\n")

png(filename = "boxplot_DateOFArrival_chip.png", width = 12, height = 8, units = "in", pointsize = 12, bg = "white", res = 350)

plot4date + 
  geom_jitter(aes(col=wateryear, shape=Run)) 

dev.off()

# jitter plot travel time and date of arrival combined
jitter <- position_jitter(width = 0.4, height = 0)

ggplot(chip.dat_v2, aes(wateryear, travel_time)) +
  geom_point(position = jitter, aes(col=Route, shape=Run, size = Salmon.Season)) + 
  ylim(0, 100)+
  facet_grid(Release_Group_SAIL~ .)+ 
  theme_minimal() 

# make in base

# define colors by route
colors <- c("#800000", "#469990", "#000075", "#bfef45", "#e6beff")
Route<- unique(chip.dat$Route)
dat.col<-data.frame(colors,Route)
chip.dat<-merge(chip.dat, dat.col, by="Route")

# Define shapes
shapes = c(0,1,2,5,6) 
Run <- unique(chip.dat$Run)
dat.pch<-data.frame(shapes,Run)
chip.dat<-merge(chip.dat, dat.pch, by="Run")

#subset by release
chip.up<-subset(chip.dat, Release_Group_SAIL == "Upper Sacramento River")  
chip.mid<-subset(chip.dat, Release_Group_SAIL == "Middle Sacramento River")
chip.td<-subset(chip.dat, Release_Group_SAIL == "Tidal Delta") 

png(filename = "all.data.png", width = 12, height = 8, units = "in", pointsize = 12, bg = "white", res = 350)

par(mfrow=c(3,1), mar=c(2,4,0.5,1))
plot(jitter(chip.up$wateryear,2), chip.up$travel_time,
     ylim=c(0,100), xlim=c(2006.5,2017.5), cex=(chip.up$Salmon.Season/75), xlab="",
     ylab="Travel Time", col=as.character(chip.up$colors),
     pch=chip.up$shapes)
title("Upper River", line = -2)
#par(new=TRUE)
plot(jitter(chip.mid$wateryear,2), chip.mid$travel_time,
     ylim=c(0,100), xlim=c(2006.5,2017.5), cex=(chip.mid$Salmon.Season/75), xlab="",
     ylab="Travel Time", col=as.character(chip.mid$colors),
     pch=chip.mid$shapes)
title("Middle River", line = -2)
#par(new=TRUE)
plot(jitter(chip.td$wateryear,2), chip.td$travel_time,
     ylim=c(0,100), xlim=c(2006.5,2017.5), cex=(chip.td$Salmon.Season/75), xlab="",
     ylab="Travel Time", col=as.character(chip.td$colors),
     pch=chip.td$shapes)
title("Tidal Delta", line = -2)
legend("topleft",c("Sacramemto River", "Sacramento Sloughs", "Central Delta Sloughs",
                   "both sloughs", "Yolo Bypass", "Late-fall",
                   "Fall", "Spring", "Winter", "FallW"),
                   pch=c(16,16,16,16,16,0,1,2,5,6), col=c("#800000",
                   "#469990","#000075","#bfef45","#e6beff", "black","black",
                   "black","black","black"),bty="n")
dev.off()

# buffering

#### data ####
chip.dat<-read.csv("masterCV_chip.csv")
head(chip.dat)

chip.dat <- subset(chip.dat, FishID !="Delta2013-043")
str(chip.dat)

unique(chip.dat$Route) #SacRiver, SacRS, cendel, both, Yolo_Bypass
unique(chip.dat$wateryear)
unique(chip.dat$Release_Group_SAIL)

####### Vectors for loops ##########
loop.years <- unique(chip.dat$wateryear)
loop.Release_Group_SAIL = unique(chip.dat$Release_Group_SAIL)
loop.df <- data.frame(Loop.ID = c("A", "A", "B", "B", "C", "C", "D", "D", "D",
                                  "E", "E", "F", "F", "G", "G", "H", "H", "H", "I","I","I","J","J","J","J",
                                  "K","K","L","L","M","M","N","N",
                                  "O", "O","O","P","P","P","Q","Q","Q",
                                  "R","R","R","S","S","S","T","T","T",
                                  "U","U","U","U",
                                  "V","V","V","V",
                                  "X","X","X","X",
                                  "Y","Y","Y","Y",
                                  "Z","Z","Z","Z","Z"), 
                      Route = c("SacRiver", "SacRS", "cendel", "SacRiver", "SacRS", "cendel", "SacRiver", "SacRS", "cendel",
                                "both", "SacRiver","both","SacRS","both","cendel","SacRiver","both","SacRS", "both","cendel","SacRiver","SacRS", "both","cendel","SacRiver",
                                "SacRiver","Yolo_Bypass", "SacRS","Yolo_Bypass","cendel","Yolo_Bypass","both","Yolo_Bypass",
                                "SacRiver", "SacRS","Yolo_Bypass","SacRiver","both","Yolo_Bypass","cendel", "SacRiver","Yolo_Bypass",
                                "both","Yolo_Bypass","cendel","SacRS","Yolo_Bypass","cendel","both","Yolo_Bypass","SacRS",
                                "both","Yolo_Bypass","cendel","SacRS",
                                "SacRiver", "SacRS","Yolo_Bypass","both",
                                "SacRiver", "SacRS","Yolo_Bypass","cendel",
                                "SacRiver", "Yolo_Bypass","cendel","both",
                                "SacRiver", "Yolo_Bypass","cendel","both","SacRS"))

########### data frame to hold data ##########
dat.net <- data.frame(Year = NA, Release_Group_SAIL = NA, Loop.ID = NA, Length.Route = NA, CV = NA, Shannon.indx = NA, Simpson.indx = NA, Length.Fish.ID = NA, mean.t = NA, SD.t = NA)

########## Nested Loop #########

for(i in unique(loop.years)){
  for(j in unique(loop.Release_Group_SAIL)){
    for(k in unique(loop.df$Loop.ID)){
      
      temp.dat <- subset(loop.df, Loop.ID == k)
      
      R.group <- factor(j, levels=unique(chip.dat$Release_Group_SAIL))
      
      temp.dat2 <- subset(chip.dat, wateryear == i & Release_Group_SAIL ==  R.group & Route %in% temp.dat$Route)
      
      if(length(temp.dat$Route) != length(unique(temp.dat2$Route)))
        next
      
      temp.mean <- mean(temp.dat2$travel_time, na.rm=TRUE)
      temp.SD <- sd(temp.dat2$travel_time, na.rm=TRUE)
      temp.CV <- temp.SD/temp.mean
      temp.shnon <- diversity(temp.dat2$travel_time, index = "shannon")
      temp.smpson <- diversity(temp.dat2$travel_time, index = "simpson")
      
      temp.net <- data.frame(Year = i,
                             Release_Group_SAIL = j,
                             Loop.ID = k,
                             Length.Route = length(unique(temp.dat2$Route)),
                             CV = temp.CV,
                             Shannon.indx = temp.shnon,
                             Simpson.indx = temp.smpson,
                             Length.Fish.ID = length(unique(temp.dat2$FishID)),
                             mean.t = temp.mean,
                             SD.t = temp.SD)
      #browser()
      dat.net <- rbind(dat.net, temp.net)
    }
  }
}

write.csv(dat.net[-1,], "diveristy.cv.chips.csv")

# double check 
dat.net.08 <- subset(dat.net, Loop.ID == "H")

head(dat.net)

par(mfrow=c(1,1), mar=c(4,4,4,4))
hist(dat.net$Shannon.indx)
hist(dat.net$Simpson.indx)
hist(dat.net$CV)
plot(dat.net$CV, dat.net$Simpson.indx, 
     col=as.character(dat.net$colors), 
     pch=dat.fin$Length.Route)
plot(dat.net$CV, dat.net$Shannon.indx, 
     col=as.character(dat.net$colors), 
     pch=dat.net$Length.Route)

colors <- c("#f58231", "#42d4f4", "#f032e6")
Release_Group_SAIL<- unique(dat.net[-1,]$Release_Group_SAIL)
dat.col<-data.frame(colors,Release_Group_SAIL)
dat.net<-merge(dat.net[-1,], dat.col, by="Release_Group_SAIL")

shapes = c(0,1,2,3,4,5,6,7,8,9) 
Year <- unique(dat.net$Year)
dat.pch<-data.frame(shapes,Year)
dat.net<-merge(dat.net, dat.pch, by="Year")

max.min<-ddply(dat.net, "Length.Route",
              summarise, max=max(CV, na.rm = TRUE),
              min=min(CV, na.rm = TRUE))

# add single CV estimates
chip.cv<-read.csv("CV.Chipps_results.csv")
head(chip.cv)
chip.cv$Length.Route<-1
chip.cv<-chip.cv[,-1]
colnames(chip.cv)[1:8] <- c("Year", "Loop.ID", "Release_Group_SAIL", "count", "mean", "SD", "CV", "Length.Route")
colnames(dat.net)[1:8] <- c("Year", "Release_Group_SAIL", "Loop.ID", "Length.Route", "CV", "count", "mean", "SD")

dat.fin<-rbind(dat.net[-1,], chip.cv)

# need to calculate for diveristy index
CV_chip.diversity <- chip.dat %>%
  group_by(wateryear, Route, Release_Group_SAIL, add=TRUE) %>%
  summarize(count = n(),
            Shannon.indx = diversity(travel_time, index = "shannon"),
            mean = mean(travel_time, na.rm=TRUE),
            SD = sd(travel_time, na.rm=TRUE))%>%
  mutate(CV=SD/mean)

head(CV_chip.diversity)
chip.cv = CV_chip.diversity
colnames(chip.cv)[1:8] <- c("Year", "Loop.ID", "Release_Group_SAIL", "count", "Shannon.indx", "mean", "SD", "CV")
dat.net<-dat.net[-1,-7]
colnames(dat.net)[1:9] <- c("Year", "Release_Group_SAIL", "Loop.ID", "Length.Route", "CV", "Shannon.indx", "count", "mean", "SD")
chip.cv<-data.frame(chip.cv)
chip.cv$Length.Route<-1
dat.fin<-rbind(dat.net, chip.cv)
# plot

# define colors
colors <- c("#f58231", "#42d4f4", "#f032e6")
Release_Group_SAIL<- unique(dat.fin$Release_Group_SAIL)
dat.col<-data.frame(colors,Release_Group_SAIL)
dat.fin<-merge(dat.fin, dat.col, by="Release_Group_SAIL")

# Define shapes
shapes = c(0,1,2,3,4,5,6,7,8,9,10) 
Year <- unique(dat.fin$Year)
dat.pch<-data.frame(shapes,Year)
dat.fin<-merge(dat.fin, dat.pch, by="Year")

png(filename = "buffering.png", width = 12, height = 8, units = "in", pointsize = 12, bg = "white", res = 350)

plot(jitter(dat.fin$Length.Route), dat.fin$CV, 
     col=as.character(dat.fin$colors), 
     pch=dat.fin$shapes,
     xlab="Number of Routes",
     ylab="CV",
     cex=2)
legend("bottomright", c("Upper River", 
                        "Middle River", 
                    "Tidal Delta", 
                    "2007","2008",
                    "2009","2010","2011","2012",
                    "2013","2014","2015",
                    "2016","2017"), 
       pch=c(16,16,16,8,0,2,1,10,7,5,9,6,3,4), col=c("#f58231", "#42d4f4", "#f032e6",
                                    "black","black","black","black","black","black",
                                    "black","black","black","black","black"), 
       bty="n", ncol=2)


dev.off()

write.csv(dat.fin, "buffering.cv.chips.csv")

sum.cv<-ddply(dat.fin, c("Year", "Release_Group_SAIL", "Length.Route"),
              summarise, mean=mean(CV, na.rm = TRUE), 
              sd   = sd(CV, na.rm = TRUE),
              N=sum(count, na.rm = TRUE))

head(sum.cv)

write.csv(sum.cv, "sum.cv.chips.csv")

# shannon diveristy index
# separte by # of routes used to calucalte cv
# this is not exactly what Stephanie and Will did, and the change in sample size is a problem

head(dat.fin)
dat.1<-subset(dat.fin, Length.Route == 1)
dat.2<-subset(dat.fin, Length.Route == 2)
dat.3<-subset(dat.fin, Length.Route == 3)
dat.4<-subset(dat.fin, Length.Route == 4)
dat.5<-subset(dat.fin, Length.Route == 5)

library(vegan)

dat.test<-na.omit(dat.5$CV)
diversity(dat.test, index = "simpson")

#1 (4.023764) 0.9805543
#2 (4.47901) 0.9879766
#3 (3.945543) 0.9799417
#4 (2.962221) 0.9465918
#5 (0.6688494) 0.4758997

# which year by release groups were more diverse? relate to number of routes used?
dat.2007<-subset(chip.dat, wateryear == 2007)
dat.2008up<-subset(chip.dat, wateryear == 2008 & Release_Group_SAIL == "Upper Sacramento River")
dat.2008mid<-subset(chip.dat, wateryear == 2008 & Release_Group_SAIL == "Middle Sacramento River")
dat.2009up<-subset(chip.dat, wateryear == 2009 & Release_Group_SAIL == "Upper Sacramento River")
dat.2009mid<-subset(chip.dat, wateryear == 2009 & Release_Group_SAIL == "Middle Sacramento River")
dat.2010up<-subset(chip.dat, wateryear == 2010 & Release_Group_SAIL == "Upper Sacramento River")
dat.2010mid<-subset(chip.dat, wateryear == 2010 & Release_Group_SAIL == "Middle Sacramento River")
dat.2011<-subset(chip.dat, wateryear == 2011)
dat.2012<-subset(chip.dat, wateryear == 2012)
dat.2013up<-subset(chip.dat, wateryear == 2013 & Release_Group_SAIL == "Upper Sacramento River")
dat.2013mid<-subset(chip.dat, wateryear == 2013 & Release_Group_SAIL == "Middle Sacramento River")
dat.2013td<-subset(chip.dat, wateryear == 2013 & Release_Group_SAIL == "Tidal Delta")
dat.2014<-subset(chip.dat, wateryear == 2014)
dat.2015up<-subset(chip.dat, wateryear == 2015 & Release_Group_SAIL == "Upper Sacramento River")
dat.2015mid<-subset(chip.dat, wateryear == 2015 & Release_Group_SAIL == "Middle Sacramento River")
dat.2015td<-subset(chip.dat, wateryear == 2015 & Release_Group_SAIL == "Tidal Delta")
dat.2016up<-subset(chip.dat, wateryear == 2016 & Release_Group_SAIL == "Upper Sacramento River")
dat.2016mid<-subset(chip.dat, wateryear == 2016 & Release_Group_SAIL == "Middle Sacramento River")
dat.2016td<-subset(chip.dat, wateryear == 2016 & Release_Group_SAIL == "Tidal Delta")
dat.2017up<-subset(chip.dat, wateryear == 2017 & Release_Group_SAIL == "Upper Sacramento River")
dat.2017mid<-subset(chip.dat, wateryear == 2017 & Release_Group_SAIL == "Middle Sacramento River")
#dat.2017td<-subset(chip.dat, wateryear == 2017 & Release_Group_SAIL == "Tidal Delta")

diversity(dat.2007$travel_time, index = "simpson") #2.369194 (shan), 0.9038729(simp)
diversity(dat.2008up$travel_time, index = "simpson") #2.793126, 0.9330295
diversity(dat.2008mid$travel_time, index = "simpson") #3.930979, 0.9775418
diversity(dat.2009up$travel_time, index = "simpson") #2.499282, 0.9048159
diversity(dat.2009mid$travel_time, index = "simpson") #3.643857, 0.9705479
diversity(dat.2010up$travel_time, index = "simpson") #3.474755, 0.9643228
diversity(dat.2010mid$travel_time, index = "simpson") #3.061328, 0.9444923

diversity(dat.2011$travel_time, index = "simpson") #4.729954, 0.9887068
diversity(dat.2012$travel_time, index = "simpson") #2.953848, 0.9422124
diversity(dat.2014$travel_time, index = "simpson") #3.750623, 0.9762514

diversity(dat.2013up$travel_time, index = "simpson") #2.783074,  0.9243295
diversity(dat.2013mid$travel_time, index = "simpson") #2.5161, 0.914724
diversity(dat.2013td$travel_time, index = "simpson") #1.481549, 0.4185766

diversity(dat.2015up$travel_time, index = "simpson") #3.661301, 0.9700738
diversity(dat.2015td$travel_time, index = "simpson") #0.6931423, 0.4999952

diversity(dat.2016up$travel_time, index = "simpson") #4.827309,0.9914088 
diversity(dat.2016mid$travel_time, index = "simpson") #6.073509, 0.9973122
diversity(dat.2016td$travel_time, index = "simpson") #5.014105, 0.9915493

diversity(dat.2017up$travel_time, index = "simpson") #5.678553, 0.9962676
diversity(dat.2017mid$travel_time, index = "simpson") #4.509543, 0.9873087

# plot diveristy by route 

png(filename = "Shannon.png", width = 12, height = 8, units = "in", pointsize = 12, bg = "white", res = 350)

plot(jitter(dat.fin$Length.Route), dat.fin$Shannon.indx, 
     col=as.character(dat.fin$colors), 
     pch=dat.fin$shapes,
     xlab="Number of Routes",
     ylab="Shannon Diversity Index",
     cex=2)

dev.off()

plot(jitter(dat.net$Length.Route), dat.net$Simpson.indx, 
     col=as.character(dat.net$colors), 
     pch=dat.fin$shapes,
     xlab="Number of Routes",
     ylab="Simpson Diversity Index",
     cex=2)

png(filename = "CV.vs.Shannon.png", width = 12, height = 8, units = "in", pointsize = 12, bg = "white", res = 350)

plot(dat.fin$CV, dat.fin$Shannon.indx, 
     col=as.character(dat.fin$colors), 
     pch=dat.fin$shapes, cex=dat.fin$Length.Route, 
     xlab="CV",
     ylab="Shannon Diversity Index", , ylim=c(0,6.2))
legend("bottomright", c("Upper River", 
                        "Middle River", 
                        "Tidal Delta", 
                        "2007","2008",
                        "2009","2010","2011","2012",
                        "2013","2014","2015",
                        "2016","2017"), 
       pch=c(16,16,16,8,0,2,1,10,7,5,9,6,3,4), col=c("#f58231", "#42d4f4", "#f032e6",
                                                     "black","black","black","black","black","black",
                                                     "black","black","black","black","black"), 
       bty="n", ncol=2)
model<-lm(dat.fin$Shannon.indx~dat.fin$CV + I(dat.fin$CV^2))
par(new=TRUE)
plot(na.omit(dat.fin$CV), predict(model), col="blue", xlab="", ylab="", ylim=c(0,6.2), pch=20)

dev.off()

# how does diveristy vary across groups

Route.diversity <- chip.dat %>%
  group_by(Route) %>%
  summarize(count = n(),
            Shannon.indx = diversity(travel_time, index = "shannon"),
            mean = mean(travel_time, na.rm=TRUE),
            SD = sd(travel_time, na.rm=TRUE))%>%
  mutate(CV=SD/mean)

year.diversity <- chip.dat %>%
  group_by(wateryear) %>%
  summarize(count = n(),
            Shannon.indx = diversity(travel_time, index = "shannon"),
            mean = mean(travel_time, na.rm=TRUE),
            SD = sd(travel_time, na.rm=TRUE))%>%
  mutate(CV=SD/mean)

rel.diversity <- chip.dat %>%
  group_by(Release_Group_SAIL) %>%
  summarize(count = n(),
            Shannon.indx = diversity(travel_time, index = "shannon"),
            mean = mean(travel_time, na.rm=TRUE),
            SD = sd(travel_time, na.rm=TRUE))%>%
  mutate(CV=SD/mean)

rel.yr.diversity <- chip.dat %>%
  group_by(Release_Group_SAIL, wateryear) %>%
  summarize(count = n(),
            Shannon.indx = diversity(travel_time, index = "shannon"),
            mean = mean(travel_time, na.rm=TRUE),
            SD = sd(travel_time, na.rm=TRUE))%>%
  mutate(CV=SD/mean)

write.csv(rel.yr.diversity, "rel.yr.diversity.csv")

rel.rout.diversity <- chip.dat %>%
  group_by(Release_Group_SAIL, Route) %>%
  summarize(count = n(),
            Shannon.indx = diversity(travel_time, index = "shannon"),
            mean = mean(travel_time, na.rm=TRUE),
            SD = sd(travel_time, na.rm=TRUE))%>%
  mutate(CV=SD/mean)

