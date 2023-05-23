####### Vectors for loops ##########
library(vegan)
### udpated data and loop.id
# for Mac setwd("~/Downloads") # done on work computer and then switched to mac
setwd("C:/Users/pgoertler/OneDrive - deltacouncil/AT Diversity ms")
loop.df<-read.csv("Loop.ID.fin.csv") # file created in Buffering4NewData.R
#dat.tt<-read.csv("dat.tt_v4.csv") # file created in tt.final.R, updated year for CM Vemco wateryear error and GS missing JSATS

loop.years <- unique(data_na$Year) # updating with final model data from HGAM
loop.Release_Group_SAIL = unique(data_na$Release_Group_SAIL)
#loop.df <- data.frame(Loop.ID = c("A", "A", "B", "B", "C", "C", "D", "D", "D",
#                                  "E", "E", "F", "F", "G", "G", "H", "H", "H", "I","I","I","J","J","J","J",
#                                  "K","K","L","L","M","M","N","N",
#                                 "O", "O","O","P","P","P","Q","Q","Q",
#                                  "R","R","R","S","S","S","T","T","T",
#                                  "U","U","U","U",
#                                  "V","V","V","V",
#                                  "X","X","X","X",
#                                  "Y","Y","Y","Y",
#                                  "Z","Z","Z","Z","Z"),
#                      Route = c("SacRiver", "SacRS", "cendel", "SacRiver", "SacRS", "cendel", "SacRiver", "SacRS", "cendel",
#                                "both", "SacRiver","both","SacRS","both","cendel","SacRiver","both","SacRS", "both","cendel","SacRiver","SacRS", "both","cendel","SacRiver",
#                                "SacRiver","Yolo_Bypass", "SacRS","Yolo_Bypass","cendel","Yolo_Bypass","both","Yolo_Bypass",
#                                "SacRiver", "SacRS","Yolo_Bypass","SacRiver","both","Yolo_Bypass","cendel", "SacRiver","Yolo_Bypass",
#                                "both","Yolo_Bypass","cendel","SacRS","Yolo_Bypass","cendel","both","Yolo_Bypass","SacRS",
#                                "both","Yolo_Bypass","cendel","SacRS",
#                                "SacRiver", "SacRS","Yolo_Bypass","both",
#                                "SacRiver", "SacRS","Yolo_Bypass","cendel",
#                                "SacRiver", "Yolo_Bypass","cendel","both",
#                                "SacRiver", "Yolo_Bypass","cendel","both","SacRS"))

########### data frame to hold data ##########
dat.net <- data.frame(Year = NA, Release_Group_SAIL = NA, Loop.ID = NA, Length.Route = NA, CV = NA, Shannon.indx = NA, Simpson.indx = NA, Length.Fish.ID = NA, mean.t = NA, SD.t = NA)

########## Nested Loop #########

for(i in unique(loop.years)){
  for(j in unique(loop.Release_Group_SAIL)){
    for(k in unique(loop.df$Loop.ID)){

      temp.dat <- subset(loop.df, Loop.ID == k)

      R.group <- factor(j, levels=unique(data_na$Release_Group_SAIL))

      temp.dat2 <- subset(data_na, Year == i & Release_Group_SAIL ==  R.group & Route %in% temp.dat$Route)

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

write.csv(dat.net[-1,], "diveristy_metrics.csv")

# double check
#dat.net.08 <- subset(dat.net, Loop.ID == "H")

#head(dat.net)

#par(mfrow=c(1,1), mar=c(4,4,4,4))
hist(dat.net$Shannon.indx)
hist(dat.net$Simpson.indx)
hist(dat.net$CV)
plot(dat.net$CV, dat.net$Simpson.indx,
     col=as.character(dat.net$colors),
     pch=dat.net$Length.Route)
plot(dat.net$CV, dat.net$Shannon.indx,
     col=as.character(dat.net$colors),
     pch=dat.net$Length.Route)

# add single CV estimates
# need to calculate for diversity index
library(tidyverse)

CV_chip.diversity <- data_na %>%
  group_by(Year, Route, Release_Group_SAIL, add=TRUE) %>%
  summarize(count = n(),
            Shannon.indx = diversity(travel_time, index = "shannon"),
            mean = mean(travel_time, na.rm=TRUE),
            SD = sd(travel_time, na.rm=TRUE))%>%
  mutate(CV=SD/mean)

head(CV_chip.diversity)
chip.cv = CV_chip.diversity
colnames(chip.cv)[1:8] <- c("Year", "Loop.ID", "Release_Group_SAIL", "count", "Shannon.indx", "mean", "SD", "CV")
dat.net<-dat.net[-1,-7]
#dat.net<-dat.net[-1,-c(7,11,12)] # updated data
colnames(dat.net)[1:9] <- c("Year", "Release_Group_SAIL", "Loop.ID", "Length.Route", "CV", "Shannon.indx", "count", "mean", "SD")
chip.cv<-data.frame(chip.cv)
chip.cv$Length.Route<-1
dat.fin<-rbind(dat.net, chip.cv)
write.csv(dat.fin, "buffering_dat.csv")

cv_summary <- dat.fin %>%
  group_by(Length.Route) %>%
  summarise(max=max(CV, na.rm = TRUE),
            min=min(CV, na.rm = TRUE))
# plot

# define colors
colors <- c("#f58231", "#911eb4", "#42d4f4")
Release_Group_SAIL<- unique(dat.fin$Release_Group_SAIL)
dat.col<-data.frame(colors,Release_Group_SAIL)
dat.fin<-merge(dat.fin, dat.col, by="Release_Group_SAIL")

# Define shapes
shapes = c(0,1,2,3,4,5,6,7,8,9,10)
Year <- unique(dat.fin$Year)
dat.pch<-data.frame(shapes,Year)
dat.fin<-merge(dat.fin, dat.pch, by="Year")

png(filename = "buffering_modeldat.png", width = 12, height = 8, units = "in", pointsize = 12, bg = "white", res = 350)

plot(jitter(dat.fin$Length.Route), dat.fin$CV,
     col=as.character(dat.fin$colors),
     pch=dat.fin$shapes,
     xlab="Number of Routes",
     ylab="CV",
     cex=1.5,
     lwd=1.75)
legend("bottomright", c("Upper River",
                        "Middle River",
                        "Tidal Delta",
                        "2007","2008",
                        "2009","2010","2011","2012",
                        "2013","2014","2015",
                        "2016","2017"),
       pch=c(16,16,16,8,1,4,2,9,0,5,10,6,3,7), col=c("#f58231", "#911eb4", "#42d4f4",
                                                     "black","black","black","black","black","black",
                                                     "black","black","black","black","black"),
       bty="n", ncol=2)


dev.off()

# buffering
buffering_tbl <- dat.fin %>%
  group_by(Release_Group_SAIL,Year,Length.Route) %>%
  summarise(min = min(CV, na.rm = TRUE),
            max = max(CV, na.rm = TRUE))

#SD vs. water year
boxplot(travel_time~Year, data= data_na)

Year<-c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)
WaterYear<-c("Dry", "Critically Dry", "Dry", "Below Normal", "Wet", "Below Normal", "Dry", "Critically Dry", "Critically Dry", "Below Normal", "Wet")

W.Y <- data.frame(Year, WaterYear)
dat.tt.yr <- merge(data_na, W.Y, by="Year")

dat.tt.yr$WaterYear = factor(dat.tt.yr$WaterYear, c("Critically Dry", "Dry", "Below Normal", "Wet"))

boxplot(travel_time~WaterYear, data= dat.tt.yr,
        ylab = "Travel Time (days)", xlab = "Sacramento Valley Water Year Index")

summary(aov(travel_time~WaterYear, data= dat.tt.yr))
#summarise(group_by(dat.tt.yr,WaterYear), count=length(unique(FishID)))
#Critically Dry   207
#Dry              172
#Below Normal     889
#Wet              546

#summary.dat <- data.frame(aggregate(travel_time~Year, data= dat.tt.yr,
#                         function (x) c(mean=mean(x), sd = sd(x))))

summary.dat <-dat.tt.yr %>%
  group_by(Year) %>%
  summarise(tt.sd=sd(travel_time))

summary.dat.yr <- merge(summary.dat, W.Y, by="Year")
summary.dat.yr$WaterYear = factor(summary.dat.yr$WaterYear, c("Critically Dry", "Dry", "Below Normal", "Wet"))

tiff(filename="SDvs.WaterYear_modeldat.tif", units="in", bg="white", height=9, width =12, res=300)

boxplot(tt.sd ~ WaterYear, data = summary.dat.yr,
        ylab = "SD in Travel Time (days)", xlab = "Sacramento Valley Water Year Index")
dev.off()

plot(dat.tt.yr$WaterYear, dat.tt.yr$travel_time)

# significant?

mod<-aov(tt.sd~WaterYear, data=summary.dat.yr)
mod<-aov(travel_time~WaterYear, data=dat.tt.yr)
summary(mod)
TukeyHSD(mod)

# for discussion
example <- subset(data_na, Year == 2017 & Group == "Winter" & Release_Group_SAIL == "Upper Sacramento River")

example <- subset(data_na, Year == 2017 & Release_Group_SAIL == "Tidal Delta")

example %>%
  group_by(Route) %>%
  summarise(mean = mean(travel_time))
