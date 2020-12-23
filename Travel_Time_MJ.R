head(alljuvtt)
str(alljuvtt)

unique(alljuvtt$Receiver)
unique(alljuvtt$Station)
unique(alljuvtt$Rkm)
unique(alljuvtt$DateTimeUTC)

sapply(X = alljuvtt, FUN = function(x) sum(is.na(x)))

loc<-alljuvtt[!duplicated(alljuvtt[,c('Receiver','Station','Rkm')]),c(5,6,9)]

length(unique(alljuvtt$TagID)) #172
chip<-subset(alljuvtt, Station == "Chipps")
length(unique(chip$TagID)) # 63

# Von already included release
unique(chip$releasetime)

# first chip detection
chip.min <- data.frame(TagID = NA, time = strptime("1900-01-01 11:11:11",format="%Y-%m-%d %H:%M:%OS", tz="UTC"))

for(i in unique(chip$TagID)){
  temp.dat <- chip[chip$TagID == i,]
  min.time <- min(temp.dat$DateTimeUTC)
  temp.results <- data.frame(TagID = i, time = as.POSIXlt(min.time, format="%Y-%m-%d %H:%M:%OS", tz="UTC"))
  chip.min<-rbind(chip.min, temp.results)
}
head(chip.min)
chip.min<-chip.min[-1,]
length(unique(chip.min$TagID)) #63

# travel time
chip.fish <- (unique(chip$TagID))
chip.rel <- alljuvtt[alljuvtt$TagID %in% chip.fish,]
length(unique(chip.rel$TagID)) #63

fin.chip<-merge(chip.rel, chip.min, by="TagID")
fin.chip$travel_time <- as.numeric(difftime(fin.chip$time, fin.chip$releasetime, units = "days"))
head(fin.chip)
summary(fin.chip)
hist(as.numeric(fin.chip$travel_time))
sapply(X = fin.chip, FUN = function(x) sum(is.na(x)))
length(unique(fin.chip$TagID)) # 63
fin.chip.rm.dup<-fin.chip[!duplicated(fin.chip[,c('TagID')]),]
write.csv(fin.chip.rm.dup[,c(1,2,3,11,12)], "chiptt_MJ.csv")

# release loc from SAIL
tt.dat <- fin.chip.rm.dup[,c(1,2,3,11,12)]
tt.dat$Release_Group_SAIL <- ifelse(tt.dat$TagGroup == "Yolo Bypass 2012" | tt.dat$TagGroup == "Yolo Bypass 2013",
                                   "Tidal Delta", "Middle Sacramento River")

# year
tt.dat$Year<- format(as.Date(tt.dat$releasetime), format = "%Y")
unique(tt.dat$Year)

# run
tt.dat$Group <- "Late-Fall"
tt.dat$origin <- "hatchery"

# transport distance
tt.dat$transport.dist <- 296.16 #check the same for Sac and Yolo release

# length, weight
d = read.csv("https://raw.githubusercontent.com/Myfanwy/Johnstonetal2018SFEWS/master/data/tagdata_index.csv")

#d = subset(d, TagGroup %in% c("Yolo Bypass 2012", "Yolo Bypass 2013"))
d.2 <- d[!duplicated(d[,c('TagID')]),c(3,5,6)]
tt.dat.2<-merge(tt.dat, d.2, by="TagID", all.x=TRUE)
head(tt.dat.2)
Stationlength(unique(tt.dat.2$TagID)) #63

# water temp at release
tt.dat.2$water.temp <- ifelse(tt.dat$Year == 2012, 13.8, # check to make sure also true for Sac release
                                    ifelse(tt.dat$Year == 2013, 14.8, NA))

# route
# investigate Yolo vs. Sac release
chip.rel.yolo<-subset(chip.rel, TagGroup %in% c("Yolo Bypass 2012", "Yolo Bypass 2013"))
unique(chip.rel.yolo$Station) # all 18 used yolo route

chip.rel.sac<-subset(chip.rel, TagGroup %in% c("Sacramento River 2012", "Sacramento River 2013"))
unique(chip.rel.sac$Station)
Station<-c("TopofSutter", "UpperMiner", "Miner", "Steamboat", "BaseSteamboat", "Sutter",
           "BlwGeorg", "Georgiana")
Entrance_Exit<-c("enter", "enter", "exit", "slough", "exit", "enter",
                 "anti-slough", "enter")
Route<-c("SacRSlough","SacRSlough","SacRSlough","SacRSlough","SacRSlough","SacRSlough",
         "CenDel","CenDel")
r.a<-data.frame(Station, Entrance_Exit, Route)
chip.sac.r.a<-merge(chip.rel.sac, r.a, by="Station", all.x = TRUE)

# sort if detected at slough, entrance or exit sites
Fish.List <- data.frame(TagID = unique(chip.sac.r.a$TagID), Ent.code = NA, Exit.Code = NA, Slough.Count = NA, antislough.code=NA)

for(i in 1:nrow(Fish.List)){
  temp.dat <- subset(chip.sac.r.a, TagID == Fish.List[i, "TagID"])
  temp.ent <- subset(temp.dat, Entrance_Exit == "enter")
  temp.exit <- subset(temp.dat, Entrance_Exit == "exit")
  temp.slough.count <- subset(temp.dat, Entrance_Exit == "slough")
  temp.antislough.code <- subset(temp.dat, Entrance_Exit == "anti-slough")
  if(nrow(temp.ent) >= 1)
    Fish.List[i, "Ent.code"] <- 1
  else
    Fish.List[i, "Ent.code"] <- 0
  if(nrow(temp.exit) >= 1)
    Fish.List[i, "Exit.Code"] <- 1
  else
    Fish.List[i, "Exit.Code"] <- 0
  if(nrow(temp.slough.count) > 1)
    Fish.List[i, "Slough.Count"] <- 1
  else
    Fish.List[i, "Slough.Count"] <- 0
  if(nrow(temp.antislough.code) >= 1)
    Fish.List[i, "antislough.code"] <- 1
  else
    Fish.List[i, "antislough.code"] <- 0
}

Fish.List <- transform(Fish.List, Total.count = Ent.code+ Exit.Code+Slough.Count)
head(Fish.List)
length(unique(Fish.List$TagID)) #45
# only 4 are @ 2, none >2
# just looked at them

question<-subset(chip.sac.r.a, TagID %in% c(2089, 2087, 2081, 2103))
tt.dat.2$Route <- ifelse(tt.dat.2$TagGroup == "Yolo Bypass 2012" | tt.dat$TagGroup == "Yolo Bypass 2013",
                                    "Yolo", "SacR")

tt.dat.2$Route <- ifelse(tt.dat.2$TagID == 2089 | tt.dat.2$TagID == 2087 | tt.dat.2$TagID == 2081 | tt.dat.2$TagID == 2103,"SacRSlough",
                         tt.dat.2$Route)

head(tt.dat.2)
write.csv(tt.dat.2, "chiptt_MJ.csv")

### for methods
mj.dat<-read.csv("chiptt_MJ.csv")
head(mj.dat)
summarise(group_by(mj.dat,Route), count=length(unique(TagID)))
#SacR          41
#SacRSlough     4
#Yolo          18
length(unique(mj.dat$TagID)) #63
