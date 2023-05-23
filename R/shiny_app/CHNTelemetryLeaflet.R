# load required packages
#if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")

#library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(RColorBrewer)
library(mapview)

# import data
#setwd("C:/Users/pgoertler/OneDrive - deltacouncil/AT Diversity ms/DFA/study-detections")
#rec<- read.csv("data/all_receivers.csv")
#rel<- read.csv("data/releaseloc.csv")
#sites<- read.csv("releaseloc4JSATS.csv")
#rec.j <- subset(sites, fnx == "Rec")
# dont want sites in sf and ocean (-122.24232744879495)
#rec.j_v2 <- subset(rec.j, LON >= -122.2423)

#rel.j <- read.csv("UniqueRelLoc.csv")
#rel.j$Type = "JSATS"
#rel.cm <- read.csv("cm.rel.csv")
#cm.rec_v2 <- read.csv("cm.rec.csv")
#ybus.rel <- read.csv("test.YBUS.rel.csv") # from Tracy's spreadsheet

#setwd("C:/Users/pgoertler/OneDrive - deltacouncil/AT Diversity ms/DFA/study-detections/YBUS/CFS_analysis")
#YBUS.rec<-read.csv("YBUS.rec4map.csv")


#rec = rbind(rec.j_v2[,c(6:8)], YBUS.rec[,c(4,5,10)], cm.rec_v2[,c(5,6,8)])
#rel = rbind(rel.j[,c(5,6,8)], rel.cm[,c(3,4,7)], ybus.rel[,c(4,5,7)])

rec <- read.csv("R/shiny_app/data/rec4shiny_2023_qc.csv")
rel <- read.csv("R/shiny_app/data/re14shiny_2023.csv")

# save master file for shiny app

#rel <- subset(sites, fnx == "Rel")
#st_layers("data/hydro_forshiny")
#setwd("~/acoustic-telemetry-synthesis/CHNTelemetrySynth (2)")# accidentally wrote over shp files
st_layers("R/shiny_app/data/For_Pascale")

#read in region distinctions
#bypass <- st_read("data/hydro_forshiny","bypass_regions")
#sacr <- st_read("data/hydro_forshiny","SacRiv_NHDPlus")
#delta <- st_read("data/hydro_forshiny","Delta_waterways")
#sfb <- st_read("data/hydro_forshiny","SFBandSuisun")
#sfb <- st_transform(sfb,crs=4326)#puts in WGS84 CRS

buffer<-st_read("R/shiny_app/data/For_Pascale/Buffer_6km_MergeCopy.shp")
buffer_4326 <- buffer %>% st_transform(crs = 4326)#puts in WGS84 CRS
hydro<-st_read("R/shiny_app/data/For_Pascale/Hydro_Poly_Join.shp")
hydro_4326 <- hydro %>% st_transform(crs = 4326)#puts in WGS84 CRS
bypass<-st_read("R/shiny_app/data/For_Pascale/bypass_regions.shp")
bypass_4326 <- bypass %>% st_transform(crs = 4326)#puts in WGS84 CRS

myCategoryColor_function <- colorFactor(c("#ffe119", "#f58231"), domain = rec$Type)

pal <- colorFactor(palette = c("#6BAED6","#2171B5","#08519C","#08306B"), domain = hydro_4326$Region)

fishIcon <- makeIcon(
  iconUrl = "https://upload.wikimedia.org/wikipedia/commons/thumb/1/10/Pictograms-nps-water-fish_hatchery.svg/256px-Pictograms-nps-water-fish_hatchery.svg.png",
  iconWidth = 24, iconHeight = 24
)

iconUrl = "<img src=https://upload.wikimedia.org/wikipedia/commons/thumb/1/10/Pictograms-nps-water-fish_hatchery.svg/256px-Pictograms-nps-water-fish_hatchery.svg.png style='width:20px;height:20px;'> Release Location"

#map

m <- leaflet() %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner")%>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite")%>%
      addControl(html = iconUrl, position = "topleft")%>%
      addLegend(title = "Receiver Technology",
                pal =  myCategoryColor_function,
                values = rec$Type, opacity = .8,
                position="topleft") %>%
      # add legend for river segments
      addLegend(title = "River Regions from Johnson et al. 2017",
                #pal =  pal,
                #values = hydro_4326$Region,
                opacity = .8,
                colors = c("#2171B5","#08306B","#6BAED6","#08519C"),
                labels = c("Hydrography (Lindley et al. 2006)", "Upper Sacramento River", "Middle Sacramento River", "Tidal Delta, Estuary & Bays"),
                position="topleft")%>%
      setView(lat =     39.2, lng = -121.615, zoom = 7.5)%>% #verona
        addPolygons(data = bypass_4326,
                    fillColor = ~pal(Region),
                    color = ~pal(Region),
                    weight = 1)%>%
        addPolygons(data = buffer_4326,
                    fillColor = ~pal(Region),
                    color = ~pal(Region),
                    weight = 1)%>%
        addPolygons(data = hydro_4326,
                    fillColor = ~pal(Region),
                    color = "#2171B5",
                    weight = 1)%>%
  addMarkers(data=rel,icon=fishIcon)%>%
  addCircleMarkers(data = rec, fillColor= ~myCategoryColor_function(Type),
                   radius= 6, color=~myCategoryColor_function(Type), weight=2, fillOpacity = .5)%>%
  addMiniMap(zoomLevelFixed=2)%>%
  addScaleBar()#addMiniMap(centerFixed=c(63, -150),zoomLevelFixed=2)


mapshot(m, url = "7.26.21.map.html")
mapshot(m, file =  "~/7.27.21.map_v3.png", remove_controls = c("zoomControl", "homeButton", "layersControl"))

# need to check data

#setwd("C:/Users/pgoertler/OneDrive - deltacouncil/AT Diversity ms/DFA/study-detections/Missing_files")
#all.j<-read.csv("all_JSATS.csv")
#head(all.j)
# need lat lon, see JSATS_4Von.R

jsats<-rbind(w.17[,c(1,2,4,9,13:18)], sb.17[,c(1,2,4,9,13:18)], w.13[,c(1,2,4,9,13:18)],
             w.14[,c(1,2,4,9,13:18)], w.15[,c(1,2,4,9,13:18)],w.16[,c(1,2,4,9,13:18)],
             m.16[,c(1,2,4,9,13:18)], d.16[,c(1,2,4,9,13:18)], m.17[,c(1,2,4,9,13:18)],
             n.16[,c(1,2,4,9,13:18)], n.17[,c(1,2,4,9,13:18)], r.17[,c(1,2,4,9,13:18)],
             sb.15[,c(1,2,4,9,13:18)], sb.16[,c(1,2,4,9,13:18)], f.13[,c(1,2,4,9,13:18)],
             f.14[,c(1,2,4,9,13:18)], f.15[,c(1,2,4,9,13:18)], m.13[,c(1,2,4,9,13:18)],
             m.14[,c(1,2,4,9,13:18)], m.15[,c(1,2,4,9,13:18)], c.16[,c(1,2,4,9,13:18)],
             c.17[,c(1,2,4,9,13:18)], d.17[,c(1,2,4,9,13:18)], fd.13[,c(1,2,4,9,13:18)],
             fd.14[,c(1,2,4,9,13:18)], f.12[,c(1,2,4,9,13:18)], b.14[,c(1,2,4,9,13:18)],
             c.12[,c(1,2,4,9,13:18)], c.13[,c(1,2,4,9,13:18)],fr.12[,c(1,2,4,9,13:18)],
             fr.13[,c(1,2,4,9,13:18)], fr.14[,c(1,2,4,9,13:18)], fr.15[,c(1,2,4,9,13:18)],
             t.13[,c(1,2,4,9,13:18)])
head(jsats)


setwd("C:/Users/pgoertler/OneDrive - deltacouncil/AT Diversity ms/DFA/study-detections")

dat<-read.csv("dat.tt_v4.csv")
head(dat)

j.tt<-subset(dat, TagType == "JSATS")
fish.j<-unique(j.tt$FishID)

new.j<-jsats[jsats$FishID %in% fish.j, ]
length(unique(new.j$FishID))

rec.j <- unique(new.j[,c(5:10)])
rec.j$Type = "JSATS"

write.csv(rec.j, "releaseloc4JSATS.csv")

# jeremy's suggestion
rel.date.j <- data.frame(FishID = NA, time = strptime("01/01/1900 11:11:11",format="%m/%d/%Y %H:%M:%OS", tz="Etc/GMT+8"), rowName = NA)

for(i in unique(new.j$FishID)){
  temp.dat <- new.j[new.j$FishID == i,]
  min.time <- min(temp.dat$time)
  #browser()
  temp.results <- data.frame(FishID = i, time = as.POSIXlt(min.time,format="%m/%d/%Y %H:%M:%OS", tz="Etc/GMT+8"), rowName = rownames(temp.dat[temp.dat$time == min(temp.dat$time), ]))
  rel.date.j<-rbind(rel.date.j, temp.results)
}

head(rel.date.j)

new.j.sub <- new.j[rel.date.j$rowName, ]
length(unique(new.j.sub$FishID))
length(unique(new.j.sub$min.time))

write.csv(new.j.sub, "NEW.releaseloc4JSATS.csv")
###############################

# routes map

routes<-read.csv("travel.time.jsats.wRoute_unique.csv")
head(routes)

new.rec.j<-merge(rec.j, routes, by = "LOC", all=TRUE)
write.csv(new.rec.j, "new.rec.j.csv")

# double checked Travel_Time_JSATS
new.route<-read.csv("new.rec.j_v2.csv") #added a color column and removed upper sites
head(new.route)
str(new.route)
new.route$check <- as.character(new.route$check)
# make the same map, but use column for color
myCategoryColor_function_v2 <- colorFactor(c("#000000", "#000075", "#800000", "#f58231", "#ffe119"), domain = new.route$Route_v2)

require(RColorBrewer)
brewer.pal(6, "GnBu")

# final route map
#setwd("~/acoustic-telemetry-synthesis")
route.buff<-st_read("R/shiny_app/data/For_Pascale/Routes/Merge_BufClip.shp")
route.buff_4326 <- route.buff %>% st_transform(crs = 4326)#puts in WGS84 CRS

pal4route <- colorFactor(palette = c("#aaffc3", "#42d4f4", "#469990", "#3cb44b"), domain = route.buff_4326$Region)

#setwd("C:/Users/pgoertler/OneDrive - deltacouncil/AT Diversity ms/DFA/study-detections")
ee<- read.csv("data/EstuaryExit.csv")

exitIcon <- makeIcon(
  iconUrl = "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8a/ISO_Exit_-_Left.svg/800px-ISO_Exit_-_Left.svg.png",
  iconWidth = 24, iconHeight = 12
)

iconUrl.ee = "<img src=https://upload.wikimedia.org/wikipedia/commons/thumb/8/8a/ISO_Exit_-_Left.svg/800px-ISO_Exit_-_Left.svg.png style='width:40px;height:20px;'> Estuary"

r <- leaflet() %>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner")%>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite")%>%
  addControl(html = iconUrl.ee, position = "topleft")%>%
  addLegend(title = "Receiver Technology",
            pal =  myCategoryColor_function,
            values = rec$Type, opacity = .8,
            position="topleft") %>%
  # add legend for river segments
  addLegend(title = "Routes",
            opacity = .8,
            colors = c("#3cb44b", "#aaffc3", "#42d4f4", "#469990"),
            labels = c("Yolo Bypass", "Central Delta", "Sacramento River", "Sacranento Sloughs"),
            position="topleft")%>%
  setView(lat =     38.54071, lng = -121.51201, zoom = 10)%>%
  setMaxBounds( lng1 =  -122.56110973518163
                , lat1 = 38.8072040833762
                , lng2 = -121.26976885601101
                , lat2 = 37.95474555039915)%>%
  addPolygons(data = route.buff_4326,
              fillColor = ~pal4route(Region),
              color = ~pal4route(Region),
              weight = 2)%>%
  addPolygons(data = hydro_4326,
              fillColor = ~pal(Region),
              color = "#2171B5",
              weight = 1) %>%
  addMarkers(data=ee,icon=exitIcon)%>%
  addCircleMarkers(data = rec, fillColor= ~myCategoryColor_function(Type),
                   radius= 6, color=~myCategoryColor_function(Type), weight=2, fillOpacity = .5)

mapshot(r, url = "route.map.html")

# routes plus sites labeled
pal4route <- colorFactor(palette = c("#1B9E77", "#7570B3", "#E7298A", "#66A61E"), domain = route.buff_4326$Region)

s <- leaflet() %>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner")%>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite")%>%
  addLegend(title = "Receiver Technology",
            pal =  myCategoryColor_function,
            values = rec$Type, opacity = .8,
            position="topleft") %>%
  # add legend for river segments
  addLegend(title = "Routes",
            colors = c("#1B9E77", "#7570B3", "#E7298A", "#66A61E"),
            labels = c("Central Delta", "Sacramento River", "Sacramento Sloughs", "Yolo Bypass"),
            position="topleft")%>%
  setView(lat =     38.54071, lng = -121.51201, zoom = 10.2)%>%
  setMaxBounds( lng1 =  -122.56110973518163
                , lat1 = 38.8072040833762
                , lng2 = -121.26976885601101
                , lat2 = 35.95474555039915)%>%
  addPolygons(data = route.buff_4326,
              fillColor = ~pal4route(Region),
              color = ~pal4route(Region),
              weight = 2)%>%
  addPolygons(data = hydro_4326,
              fillColor = ~pal(Region),
              color = "#6699CC",
              weight = 1) %>%
  addCircleMarkers(data = rec, fillColor= ~myCategoryColor_function(Type),
                   radius= 6, color=~myCategoryColor_function(Type), weight=2, fillOpacity = .5)%>%
  addLabelOnlyMarkers(
    lng = -121.684739, lat = 38.158994,
    label = "Rio Vista Bridge",
    labelOptions = labelOptions(noHide = T))%>%
  addLabelOnlyMarkers(
    lng = -121.505, lat = 38.46467,
    label = "Freeport",
    labelOptions = labelOptions(noHide = T))%>%
  addLabelOnlyMarkers(
    lng = -121.92957, lat = 38.047892,
    label = "Chipps Island",
    labelOptions = labelOptions(noHide = T))%>%
  addLabelOnlyMarkers(
    lng = -121.75524, lat = 38.07568,
    label = "Decker Island",
    labelOptions = labelOptions(noHide = T))%>%
  addLabelOnlyMarkers(
    lng = -121.751644, lat = 38.026202,
    label = "Antioch Bridge",
    labelOptions = labelOptions(noHide = T))%>%
  addLabelOnlyMarkers(
    lng = -122.12150, lat = 38.04288,
    label = "Benicia Bridge",
    labelOptions = labelOptions(noHide = T))

# side by side
mapshot(m, file =  "map_12_28_22.png", remove_controls = c("zoomControl", "homeButton", "layersControl"))
mapshot(s, file = "route_map_12_28_22.png", remove_controls = c("zoomControl", "homeButton", "layersControl"))
