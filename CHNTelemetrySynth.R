
# load required packages
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")

library(shiny)
library(leaflet)
library(tidyverse)
library(sf)

# import data
rec<- read.csv("data/all_receivers.csv")
#rec$Year<-as.numeric(rec$Year)
rel<- read.csv("data/releaseloc.csv")
#rel$Year<-as.numeric(rel$Year)
#st_layers("data/hydro_forshiny")
st_layers("data/For_Pascale")

#read in region distinctions 
#bypass <- st_read("data/hydro_forshiny","bypass_regions")
#sacr <- st_read("data/hydro_forshiny","SacRiv_NHDPlus")
#delta <- st_read("data/hydro_forshiny","Delta_waterways")
#sfb <- st_read("data/hydro_forshiny","SFBandSuisun")
#sfb <- st_transform(sfb,crs=4326)#puts in WGS84 CRS

buffer<-st_read("data/For_Pascale/Buffer_6km_MergeCopy.shp")
buffer_4326 <- buffer %>% st_transform(crs = 4326)#puts in WGS84 CRS
hydro<-st_read("data/For_Pascale/Hydro_Poly_Join.shp")
hydro_4326 <- hydro %>% st_transform(crs = 4326)#puts in WGS84 CRS
bypass<-st_read("data/For_Pascale/bypass_regions.shp")
bypass_4326 <- bypass %>% st_transform(crs = 4326)#puts in WGS84 CRS


myCategoryColor_function <- colorFactor(palette=c("#FF7F00","#FDBF6F"), domain = rec$Type) 
pal <- colorFactor(palette = c("#C6DBEF","#6BAED6","#2171B5","#08306B"), domain = hydro_4326$Region)

#ui
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", height = '100%', width = '100%'),
  absolutePanel(top = 10, right = 10, 
                id="controls",
                style="z-index:500;",
                class = "panel panel-default",
                draggable = TRUE,
                tags$div(tags$h3("Mapped Release and Receiver Locations:"), 
                         tags$p("Visualize the number of recievers with Year filter."), 
                         tags$p("Click on fish symbols for additional release information."), 
                         tags$p("Please send questions to:", tags$a("Pascale Goertler", href="mailto:Pascale.Goertler@deltacouncil.ca.gov?subject=CHNTelemetrySynth"))),
                #checkboxGroupInput("choices", "Tag Type",choices= list("JSATS","Vemco"),
                #selected=c("Vemco","JSATS")),
                sliderInput(inputId = "choices", label = "Choose a Range of Years",
                            value = c(2013,2017), min = 2007, max = 2017, sep = "")
                # verbatimTextOutput("my_rows")#to show number of rows displayed in map
  )
)

#checkboxGroupInput("choices", "Visualize the number of recievers with optional filters of Year and Tag Type",
#choices =  list("2007","2008","2009","2010","2011","2012","2013", "2014","2015","2016","2017"),
#selected=c("2007","2008","2009","2010","2011","2012","2013", "2014","2015","2016","2017")),

#custom icon
fishIcon <- makeIcon(
  iconUrl = "https://upload.wikimedia.org/wikipedia/commons/thumb/1/10/Pictograms-nps-water-fish_hatchery.svg/256px-Pictograms-nps-water-fish_hatchery.svg.png",
  iconWidth = 24, iconHeight = 24
)

#server
server <- function(input, output) {
  
  filteredData <- reactive( rec %>% filter(Year %in% input$choices) )
  filteredData2 <- reactive( rel %>% filter(Year %in% input$choices) )
  
  output$map <- renderLeaflet({ leaflet() %>% 
      addProviderTiles(providers$Stamen.Toner, group = "Toner")%>% 
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite")%>%
      addLegend(title = "Receiver Technology", 
                pal =  myCategoryColor_function,
                values = rec$Type, opacity = .8, 
                position="topleft") %>%
      # add legend for river segments
      addLegend(title = "River Regions from <a href='https://watershed.ucdavis.edu/files/biblio/Johnson_2017_SFEWS.pdf'>Johnson et al. 2017</a>", 
                #pal =  pal,
                #values = hydro_4326$Region, 
                opacity = .8, 
                colors = c("#08306B","#C6DBEF","#2171B5","#6BAED6"),
                labels = c("Upper Sacramento River", "Middle Sacramento River", "Hydrography (<a href='https://escholarship.org/uc/item/1ss794fc'>Lindley et al. 2006</a>)", "Tidal Delta, Estuary & Bays"),
                position="topleft")%>% 
      setView(lat =     38.63700, lng = -121.2210, zoom = 7)%>%
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479")
    })
  
  observe({
    recf<- filteredData()
    if(nrow(recf) != 0){
      leafletProxy("map", data = filteredData()) %>%
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
        clearMarkers() %>% 
        addCircleMarkers(popup= paste("Location: ",recf$Location#,"<br>",
                                      #"River km:", recf$Rkm #river km in popup (but some are out-dated/inconsistent)
        ),
        fillColor= ~myCategoryColor_function(Type),
        radius= 6, color=NA, weight=2, fillOpacity = .8)
    }
    else{
      leafletProxy("map", data = recf) %>% 
        clearMarkers()}
  })
  #  output$my_rows <- renderPrint({ filteredData() %>% nrow() })#shows how many rows are displayed on map
  observe({
    relf<- filteredData2()
    if(nrow(relf) != 0){leafletProxy("map", data = filteredData2()) %>%
       
        #release locations
        addMarkers(data=relf,icon=fishIcon, popup= paste("Release Location: ", relf$Release.Location,"<br>",
                                          "Study: ", relf$Study,"<br>",
                                          "Year: ",relf$Year,"<br>",
                                          "Run: ",relf$Run)) #add number of fish released each year/location? 
    }
    else{
      leafletProxy("map", data = relf) %>% 
        clearMarkers()}
  })
}

#shiny
shinyApp(ui = ui, server = server)

