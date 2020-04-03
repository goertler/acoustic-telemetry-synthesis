
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
rel<- read.csv("data/releaseloc.csv")
st_layers("data/hydro_forshiny")

#read in region distinctions 
bypass <- st_read("data/hydro_forshiny","bypass_regions")
sacr <- st_read("data/hydro_forshiny","SacRiv_NHDPlus")
delta <- st_read("data/hydro_forshiny","Delta_waterways")
sfb <- st_read("data/hydro_forshiny","SFBandSuisun")
sfb <- st_transform(sfb,crs=4326)#puts in WGS84 CRS

myCategoryColor_function <- colorFactor("Dark2", rec$Type) 
factpal <- colorFactor(topo.colors(5), sacr$River_Seg) # color palette for river segments according to Johnson et al 2017

#ui
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", height = '100%', width = '100%'),
  absolutePanel(top = 10, right = 10, 
                checkboxGroupInput("choices", "Water Year", choices =  list("2007","2008","2009","2010","2011","2012","2013", "2014","2015","2016","2017"),
                                   selected=c("2007","2008","2009","2010","2011","2012","2013", "2014","2015","2016","2017")),
                #checkboxGroupInput("choices", "Tag Type",choices= list("JSATS","Vemco"),
                #selected=c("Vemco","JSATS")),
                # verbatimTextOutput("my_rows")#to show number of rows displayed in map
  )
)

#server
server <- function(input, output) {
  
  filteredData <- reactive( rec %>% filter(Year %in% input$choices) )
  filteredData2 <- reactive( rel %>% filter(Year %in% input$choices) )
  
  output$map <- renderLeaflet({ leaflet() %>% addTiles() %>%
      addLegend(title = "Receiver Technology", 
                pal =  myCategoryColor_function,
                values = rec$Type, opacity = .8, 
                position="topleft") %>%
      # add legend for river segments
      addLegend(title = "River Regions", 
                pal =  factpal,
                values = sacr$River_Seg, opacity = .8, 
                position="topleft")%>% 
      setView(lat =     38.63700, lng = -121.2210, zoom = 7)})
  
  observe({
    recf<- filteredData()
    if(nrow(recf) != 0){
      
      leafletProxy("map", data = filteredData()) %>% 
        addPolygons(data= bypass,color = ~factpal(River_Seg), weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5) %>% # adds bypass
        addPolygons(data= sacr,color = ~factpal(River_Seg), weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5) %>% # sac river
        addPolygons(data= delta,color = ~factpal(River_Seg), weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5) %>% # delta 
        addPolygons(data= sfb,color = ~factpal(River_Seg), weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5)%>% # adds sfb and suisun
        
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
    if(nrow(relf) != 0){
      
      leafletProxy("map", data = filteredData2()) %>% 
        addPolygons(data= bypass,color = ~factpal(River_Seg), weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5) %>% # adds bypass
        addPolygons(data= sacr,color = ~factpal(River_Seg), weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5) %>% # sac river
        addPolygons(data= delta,color = ~factpal(River_Seg), weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5) %>% # delta 
        addPolygons(data= sfb,color = ~factpal(River_Seg), weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.5)%>% # adds sfb and suisun
        
        #release locations
        addMarkers(data=relf,popup= paste("Release Location: ",rel$Release.Location,"<br>",
                                          "Study: ", relf$Study,"<br>",
                                          "Year: ",relf$Year,"<br>",
                                          "Run: ",relf$Run),) #add number of fish released each year/location? 
    }
    else{
      leafletProxy("map", data = relf) %>% 
        clearMarkers()}
  })
}

#shiny
shinyApp(ui = ui, server = server)

