# load required packages
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")
if(!require(shinyjs)) install.packages("shinyjs", repos = "http://cran.us.r-project.org")

library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(shinyjs)

# data
rec <- read.csv("data/rec4shiny_2023_qc.csv")
rec$Year <- as.numeric(rec$Year)
rel <- read.csv("data/re14shiny_2023.csv")

st_layers("data/For_Pascale")

buffer<-st_read("data/For_Pascale/Buffer_6km_MergeCopy.shp")
buffer_4326 <- buffer %>% st_transform(crs = 4326)#puts in WGS84 CRS
hydro<-st_read("data/For_Pascale/Hydro_Poly_Join.shp")
hydro_4326 <- hydro %>% st_transform(crs = 4326)#puts in WGS84 CRS
bypass<-st_read("data/For_Pascale/bypass_regions.shp")
bypass_4326 <- bypass %>% st_transform(crs = 4326)#puts in WGS84 CRS

myCategoryColor_function <- colorFactor(c("#ffe119", "#f58231"), domain = rec$Type)

pal <- colorFactor(palette = c("#D2D2D2","#6E6E6E","#2171B5","#A0A0A0"), domain = hydro_4326$Region)

#custom icon
fishIcon <- makeIcon(
  iconUrl = "https://upload.wikimedia.org/wikipedia/commons/thumb/1/10/Pictograms-nps-water-fish_hatchery.svg/256px-Pictograms-nps-water-fish_hatchery.svg.png",
  iconWidth = 24, iconHeight = 24
)

iconUrl = "<img src=https://upload.wikimedia.org/wikipedia/commons/thumb/1/10/Pictograms-nps-water-fish_hatchery.svg/256px-Pictograms-nps-water-fish_hatchery.svg.png style='width:20px;height:20px;'> Release Location"


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
                         tags$p("Visualize the number of receivers with Year filter."),
                         tags$p("Click on fish symbols for additional release information."),
                         tags$p("Please send questions to:", tags$a("Pascale Goertler", href="mailto:Pascale.Goertler@water.ca.gov?subject=CHNTelemetrySynth"))),
                #sliderInput(inputId = "choices", label = "Choose a Range of Years",
                #            value = c(2013,2017), min = 2007, max = 2017, sep = ""),
                actionButton("Info", "Click for more information.", style="simple", color="primary", icon=icon("question-circle")),
                hidden(
                  div(id='text_div',
                      verbatimTextOutput("text"))
                ) ) )


#server
server <- function(input, output, session) {
  observeEvent(input$Info, {
    toggle('text_div')
    output$text <- renderText({"This map displays location information for the in-progress study:
      Using Acoustic Data to Describe Diversity in Juvenile Chinook Salmon.

      While metrics such as growth, condition and survival are commonly
      used to measure fish performance, life history diversity has emerged
      as an equally important factor. However, juvenile salmon life history
      diversity is difficult to quantify, and resource managers have
      traditionally relied upon an oversimplified approach.
      Oversimplified descriptions of juvenile life history may be ignoring
      critical aspects of phenotypic diversity that are important to
      population resilience and salmon conservation. To address this issue,
      we use acoustic telemetry data to better assess diversity in migration
      timing with data collected for survival studies. This is a logical
      next step for acoustic data; towards understanding how juvenile salmon
      use these river reaches of interest to navigate the risks and rewards
      of freshwater residency. "})
  })

  #filteredData <- reactive( rec %>% filter(Year %in% input$choices) )
  #filteredData2 <- reactive( rel %>% filter(Year %in% input$choices) )

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
                colors = c("#A0A0A0","#D2D2D2","#6E6E6E","#2171B5"),
                labels = c("Upper Sacramento River", "Middle Sacramento River", "Tidal Delta, Estuary & Bays", "Hydrography (<a href='https://escholarship.org/uc/item/1ss794fc'>Lindley et al. 2006</a>)"),
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
    #recf<- filteredData()
    #if(nrow(recf) != 0){
      leafletProxy("map", data = rec) %>%
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
        addCircleMarkers(popup= paste("Location: ",rec$Location#,"<br>",
        ),
        fillColor= ~myCategoryColor_function(Type),
        radius= 6, color=NA, weight=2, fillOpacity = .8)
    #}
    #else{
    #  leafletProxy("map", data = recf) %>%
    #    clearMarkers()}
  })
  observe({ leafletProxy("map", data = rel) %>%
    #relf<- filteredData2()
    #if(nrow(relf) != 0){leafletProxy("map", data = filteredData2()) %>%

        #release locations
        addMarkers(data=rel,icon=fishIcon, popup= paste("Release Location: ", rel$Release.Location,"<br>",
                                                         ifelse(is.na(rel$link), paste("Study:",rel$Study,"<br>"), paste("Study: <a href='",rel$link,"'>",rel$Study,"</a><br>")),
                                                         "Year: ",rel$Year,"<br>",
                                                         "Run: ",rel$Run)) #add number of fish released each year/location?
    #}
    #else{
    #  leafletProxy("map", data = relf) %>%
    #    clearMarkers()}
  })
}

#shiny
shinyApp(ui = ui, server = server)

