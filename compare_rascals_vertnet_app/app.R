rm(list=ls())
library(leaflet)
library(sf)
library(tidyverse)
library(shiny)
library(ggmap)
library(classInt)
library(RColorBrewer)

load("df.rda")
load("county.rda")

st_bbox(county)

#######################################
#######################################
#######################################
#######################################

server <- function(input, output) {
  
  filteredData <- reactive({
  df %>% 
      filter(scientific_name %in% input$species) %>%
      filter(year >= input$year[1] & year <= input$year[2]) 
  })
  
  output$map <- renderLeaflet({
    
    leaflet(county) %>%
      setView( lng=-117.7388, lat=34.17189,zoom=7) %>%
      addProviderTiles(providers$Stamen.Toner,group="Stamen Toner") %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik,group="Open Street Map") %>%
      addProviderTiles(providers$Esri.WorldImagery,group="Esri WorldImagery") %>%
      addProviderTiles(providers$Stamen.Terrain,group="Stamen Terrain") %>%
      addPolygons(stroke = TRUE, fill= FALSE, color ="blue", weight = 1) %>%
      addLayersControl(
        baseGroups = c("Stamen Toner", "Open Street Map", "Esri WorldImagery", "Stamen Terrain")) 
     
  })
  
  observe({
    pal <- 
      colorFactor(palette = c("blue", "red"), 
                  levels = c("Rascals", "VertNet"))
    
    leafletProxy("map", data = filteredData()) %>% 
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers( color = ~pal(source),
                        radius = 3,
                         popup = ~paste(
                         "",url_link, 
                         "<br>", image, sep=" ")) %>%
      addLegend("bottomleft", pal = pal, opacity = 1,
                values = ~source, 
                title = "Data source")
      
  })

  
   output$barplot <- renderPlot({
    
    df %>%
      st_set_geometry(NULL) %>%
      group_by(source, scientific_name, year ) %>%
      summarise(counts= n()) %>%
      spread(source, counts, fill= 0) %>%
      gather(source, counts, Rascals:VertNet) %>%
      filter(scientific_name %in% input$species) %>%
      filter(year >= input$year[1] & year <= input$year[2]) %>%
      group_by(scientific_name, source) %>%
      summarise(n=sum(counts)) %>%
      ggplot(aes(y= n, x= source, fill= source) )+
      geom_bar(stat = "identity", width = 0.3)+
      scale_fill_manual("",values = c("blue", "red"))+
      coord_flip()+
      theme_classic()+
      ylab("Number of records")+
      guides(fill=FALSE)})
  
   
   output$impplot <- renderPlot({
     
     filteredData() %>%
       filter(source =="Rascals" ) %>%
       ggplot(aes(x= imp, color = scientific_name, fill=scientific_name ) )+
       geom_histogram(alpha=0.5,addDensityCurve=TRUE)+
       xlab("Percentage of imperviousness")+
       ylab("Number of records")+
       theme_classic()+
       theme(legend.position = c(0.8, 0.8), 
             legend.title = element_blank())})
   
}




ui <- fluidPage(
  titlePanel("Comparision of RASCals and VetNet records"),
  sidebarLayout(
    sidebarPanel(selectizeInput(inputId = "species",
                                label = "choose your species:",
                                multiple = TRUE,
                                choices = sort(unique((df$scientific_name)))),
                  sliderInput(inputId = "year",
                              label = "choose date range:",
                              min = 1900,
                              max = 2019,
                              value = c(1900,2019)),
                  plotOutput(outputId = "barplot", height="120px"),
                  br(),
                  h6("Distribution of RASCals records"),
                  plotOutput(outputId = "impplot", height="250px")),
    mainPanel(leafletOutput("map",  width="100%",height="600px"))
                  ))

shinyApp(ui = ui, server = server)



