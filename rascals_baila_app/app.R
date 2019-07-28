rm(list=ls())

library(sf)
library(tidyverse)
library(leaflet)
library(shiny)
library(ggmap)
library(classInt)
library(RColorBrewer)
library(dendextend)

load("baila_sf.rda")
load("rascal_clean.rda")

boundary <- 
  baila_sf %>%
  select(bg_id)

df <- 
  baila_sf %>% 
  st_set_geometry(NULL) %>% 
  select(-"bg_id")

names(df)
names(rascal_sp)

rascal_sp<-
  rascal_sp %>% 
  select(scientific_name, url, image_url) %>%
  mutate(url_link = 
           paste0("<a href='", url,"'>", paste0(url),"</a>" )) %>%
  mutate(image = 
           paste0("<img width='300' height='300' src='", image_url, "'/>"))


#######################################
#######################################
#######################################
#######################################

server <- function(input, output) {
  
  rascal_filter <- reactive({
    rascal_sp %>% filter(scientific_name %in% input$species) 
  })
  
  k <- eventReactive(input$go, {input$cluster})
  
  cluster <- eventReactive(input$go, {
    df %>% 
      select (input$variable) %>%
      scale()  %>% 
      dist (method = "euclidean") %>% 
      hclust(method = "ward.D2") %>% 
      as.dendrogram %>%
      cutree(k=input$cluster, order_clusters_as_data=FALSE) %>%
      as.data.frame() %>% 
      rownames_to_column("id") %>% 
      mutate(id = as.numeric(id)) %>% 
      rename("urban_type" = ".") %>% 
      arrange(id)
  })
  
  newcolor <- eventReactive(input$go, {
    rev (colorRampPalette(brewer.pal(9,"Spectral"))(input$cluster))})
  
  combine_urban_type<- function(x) {
    x <- cluster()
    baila_sf%>%
      mutate(urban_type = x$urban_type) %>% 
      mutate(urban_type = as.factor(urban_type))}
  
  
  output$map <- renderLeaflet({
    
    leaflet(boundary) %>%
      setView( lng=-118.2074, lat=34.05033,zoom=10) %>%
      addProviderTiles(providers$Stamen.Toner,group="Stamen Toner") %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik,group="Open Street Map") %>%
      addProviderTiles(providers$Esri.WorldImagery,group="Esri WorldImagery") %>%
      addProviderTiles(providers$Stamen.Terrain,group="Stamen Terrain") %>%
      addPolygons(stroke = TRUE, fill= FALSE, color ="blue", weight = 0.3) %>%
      addLayersControl(
        baseGroups = c("Stamen Toner",  "Open Street Map",
                       "Esri WorldImagery",  "Stamen Terrain")) 
  })
  
  observeEvent(input$go,{
    withProgress(message = 'creating typology map', value = 0.8, {
      Sys.sleep(0.1)
      
      baila_sf_type <- combine_urban_type(cluster) 
      
      pal <- colorFactor(newcolor(), levels = levels(baila_sf_type$urban_type))
      
      leafletProxy("map", data = baila_sf_type) %>% 
        clearShapes() %>%
        clearControls() %>%
        addPolygons( stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.8,
                     fillColor = ~pal(urban_type), weight = 0.3) %>%
        addLegend("bottomleft", pal = pal, opacity = 1,
                  values = ~urban_type, 
                  title = "Urban Type")
    })
    
  })
  

  
 pal_sp<- function(x) {
   x <- rascal_filter()
   colorFactor(palette = "Set1", 
               levels = x$scientific_name)
 }
    
  
  
  observe({
    
    pal_sp_sp <- pal_sp(rascal_filter()) 
    
    leafletProxy("map", data = rascal_filter()) %>%
      clearMarkers() %>%
      addCircleMarkers(radius = 3,
                       color = ~pal_sp_sp(scientific_name),
                       popup = ~paste(
                         "<h5 style='color: blue'>iNaturalist</h5>",
                         # "Name:",scientific_name, 
                         "",url_link, 
                         "<br>", image, sep=" "))
  })
  
  output$barplot <- renderPlot({
    baila_sf_type <- combine_urban_type(cluster) 

    rascal_filter() %>%
      st_intersection(baila_sf_type) %>%
      st_set_geometry(NULL) %>%
      group_by(scientific_name, urban_type) %>%
      summarise(count = n()) %>%
      ggplot(aes(x=urban_type, y = count, fill= scientific_name)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.5) +
      scale_fill_brewer(palette = "Set1") +
      theme_classic() +
      theme(legend.position = "bottom", 
            legend.title = element_blank())+
      xlab("")
    
  })
  
}


ui <- fluidPage(
  titlePanel("Rascals data combined with urban habitat classification"),
  sidebarLayout(
    sidebarPanel( selectizeInput("variable", "Select your input variables:",
                                 multiple = TRUE,
                                 choices = sort(unique(names(df)))),
                  numericInput("cluster", "Number of Cluster:", "", min = 1),
                  actionButton("go", "Enter"),
                  selectizeInput("species", "Select your species:",
                                 multiple = TRUE,
                                 choices = sort(unique((rascal_sp$scientific_name)))),
                  plotOutput(outputId = "barplot", height="300px")),
    mainPanel(leafletOutput("map",  width="100%",height="600px"))
  ))

shinyApp(ui = ui, server = server)


