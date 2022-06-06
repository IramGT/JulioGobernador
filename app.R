## LIBRARIES

library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)
library(viridis)
library(readr)
library(utils)
library(raster)
library(tmap)
library(shiny)
library(bslib)
library(shinyjs)

## DATA BASES IN GOOGLE SHEETS

resultados <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQ7RbYo1skw_v26JuyhzHSkCc9vVB_wNO19IJ0bKy1u90R2IQW6AtmEISN7lCzi2UkomKxA06CjRBMN/pub?gid=1897516273&single=true&output=csv")
apertura <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSueEGrWf7jYeLXNzlzNqsWqLf5PqJHlpj-UeyW8CK8RHR4joHy1t9Pa8P9aO97mvmBDMh5SvVfGwNQ/pub?gid=512671862&single=true&output=csv")

## SHAPE FILE

shp_seccion <- read_sf(dsn = "seccionHidalgo", layer = "SeccionMineral")

shp_seccion<-  shp_seccion %>% 
  rename("SECCION" = "seccion")
## LEFT JOIN DATA BASE AND THE SHAPEFILE

shp_seccion <- resultados %>% 
  mutate(MORENA =round(100 * `JULIO MENCHACA` / (`JULIO MENCHACA`+`CAROLINA V`+`FRANCISCO X`+ `JOSE LUIS L`),1)) %>% 
  left_join(shp_seccion, .)

#### UI ####
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "simplex"),
  tags$style(type = "text/css", "html, body {width:100%; height:100%} #map{height:80vh !important;}"),
  tabsetPanel(
    tabPanel("MAP", 
             # Application title
             hr(), 
             
             # Sidebar with a slider input for number of bins
             # Show a plot of the generated distribution
             fluidRow(
               width = 12,selectInput(inputId = "select_1", label = "", selected = "MINERAL DE LA REFORMA", choices = append("MINERAL DE LA REFORMA", resultados$SECCION)), leafletOutput("map_1"))
             
    ),
    tabPanel("RES", h3(img(src = "JM.jpg", height = 85, width = 167)) ,DT::dataTableOutput("ziptable1")),
    tabPanel("SEC", h3(img(src = "JM.jpg", height = 85, width = 167)) ,DT::dataTableOutput("ziptable2")),
    tabPanel("CAS", h3(img(src = "JM.jpg", height = 85, width = 167)) ,DT::dataTableOutput("ziptable3"))
  ))

#### SERVER ####

server <- function(input, output) {
  
  output$map_1 <- renderLeaflet({
    tm <- shp_seccion %>% 
      filter(MunicipioN == input$select_1) %>%
      tm_shape() +
      tm_polygons(col = "MORENA", id = "SECCION", midpoint = 60   , n = 5, style = "pretty", alpha = .5, palette = "YlOrRd", popup.vars = c("%JM" ="MORENA", "JULIO M" = "JULIO MENCHACA", "CAROLINA" = "CAROLINA V", "FRANCISCO" = "FRANCISCO X", "JOSE LUIS" = "JOSE LUIS L"), legend.show = FALSE) +
      tm_basemap("OpenStreetMap")  
    
    tmap_leaflet(tm)
  })
  
  output$ziptable2 <- DT::renderDataTable({
    tabla <- resultados 
    
    DT::datatable(tabla, options = list(pageLength = 50))
  })  
  
  output$ziptable1 <- DT::renderDataTable({
    tabla <- apertura %>% 
      filter(Localidad == "TOTAL" | Localidad == "FINAL") %>% 
      dplyr::select(-c(seccion, Casilla))  
    
    DT::datatable(tabla, options = list(pageLength = 200), escape = FALSE)
  })
  
  output$ziptable3 <- DT::renderDataTable({
    tabla <- apertura
    
    
    DT::datatable(tabla, options = list(pageLength = 200), escape = FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)