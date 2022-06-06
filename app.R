## LIBRARIES

library(sf)
library(ggplot2)
library(dplyr)
library(viridis)
library(readr)
library(utils)
library(raster)
library(shiny)
library(bslib)
library(shinyjs)
library(DT)

## DATA BASES IN GOOGLE SHEETS

# Bases de datos
apertura <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSueEGrWf7jYeLXNzlzNqsWqLf5PqJHlpj-UeyW8CK8RHR4joHy1t9Pa8P9aO97mvmBDMh5SvVfGwNQ/pub?gid=512671862&single=true&output=csv")
rseccion <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQ7RbYo1skw_v26JuyhzHSkCc9vVB_wNO19IJ0bKy1u90R2IQW6AtmEISN7lCzi2UkomKxA06CjRBMN/pubhtml?gid=1897516273&single=true")


#### UI ####
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "simplex"),
  tags$style(type = "text/css", "html, body {width:100%; height:100%} #map{height:80vh !important;}"),
    tabsetPanel(
    tabPanel("Julio Gobernador", DT::dataTableOutput("ziptable1")),
    tabPanel("SECCION", DT::dataTableOutput("ziptable3")),
    tabPanel("Casillas", DT::dataTableOutput("ziptable2"))
    
    ))

#### SERVER ####

server <- function(input, output) {
  
  output$ziptable1 <- DT::renderDataTable({
    tabla <- apertura %>% 
      filter(Localidad == "TOTAL" | Localidad == "FINAL") %>% 
      dplyr::select(-c(seccion, Casilla))  
    
    DT::datatable(tabla, options = list(pageLength = 200), escape = FALSE)
  })
  
  output$ziptable3 <- DT::renderDataTable({
    tabla <- rseccion  
    
    DT::datatable(tabla, options = list(pageLength = 200), escape = FALSE)
  })
    
    output$ziptable2 <- DT::renderDataTable({
      tabla <- apertura
        
      
      DT::datatable(tabla, options = list(pageLength = 200), escape = FALSE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


