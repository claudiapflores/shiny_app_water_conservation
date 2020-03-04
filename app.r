## Load Packages

library(tidyverse)
library(shiny)
library(shinydashboard)
library(devtools)
library(shinyWidgets)
library(shinythemes)
# library(shiny)
# library(shinydashboard)
library(leaflet)
library(tmap)
library(tmaptools)
library(mapview)
library(sf)
# library(kableExtra)


### User Interface

ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Understanding California Municipal Water Supply and Use"),
  sidebarLayout(
    sidebarPanel("",
                 selectInput(inputId = "supplier_select",
                             label = "Choose a Supplier:",
                             choices = unique(water_total$supplier_name)
                 ),
                 checkboxGroupInput(inputId = "reports_select",
                                    label = "Choose one or more:",
                                    choices = list("Reported" = 1, "Warning Issued" = 2, "Follow-Up" = 3),
                                    selected = 1),
                 sliderInput("slider", "Reporting Month and Year", min = as.Date("2015-04-01"),max =as.Date("2019-09-01"),value=as.Date("2019-09-01"),timeFormat="%b %Y"),
                 textOutput("SliderText")
    ),
    mainPanel( tabsetPanel(
      tabPanel("Water Supplier Results",
               p("Map of California Water Suppliers:"),
               tmapOutput(outputId = "water_map"),
               p("Other Outputs")),
      tabPanel("User Information"),
      tabPanel("Background"),
      tabPanel("Data")
    ),
              #p(""),
              #plotOutput(outputId = "water_map"),
              #p(""),
              #plotOutput(outputId = "water_map"),
    )
  )
)
  
  
  
  
server <- function(input, output) {
  # reactive map dataframe
  water_reactive <- reactive({
    water_total %>% 
      select(supplier_name) %>% 
      filter(supplier_name == input$supplier_select)
  })
  
  # reactive map

  
  output$water_map <- renderLeaflet({
    
    # tmap_mode("view")
    map <- tm_basemap("Hydda.Base") +
      tm_shape(water_reactive) +
      tm_fill("supplier_name")
    
    tmap_leaflet(map)
    
  })
  
  
  
  
  
}

shinyApp(ui = ui, server = server)