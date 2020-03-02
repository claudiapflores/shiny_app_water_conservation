## Load Packages

# library(tidyverse)
library(shiny)
library(shinydashboard)
library(devtools)
library(shinyWidgets)
library(shinythemes)
# library(shiny)
# library(shinydashboard)
# library(leaflet)
# library(tmap)
# library(tmaptools)
# library(kableExtra)


### User Interface

ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Understanding California Municipal Water Supply and Use"),
  sidebarLayout(
    sidebarPanel("",
                 selectInput(inputId = "supplier_select",
                             label = "Choose a Supplier:",
                             choices = unique(water_merged$supplier_name)
                 ),
                 checkboxGroupInput(inputId = "reports_select",
                                    label = "Choose one or more:",
                                    choices = list("Reported" = 1, "Warning Issued" = 2, "Follow-Up" = 3),
                                    selected = 1),
                 sliderInput("slider", "Reporting Month and Year", min = as.Date("2015-04-01"),max =as.Date("2019-09-01"),value=as.Date("2019-09-01"),timeFormat="%b %Y"),
                 textOutput("SliderText")
    ),
    mainPanel( tabsetPanel(
      tabPanel("Water Supplier Results"),
      tabPanel("User Information"),
      tabPanel("Background"),
      tabPanel("Data")
    ),
              p("State of California:"),
              tableOutput(outputId = "candy_table"),
              p(""),
              plotOutput(outputId = "costume_graph"),
    )
  )
)
  
  
  
  
server <- function(input, output) {}

shinyApp(ui = ui, server = server)