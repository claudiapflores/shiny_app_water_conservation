## Load Packages

# library(tidyverse)
# library(shiny)
# library(shinydashboard)
# library(leaflet)
# library(tmap)
# library(tmaptools)
# library(kableExtra)

# Read in data... not yet, waiting for finalized/wrangled data with spatial

### User Interface

ui <- fluidPage(
  titlePanel("California Municiple Water Supply and Management"),
  sidebarPanel("Widgets",
               dropdownMenu() #widgets
               ),
  mainPanel("outputs")
)








server <- function(input, output) {}

shinyApp(ui = ui, server = server)