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

ui <- dashboardPage(
  dashboardHeader(title = "Understanding California Municiple Water Supply and Use",
                  titleWidth = 600),
  dashboardSidebar(
    sidebarMenu(
      menuItem("User Information", tabName = "Info", icon = icon("book-reader")),
      menuItem("Water Suppliers", tabName = "supplier", icon = icon("tint")),
      menuItem("Reports", tabName = "Reports", icon = icon("exclamation"))
      # menuItem(whatever other tabs we might want)
      )
    ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "supplier",
        h2("California Water Suppliers by Name and Location"),
        selectInput(inputId = "supplier", label = "Municipality", choices = c(unique(water_conservation_dup$supplier_name)))
      ),
      tabItem(
        tabName = "Info",
        h2("Application and Data Information")
      ),
      tabItem(
        tabName = "Reports",
        h2("Water Misuse Reports, Follow-ups, and Warnings")
      )
    )
  )
)

 









server <- function(input, output) {}

shinyApp(ui = ui, server = server)
