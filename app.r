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
                 sliderInput("slider2", "Reporting Month and Year", min = as.Date("2015-04-01"), max = as.Date("2019-09-01"), value = c(as.Date("2015-04-01","2019-09-01")), timeFormat = "%b %Y"),
                 textOutput("SliderText")
    ),
  
    mainPanel( tabsetPanel(
      tabPanel("Water Supplier Results",
               p("Map of California Water Suppliers:"),
               plotOutput(outputId = "water_map"),
               p("Other Outputs")),
      tabPanel("User Information", htmlOutput("tab1")),
      tabPanel("Background", htmlOutput("tab2")),
      tabPanel("Data", htmlOutput("tab3"))
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
  output$water_map <- renderPlot({
    ggplot() +
      geom_sf(data = water_reactive(), aes(fill = supplier_name), show.legend = FALSE) +
      geom_sf(data = ca_counties, alpha = 0.2) +
      theme_minimal()
  })
  
  # reactive for date
  date <- reactive({ water_merged %>% 
      select(reporting_month)
  })
  
  output$slider2 <- renderPrint({ input$date })
  
  # text for tabs
  {
    output$tab1 <- renderText({
      "this is text1"
    })
    output$tab2 <- renderText({
      "this is text2"
    })
    output$tab3 <- renderText({paste(
      "The water conservation data used for this project is from the California State Water Resources Control Board and the spatial data is from the California Department of Water Resources. The California State Water Resources Control Board collected data from June 2014 to November 2019 from 409 municipalities. All conversions for the conservation data were completed by the State Water Board and can be viewed
<a href='https://www.waterboards.ca.gov/waterrights/water_issues/programs/drought/docs/ws_tools/guidance_estimate_res_gpcd.pdf' onclick='detect_click(this)'>here.</a>
<br>
<br>
<b>   
Raw datasets and metadata: </b><br>
- <a href='https://www.waterboards.ca.gov/water_issues/programs/conservation_portal/conservation_reporting.html' onclick='detect_click(this)'>Water Conservation</a>
<br>
- <a href='https://www.waterboards.ca.gov/water_issues/programs/conservation_portal/docs/2019jul/uw_supplier_data_definitions.pdf' onclick='detect_click(this)'>Metadata (Water Conservation)</a>
<br>
- <a href='http://atlas-dwr.opendata.arcgis.com/datasets/45d26a15b96346f1816d8fe187f8570d_0' onclick='detect_click(this)'>Water Districts (Spatial)</a>
<br>
<br>
<b>
References:</b><br>
Gomberg, Max et al. 2019. Water Conservation and Production Reports. California State Water Resources Control Board. 
<br>
<a href='https://www.waterboards.ca.gov/water_issues/programs/conservation_portal/conservation_reporting.html' onclick='detect_click(this)'>Website</a>
<br>
Department of Water Resources Atlas. 2019. Water Districts. CA Department of Water Resources GIS Data. 
<br>
<a href='http://atlas-dwr.opendata.arcgis.com/datasets/45d26a15b96346f1816d8fe187f8570d_0' onclick='detect_click(this)'>Website</a>")
    })
  }
  
}

shinyApp(ui = ui, server = server)