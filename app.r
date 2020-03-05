## Load Packages

library(tidyverse)
library(shiny)
library(shinydashboard)
library(devtools)
library(shinyWidgets)
library(shinythemes)
# library(shinydashboard)
library(leaflet)
library(tmap)
library(tmaptools)
library(mapview)
library(sf)
library(kableExtra)


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
                 sliderInput("date_select", "Reporting Month and Year", min = as.Date("2015-04-01"), max = as.Date("2019-09-01"), value = c(as.Date("2015-04-01","2019-09-01")), timeFormat = "%b %Y"),
                 dateRangeInput("date_select_2", "Reporting Year-Month-Date", start = "2015-04-01", end = "2019-09-01", format = "yyyy-mm-dd", startview = "month", weekstart = 0, separator = "to")
    ),
  
    mainPanel( tabsetPanel(
      tabPanel("Water Supplier Results",
               p("Map of California Water Suppliers:"),
               leafletOutput(outputId = "water_map"),
               p("Other Outputs"),
      tableOutput(outputId = "date_table")),
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

  
  output$water_map <- renderLeaflet({
    
    # tmap_mode("view")
    map <- tm_basemap("Hydda.Base") +
      tm_shape(water_reactive()) +
      tm_fill("supplier_name")
    
    tmap_leaflet(map)
    
  })
  
  # reactive for date widget
  date_reactive <- reactive({
    water_merged %>%
      filter(yy_mm_dd == input$date_select_2) %>%
      select(supplier_name, mandatory_restrictions, stage_invoked)
  
  output$date_table <- renderTable({date_reactive()})
  })
  
  # text for tabs
  {
    output$tab1 <- renderText({
      "this is text1"
    })
    output$tab2 <- renderText({
      "From 2011 to 2017, California experienced an intense, historical drought, which eventually imposed an executive order of a 25 percent reduction in statewide water usage (Executive Order B-29-2015. In 2014, the California State Water Resources Control Board introduced regulation for drought emergency water conservation. Water suppliers with more than 3,000 connections were required to submit monthly reports to the State Water Board that included residential gallons-used-per-capita, number of water days allowed, and whether the supplier was under mandatory restrictions. After November 2017, reporting to the State Water Board became voluntary. However, shortly after in May 2018, Governor Jerry Brown signed Senate Bill 606 and Assembly Bill 1668, which reauthorized the mandatory reporting. 
<br>
Throughout the drought, many efforts were made to get residents to reduce their water usage. Since declaring the end of the drought in April 2017, residential water use per capita has on average become stagnant. To promote and encourage a more water saving conscious lifestyle, Governor Jerry Brown continued the monitoring of wasteful water use practice through Executive Order B-40-17. Monitoring was first introduced in the drought emergency water conservation regulation in July 2014. The follow activities are prohibited: 
<br>
- Using potable water to wash sidewalks and driveways<br>
- Allowing more than incidental runoff when irrigating turf and other ornamental landscapes<br>
- Using hoses without automatic shutoff nozzles to wash motor vehicles<br>
- Using potable water in ornamental fountains or decorative water features that do not recirculate the water <br>
- Irrigating turf and ornamental landscape during and within 48 hours following measurable rainfall <br>
- Hotels and motels laundering towels and linens daily without providing guests the option of using them again <br>
- During a drought emergency, the serving of drinking water in restaurants and bars without it being requested <br>
- As of January 1, 2025, irrigating turf on public street medians and parkways unless the turf serves a community recreational or civic function, the turf is irrigated incidentally with trees, or the turf is watered with recycled water by an irrigation system installed prior to January 1, 2018.<br>
<br>
<b>References:</b>
<br>
“Regulation Rulemaking on Urban Water Supplier Monthly Water Conservation Report.” 2020. California Water Resources Control Board.
<br>
<a href='https://www.waterboards.ca.gov/water_issues/programs/conservation_portal/water_conservation_reports/' onclick='detect_click(this)'>Website</a>
<br>
“Changes to the Proposed Regulation on Wasteful Water Use Practices.” 2018. California Water Resources Control Board. 
<br>
<a href='https://www.waterboards.ca.gov/water_issues/programs/conservation_portal/regs/docs/factsheet_reg_013118.pdf' onclick='detect_click(this)'>Website</a>"
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