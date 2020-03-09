## Load Packages

library(tidyverse)
library(shiny)
library(shinydashboard)
library(devtools)
library(shinyWidgets)
library(shinythemes)
library(leaflet)
library(tmap)
library(tmaptools)
library(mapview)
library(sf)
library(kableExtra)
library(gt)
library(ggthemes)


### User Interface

ui <- navbarPage("Understanding California Municipal Water Supply and Use",
  theme = shinytheme("flatly"),
  tabPanel("User Information",
           mainPanel(width = 20,
             HTML('<center><img src="pic4.jpg", height="350px"/></center>'),
             htmlOutput("tab1"),
             HTML('<br>
                  <br>'),
             HTML('<img src="bren.png", height="125px", style="float:left"/>'),
             HTML('<br>
                  <br>'))),
  tabPanel("Water Suppliers",
    sidebarPanel(
                 selectInput(inputId = "supplier_select",
                             label = "Choose a Supplier:",
                             choices = unique(water_total$supplier_name)
                 )),
    mainPanel(
               leafletOutput(outputId = "water_map"),
               HTML('<br>'),
               HTML('<b>Residential Gallons Per Capita Day Water Use From April 2015 to September 2019</b>'),
               plotOutput(outputId = "per_capita_use")
      )),
  tabPanel("Hydrological Regions",
           sidebarPanel(
             checkboxGroupInput(inputId = "hydrologic_region",
                          label = "Choose One or More Hydrologic Region:",
                          choices = c(unique(water_merged$hydrologic_region)),
                          selected = "South Lahontan"),
             #sliderInput(inputId = "date_select_year",
                         #label = "Select Year", 
                         #min = 2015, 
                         #max = 2019, 
                         #value = c(2015, 2019), 
                         #sep = ""),
             sliderInput("date_select", 
                         "Reporting Month and Year:", 
                         min = as.Date("2015-04-01"), 
                         max = as.Date("2019-09-01"), 
                         value = as.Date(c("2015-04-01","2019-09-01")), 
                         timeFormat = "%b %Y"),
           ),
           mainPanel(
             gt_output(outputId = "datetable"),
             leafletOutput(outputId = "water_map_2")
             
             #static map output (uncheck to see)
             # plotOutput(outputId = "water_map_static")
           )),
  tabPanel("Background",
           mainPanel(width = 20,
             htmlOutput("tab2")
           )),
  tabPanel("Data",
           mainPanel(width = 20,
             htmlOutput("tab3")
           )),
  tabPanel("App Developers",
           mainPanel(width = 20,
             htmlOutput("tab4")
           ))
  
  )


### Server Interface
server <- function(input, output) {
  # reactive map code below 
  # reactive map dataframe
  water_reactive <- reactive({
    water_total %>% 
      select(supplier_name, `Population Served`) %>% 
      filter(supplier_name == input$supplier_select)
  })
  
  # reactive map
  output$water_map <- renderLeaflet({
    
    # tmap_mode("view")
    map <- tm_basemap("CartoDB.Positron") +
      tm_shape(water_reactive()) +
      tm_fill("Population Served", alpha = 0.8, colour = "blue", legend.show = FALSE) +
      tm_polygons("supplier_name", id = "Population Served") 
      
    tmap_leaflet(map)
    
  })
  
  # reactive table code below
  # reactive table dataframe
  datetable <- reactive({
    water_merged %>%
      filter(yy_mm_dd == input$date_select, hydrologic_region == input$hydrologic_region) %>%
      group_by(hydrologic_region) %>%
      summarize(
        tot_complaints = sum(complaints_received),
        follow_ups = sum(follow_up_actions),
        tot_warnings = sum(warnings_issued)
      )
  })
  
  # reactive table
  output$datetable <- render_gt({
    
     datetable() %>%
      gt() %>%
      tab_header(
        title = "Total Reports, Follow Ups, Warnings Issued"
      ) %>%
      tab_options(
        table.width = pct(100)
      ) %>%
      tab_footnote(
        footnote = "By Hydrologic Region from 2015 - 2019",
        location = cells_title()
      ) %>%
      tab_style(
        cell_borders(
          sides = "all",
          color = "black",
          style = "solid",
          weight = px(2)
        ),
        locations = list(
          cells_body(
          columns = everything(),
          row = everything()
        )
      )) %>% 
      cols_label(
        hydrologic_region = md("**Hydrologic Region**"),
        tot_complaints = md("**Total Complaints**"),
        follow_ups = md("**Follow Ups**"),
        tot_warnings = md("**Total Warnings**")
      ) %>% 
      cols_align(
        align = c("center"), columns = TRUE
        )
  })
  
  
  # reactive plot for per capita usage code below
  # reactive plot for per capita usage dataframe
  per_capita_use_reactive <- reactive({
    water_merged %>% 
      select(supplier_name, 
             yy_mm_dd, 
             reported_residential_gallons_per_capita_day_r_gpcd_starting_in_september_2014) %>% 
      filter(supplier_name == input$supplier_select)
  })
  
  # reactive plot reactive plot for per capita usage

  output$per_capita_use <- renderPlot({
    ggplot(data = per_capita_use_reactive(), 
           aes(x = yy_mm_dd, 
               y = reported_residential_gallons_per_capita_day_r_gpcd_starting_in_september_2014)) +
      geom_col(color = "blue", 
               fill = "blue", 
               alpha = 0.5) +
      # geom_line(color = "firebrick") +
      #theme(axis.text.x = element_text(angle = 90, 
                                      # hjust = 1, 
                                      # vjust = 0.5)) +
      theme_classic() +
      theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 15))) +
      theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 15, l = 0))) +
      labs(title = "",
           x = "Month and Year",
           y = "Gallons Per Capita Day Water Use") 
  })
  
  # hydrologic region map code below
  # dataframe for hydrologic region
  hydro_reactive <- reactive({
    hydrologic_spatial %>% 
      select(hr_name) %>% 
      filter(hr_name == input$hydrologic_region)
    })
  
  # reactive map for hydrologic region
  
   output$water_map_2 <- renderLeaflet({
    
     tmap_mode("view")
    map <- tm_basemap("CartoDB.Positron") +
      tm_shape(hydro_reactive()) +
      tm_fill("hr_name", alpha = 0.5, legend.show = FALSE) +
      tm_polygons("hr_name", id = "hr_name", alpha = 0.1) 
    
    
    tmap_leaflet(map)
    
  })
  # static map for hydrologic region
  
  # output$water_map_static <- renderPlot({
    
    #ggplot(hydro_reactive()) +
     # geom_sf(data = ca_counties,
      #        size = 0.1,
       #       color = "black") +
      #geom_sf(aes(fill = hr_name),
       #       alpha = 0.4) +
      #theme_bw()
    
  #})

  
  # text for tabs
  {
    output$tab1 <- renderText({
      "<br>
      <p> 
      Understanding residential water use in California is vital to understand the sustainable management of water. This application uses reported water conservation data to visualize residential water use and behavior from April 2015 to September 2019.<br>
      <br>
      <b>Water Suppliers:</b>
      <br>
      <img src='pic9.png', height='300px', style='float:right'/>
      From the dropdown menu, choose a supplier from the 406 suppliers listed to plot the service area of the supplier on the interactive map. Click on the service area polygon to view population served of the supplier. Additionally, visualize the residential gallons per capita day water use for each water supplier from April 2015 to September 2019.<br>
      <br>
      <b>Hydrologic Regions:</b>
      <br>
      Select one or more hydrologic regions in California to view the total annual amount of: water waste complaints or violation of conservation rules received by suppliers, follow-up activities by the suppliers to the water waste complaints or violation of conservation rules, and warnings issued by the suppliers to residential customers. The static map displays where each hydrologic region is located in the state.
      </p>
      <br>
      <b>Background:</b>
      <br>
      Due to expected frequent and prolonged droughts, California continues to develop policy to sustainably manage waters of the state. Read about the policy (and inspiration) behind the creation of this application. 
      </p>
      <b>Data and App Developers:</b>
      <br>
      Learn about the source of data used to create this application and the team behind the screen.
      </p>
      <br>"
    })
    output$tab2 <- renderText({
      "<h1>Background</h1>
      <br>
      <center><img src='pic2.jpg', height='450px', style=''/></center>
      <br>
      From 2011 to 2017, California experienced an intense, historical drought, which eventually imposed an executive order of a 25 percent reduction in statewide water usage (Executive Order B-29-2015). In 2014, the California State Water Resources Control Board introduced regulation for drought emergency water conservation. Water suppliers with more than 3,000 connections were required to submit monthly reports to the State Water Board that included residential gallons-used-per-capita, number of water days allowed, and whether the supplier was under mandatory restrictions. After November 2017, reporting to the State Water Board became voluntary. However, shortly after in May 2018, Governor Jerry Brown signed Senate Bill 606 and Assembly Bill 1668, which reauthorized the mandatory reporting. 
<br>
<br>
Throughout the drought, many efforts were made to get residents to reduce their water usage. Since declaring the end of the drought in April 2017, residential water use per capita has on average become stagnant. To promote and encourage a more water saving conscious lifestyle, Governor Jerry Brown continued the monitoring of wasteful water use practice through Executive Order B-40-17. Monitoring was first introduced in the drought emergency water conservation regulation in July 2014. The follow activities are prohibited: 
<br>
<br>
- Using potable water to wash sidewalks and driveways<br>
- Allowing more than incidental runoff when irrigating turf and other ornamental landscapes<br>
- Using hoses without automatic shutoff nozzles to wash motor vehicles<br>
- Using potable water in ornamental fountains or decorative water features that do not recirculate the water <br>
- Irrigating turf and ornamental landscape during and within 48 hours following measurable rainfall <br>
- Hotels and motels laundering towels and linens daily without providing guests the option of using them again <br>
- During a drought emergency, the serving of drinking water in restaurants and bars without it being requested <br>
- As of January 1, 2025, irrigating turf on public street medians and parkways unless the turf   serves a community recreational or civic function, the turf is irrigated incidentally with     trees, or the turf is watered with recycled water by an irrigation system installed prior to   January 1, 2018.
<br>

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
    output$tab3 <- renderText({
      "<h1>Data</h1>
      <br>
      <center><img src='pic1.jpg', height='450px', style=''/></center>
      <br>
      <p>The water conservation data used for this application is from the California State Water Resources Control Board and the spatial data is from the California Department of Water Resources. The California State Water Resources Control Board collected data from April 2015 to November 2019 from 409 municipalities. All conversions for the conservation data were completed by the State Water Board. The methodology can be viewed
<a href='https://www.waterboards.ca.gov/waterrights/water_issues/programs/drought/docs/ws_tools/guidance_estimate_res_gpcd.pdf' onclick='detect_click(this)'>here.</a></p>
<br>
<b>Raw datasets and metadata:</b> 
<img src='pic7.jpg', height='150px', style='float:right'/>
<img src='dwr.png', height='150px', style='float:right'/>
<br>
- <a href='https://www.waterboards.ca.gov/water_issues/programs/conservation_portal/conservation_reporting.html' onclick='detect_click(this)'>Water Conservation</a>
<br>
- <a href='https://www.waterboards.ca.gov/water_issues/programs/conservation_portal/docs/2019jul/uw_supplier_data_definitions.pdf' onclick='detect_click(this)'>Metadata (Water Conservation)</a>
<br>
- <a href='http://atlas-dwr.opendata.arcgis.com/datasets/45d26a15b96346f1816d8fe187f8570d_0' onclick='detect_click(this)'>Water Districts (Spatial)</a>
<br>
- <a href='http://atlas-dwr.opendata.arcgis.com/datasets/2a572a181e094020bdaeb5203162de15_0'>Hydrological Regions (Spatial)</a>
<br>
<br>
<br>
<br>
<b>References:</b>
<br>
Gomberg, Max et al. 2019. Water Conservation and Production Reports. California State Water Resources Control Board. 
<br>
<a href='https://www.waterboards.ca.gov/water_issues/programs/conservation_portal/conservation_reporting.html' onclick='detect_click(this)'>Website</a>
<br>
Department of Water Resources Atlas. 2019. Water Districts. CA Department of Water Resources GIS Data. 
<br>
<a href='http://atlas-dwr.opendata.arcgis.com/datasets/45d26a15b96346f1816d8fe187f8570d_0' onclick='detect_click(this)'>Website</a>
<br>
<br>
<b>Photo Credit:</b>
<br>
Patrick T. Fallon.
<a href='https://www.gettyimages.com/photos/patrick-t.-fallon?mediatype=photography&phrase=patrick%20t.%20fallon&sort=mostpopular'>Getty Images.</a>
<br>
Allison Horst. UCSB Bren School.
<br>
<br>
<br>
  " })
    output$tab4 <- renderText({"
    <h1>App Developers  <img src='206.png', height='55px', style=''/><img src='244.png', height='60px', style=''/></h1>
    <br>
    <b>Claudia Flores</b> <br> <img src='claudia.jpg', height='300px', style='float:right'/> 
    <p>Claudia graduated from the University of California, Los Angeles (UCLA) with a Bachelor of Science in Environmental Science and a concentration in Systems and Societies in 2017. As an undergraduate, Claudia interned with GlobeMed and Mpoma Community HIV/AIDS Initiative in Uganda to work on water access, sanitation, and hygiene projects. Claudia’s work in Uganda allowed her to develop an interest in policy and water resources. To continue to pursue her interests, Claudia worked with other organizations such as Los Angeles Waterkeeper, Heal the Bay’s Santa Monica Pier Aquarium, and the Southern California Wetlands Recovery Project. Upon graduating, she was a researcher for the Water Program at the UCLA Luskin Center for Innovation. Claudia worked on developing scenarios for a statewide drinking water assistance program for low-income households. After the Luskin Center, she worked at the Environmental Finance Center at the University of North Carolina, Chapel Hill, where she evaluated criteria required to receive principal forgiveness for the Georgia State Revolving Fund programs. At the Bren School, Claudia plans to find innovative and economically feasible solutions for water infrastructure maintenance and sustainable practices in water resource management.</p>
    <br>
    <br>
<b>Alex Milward</b> <br> <img src='alex.jpg', height='300px', style='float:right'/>
      <p>Alex Milward grew up in a small, rural town on the west side of California’s Sierra Nevada Mountains.  Here he enjoyed an immersion in nature and developed a passion for the natural world.  However, he was also witness to the environmental degradation that often accompanies an area rich in natural resources such as timber and water.  This led to a desire to pursue a career in environmental science and sustainability. Alex carried this passion to his undergraduate studies at the University of California, Santa Barbara, where he received a Bachelor of Science in Environmental Studies and Physical Geography in 2018. At UCSB, Alex conducted research alongside Dr. Jennifer King on wetland habitat restoration and its capacity to sequester atmospheric gases.  With this research came a curiosity of the influence that water was has on wetland habitats and the environment at large.  At Bren, Alex is specializing in water resources management, with a focus in environmental data science.  His goal is to analyze watersheds in the American west and develop new sustainability methods in regards to water appropriation, storage, and distribution to ensure the conservation of such a critical resource.</p>
      <br>
      <br>
<b>Derek Nguyen</b> <br> <img src='derek.jpg', height='300px', style='float:right'/>
      <p>Derek Nguyen graduated from the University of California, Santa Barbara in 2019 with a Bachelor of Arts in Environmental Studies. During his undergraduate career, Derek conducted research on dry groundwater wells in the Western 17 states. His research helped to measure other locations potentially experiencing impacts from groundwater depletion. Derek also served as the Food Recovery Project Coordinator of UCSB’s Sustainability Program where he developed and implemented food recovery programs on-campus to reduce food waste and food insecurity within the student body. Derek is currently an intern at the Goleta Water District where he assists the Engineering Department in collecting and analyzing water quality samples from various water zones within the district. Derek wants to specialize in Water Resources Management with afocus in Environmental Data Science to learn more about water use efficiency and demand management. He aspires to promote equity and resiliency of water systems especially in underserved communities.</p>
      <br>
      <br>
      "})
  }
  
}

shinyApp(ui = ui, server = server)