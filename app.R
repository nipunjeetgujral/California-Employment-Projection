# library ####
library(tidyverse)
library(plotly)
library(shiny)
library(shinydashboard)
library(readxl)
library(leaflet)


# data import and cleaing ####

# Population Growth
Population_Growth <- readr::read_csv("./Data/gender_per_county.csv")

# Revenue 
Revenue <- readxl::read_xlsx("./Data/population_breakdown.xlsx", sheet = "Revenue (2017-18)")

# Expenditures
Expenditures <- readxl::read_xlsx("./Data/population_breakdown.xlsx", sheet = "Expenditures (2017-18)")

# Employees
Housing <- readxl::read_xlsx("./Data/population_breakdown.xlsx", sheet = "County Profile")

# Map
Map <- rgdal::readOGR("./Data/California Counties.geojson")

# data for map
ordered_county_names <- c("Del Norte", "Siskiyou", "Modoc", "Humboldt", "Trinity",
                          "Shasta", "Lassen", "Tehama", "Plumas", "Butte", "Mendocino",
                          "Glenn", "Sierra", "Yuba", "Lake", "Nevada", "Colusa", "Sutter",
                          "Placer", "El Dorado", "Yolo", "Alpine", "Sonoma", "Napa",
                          "Sacramento", "Mono", "Amador", "Solano", "Calaveras", 
                          "Tuolumne", "Marin", "San Joaquin", "Contra Costa", "Stanislaus",
                          "Alameda", "Mariposa", "San Francisco", "Madera", "San Mateo",
                          "Merced", "Fresno", "Santa Clara", "Inyo", "Santa Cruz",
                          "San Benito", "Monterey", "Tulare", "Kings", "San Bernardino",
                          "Kern", "San Luis Obispo", "Santa Barbara", "Los Angeles",
                          "Riverside", "Orange", "Imperial", "San Diego", "Ventura")


california_2019 <- readxl::read_xlsx("./Data/population_breakdown.xlsx", sheet = "People") %>% 
  dplyr::rename("Population" = "Population (January 2019)") %>% 
  dplyr::mutate(Log_pop = log10(Population),
                County = factor(County, levels = ordered_county_names)) %>% 
  dplyr::arrange(County)


map_information <- california_2019 %>% dplyr::select("County", "Population", "Log_pop")


# ui ####
ui <- shinydashboard::dashboardPage(

  
  # title ####
  shinydashboard::dashboardHeader(
    title = "California 2019",
    titleWidth = 300
  ),

  # side menu ####
  shinydashboard::dashboardSidebar(disable = TRUE),

  # boddy ####
  shinydashboard::dashboardBody(
    
    tags$head(
      tags$style(
        HTML('
        /*
          failed colors
        */
        
        /*
          approved colors
          . 8C8C8C
        */
        /* body 495D60 8C8C8C*/
        .content-wrapper, .right-side {background-color: #ffffff;}'
        )
      )
    ),
    
    # standard stats ####
    
    shiny::fluidRow(
      
      # Size 
      shinydashboard::infoBox(
        shiny::h3("Size"),
        HTML(paste0("163,696 miles", tags$sup("2"))),
        icon = icon("globe"),
        color = "orange",
        fill = TRUE,
        width = 3
      ),
      
      # Population
      shinydashboard::infoBox(
        shiny::h3("Population"),
        paste(
          base::sprintf(format(39542770,
                               format = "d",
                               big.mark = ",")),
                      "Individuals"),
        icon = icon("users"), 
        color = "orange",
        fill = TRUE,
        width = 3
      ),
      
      # Average Gas Price
      shinydashboard::infoBox(
        shiny::h3("Average Gas Price"),
        "$3.79 / Gallon",
        icon = icon("gas-pump"), 
        color = "orange",
        fill = TRUE,
        width = 3
      ),
      
      # Members of Congress
      shinydashboard::infoBox(
        shiny::h3("Members of Congress"),
        paste("52 Representatives"),
        icon = icon("landmark"), 
        color = "orange",
        fill = TRUE,
        width = 3
      )
      
    ),

  
    # plots ####
    shiny::fluidPage(
      # map ####
      
      # background color
      tags$head(tags$style(HTML(".leaflet-container { background: #fff; }"))),
      
      # map object
      shiny::column(
        7,
        shinydashboard::box(shiny::column(4, 
                                          offset = 4, 
                                          align = "center",
                                          shiny::selectInput(inputId = "county",
                                                             label = shiny::h4(shiny::strong("Select a County")),
                                                             choices = c("None", sort(ordered_county_names))),
                                          shiny::textOutput(outputId = "OUTPUT_TEXT")),
                            shiny::textOutput(outputId = "county_output"),
                            leaflet::leafletOutput(outputId = "plot1", height=1250),
                            width = 14, 
                            height = 1425,
                            title = "Map",
                            solidHeader = TRUE, 
                            collapsible = FALSE,
                            status = "primary"),
      ),
      
      # population statistics ####
      shiny::column(
        5,
        shinydashboard::box(
          #tags$style(HTML(".box.box-solid.box-primary{background:#000000}")),
          plotlyOutput(outputId = "plot2"),
          title = "Revenue",
          width = 12, 
          solidHeader = TRUE, 
          collapsible = TRUE,
          status = "primary"),
        shinydashboard::box(
          #tags$style(HTML(".box.box-solid.box-primary{background:#000000}")),
          plotlyOutput(outputId = "plot3"), 
          title = "Expenditures", 
          width = 12, 
          solidHeader = TRUE, 
          collapsible = TRUE,
          status = "primary"),
        shinydashboard::box(
          #tags$style(HTML(".box.box-solid.box-primary{background:#000000}")),
          plotlyOutput(outputId = "plot4"), 
          title = "Industry",
          width = 12, 
          solidHeader = TRUE,
          collapsible = TRUE,
          status = "primary")
      ),
      
      # forcasts ####
      fluidRow(
        shinydashboard::box(plotlyOutput(outputId = "plot5"), 
                            width  = 6,
                            title = "Forecasted Population Growth", solidHeader = TRUE, collapsible = TRUE,
                            status = "primary"),
        shinydashboard::box(plotlyOutput(outputId = "plot6"), 
                            width  = 6,
                            title = "Unemployment", solidHeader = TRUE, collapsible = TRUE,
                            status = "primary"),
        
      )
    )
  )
)
  

# server ####
server <- function(input, output, ...){
  
  # County map - leaflet - output #
  output$plot1 <- leaflet::renderLeaflet({
    
    labels <- base::sprintf("<strong> <h2> %s </h2> </strong>
                        
                            <h3>Population: %s</h3>
                            <h3>Total Revenue: </h3>
                            <h3>Size: </h3>
                            <h3>GDP: </h3>",
                            Map_Information$County,
                            format(Map_Information$Population, 
                                   format = "d", 
                                   big.mark = ",")) %>% 
      lapply(htmltools::HTML)
    
    
    highlight_options <- leaflet::highlightOptions(weight = 5,
                                                   color = "#8C8C8C",
                                                   dashArray = "",
                                                   fillOpacity = 0.7,
                                                   bringToFront = TRUE)
    
    pal <- leaflet::colorBin("Blues",
                             domain = Map_Information$Log_pop,
                             bins = c(Inf, seq(7, 3, -0.25), -Inf))
    
    pal_visual <- leaflet::colorBin("Blues",
                                    domain = Map_Information$Population,
                                    bins = c(11000000,
                                             seq(max(signif(Map_Information$Population, 2)),
                                                 min(signif(Map_Information$Population, 2)),
                                                 -1000000),
                                             0))
    
    Map %>% 
      leaflet::leaflet() %>% 
      leaflet::setView(lat = 37.501684, 
                       lng = -119.238017, 
                       zoom = 7) %>%
      leaflet::addPolygons(fillColor = ~pal(Map_Information$Log_pop),
                           weight = 2,
                           opacity = 1,
                           color = "lightgrey",
                           dashArray = "1",
                           fillOpacity = 0.7,
                           highlight = highlight_options,
                           label = labels) %>% 
      leaflet::addLegend(pal = pal_visual, 
                         values = Map_Information$Population,
                         opacity = 0.7, 
                         title = NULL,
                         position = "topright")
      
    })
  
  # Revenue - pie chart - output #
  output$plot2 <- plotly::renderPlotly({
    Revenue_Display(Revenue, input$county) 
  })
  
  # Expenditures - pie chart - output #
  output$plot3 <- plotly::renderPlotly({
    Expenditures_Display(Expenditures, input$county)
  })
  
  # Industry Display - pie chart - output ####
  output$plot4 <- renderPlotly({
    Industry_Display(Industry, input$county)
  })
  
  # Forecasted Population Growth - line plot - output #
  output$plot5 <- plotly::renderPlotly({
    Population_Growth_Display(Population_Growth, input$county)
  })
  
  # Recorded Unemployment - line plot - output #
  output$plot6 <- renderPlotly({
    Unemployment_Display(Unemployment, input$county)
  })
  
}

# output ####
shiny::shinyApp(ui = ui, server = server)


