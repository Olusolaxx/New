## app.R ##

## Load packages

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(readr)
library(leaflet)

marine <- "ships.csv"
mdata <- read_csv(marine, col_types = cols())

# Count all ship types
all_count <- mdata %>% select(ship_type) %>% count(ship_type, sort = TRUE) %>% group_by(ship_type)


# Sampling of data
set.seed(1375)
mysample <- sample_n(mdata, size = 60)

# Sample ship_type count
type_count <- mysample %>% select(ship_type) %>% count(ship_type, sort = TRUE) %>% group_by(ship_type)

# Map data
mysample <- mysample %>% mutate(DISTANCE = SPEED * ELAPSED)
map_data <- mysample %>% filter(!is.na(LAT), !is.na(LON), !is.na(COURSE), !is.na(HEADING), 
                                !is.na(SHIPNAME), !is.na(DATETIME), !is.na(ship_type), 
                                !is.na(DISTANCE)) %>% 
    select(LAT, LON, COURSE, HEADING, SHIPNAME, DATETIME, ship_type, DISTANCE)


#Define ui
ui <- dashboardPage(
    
    dashboardHeader(title = "Maritime dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Visuals", tabName = "visuals", icon = icon("chart-bar")),
            menuItem("Geospatial", tabName = "geospatial", icon = icon("digital-ocean"))
        )
    ),
    dashboardBody(
        tabItems(
            # First tab
            tabItem(tabName = "visuals",
                    # Boxes for graphics
                    fluidRow(
                        box(
                            title = "Chart", background = "maroon", solidHeader = TRUE,
                            plotOutput("plot", height = 300)),
                        
                        box(width = 5, background = "aqua", solidHeader = TRUE,
                            title = "Data choice",
                            selectInput("data", "", choices = c("type_plot", "all_plot"), selected = 
                                            "type_plot")
                            
                        )
                    ),
                    
                    dataTableOutput("MaritimeTable")
            ),
            # Second tab
            tabItem(tabName = "geospatial",
                    h2("Maritime graph"),
                    
                    # Boxes for list menu
                    fluidRow(
                      column(10,
                        box(
                          title = "Name of ship", background = "olive", solidHeader = TRUE,
                          selectInput(inputId = "ship_name",
                                      label = "Choose ship",
                                      selected = NULL,
                                      choices = unique(mysample$SHIPNAME)),
                      ),
                      column(12,
                        box(width = 10,
                          title = "Type of ship", background = "navy", solidHeader = TRUE,
                          selectInput(inputId = "shipType",
                                      label = "Choose type",
                                      selected = "All",
                                      choices = c("All", "Cargo", "Tanker", "Tug",
                                                  "Unspecified", "Fishing", "Pleasure", "Passenger"))),
                      ),
                        
                            
                               
                            #uiOutput("ship"),
                           
                            
                        )
                    ),
                    fluidRow(
                        box(width = 9, background = "fuchsia", solidHeader = TRUE,
                            leafletOutput("map", width = "100%")
                        )
                    ),
                    
                    dataTableOutput("GeospatialTable")
            )
        )
        
    )
  )




server <- function(input, output){ 
    
    output$plot <- renderPlot({
        if(input$data == "type_plot"){ 
            
            # Plot sample ship_type
            bplot <- ggplot(type_count, aes(ship_type, n, fill = ship_type)) + geom_col() +
                geom_text(aes(label = n, vjust = -0.5)) +
                theme_minimal() +
                theme(legend.position = "none", axis.title.y = element_text(angle = 0)) +
                labs(title = "Top vessel types")
        }
        
        
        if(input$data == "all_plot"){
            
            # All types plot
            bplot <- ggplot(all_count, aes(ship_type, n, fill = ship_type)) + geom_col() +
                geom_text(aes(label = n, vjust = -0.5)) +
                theme_minimal() +
                theme(legend.position = "none", axis.title.y = element_text(angle = 0)) +
                labs(title = "All vessel types")
            
        }
        
        bplot
        
    })
    
    output$MaritimeTable <- renderDataTable(mysample)
   
    
    output$map <- renderLeaflet({
        map_labels <- paste0(sep = "<br/>", "Name of ship:", map_data$SHIPNAME,
                             "<br/>", "Ship type:", map_data$ship_type,
                             "<br/>", "Distance:", map_data$DISTANCE)
        map <- leaflet(map_data, height = 2000, width = 2000) %>% addTiles() %>% 
            addMarkers(~ LON, ~ LAT, clusterOptions = markerClusterOptions(), popup = ~ map_labels)
    })
    
    
    
    output$GeospatialTable <- renderDataTable(map_data)
    
}


shinyApp(ui, server)