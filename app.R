## Load R Packages
#Shiny-related
library(shiny)
library(dashboardthemes)
library(shinythemes)
library(shinydashboard)
library(DT)
#GIS
library(leaflet)
library(spatialEco)
library(sp)
library(raster)
#Data
library(dplyr)
library(reshape)
library(jsonlite)
library(tidyverse)


## Project variables
read_json("db_vars.json") %>%
  attach()


## PYTHON VIRTUAL ENVIRONMENT SETTINGS
python_packages <- c("pip==20.2.1","pymongo","pandas")
reticulate::virtualenv_create(envname = "python_environment", python = "python3")
reticulate::virtualenv_remove(envname = "python_environment", packages = "pip")
reticulate::virtualenv_install(envname = "python_environment",
                               packages = python_packages,
                               ignore_installed = TRUE)
reticulate::use_virtualenv("python_environment", required = TRUE)


## Session settings (helper functions, etc.)
source("R/session_settings.R")


## Create body of dashboard page
ui <-
  fluidPage(
    title = "Augury",
    tags$head(tags$style(css)),
    theme = shinytheme("cosmo"),
    fluidRow(column(12, align = "center",
                    h1(
                      HTML("<b><u>Augury: Dashboard for Visualizing Local eBird Records</u></b>")
                    )
                  )
                ),
    fluidRow(
      column(2, align = "left",
        div(style = "margin-left:20%; margin-right:0%; margin-top:12%;text-align:center",
            selectInput("collection",
                        label = "Location",
                        choices = names(MONGODB$COLLECTION)),
            sliderInput(
              inputId="radius",
              label="Search Radius (km)",
              min = 5,
              max = 50,
              step = 5,
              value = 50
            ),
            selectInput(
              inputId="species",
              label="Species",
              choices = "",
              selectize = TRUE,
              multiple = TRUE, 
              width = "100%"#,
              #options = list(maxItems=5)  # select up to 5 species
            ),
            selectInput("start_date", "Start Date.", choices = ""),
            selectInput("end_date", "End Date", choices = ""),
            checkboxInput("kde", label = "Show Record Hotspots?", value = FALSE),
            actionButton("find", "Show Records", width = "100%")
          
        )
      ),
      column(8, align = "center",
        div(style = "margin-left:5%; margin-top:3.5%;", #margin-right:25%",
          div(style="border:2px groove black; overflow:hidden", 
            fluidRow(leafletOutput("map"))
          ),
          br(),
          div(style="border:2px groove black; overflow:hidden",
            fluidRow(plotOutput("plot", height = "250px"))
          )
        )
      ),
      column(2, align = "center",
             div(style = "margin-left:2%; margin-top:12%",
          fluidRow(tableOutput("table")),
             )
      )  # end column
    ) # end fluidRow
  )
        
## Server logic
server = function(input, output, session) {
  
  # Populate species menu with available options
  distinctSpecies <- reactive({
    augury <- mongodb_client()
    d <- augury$distinct_species(MONGODB$COLLECTION[[input$collection]],
                                 input$radius/87,
                                 LOCATION[[input$collection]])
    sprintf("%s unique species found.", nrow(d))
    return(d)
  })
  
  observe ({
    # Get data
    data <- distinctSpecies()
    # Populate species selection dropdown menu
    menuOptions <- data$Species %>% sort()
    # Update menu options
    updateSelectizeInput(session, "species",
                         selected = "",
                         choices = c("Select all", menuOptions)
                         )
  })
  
  # Display data ranges, depending on the selected species
  observeEvent(input$species, {
    if("Select all" %in% input$species){
      speciesList <- distinctSpecies()$Species %>% as.list()
    }
    else{
      speciesList <- input$species %>% as.list()
    }
    augury <- mongodb_client()
    dates <-
      augury$date_range(MONGODB$COLLECTION[[input$collection]],
                        speciesList,
                        LOCATION[[input$collection]],
                        input$radius/87) %>%
      lapply(., as.Date)
    
    dateSequence <- seq(dates[[1]], dates[[2]], 1)
    updateSelectInput(session,
                      "start_date",
                      choices = dateSequence,
                      selected = dateSequence[1])
    updateSelectInput(session,
                      "end_date",
                      choices = rev(dateSequence),
                      selected = rev(dateSequence)[1])
    
  })
  
  map_fun <- eventReactive(input$find, {

    # If temporary data file already exists, remove it
    if(file.exists("temp_data.rds")){
      file.remove("temp_data.rds")
    }
    
    if("Select all" %in% input$species){
      speciesList <- distinctSpecies()$Species %>% as.list()
    }
    else{
      speciesList <- input$species %>% as.list()
    }

    augury <- mongodb_client()
    plotdata <- augury$spatial_query(
      COLLECTION = MONGODB$COLLECTION[[input$collection]],
      PROXIMITY = input$radius/87,
      species = speciesList,
      LOCATION = LOCATION[[input$collection]],
      MIN_DATE = strftime(input$start_date,
                          format = "%Y-%m-%d 00:00", usetz = FALSE),
      MAX_DATE = strftime(input$end_date,
                          format = "%Y-%m-%d 23:59", usetz = FALSE)
    ) %>%
    #st_as_sf(coords = c("lng","lat"), crs=4326) %>%
    #st_jitter(factor = 0.005) %>%
    mutate(howMany = replace_na(howMany, 1)) %>%
    mutate(weight = log(howMany) + 3)
    
    ## Create a map
    # Data points color palette
    pal <- colorFactor(
      palette = 'RdYlBu',
      domain = plotdata$comName
    )
    # Data points labels
    label <- create_labels(plotdata)
    plotdata$label <- label
    # Coordinates to center map
    CRDS = LOCATION[[input$collection]]
    # Leaflet plot
    map <- leaflet(plotdata) %>% 
      setView(lng = CRDS$LONGITUDE, lat = CRDS$LATITUDE, zoom = 9) %>% 
      #addTiles() %>%
      addProviderTiles(providers$Stamen.Terrain) %>%
      addCircleMarkers(lng = ~lng, lat = ~lat,
                       weight = ~weight,
                       col = "gray50",
                       fillColor = ~pal(comName),
                       radius = ~weight,
                       fillOpacity = 0.85,
                       stroke = TRUE,
                       label = lapply(label, HTML),
                       group = 'Points')
    # Optional: add kernel density of all database records
    if(input$kde){
      r <- kde_raster(MONGODB$COLLECTION[[input$collection]])
      # c("#0C2C84", "#41B6C4", "#FFFFCC")
      pal <- colorNumeric(palette = viridis::viridis(10, option = "C", begin = 0.05), 
                          values(r),
                          na.color = "transparent")
      r[r<0.005] <- NA
      map <- map %>%
        addRasterImage(r, colors = pal, opacity = 0.7)
      
    }
    # Save temp. plotting data for other functions
    saveRDS(plotdata, "temp_data.rds")
    return(map)
    
  })
  
  plot_fun <- eventReactive(map_fun(), {
    
    # Read in temp data
    plotData <- readRDS("temp_data.rds") %>%
      mutate(Date = as.Date(obsDt))
    dateRange = with(plotData, seq(min(Date),max(Date), 1)) %>% 
      as.character()
    plotData <- plotData %>%
      mutate(Date=as.character(Date)) %>%
      mutate(Date=factor(Date, dateRange))
    plotTable <- with(plotData, table(Date, comName)) %>%
      melt() %>%
      mutate(Date=as.Date(Date))
    plt <- ggplot(plotTable) +
      ggtitle("Species Records Over Time") +
      # stat_identity(aes(x=Date, y=value, group=comName, fill=comName), geom="bar", beside=TRUE) +
      geom_bar(aes(x=Date, y=value, group=comName, fill=comName), stat="identity") +
      theme_gray() +
      plot_theme +
      theme(
            legend.position = "bottom", 
            plot.margin = unit(c(1, 1, 0.2, 1), "cm")
            ) +
      scale_x_date("") +
      scale_y_continuous("Number of Records",
                         limits = c(0, NA),
                         breaks = seq(0, max(plotTable$value))) +
      scale_fill_brewer(palette = "RdYlBu")
    
    return(plt)
    
  })
  
  table_fun <- eventReactive(map_fun(), {
    plotData <- readRDS("temp_data.rds")
    tab <- table(plotData$comName) %>%
      as.data.frame()
    names(tab) <- c("Species", "Records")
    return(tab)
  })

  # Periodically update the MongoDB database with new records
  timelapse = 1000*60*4 # 4 hours in milliseconds
  update_records <- reactivePoll(
    timelapse,
    session,
    # This function returns the time that log_file was last modified
    checkFunc = function() {
      Sys.time()
    },
    # This function returns the content of log_file
    valueFunc = function() {
      # Run database update script
      reticulate::py_run_file("Python/update_database.py")
    }
  )
  
  output$map <- renderLeaflet(map_fun())
  output$plot <- renderPlot(plot_fun())
  output$table <- renderTable(table_fun())
}

shinyApp(ui = ui, server = server)
