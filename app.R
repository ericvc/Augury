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
library(MODISTools)
library(sf)
#Data
library(ggplot2)
library(dplyr)
library(reshape)
library(jsonlite)
library(tidyverse)


## Project variables
db_vars <- read_json("db_vars.json") %>%
  attach()

saveRDS(data.frame(), "temp_data.rds")
saveRDS(data.frame(), "raster_data.rds")


## PYTHON VIRTUAL ENVIRONMENT SETTINGS
# python_packages <- c("pip==20.2.1","pymongo","pandas")
# reticulate::virtualenv_create(envname = "python_environment", python = "python3")
# reticulate::virtualenv_remove(envname = "python_environment", packages = "pip")
# reticulate::virtualenv_install(envname = "python_environment",
#                                packages = python_packages,
#                                ignore_installed = TRUE)
# reticulate::use_virtualenv("python_environment", required = TRUE)


## Get WorldClim (bio) variables, if not present in local directory
if(!dir.exists("WorldClim_Data")){
  dir.create("WorldClim_Data")
  getData('worldclim', var='bio', res=2.5, path = "WorldClim_Data/") #123 MB
}


## MODIS Remote Sening Data Menu Options
products <- mt_products()


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
                    ),
                    HTML("<p style='font-size:14px'>Add data to the map using options on the sidebar, including eBird observation records and geospatial layers from MODIS satellites or WorldClim climatic records.</p>")
                  )
                ),
    fluidRow(
      # Input options
      column(2, align = "left",
        div(style = "margin-left:20%; margin-right:0%; margin-top:0%;text-align:center",
            selectInput("collection",
                        label = HTML("<h5><b>1) Location</b></h5>"),
                        choices = names(MONGODB$COLLECTION)),
            HTML("<hr>"),
            HTML("<h5><b>2) Find Data</b></h5>"),
            tabsetPanel(type = "tabs",
              # Select bird observations records
              tabPanel(h6("eBird Obs."),
                br(),
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
                checkboxInput("kde", label = "Show Record Hotspots?", value = FALSE),
                #actionButton("find", "Show Records", width = "80%"),
                actionButton(inputId = "reset", label = "Reset Fields", width = "80%")
              ),
              # Select remote sensing data products
              tabPanel(h6("GIS Layers"),
                HTML("<h5>"),
                selectInput(inputId = "dataset", label = "Data Set", choices = c("","BioClim","MODIS")),
                selectInput(inputId = "product", label = "MODIS Product", choices = c("",products$product)),
                uiOutput("prod__desc"),
                selectInput(inputId = "band", label = "Product Band", choices = ""),
                uiOutput("band__desc"),
                selectInput(
                  inputId = "aggfun",
                  label = "Aggregation Function",
                  choices = c("Mean", "Median", "Minimum", "Maximum", "Variance", "Mean / Std. Dev"),
                ),
                uiOutput("unit__desc"),
                actionButton(inputId = "add_layer", label = "Add Layer", width="80%")
              ) 
            ),
            # br(),
            # br(),
            HTML("<hr>"),
            HTML("<h5><b>3) Date Range</b></h5>"),
            selectInput("start_date", "Start Date", choices = ""),
            selectInput("end_date", "End Date", choices = "") # end menu options
          ) 
        ),
      # Display map, plots, table, etc. (output)
      column(8, align = "center",
        div(style = "margin-left:5%; margin-top:3.5%;", #margin-right:25%",
          div(style="min-width:auto; min-height:auto",#hidden", 
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
  
  ## Populate species menu with available options
  distinctSpecies <- reactive({
    
    augury <- mongodb_client()
    d <- augury$distinct_species(MONGODB$COLLECTION[[input$collection]],
                                 input$radius/87)
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
  
  
  ## Reset menu options action button is used
  observeEvent(input$collection, {

    # # Species
    # updateSelectizeInput(session, "species", selected = "")

    # Search Radius
    updateSliderInput(session, "radius", value = 50)

    # Start Date
    updateSelectInput(session,
                      "start_date",
                      choices = "",
                      selected = "")

    # End Date
    updateSelectInput(session,
                      "end_date",
                      choices = "",
                      selected = "")

    # Clear cache files
    saveRDS(data.frame(), "temp_data.rds")
    saveRDS(data.frame(), "raster_data.rds")

  })

  
  ## Reset menu options action button is used
  observeEvent(input$reset, {
    
    # Species    
    updateSelectizeInput(session, "species", selected = "")
    
    # Search Radius
    updateSliderInput(session, "radius", value = 50)
    
    # Start Date
    updateSelectInput(session,
                      "start_date",
                      choices = "",
                      selected = "")
    
    # End Date
    updateSelectInput(session,
                      "end_date",
                      choices = "",
                      selected = "")
    
    # Clear cache file
    saveRDS(data.frame(), "temp_data.rds")
    
    })
  
  
  ## Display data ranges, depending on the selected species
  observeEvent(input$species, {
    
    if("Select all" %in% input$species){
      speciesList <- distinctSpecies()$Species %>% sort() %>% as.list()
    }
    else{
      speciesList <- input$species %>% sort() %>% as.list()
    }
    augury <- mongodb_client()
    dates <-
      augury$date_range(MONGODB$COLLECTION[[input$collection]],
                        speciesList,
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

  
  ## Save temporary data to local storage, given input requirements
  observeEvent(input$start_date, {

    req(input$species!="", input$start_date!="", input$end_date!="")
    if(!is.null(input$species) & input$species != "" & input$start_date != ""){
      
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
        MIN_DATE = strftime(input$start_date,
                            format = "%Y-%m-%d 00:00", usetz = FALSE),
        MAX_DATE = strftime(input$end_date,
                            format = "%Y-%m-%d 23:59", usetz = FALSE)
      ) 
      
      if(nrow(plotdata) > 0){
        
        plotdata <- plotdata %>%
          mutate(howMany = replace_na(howMany, 1)) %>%
          mutate(weight = log(howMany) + 3) %>%
          arrange(comName) %>%
          st_as_sf(coords = c("lng","lat"), crs=4326, remove=FALSE) %>%
          st_jitter(., amount = 0.007)
  
        # Data points labels
        label <- create_labels(plotdata)
        plotdata$label <- label
        
        # Data points color palette
        pal <- colorFactor(
          palette = 'RdYlBu',
          ordered = TRUE,
          domain = plotdata$comName
        )
        plotdata$cols <- pal(plotdata$comName)
        
      }
    
    }
    
    else{
      plotdata <- data.frame()
    }

    # Save temp. plotting data for other functions
    saveRDS(plotdata, "temp_data.rds")

  })
  
  
  ## Save temporary MODIS product raster data to local storage
  observeEvent(input$add_layer, {
    
    # Get coordinate center of selected location
    lonlat <- LOCATION[[MONGODB$COLLECTION[[input$collection]]]]
    # Create a spatial mask for the area surrounding that location
    m <- circularPolygon(lonlat, radius = input$radius)
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    if(input$dataset=="MODIS"){
      
      progress$set(message = "Downloading remote data set(s)...", value = 0)
      progress$inc(0.5)
      
      bands <- mt_bands(input$product)
      x <- filter(bands, band==input$band)
      if(unit_desc() == "class"){
        start_date <- format(Sys.time()-(5*365*24*60*60), "%Y-%m-%d")
        end_date <- format(Sys.time(), "%Y-%m-%d")
      }
      else{
        start_date <- input$start_date
        end_date <- input$end_date
      }
      
      subset <- mt_subset(product = input$product,
                          lat = lonlat["LATITUDE"],
                          lon = lonlat["LONGITUDE"],
                          band = input$band,
                          start = start_date,
                          end = end_date,
                          km_lr = input$radius*2,
                          km_ab = input$radius*2,
                          progress = FALSE)
      progress$inc(0.67)
      progress$set(message = "Converting data to raster object...", value = 0.67)
      
      ras <- mt_to_raster(subset, reproject = TRUE)
      
      progress$inc(0.75)
      progress$set(message = "Applying spatial mask to raster data...", value = 0.75)
  
      mras <- mask(ras, m)
      
      progress$inc(1)
      progress$set(message = "Finished", value = 1)
    
    }
    
    if(input$dataset == "BioClim"){
      
      progress$set(message = "Read BioClim data from storage...", value = 0)
      progress$inc(0.5)
      
      rasterFile <- sprintf("WorldClim_Data/wc2-5/%s.bil", input$band)
      ras <- raster(rasterFile) %>%
        crop(., m)
      
      progress$inc(0.75)
      progress$set(message = "Applying spatial mask to raster data...", value = 0.75)
      
      mras <- mask(ras, m)
      
      progress$inc(1)
      progress$set(message = "Finished", value = 1)
      
    }
    
    saveRDS(mras, "raster_data.rds")
    
  })
  
  
  ## File reader reactive function
  fileReader <- reactiveFileReader(1000, session, "temp_data.rds", readRDS)
  
  ## File reader reactive function
  fileReader2 <- reactiveFileReader(1000, session, "raster_data.rds", readRDS)
  
  
  ## Create function for plotting basemap
  map_fun <- eventReactive(c(fileReader(), fileReader2(), input$kde), {
    
    # Get mapping data
    plotdata <- readRDS("temp_data.rds")
    labs <- plotdata$label
    cols <- plotdata$cols
    
    ras <- readRDS("raster_data.rds")
    
    # Coordinates for map center
    CRDS = LOCATION[[MONGODB$COLLECTION[[input$collection]]]]
    
    # Leaflet plot base map
    map <- leaflet(plotdata) %>% 
      setView(lng = CRDS$LONGITUDE, lat = CRDS$LATITUDE, zoom = 9) %>% 
      addProviderTiles(providers$Stamen.TerrainBackground)
    
    # Optional: add remote sensing data as a RasterLayer
    if(attributes(ras)$class %in% c("RasterBrick","RasterLayer") & !input$kde){
      
      if(nlayers(ras) > 1){
        
        if(input$aggfun == "Mean"){
          ras <- calc(ras, mean)
        }    
        
        if(input$aggfun == "Median"){
          ras <- calc(ras, median)
        }    
        
        if(input$aggfun == "Minimum"){
          ras <- calc(ras, min)
        }  
        
        if(input$aggfun == "Maximum"){
          ras <- calc(ras, max)
        }  
        
        if(input$aggfun == "Variance"){
          ras <- calc(ras, var)
        }
        
        if(input$aggfun == "Mean / Std. Dev"){
          funx <- function(x){ 
            if(x != 0){
              y <- mean(x) / sd(x) 
            }
            else{
              y <- NA
            }
            return(y)
          }
          ras <- calc(ras, funx)
        }
        
      }
      
      pal <- colorNumeric(palette = viridis::viridis(10, option = "C", begin = 0.05), 
                          values(ras),
                          na.color = "transparent")
      map <- map %>%
        addRasterImage(ras, colors = pal, opacity = 0.75, project = TRUE) %>%
        addLegend(pal = pal, values = values(ras), title = unit_desc(), position = "bottomright")
      
    }
    
    # Optional: add observation markers and info box
    if(!is.null(input$species) & !is.null(input$start_date) & nrow(plotdata) > 0){
      
      map <- map %>%
        addCircleMarkers(
                       weight = ~weight, 
                       color = "black",
                       fillColor = ~cols,
                       radius = ~weight,
                       fillOpacity = 1,
                       stroke = TRUE,
                       label = lapply(labs, HTML),
                       group = 'Points')
    }
    
    # Optional: add kernel density of all database records
    if(input$kde){
      r <- kde_raster(MONGODB$COLLECTION[[input$collection]])
      pal <- colorNumeric(palette = viridis::viridis(10, option = "C", begin = 0.05), 
                          values(r),
                          na.color = "transparent")
      r[r<0.001] <- NA
      map <- map %>%
        addRasterImage(r, colors = pal, opacity = 0.7)
    }
    
    # Add toner lines and labels on top of all other information
    map <- map %>%
      addProviderTiles(providers$Stamen.TonerLines,
                     options = providerTileOptions(opacity = 0.95)) %>%
      addProviderTiles(providers$Stamen.TonerLabels)

    return(map)
    
  })
  
  
  ## Reactive function plotting records over time
  plot_fun <- eventReactive({fileReader()
    fileReader2()}, {
    
    plotData <- readRDS("temp_data.rds")
    
      if(nrow(plotData) > 1){
        plotData <- plotData %>%
          mutate(Date = as.Date(obsDt))
        dateRange = with(plotData, seq(min(Date), max(Date), 1)) %>%
          as.character()
        plotData <- plotData %>%
          mutate(Date = as.character(Date)) %>%
          mutate(Date = factor(Date, dateRange))
        plotTable <- with(plotData, table(Date, comName)) %>%
          melt() %>%
          mutate(Date = as.Date(Date))

        # Data points color palette
        plt <- ggplot(plotTable) +
          ggtitle("Species Records Over Time") +
          geom_bar(aes(
            x = Date,
            y = value,
            group = comName,
            fill = comName
          ), stat = "identity") +
          theme_gray() +
          plot_theme +
          theme(legend.position = "bottom",
                plot.margin = unit(c(1, 1, 0.2, 1), "cm")) +
          scale_x_date("") +
          scale_y_continuous("Number of Records",
                             limits = c(0, NA)) +
          scale_fill_manual(values = unique(plotData$cols))
        
        return(plt)
        
      }
    
  })
  
  
  ## Records summary sidebar
  table_fun <- eventReactive(fileReader(), {
    
    plotData <- readRDS("temp_data.rds")
    if(nrow(plotData) > 0){
      tab <- table(plotData$comName) %>%
        as.data.frame()
      names(tab) <- c("Species", "Records")
      return(tab)
    }
    
  })


  ## Update data set selection menu based on input value
  observe({
    
    req(input$dataset != "")
    if(input$dataset=="MODIS"){
      req(input$product != "")
      data <- mt_bands(input$product)
    }
    if(input$dataset=="BioClim"){
      data <- read.csv("bioclim.csv")
    }
    menuOptions <- data$band
    updateSelectInput(session, "band", choices = menuOptions)
    
  })
  
  
  ## Display data set descriptions
  prod_desc <- reactive({
    req(input$product == "MODIS")
    x <- filter(products, product==input$product)
    args. <- x$description
    tag <-
      HTML(
        paste0(
          #"<b style='margin-left:1px;color:gray90;text-align:center;'><u>Description: </u></b>",
          "<p style='margin-left:1px;margin-right:1px;font-size:11px;text-align:center'><i>",
          paste0(args., collapse = ",  "),
          "</i></p>",
          collapse = ""
        )
      )
    return(tag)
  })
  
  
  ## Display MODIS band descriptions
  band_desc <- reactive({
    req(input$dataset != "")
    if(input$dataset == "MODIS"){
      req(input$product != "")
      bands <- mt_bands(input$product)
      x <- filter(bands, band==input$band)
    }
    if(input$dataset == "BioClim"){
      bands <- read.csv("bioclim.csv", header=TRUE)
      x <- filter(bands, band==input$band)
    }
    args. <- x$description
    tag <-
      HTML(
        paste0(
          #"<b style='margin-left:1px;color:gray90;text-align:center;'><u>Description: </u></b>",
          "<p style='margin-left:1px;margin-right:1px;font-size:11px;text-align:center'><i>",
          paste0(args., collapse = ",  "),
          "</i></p>",
          collapse = ""
        )
      )
    
    return(tag)
    
  })
  
  
  ## Display MODIS band units descriptions
  unit_desc <- reactive({
    
    req(input$dataset != "")
    
    if(input$dataset=="MODIS"){
      req(input$product != "")
      bands <- mt_bands(input$product)
      x <- filter(bands, band==input$band)
    }
    
    if(input$dataset=="BioClim"){
      bands <- read.csv("bioclim.csv", header=TRUE)
      x <- filter(bands, band==input$band)
    }
    args. <- x$units
    tag <-
      HTML(
        paste0(
          #"<b style='margin-left:1px;color:gray90;text-align:center;'><u>Description: </u></b>",
          "<p style='margin-left:1px;margin-right:1px;font-size:12px;text-align:center'><i>",
          paste0(args., collapse = ",  "),
          "</i></p>",
          collapse = ""
        )
      )
    
    return(tag)
    
  })
  
  
  ## Periodically update the MongoDB database with new records
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
  
  
  ## Outputs
  # Menu features
  output$prod__desc <- renderUI(prod_desc())
  output$band__desc <- renderUI(band_desc())
  output$unit__desc <- renderUI(unit_desc())
  # Visualizations
  output$map <- renderLeaflet(map_fun())
  output$plot <- renderPlot(plot_fun())
  output$table <- renderTable(table_fun())
  
}

shinyApp(ui = ui, server = server)
