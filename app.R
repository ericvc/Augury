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


##### Setup Workspace


#### Pre-configured session settings and helper functions
source("R/session_settings.R")


#### Project Variables

## Database Options
db_vars <- read_json("db_vars.json") %>%
  attach()


## MODIS Remote Sening Data Menu Options
products <- mt_products()


## Create temporary data folders
if(!dir.exists("temp")){
  dir.create("temp")
}

if(!dir.exists("temp/record_hotspots")){
  dir.create("temp/record_hotspots")
}

# Create cache files (will throw errors if non-existant)
saveRDS(data.frame(), "temp/temp_data.rds")
saveRDS(data.frame(), "temp/raster_data.rds")


## Get WorldClim (bio) variables, if not present in local directory
if(!dir.exists("GeoData")){
  
  # Create local directory
  dir.create("GeoData")
  
  # Get BIO data set (downloads to directory 'GeoData/wc0.5')
  getData('worldclim', var='bio', res=0.5, lon=-122, lat=38, path = "GeoData/") #123 MB
  
  # Get WordClim data set (downloads to directory 'GeoData/wc0.5')
  getData('worldclim', var='bio', res=0.5, lon=-122, lat=38, path = "GeoData/") #123 MB
  
}
  

#### Configure Virtual Environment Settings
# python_packages <- c("pip==20.2.1","pymongo","pandas")
# reticulate::virtualenv_create(envname = "python_environment", python = "python3")
# reticulate::virtualenv_remove(envname = "python_environment", packages = "pip")
# reticulate::virtualenv_install(envname = "python_environment",
#                                packages = python_packages,
#                                ignore_installed = TRUE)
# reticulate::use_virtualenv("python_environment", required = TRUE)


##### UI Menus and Layout
ui <-
  fluidPage(
    title = "Augury",
    tags$head(tags$style(css)),
    theme = shinytheme("cosmo"),
    verticalLineLeft,
    verticalLineRight,
    fluidRow(
      # Input options
      column(2, align = "left",
        div(style = "margin-left:5%; margin-right:0%; margin-top:15%;text-align:center",
            selectInput("collection",
                        label = HTML("<h5><b>1) Select Location</b></h5>"),
                        choices = names(MONGODB$COLLECTION)),
            HTML("<hr>"),
            HTML("<h5><b>2) Add Data</b></h5>"),
            tabsetPanel(type = "tabs",
              # Select bird observations records
              tabPanel(HTML("<h5>eBird<br/>Records</h5>"),
                HTML("<h6>"),
                sliderInput(
                  inputId="radius",
                  label=HTML("<h5><b>Search Radius (km)</b></h5>"),
                  min = 5,
                  max = 50,
                  step = 5,
                  value = 50
                ),
                selectInput(
                  inputId="species",
                  label=HTML("<h5><b>Choose Species</b></h5>"),
                  choices = "",
                  selectize = TRUE,
                  multiple = TRUE, 
                  width = "100%"#,
                  # options = list("maxItems"=5)  # select up to 5 species
                ),
                HTML("</h6>"),
                #HTML("<h5><b> - OR - </b></h5>"),
                HTML("<div style='text-align:center'>"),
                checkboxInput("kde", label = HTML("<b>Show Record Hotspots</b>"), value = FALSE),
                HTML("</div>"),
              ),
              # Select remote sensing data products
              tabPanel(HTML("<h5>Geospatial<br/>Layers</h5>"),
                HTML("<h6>"),
                selectInput(inputId = "dataset", label = "Data Set", choices = c("","BioClim","MODIS")),
                selectInput(inputId = "product", label = "Product", choices = ""),
                uiOutput("prod__desc"),
                selectInput(inputId = "band", label = "Band", choices = ""),
                uiOutput("band__desc"),
                conditionalPanel("input.dataset =='MODIS'",
                  selectInput(
                    inputId = "aggfun",
                    label = "Aggregation Function",
                    choices = c("Mean", "Median", "Minimum", "Maximum", "Variance", "Mean / Std. Dev"),
                  ),
                  uiOutput("unit__desc")
                ),
                actionButton(
                  inputId = "add_layer", 
                  label = "Add Layer", 
                  width="80%"
                )
              ) 
            ),
            HTML("</h6><hr>"),
            HTML("<h5><b>3) Select Date Range</b></h5>"),
            HTML("<h6>"),
            splitLayout(
              selectInput("start_date", "Start Date", choices = ""),
              selectInput("end_date", "End Date", choices = "")  # end menu options
            ),
            HTML("<hr>"), # horizontal rule between main title and program description
            actionButton(inputId = "reset", label = "Reset Fields", width = "80%"),
            HTML("</h6>")
          ) 
        ),
      column(8, align = "center",
        HTML("<div style='font-size:42px;text-align:center'><b><u>Augury: Dashboard for Visualizing Local eBird Records</u></b></div>"),
        HTML("<p style='font-size:17px; text-align:left;margin-left:2%'>Create customized visualizations of historical eBird observation records using the sidebar options. Search for additional up-to-date data products from MODIS satellites or WorldClim climatic record libraries. Zoom in on the map view clusters of data points and hover your mouse over observations to view information about each record.</p>"),
        HTML("<hr>")
      ),
      # Display map, plots, table, etc. (output)
      column(8, align = "center",
        div(style = "margin-left:5%; margin-top:1%;", #margin-right:25%",
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
        div(style = "margin-left:15%; margin-top:-53%; font-size:13px; margin-right:-25%",
          HTML("<p style='font-size:16px'><b><u>Review Records</u></b></p>"),
          fluidRow(
            HTML("<div style='font-size:80%'>"),
            dataTableOutput("table"),
            HTML("</div>")
          ),
        )
      )  # end column
    ) # end fluidRow
  )
        

##### Server logic
server = function(input, output, session) {
  
  #### Menu option observers and reactives (dynamic UI menu options)
  
  ### eBird Records
  
  ## Reset menu options action button is used
  observeEvent(input$collection, {
    
    # # Species
    # updateSelectizeInput(session, "species", selected = "")
    
    # Search Radius
    updateSliderInput(session, "radius", value = 50)

    
    # Record hotspots (prevents hang up when switching locations and value = TRUE)
    updateCheckboxInput(session, "kde", value = FALSE)
        
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
    
    # Product
    updateSelectInput(session,
                      "dataset",
                      choices = c("BioClim","MODIS"),
                      selected = "BioClim")
    
    # Clear cache files
    saveRDS(data.frame(), "temp/temp_data.rds")
    saveRDS(data.frame(), "temp/raster_data.rds")
    
  })
  
  
  ## Populate species menu with available options
  distinctSpecies <- reactive({
    
    augury <- mongodb_client()
    d <- augury$distinct_species(MONGODB$COLLECTION[[input$collection]],
                                 input$radius/87)
    sprintf("%s unique species found.", nrow(d))
    return(d)
    
  })
  
  
  ## Query remote database collection and get list of species with records
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
  
  
  ## Display data ranges, depending on the selected species
  observe({
    
    req(input$species != "")
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
  observe({
    
    req(input$species!="", input$start_date!="", input$end_date!="")
    if(!any(is.null(input$species)) & input$species != ""){
      
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
                            format = "%Y-%m-%d 00:00", 
                            usetz = FALSE),
        MAX_DATE = strftime(input$end_date,
                            format = "%Y-%m-%d 23:59", 
                            usetz = FALSE)
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
    saveRDS(plotdata, "temp/temp_data.rds")
    
  })
  
  
  ## Reset menu options when the action button is used
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
    saveRDS(data.frame(), "temp/temp_data.rds")
    saveRDS(data.frame(), "temp/raster_data.rds")
    
  })
  
  
  ### GIS Layer Menu Options
  
  ## Update 'product' selection menu based on input value of 'dataset'
  observe({
    
    req(input$dataset %in% c("MODIS","BioClim"))
    if(input$dataset == "MODIS"){
      menuOptions <- c("", products$product)
    }
    if(input$dataset == "BioClim"){
      data <- read.csv("bioclim.csv", header=TRUE, stringsAsFactors = FALSE)
      menuOptions <- c("", data$product)
    }
    updateSelectInput(session, "product", choices = menuOptions)
    
  })
  
  
  ## Update 'band' selection menu based on input value of 'product'
  observe({
    
    req(input$product!="")
    if(input$dataset=="MODIS"){
      req(input$product %in% products$product)
      data <- mt_bands(input$product)
    }
    if(input$dataset=="BioClim"){
      data <- read.csv("bioclim.csv", header=TRUE, stringsAsFactors = FALSE) %>%
        filter(product == input$product)
      #req(input$product %in% unique(data$product))
    }
    menuOptions <- c("", data$band)
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
    
    req(input$dataset != "", input$product != "")
    if(input$dataset == "MODIS"){
      req(input$product %in% products$product)
      bands <- mt_bands(input$product)
      x <- filter(bands, band==input$band)
    }
    if(input$dataset == "BioClim"){
      data <- read.csv("bioclim.csv", header=TRUE, stringsAsFactors = FALSE)
      req(input$product %in% unique(data$product))
      x <- filter(data, band==input$band)
    }
    args. <- x$description
    tag <-
      HTML(
        paste0(
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
      req(input$product %in% products$product)
      bands <- mt_bands(input$product)
      x <- filter(bands, band==input$band)
    }
    
    if(input$dataset=="BioClim"){
      req(input$product %in% c("Precipitation","Temperature"))
      bands <- read.csv("bioclim.csv", header=TRUE, stringsAsFactors = FALSE)
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
  
  
  #### UI Functionality
  
  ### Data and visualization functions
  
  ## Save temporary MODIS product raster data to local storage
  observeEvent(input$add_layer, {
    
    # Ensure that record hotspots are not already shown and species selection is not empty
    req(!input$kde)
    
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

      # Get raster data using 'tryCatch' wrapper function to handle errors related to date range search
      subset <- tryCatch(
        {
          # Try to get data using default search arguments
          mt_subset(
            product = product,
            lat = lonlat["LATITUDE"],
            lon = lonlat["LONGITUDE"],
            band = band,
            start = start_date,
            end = end_date,
            km_lr = radius * 2,
            km_ab = radius * 2,
            progress = FALSE
          )
      
        },
        
        # If error given, try with alternate search parameters related to the range of dates searched
        error = function(e){
        
          # Expand date range
          start_date <- format(Sys.time()-(5*365*24*60*60), "%Y-%m-%d") # 5 years ago
          end_date <- format(Sys.time(), "%Y-%m-%d") # Right now
          
          subset <- mt_subset(product = input$product,
                              lat = lonlat["LATITUDE"],
                              lon = lonlat["LONGITUDE"],
                              band = input$band,
                              start = start_date,
                              end = end_date,
                              km_lr = input$radius*2,
                              km_ab = input$radius*2,
                              progress = FALSE)
          
          return(subset)
          
        }
      
      )
      
      progress$inc(0.67)
      progress$set(message = "Converting data to raster object...", value = 0.67)
      
      if(x$units == "class"){
      
        ras <- mt_to_raster(subset) %>% 
          projectRaster(from=., crs = CRS("+init=epsg:4326"), method = "ngb") %>%
          calc(., median)
      
      }
      
      else{
      
        ras <- mt_to_raster(subset) %>% 
          projectRaster(., crs = CRS("+init=epsg:4326"), method = "bilinear")
          
      }
      
      progress$inc(0.75)
      progress$set(message = "Applying spatial mask to raster data...", value = 0.75)
  
      mras <- mask(ras, m)
      
      progress$inc(1)
      progress$set(message = "Finished", value = 1)
    
    }
    
    if(input$dataset == "BioClim"){
      
      progress$set(message = "Read BioClim data from storage...", value = 0)
      progress$inc(0.5)
      
      rasterFile <- sprintf("GeoData/wc0.5/%s_11.bil", input$band)
      ras <- raster(rasterFile, crs=CRS("+init=epsg:4326")) %>%
        crop(., m)
      
      progress$inc(0.75)
      progress$set(message = "Applying spatial mask to raster data...", value = 0.75)
      
      mras <- mask(ras, m)
      
      progress$inc(1)
      progress$set(message = "Finished", value = 1)
      
    }
    
    saveRDS(mras, "temp/raster_data.rds")
    
  })
  
  
  ## Create function for plotting basemap
  map_fun <- eventReactive(c(plotDataReader(), spatialDataReader(), input$kde, input$collection), {
    
    # Get mapping data
    plotdata <- readRDS("temp/temp_data.rds")
    labs <- plotdata$label
    cols <- plotdata$cols
    
    ras <- readRDS("temp/raster_data.rds")
    
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
      
      pal <- colorNumeric(palette = rev(c("#0C2C84", "#41B6C4", "#FFFFCC")),
                          values(ras),
                          na.color = "transparent")
      map <- map %>%
        addRasterImage(ras, colors = pal, opacity = 0.8, project = TRUE) %>%
        addLegend(pal = pal, values = values(ras), title = unit_desc(), position = "bottomleft", opacity = 1)
      
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
      
      # Check if saved versions already exists
      fileName <- sprintf("temp/record_hotspots/%s_%s_km+radius_ebird_record_hotspots.tif", 
                          input$collection, input$radius)
      
      # Check if a cached version already exists
      if(file.exists(fileName)){

        # Check if existing version is less than 24 hours old
        if(Sys.time() - file.info(fileName)$ctime > 24){

          rkde <- kde_raster(MONGODB$COLLECTION[[input$collection]], PROXIMITY = input$radius)
          rkde[rkde<0.001] <- NA
          writeRaster(rkde, fileName, overwrite=TRUE)

        }
        
        else{
          
          rkde <- raster(fileName)
          
        }

      }
      
      # If file does not exist, create records 'hotspot' density map
      else{
       
        rkde <- kde_raster(MONGODB$COLLECTION[[input$collection]], PROXIMITY = input$radius)
        rkde[rkde<0.001] <- NA
        writeRaster(rkde, fileName, overwrite=TRUE)
        
      }
      
      # Create color palette for the raster image
      pal <- colorNumeric(palette = c("#0C2C84", "#41B6C4", "#FFFFCC"), 
                          values(rkde),
                          na.color = "transparent")
      
      # Add hotspot raster to leaflet map
      map <- map %>%
        addRasterImage(rkde, colors = pal, opacity = 0.7)
    
    }
    
    # Add toner lines and labels on top of all other information
    map <- map %>%
      addProviderTiles(providers$Stamen.TonerLines,
                       options = providerTileOptions(opacity = 0.95)) %>%
      addProviderTiles(providers$Stamen.TonerLabels)

    return(map)
    
  })
  
  
  ## Reactive function plotting counts of records over time
  plot_fun <- eventReactive(c(plotDataReader(), spatialDataReader()), {
    
    plotData <- readRDS("temp/temp_data.rds")

      uniqueDates = plotData %>% unique() %>% length()
      if(nrow(plotData) > 1 & uniqueDates > 1){
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
          geom_bar(
            aes(
              x = Date,
              y = value,
              group = comName,
              fill = comName
            ), 
            stat = "identity"
          ) +
          theme_gray() +
          plot_theme +
          theme(legend.position = "bottom",
                plot.margin = unit(c(1, 1, 0.2, 1), "cm")) +
          scale_x_date("") +
          scale_y_continuous("Number of Records", limits = c(0, NA)) +
          scale_fill_manual(values = unique(plotData$cols))
        
        return(plt)
        
      }
    
  })
  
  
  ## Records summary sidebar (placeholder)
  table_fun <- eventReactive(plotDataReader(), {

    plotData <- readRDS("temp/temp_data.rds")
    
    # Remove geometry
    plotData$geometry <- NULL
    
    # Subset columns for display
    if(nrow(plotData)>0){
      
      plotData <- plotData %>%
        mutate(Date = as.Date(obsDt)) %>%
        select(c("comName", "speciesCode", "Date", "locName","howMany","subId")) %>%
        dplyr::rename(Name = comName) %>%
        dplyr::rename(Code = speciesCode) %>%
        dplyr::rename(Location = locName) %>%
        dplyr::rename(Count = howMany)
    
    }

    return(plotData)
    
  })
  
  
  ### Data Input/Output
  
  ## File reader reactive function for eBird records
  plotDataReader <- reactiveFileReader(1000, session, "temp/temp_data.rds", readRDS)
  
  
  ## File reader reactive function for spatial data
  spatialDataReader <- reactiveFileReader(1000, session, "temp/raster_data.rds", readRDS)
  
  
  ## Periodically update the MongoDB database with new records
  timelapse <- 1000*60*4 # 4 hours in milliseconds
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
  
  
  #### Output Objects
  
  ### Menu descriptions text
  output$prod__desc <- renderUI(prod_desc())
  output$band__desc <- renderUI(band_desc())
  output$unit__desc <- renderUI(unit_desc())
  
  ### Visualizations
  output$map <- renderLeaflet(map_fun())
  output$plot <- renderPlot(plot_fun())
  output$table <- renderDataTable(
    table_fun(),
    rownames = FALSE,
    escape = FALSE,
    options = list(
      scrollX = "200px",
      scrollY = "650px",
      scrollCollapse = TRUE,
      lengthMenu = list(c(1, 5, 10), c('1', '5', '10')),
      pageLength = 100
    )
  )
  
}


##### Launch program
shinyApp(ui = ui, server = server)

