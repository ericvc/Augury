## ggplot theme
plot_theme <- theme(text = element_text(family="sans"),
                    legend.position="bottom",
                    panel.background = element_rect(fill=alpha("white",0)),
                    panel.border = element_rect(fill=alpha("white",0)),
                    legend.title = element_blank(),
                    legend.text = element_text(size=13),
                    axis.text = element_text(size=13.5),
                    axis.title = element_text(size=14),
                    strip.text = element_text(size=13),
                    plot.title = element_text(face="bold", size=14.5, hjust=0.5),
                    plot.subtitle = element_text(size=13.5),
                    plot.caption = element_text(hjust=0, size=13.5, face="italic")
)


## CSS Customization
css <- HTML("
  body {
      background-color:#F2F2F2;
      margin-right:5%;
      margin-left:3%;
  }")

  
## MongoDB client creator
#Sys.setenv(RETICULATE_PYTHON = "/home/eric/miniconda3/bin/python3.7")
reticulate::source_python("Python/augury.py")
mongodb_client <- function(){
  ## Create a MongoDB client via Python wrapper
  x <- Augury(MONGODB$USERNAME,
              MONGODB$PASSWORD,
              MONGODB$ORGANIZATION,
              MONGODB$CLUSTER,
              MONGODB$DATABASE)
  return(x)

  }


## Function for calculating a KDE of all record locations to overlay on map
kde_raster <- function(COLLECTION){
  ##Given a set of coordiantes points, create a raster in ggplot2
  
  # Get all records from MongoDB collection
  mdb_client <- mongodb_client()
  df <- mdb_client$location_records(COLLECTION)
  
  # convert data to SpatialPointsDataFrame
  p4s <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  spdf <-
    SpatialPointsDataFrame(coords = df[, c("lng", "lat")],
                           data = df,
                           proj4string = p4s)
 
  # calculate unweighted KDE (output in raster format)	
  pts_kde <-
    sp.kde(
      x = spdf,
      y = spdf$weight,
      bw = 0.01,
      nc = 1000,
      nr = 1000,
      standardize = TRUE,
      scale.factor = 10000
    )
  
  return(pts_kde)

}


## Point labeller function
create_labels <- function(data){
  label <- c()
  for(i in 1:nrow(data)){
    lab <- with(data,
                paste(
                  paste0("<b>",comName[i],"</b>", " (",howMany[i],")"),
                  paste("Location: ","<i>",locName[i], "</i>", sep=""),
                  paste0("Coordinates: ", round(lat[i], 2), ", ", round(lng[i], 2)),
                  paste0("Date: ", strftime(obsDt[i], format = "%Y-%m-%d")),
                  sep = "<br/>"
                )
            )
    label[i] <- paste0("<div style=text-align:left>", lab, "</div>")
  }
  return(label)
}
