
# Make a Cool Map
# Download District Data From NCES at http://data.deptofed.opendata.arcgis.com/datasets/1e1426f35aef40c48ee8f20e0b9e5bac_0.zip

# Input Data into R

library(foreign)
library(leaflet)
library(leafletR)
library(rgdal)
library(rgeos)
library(sp)
url <- "http://data.deptofed.opendata.arcgis.com/datasets/1e1426f35aef40c48ee8f20e0b9e5bac_0.zip"

content <- download.file(url, "CCD1314.zip")

wd <- paste0(getwd(),"/","CCD1314.zip")

unzip(wd, junkpaths = TRUE)

spssdata <- read.spss("PPEDATA2014.sav")

# now the shapefile

shpdata <- readOGR(getwd(), "District_Level_Common_Core_of_Data_20132014")

shpdata1 <- shpdata

# transform data to usable format for leaflet

i <- 1

while (i <52) {

  shpdata = shpdata1

  shpdata = shpdata[substring(as.numeric(shpdata$STATE2), 1, 4) == i, ]
  
  x <- rownames(shpdata@data)
  
  rownames(shpdata@data) <- c(0:(as.numeric(length(shpdata@data[, 1]))-1))
  
  shpdata <- spTransform(shpdata, CRS("+init=epsg:4326"))

# separate district data from mapping data to add spssdata

  shpDistrictData <- shpdata@data 
  
  #change district data GeoID to NCESID
  
  shpDistrictData$NCESID <- as.numeric(as.character(shpDistrictData$GEOID))
  
  joinedData <- merge(shpDistrictData, spssdata, by = "NCESID", all.x = TRUE)
  
  
  # create spatial polygons
  
  shpdata2 <-gSimplify(shpdata, tol = 0.01, topologyPreserve = TRUE)
  
  shpdata3 <-gSimplify(shpdata, tol = 0.05, topologyPreserve = TRUE)
  
  #change rownames to match data
  
  rownames(joinedData) <- x

  # write the geojson
  
  shpdata4 <- SpatialPolygonsDataFrame(shpdata2, data = joinedData)
  shpdata5 <- SpatialPolygonsDataFrame(shpdata3, data = joinedData)
  
  # make data way smaller
  
  shpdata4@data <- shpdata4@data[c('NAME', 'STATE2', 'ADJPPE')]
  shpdata5@data <- shpdata5@data[c("NAME", "STATE2", "ADJPPE")]
  


  writeOGR(shpdata4, paste0("MapFolder/Shapefile/District_Level_Common_Core_of_Data_20132014", i, ".geojson"), layer = "", driver = "GeoJSON")
  writeOGR(shpdata5, paste0("MapFolder/Shapefile/District_Level_Common_Core_of_Data_20132014reduced", i, ".geojson"), layer = "", driver = "GeoJSON")

# lets quantile the data now for coloring

quants <- round(quantile(shpdata4$ADJPPE, probs = seq(0, 1, 0.20), na.rm = TRUE), 0)
quants[1]<-0
quants


# Lets get the look down

hoverover <- c("NAME", "ADJPPE")

style <- styleGrad(prop="ADJPPE", breaks=quants, right=FALSE, style.par = "col",
               style.val = rev(heat.colors(5)), leg = "Per-Pupil Expenditures (RAW)", lwd = 1)


# creates the map

map <- leaflet(data = paste0("MapFolder/ShapeFile/District_Level_Common_Core_of_Data_20132014", i, ".geojson"), dest = "MapFolder/Shapefile", style = style,
             title = paste0("index ", shpdata4@data$STATE2[i]), base.map="osm",
             incl.data = TRUE,  popup = hoverover)
i <- i+1
}




