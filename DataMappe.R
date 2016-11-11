
# Make a Cool Map
# Download District Data From NCES at http://data.deptofed.opendata.arcgis.com/datasets/1e1426f35aef40c48ee8f20e0b9e5bac_0
# Extract it into a file

# Input Data into R

library(foreign)
library(leaflet)
library(leafletR)
library(rgdal)
library(rgeos)
library(sp)

spssdata <- read.spss("PPEDATA2014.sav")

# now the shapefile
shpdata <- readOGR("Map Folder/Shapefile", "District_Level_Common_Core_of_Data_20132014")

# transform data to usable format for leaflet

shpdata <- spTransform(shpdata, CRS("+init=epsg:4326"))

# separate district data from mapping data to add spssdata

shpDistrictData <- shpdata@data

#view variables

View(shpDistrictData)

#change district data GeoID to NCESID

shpDistrictData$NCESID <- shpDistrictData$GEOID

joinedData <- merge(shpDistrictData, spssdata, by = "NCESID", all.x = TRUE)

# create spatial polygons

shpdata <-gSimplify(shpdata, tol = 0.01, topologyPreserve = TRUE)

#change rownames to match data

rownames(joinedData) <- c(0:13584)

# write the geojson

shpdata <- SpatialPolygonsDataFrame(shpdata, data = joinedData)

writeOGR(shpdata, "Map Folder/Shapefile/District_Level_Common_Core_of_Data_20132014.geojson", layer = "", driver = "GeoJSON")

# lets quantile the data now for coloring

quants <- round(quantile(shpdata$ADJPPE, probs = seq(0, 1, 0.20), na.rm = TRUE), 0)
quants[1]<-0
quants

# Lets get the look down

hoverover <- c("NAME", "ADJPPE")

style <- styleGrad(prop="ADJPPE", breaks=quants, right=FALSE, style.par = "col",
               style.val = rev(heat.colors(5)), leg = "Per-Pupil Expenditures (RAW)", lwd = 1)

# creates the map

map <- leaflet(data = "Map Folder/Shapefile/District_Level_Common_Core_of_Data_20132014.geojson", dest = "Map Folder/Shapefile", style = style,
             title="index", base.map="osm",
             incl.data = TRUE,  popup = hoverover)


browseURL(map)



