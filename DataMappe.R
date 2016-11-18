
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
numlist <- c("01", "02", "04", "05", "06", "08", "09", as.character(10:13), as.character(15:42), as.character(44:51), as.character(53:56))
i <- 1
  
  while (i <52) { 
    
  shpdata <- shpdata1
  
  shpdata = shpdata[substring(shpdata$GEOID, 1, 2) == numlist[i], ]
  
  spssdata <- as.data.frame(spssdata)
  spssdata$NCESID <-  as.character(spssdata$NCESID)
  if (i == 3) {
    i <- i + 1
  }
  if (i == 7) {
    i <- i + 1
  }
  if (i < 10) {
    y <- spssdata$NCESID[substring(spssdata$NCESID, 1, 1) == as.character(i)]
    y <- y[as.numeric(y) < 1000000]
  }
  if (i > 10) {
    y <- spssdata$NCESID[substring(spssdata$NCESID, 1, 2) == numlist[i]]
    y <- y[as.numeric(y) > 1000000]
  }
  spssdata2 <- subset(spssdata, NCESID %in% y)
  shpdata <- spTransform(shpdata, CRS("+init=epsg:4326"))

  # separate district data from mapping data to add spssdata

  shpDistrictData <- shpdata@data[, c("OBJECTID", "GEOID", "NAME")]
  
  
  # create spatial polygons
  
  shpdata2 <-gSimplify(shpdata, tol = 0.01, topologyPreserve = TRUE)
  
  
  #change rownames to match data
  

  # write the geojson
  
  shpdata4 <- SpatialPolygonsDataFrame(shpdata2, data = shpDistrictData)
  
  
  shpdata4@data$NCESID <- as.numeric(as.character(shpdata4$GEOID))
  
  shpdata4@data <- merge(shpdata4@data, spssdata2, by="NCESID", all.x = TRUE)
  
  shpdata4@data[is.na(shpdata4@data$NCESID)] <- 0
  
  shpdata4@data <-shpdata4@data[order(shpdata4@data$OBJECTID), ] 
  
  writeOGR(shpdata4, paste0("MapFolder/Shapefile/District_Level_Common_Core_of_Data_20132014", i, ".geojson"), layer = "", driver = "GeoJSON")
 
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
             title = paste0("index ", i), base.map="osm",
             incl.data = TRUE,  popup = hoverover)
  i <- i+1
}




