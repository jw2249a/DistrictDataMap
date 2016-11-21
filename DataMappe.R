
# Make a Cool Map
# Download District Data From NCES at http://data.deptofed.opendata.arcgis.com/datasets/1e1426f35aef40c48ee8f20e0b9e5bac_0.zip

# Input Data into R

library(foreign)
library(leaflet)
library(leafletR)
library(rgdal)
library(rgeos)
library(sp)
library(geojsonio)

#input url of map api 

url <- "http://data.deptofed.opendata.arcgis.com/datasets/1e1426f35aef40c48ee8f20e0b9e5bac_0.zip"

#unzip it

content <- download.file(url, "CCD1314.zip")

wd <- paste0(getwd(),"/","CCD1314.zip")

unzip(wd, junkpaths = TRUE)

#load spss data

spssdata <- read.spss("PPEDATA2014.sav")

# now the shapefile

shpdata <- readOGR(getwd(), "District_Level_Common_Core_of_Data_20132014")

shpdata1 <- shpdata

# create fips codes for 50 states and DC

numlist <- c("01", "02", "04", "05", "06", "08", "09", as.character(10:13), as.character(15:42), as.character(44:51), as.character(53:56))

# function to run all the states 
i <- 1
  
  while (i < length(numlist)) { 
    
  # set constant data to reset 
    
  shpdata <- shpdata1
  
  
  # transfer spss data into usable format/ round ADJPPE
  
  spssdata <- as.data.frame(spssdata)
  spssdata$ADJPPE <- round(spssdata$ADJPPE, digits = 0)
  spssdata$NCESID <-  as.character(spssdata$NCESID)
  
  #filter and join data by states
  
  if (i == 7) {
    shpdata = shpdata[substring(shpdata$GEOID, 1, 2) == numlist[7], ]
    y <- spssdata$NCESID[substring(spssdata$NCESID, 1, 1) == as.character(9)]
    y <- y[as.numeric(y) < 1000000]
  }
  if (i < 3) {
    y <- spssdata$NCESID[substring(spssdata$NCESID, 1, 1) == as.character(i)]
    y <- y[as.numeric(y) < 1000000]
    shpdata = shpdata[substring(shpdata$GEOID, 1, 2) == numlist[i], ]
  }
  if (i >= 8) {
    y <- spssdata$NCESID[substring(spssdata$NCESID, 1, 2) == numlist[i]]
    y <- y[as.numeric(y) > 1000000]
    shpdata = shpdata[substring(shpdata$GEOID, 1, 2) == numlist[i], ]
  }
  if (i == 3) {
    shpdata = shpdata[substring(shpdata$GEOID, 1, 2) == numlist[3], ]
    y <- spssdata$NCESID[substring(spssdata$NCESID, 1, 1) == as.character(4)]
    y <- y[as.numeric(y) < 1000000]
  }
  if (i == 4) {
    shpdata = shpdata[substring(shpdata$GEOID, 1, 2) == numlist[4], ]
    y <- spssdata$NCESID[substring(spssdata$NCESID, 1, 1) == as.character(5)]
    y <- y[as.numeric(y) < 1000000]
  }
  if (i == 5) {
    shpdata = shpdata[substring(shpdata$GEOID, 1, 2) == numlist[5], ]
    y <- spssdata$NCESID[substring(spssdata$NCESID, 1, 1) == as.character(6)]
    y <- y[as.numeric(y) < 1000000]
  }
  if (i == 6) {
    shpdata = shpdata[substring(shpdata$GEOID, 1, 2) == numlist[6], ]
    y <- spssdata$NCESID[substring(spssdata$NCESID, 1, 1) == as.character(8)]
    y <- y[as.numeric(y) < 1000000]
  }
  # subset spss data by states
  spssdata2 <- subset(spssdata, NCESID %in% y)
  
  # get rid of excess variables
  
  spssdata2 <- spssdata2[c("NCESID", "DISTRICTNAME", "ADJPPE")]
  
  #change map format
  
  shpdata <- spTransform(shpdata, CRS("+init=epsg:4326"))

  # separate district data from mapping data to add spssdata

  shpDistrictData <- shpdata@data[, c("OBJECTID", "GEOID")]
  
  # create spatial polygons
  
  shpdata2 <-gSimplify(shpdata, tol = 0.01, topologyPreserve = TRUE)
  
  
  # write the geojson
  
  shpdata4 <- SpatialPolygonsDataFrame(shpdata2, data = shpDistrictData)
  
  # get rid of those grody nulls
  
  shpdata5 <- lapply(shpdata4@data, function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
  })
  shpdata5$NCESID <- as.numeric(as.character(shpdata5$GEOID))
 
   # same
  
  shpdata5 <- lapply(shpdata5, function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
  })
  
  #long gross merge that doesnt mess up data
  
  shpdata5 <- merge(shpdata5, spssdata2, by = "NCESID", all.x = TRUE)
  shpdata5 <- shpdata5[c("OBJECTID", "DISTRICTNAME", "ADJPPE")]
  shpdata5 <- merge(shpdata4, shpdata5, by = "OBJECTID", all.x = TRUE)
  
  # write a geojson file 
  
  writeOGR(shpdata5, paste0("MapFolder/Shapefile/District_Level_Common_Core_of_Data_20132014", i, ".geojson"), layer = "", driver = "GeoJSON")
  
# lets quantile the data now for coloring

  quants <- round(quantile(spssdata$ADJPPE, probs = seq(0, 1, 0.20), na.rm = TRUE), 0)
  quants[1]<-0
# Lets get the look down

  hoverover <- c("DISTRICTNAME", "ADJPPE")
  colouryform <- c("#17414B", "#5AB2C5", "#F7969C","#BD4F4D", "#7A2425")
  style <- styleGrad(prop="ADJPPE", breaks=c(quants), right=FALSE, style.par = "col",
               style.val = rev(colouryform), fill.alpha = 1, leg = "Per-Pupil Expenditures", lwd = 2, alpha = 1, col = "white")

  # creates the map
  
  map <- leaflet(data = paste0("MapFolder/ShapeFile/District_Level_Common_Core_of_Data_20132014", i, ".geojson"), dest = "MapFolder/Shapefile",
             title = paste0("index ", i), base.map="osm",
             incl.data = TRUE, style = style,  popup = hoverover)
 
  
  
  i <- i+1
}
 




