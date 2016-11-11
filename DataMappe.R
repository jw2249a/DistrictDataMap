
# Make a Cool Map
# Download District Data From NCES at http://data.deptofed.opendata.arcgis.com/datasets/1e1426f35aef40c48ee8f20e0b9e5bac_0

# Input DBF File into R to add QC Data

library(foreign)

dbfdata <- read.dbf("Map Folder/District_Level_Common_Core_of_Data_20132014.dbf", as.is = T)

spssdata <- read.spss("PPEDATA2014.sav")

spssdata <- data.frame(spssdata)

dbfdata$NCESID <- as.numeric(dbfdata$GEOID)

total <- merge(dbfdata, spssdata, by = "NCESID")


write.dbf(total, "Map Folder/District_Level_Common_Core_of_Data_20132014.dbf")

