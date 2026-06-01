setwd("D:/MyStuff/Masters2/Research Project/R Project")

library(tidyverse)
library(sf)
library(dplyr)
library(terra)
library(tidyterra)

terraOptions(
  memfrac = 0.8
)

uganda_dem <- rast("../DEMs/Uganda_SRTM30meters.tif")

plot(uganda_dem)

writeRaster(
  uganda_dem,
  "../Output/DEM_working.tif",
  datatype = "FLT4S",
  overwrite = TRUE
)
uganda_dem <- rast("../Output/DEM_working.tif")

# filled_dem <- fillHoles(uganda_dem) # uses way more RAM than I have available, let's try without

flow_dir <- terrain(uganda_dem, v = "flowdir", filename = "../Output/Flow_Dir.tif", overwrite = T)
slope <- terrain(uganda_dem, v = "slope", unit = "radians", filename = "../Output/Slope_Radians.tif", overwrite = T)
flow_accum <- flowAccumulation(flow_dir, weight = NULL, filename = "../Output/Flow_Accum.tif", overwrite = T) # took forever but did complete eventually

my_tan <- function(slope){
  if (slope > 0)
    tan(slope) # How to get precision here?
}