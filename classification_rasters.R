setwd("D:/MyStuff/Masters2/ResearchProject/RunThrough_Full")

library(tidyverse)
library(sf)
library(dplyr)
library(terra)

#===============
# Paths etc

CUSTOM_LAYER_PATH <- "custom_data_layers"
SENTINEL_PATH <- "../Sentinel/S2B_MSIL2A_20250730T080609_N0511_R078_T35MRU_20250730T103041.SAFE/GRANULE/L2A_T35MRU_A043862_20250730T082708/IMG_DATA/R20m/T35MRU_20250730T080609_B01_20m.jp2"

#===============

#### Workflow to produce classification rasters ####
prepare_geom <- function(sf){
  # Dissolve polygons, project to UTM 35S for buffering, negatively buffer
  new_geom <- st_transform(sf, crs = 32735) %>% 
              st_make_valid() %>%
              st_union() %>% 
              st_make_valid() %>%
              st_buffer(-5) %>% 
              st_make_valid() %>% 
              st_cast("POLYGON")
  
  new_sf <- st_sf(geometry = new_geom)
  # Calculate areas
  new_sf$area_m2 <- st_area(new_sf)
  
  # Sieve out the too-small areas
  min_area <- units::set_units(1000, "m^2")
  new_sf <- subset(new_sf, area_m2 > min_area, select = c('geometry'))
  
  # Project back to 4326
  #new_sf <- st_transform(new_sf, crs = 4326)
  return(new_sf)
}
# Step 1 - Prepare the training data
# We need:
# 1 - Agricultural Wetland
# 2 - Papyrus 
# 3 - Broad Wetland 
# 4 - Agriculture 
# 5 - Forest/ Scrub 
# 6 - Cloud 
# 7 - Open water 
# 8 - Urban 
# 9 - Cloud Shadow

agriwet <- st_read("Agricultural_wetland.shp")
train_agriwet <- prepare_geom(agriwet)
st_write(train_agriwet, "agri_wet_sieved.shp", append = F)

papyrus <- st_read("Papyrus_merged.shp")
train_papyrus <- prepare_geom(papyrus)
st_write(train_papyrus, "papyrus_sieved.shp", append = F)

other <- st_read("Other_wetland_merged.shp")
train_broad <- prepare_geom(other)
st_write(train_broad, "other_wetland_sieved.shp", append = F)

# Step 2- Add classification values (as above)

# TODO: add a class name too
train_agriwet$class <- 1
train_papyrus$class <- 2
train_broad$class <- 3

# Add in other classifications from manual polygons
train_agri <- st_read(sprintf("%s/Agriculture.shp", CUSTOM_LAYER_PATH)) %>% 
  subset(select = c('geometry'))
train_forest <- st_read(sprintf("%s/Forest.shp", CUSTOM_LAYER_PATH)) %>% 
  subset(select = c('geometry'))
train_cloud <- st_read(sprintf("%s/Cloud.shp", CUSTOM_LAYER_PATH)) %>% 
  subset(select = c('geometry'))
train_water <- st_read(sprintf("%s/OpenWater.shp", CUSTOM_LAYER_PATH)) %>% 
  subset(select = c('geometry'))
train_urban <- st_read(sprintf("%s/Urban.shp", CUSTOM_LAYER_PATH)) %>% 
  subset(select = c('geometry'))
train_shadow <- st_read(sprintf("%s/CloudShadow.shp", CUSTOM_LAYER_PATH)) %>% 
  subset(select = c('geometry'))

train_agri$class <- 4
train_forest$class <- 5
train_cloud$class <- 6
train_water$class <- 7
train_urban$class <- 8
train_shadow$class <- 9

# Step 3 - merge all shapes into 1 dataset

# Use one of the Sentinel rasters as the CRS and the template/ sample
sentinel <- rast(SENTINEL_PATH)

dataset <- rbind(train_agriwet, 
                 train_papyrus, 
                 train_broad,
                 train_agri,
                 train_forest,
                 train_cloud,
                 train_water,
                 train_urban,
                 train_shadow) %>% 
  st_transform(crs = st_crs(sentinel))

# Step 4 - rasterize dataset, using the existing sentinel image as sample

rast <- rasterize(vect(dataset), sentinel, field = "class")

# Step 5 - Write to file

writeRaster(
  rast,
  "Training_Raster.tif",
  datatype = "INT1U",
  overwrite = TRUE
)
