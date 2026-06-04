setwd("D:/MyStuff/Masters2/Research Project/R Project")

library(tidyverse)
library(sf)
library(dplyr)
library(terra)

#### Workflow to produce classification rasters ####
prepare_geom <- function(sf){
  # Dissolve polygons, project to UTM36N for buffering, negatively buffer
  new_geom <- st_union(sf) %>% 
    st_make_valid() %>% 
    st_transform(crs = 32735) %>% 
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

agriwet <- st_read("../PapyrusData/Merged_VRC/Agricultural_wetland.shp")
papyrus <- st_read("../PapyrusData/Merged_VRC/Papyrus_merged.shp")
broad <- st_read("../PapyrusData/Merged_VRC/Other_wetland_merged.shp")

train_agriwet <- prepare_geom(agriwet)
train_papyrus <- prepare_geom(papyrus)
train_broad <- prepare_geom(broad)

# st_write(train_agriwet, dsn = "../PapyrusData/Merged_VRC/Agri_training.shp", append = F)
# st_write(train_papyrus, dsn = "../PapyrusData/Merged_VRC/Papyrus_training.shp", append = F)
# st_write(train_broad, dsn = "../PapyrusData/Merged_VRC/Broad_training.shp", append = F)

# Step 2- Add classification values (as above)

# TODO: add a class name too
train_agriwet$class <- 1
train_papyrus$class <- 2
train_broad$class <- 3

# Add in other classifications from manual polygons
train_agri <- st_read("../PapyrusData/Custom Data Layers/Agriculture.shp") %>% 
  subset(select = c('geometry'))
train_forest <- st_read("../PapyrusData/Custom Data Layers/Forest_Scrub.shp")%>% 
  subset(select = c('geometry'))
train_cloud <- st_read("../PapyrusData/Custom Data Layers/Clouds.shp")%>% 
  subset(select = c('geometry'))
train_water <- st_read("../PapyrusData/Custom Data Layers/OpenWater.shp")%>% 
  subset(select = c('geometry'))
train_urban <- st_read("../PapyrusData/Custom Data Layers/Urban.shp")%>% 
  subset(select = c('geometry'))
train_shadow <- st_read("../PapyrusData/Custom Data Layers/Cloud Shadow.shp")%>% 
  subset(select = c('geometry'))

train_agri$class <- 4
train_forest$class <- 5
train_cloud$class <- 6
train_water$class <- 7
train_urban$class <- 8
train_shadow$class <- 9

# Step 3 - merge all shapes into 1 dataset

# TODO: Confirm this is the right sentinel to base the raster on given the cell size, CRS etc
sentinel <- rast("../PapyrusData/Sentinel_2025-08-04/S2C_MSIL2A_20250804T080631_N0511_R078_T35MRU_20250804T133415.SAFE/GRANULE/L2A_T35MRU_A004767_20250804T082807/IMG_DATA/R20m/T35MRU_20250804T080631_AOT_20m.jp2")
# TODO: We need to knit a further west raster on to capture Lake Mutanda
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

# Step 4 - rasterize dataset, use the existing sentinel image as sample

rast <- rasterize(vect(dataset), sentinel, field = "class")

# Step 5 - Write to file

writeRaster(
  rast,
  "../PapyrusData/Merged_VRC/Training_Raster.tif",
  datatype = "INT1U",
  overwrite = TRUE
)

#### the R20m sentinel images are missing bands, fill them in from different resolutions
sentinel_10m_B08 <- rast("../PapyrusData/Sentinel_2025-08-04/S2C_MSIL2A_20250804T080631_N0511_R078_T35MRU_20250804T133415.SAFE/GRANULE/L2A_T35MRU_A004767_20250804T082807/IMG_DATA/R10m/T35MRU_20250804T080631_B08_10m.jp2")

sentinel_60m_B09 <- rast("../PapyrusData/Sentinel_2025-08-04/S2C_MSIL2A_20250804T080631_N0511_R078_T35MRU_20250804T133415.SAFE/GRANULE/L2A_T35MRU_A004767_20250804T082807/IMG_DATA/R60m/T35MRU_20250804T080631_B09_60m.jp2")

sentinel_20m_B08 <- aggregate(sentinel_10m_B08, fact = 2, fun = mean)
sentinel_20m_B09 <- disagg(sentinel_60m_B09, fact = 3, method = 'bilinear')

writeRaster(
  sentinel_20m_B08,
  "../PapyrusData/Sentinel_2025-08-04/S2C_MSIL2A_20250804T080631_N0511_R078_T35MRU_20250804T133415.SAFE/GRANULE/L2A_T35MRU_A004767_20250804T082807/IMG_DATA/R20m/T35MRU_20250804T080631_B08_20m_agg.tif",
  overwrite = TRUE
)
writeRaster(
  sentinel_20m_B09,
  "../PapyrusData/Sentinel_2025-08-04/S2C_MSIL2A_20250804T080631_N0511_R078_T35MRU_20250804T133415.SAFE/GRANULE/L2A_T35MRU_A004767_20250804T082807/IMG_DATA/R20m/T35MRU_20250804T080631_B09_20m_disagg.tif",
  overwrite = TRUE
)

## INPUT ABOVE INTO MULTISPEC, GENERATE CLASSIFIED OUTPUT THERE
#
#
#
#
#
#
# NOW READ BACK IN TO CLEAN UP OUTPUT

# For comparison
multispec_out <- rast("../PapyrusData/Multispec Output/T35MRU_20250804T080631_B_output1.tif")
plot(multispec_out)

multispec_result <- rast("../PapyrusData/Multispec Output/ClassifiedOutput_1.tif")
plot(multispec_result)
crs(multispec_result) <- crs(sentinel)

# Turn the resulting classified raster back into polygons 
classified_geom <- as.polygons(multispec_result, dissolve = T)

# Filter out some noise to make the data manageable
classified_geom_sf <- st_as_sf(classified_geom)
classified_geom_sf$area_m2 <- st_area(classified_geom_sf)

min_area <- units::set_units(2000, "m^2")
classified_geom_sf <- subset(classified_geom_sf, ClassifiedOutput_1 %in% c(1,2,3) || area_m2 > min_area)

st_write(classified_geom_sf, dsn = "../Output/Classified_Vector_Sieved.shp", append = F)
