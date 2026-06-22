setwd("D:/MyStuff/Masters2/ResearchProject/R Project")

library(tidyverse)
library(sf)
library(dplyr)
library(terra)

# Reminder:
# 1 - Agricultural Wetland
# 2 - Papyrus 
# 3 - Broad Wetland 
# 4 - Agriculture 
# 5 - Forest/ Scrub 
# 6 - Cloud 
# 7 - Open water 
# 8 - Urban 
# 9 - Cloud Shadow 

## EVERYTHING OUTPUT BY MULTISPEC NEEDS A CRS RE-ASSIGNING
sentinel <- rast("../PapyrusData/Sentinel_2025-08-04/S2C_MSIL2A_20250804T080631_N0511_R078_T35MRU_20250804T133415.SAFE/GRANULE/L2A_T35MRU_A004767_20250804T082807/IMG_DATA/R20m/T35MRU_20250804T080631_AOT_20m.jp2")
multispec_result <- rast("../RunThrough3/T35MRU_20250804T080631_B_echocl.tif")
crs(multispec_result) <- crs(sentinel)

writeRaster(
  multispec_result,
  "../RunThrough3/Classified_withCRS.tif",
  overwrite = TRUE
)

### Generalize pipeline ###

# 1. 'Region Group', with number of nearest neighbours = 4, Zone grouping method 'Within', and 'Add link field to output' on

regions <- patches(multispec_result, directions = 4, values = T)

#?? in arcGIS there's a link field option that I'm not sure how to match

# 2. 'Input false raster or constant value' of 1 piped into 'Set Null' where COUNT < 5, creating NibbleMask raster

  
# 3. 'Nibble' the NibbleMask from above, Using NoData values if they are the nearest neighbour, and nibbling NoData cells
  
# 4. 'Set Null' on all values not in c(2, 3)



### From ChatGPT:
r <- rast("../RunThrough3/Classified_withCRS.tif")

#-----------------------------------------------------------
# Region Group
# ArcGIS: FOUR, WITHIN
#-----------------------------------------------------------

patches_r <- patches(
  r,
  directions = 4,
  values = T
)

#-----------------------------------------------------------
# Compute patch sizes
#-----------------------------------------------------------

f <- freq(patches_r)

size_r <- classify(
  patches_r,
  cbind(f$value, f$count)
)

#-----------------------------------------------------------
# Small patches (<5 cells)
#-----------------------------------------------------------

small_patch <- size_r < 5

#-----------------------------------------------------------
# Approximate Nibble
#-----------------------------------------------------------

modal_r <- focal(
  r,
  w = matrix(1,3,3),
  fun = modal,
  na.policy = "omit"
)

generalised <- ifel(
  small_patch,
  modal_r,
  r
)

#-----------------------------------------------------------
# Keep only classes 2 and 3
#-----------------------------------------------------------

output <- ifel(
  generalised %in% c(2,3),
  generalised,
  NA
)

writeRaster(
  output,
  "../RunThrough3/Generalised_by_chat.tif",
  overwrite = TRUE
)

### end chat gpt ###

# Compare chat gpt's with the ArcGIS one:
freq(output)
arc <- rast("../RunThrough3/Generalised_output.tif")
freq(arc)


#### The following polygonisation makes everything very slow

# Turn the resulting classified raster back into polygons 
classified_geom <- as.polygons(multispec_result, dissolve = T)

# Filter out some noise to make the data manageable
classified_geom_sf <- st_as_sf(classified_geom)
classified_geom_sf$area_m2 <- st_area(classified_geom_sf)

min_area <- units::set_units(2000, "m^2")
classified_geom_sf_sub <- subset(classified_geom_sf, ClassifiedOutput_1 %in% c(1,2,3) | area_m2 > min_area)


papyrus_geoms <- subset(classified_geom_sf, ClassifiedOutput_1 == 2)
broad_geoms <- subset(classified_geom_sf, ClassifiedOutput_1 == 3)
# Total areas for papyrus and broad wetland:
sum(papyrus_geoms$area_m2)
sum(broad_geoms$area_m2)

st_write(papyrus_geoms, dsn = "../Output/Papyrus_classified.shp", append = F)
st_write(broad_geoms, dsn = "../Output/Broad_Classified.shp", append = F)
st_write(classified_geom_sf_sub, dsn = "../Output/Classified_Vector_Sieved.shp", append = F)

# DO the same for Daveron's classification to compare areas
dav <- rast("../PapyrusData/DaveronClassification/Generalised_FullImg_20m_4326_echocl/Generalised_FullImg_20m_4326_echocl.tif")
dav_poly <- as.polygons(dav, dissolve = T)
dav_geom <- st_as_sf(dav_poly)
dav_geom$area_m2 <- st_area(dav_geom)
sum(dav_geom[dav_geom$Band_1 == 2,]$area_m2)
sum(dav_geom[dav_geom$Band_1 == 3,]$area_m2)


