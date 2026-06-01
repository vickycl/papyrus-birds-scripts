setwd("D:/MyStuff/Masters2/Research Project/R Project")

library(tidyverse)
library(sf)
library(dplyr)

#### MERGED FILE CLEANING ####
# read in all the shape files
merged_shp <- st_read("../Combined Shape Files/Merged_All_Shapes.shp") %>% 
              st_transform(crs = 4326)

# Cross reference the vegetation fields to get all the descriptions
table(merged_shp$VegCode, merged_shp$Descriptio, useNA = "always")
table(merged_shp$VegCode, merged_shp$VegNme, useNA = "always")
table(merged_shp$Descriptio, merged_shp$VegNme, useNA = "always")

merged_shp[!is.na(merged_shp$Descriptio) & !is.na(merged_shp$VegNme),]
merged_shp[!is.na(merged_shp$Descriptio) & !is.na(merged_shp$VegNme) & !is.na(merged_shp$VegCode),]

# Still don't know what veg codes 5 & 7 mean- only present in Bukiro-Kibale
# 1 = Cut
# 2 = short regenerating
# 3 = tall regenerating
# 4 = regenerated
# 5 = ? - (possibly Mature Papyrus (sparse))
# 6 = Mature Papyrus (dense)
# 7 = ?
# 8 = Other vegetation / short / Mixed vegetation
# 12 = Shrubs

# Remove Bukiro-Kibale as they have weird veg codes and are often shifted geographically from where we expect
merged_shp <- subset(merged_shp, !grepl("Bukiro-Kibale", source, fixed = T))

# Treat Veg code as source of truth for the classifier- only a few are missing it

# Our classifiers of interest are: Papyrus, Broad Wetland, Agricultural Wetland, Open water
merged_shp$Classifier[which(merged_shp$VegCode %in% c(1, 2, 3, 4, 6))] <- "Papyrus"
merged_shp$Classifier[which(merged_shp$VegCode %in% c(8, 12))] <- "Broad Wetland"

merged_shp$Descriptio[which(merged_shp$Descriptio %in% c("Mature papyrus", 
                                                         "Mature Papyrus"))] <- "Mature papyrus"
merged_shp$Descriptio[which(merged_shp$Descriptio %in% c("Burnt papyrus", 
                                                         "Burnt"))] <- "Burnt"
merged_shp$Descriptio[which(merged_shp$Descriptio %in% c("Mixed Vegetation"))] <- "Mixed vegetation"
merged_shp$Descriptio[which(merged_shp$Descriptio %in% c("Papyrus regenerating", 
                                                         "Regenerating papyrus", 
                                                         "Regenerating Papyrus"))] <- "Regenerating papyrus"
merged_shp$Descriptio[which(merged_shp$Descriptio %in% c("Sparce papyrus"))] <- "Sparse papyrus"

# Fix description where we only have VegNme
merged_shp$Descriptio[which(merged_shp$VegNme %in% c("Trees and shrubs"))] <- "Shrubs"
merged_shp$Descriptio[which(merged_shp$VegNme %in% c("Other wetland vegetation_short"))] <- "Mixed vegetation"

# Add classification for Descriptions
merged_shp$Classifier[which(merged_shp$Descriptio %in% c("Regenerating papyrus",
                                                         "Sparse papyrus",
                                                         "Regenerated papyrus",
                                                         "Burnt"))] <- "Papyrus"
merged_shp$Classifier[which(merged_shp$Descriptio %in% c("Mixed vegetation",
                                                         "Shrubs"))] <- "Broad Wetland"
merged_shp$Classifier[which(merged_shp$Descriptio %in% c("Open water"))] <- "Open water"

merged_shp$Descriptio[which(is.na(merged_shp$Classifier))]
merged_shp$VegNme[which(is.na(merged_shp$Classifier))]

# These remaining entries are duplicates of Bunyonyi 10 with no description
# TODO: (Check I did this right and the other bunyonyi 9 are still present)
merged_shp <- merged_shp[which(merged_shp$source != "C:\\Users\\DAVERO~1\\AppData\\Local\\Temp\\RtmpQD5IdZ\\merged_shapes_671448971c34/Bunyonyi Habitats/Bunyonyi 9/Bunyonyi 9.shp"),]

# For now I think we have to ignore veg code 5 and 7 as we can't be certain whether they are papyrus or other vegetation (or something else)

output <- merged_shp[which(!(merged_shp$VegCode %in% c(5, 7))), c("source", "geometry", "Classifier")] %>% 
          st_transform(crs = 4326)
papyrus <- rbind(output[which(output$Classifier == "Papyrus"),])
all_wetland <- rbind(output[which(output$Classifier %in% c("Papyrus", "Broad Wetland")),])
other_wetland <- rbind(output[which(output$Classifier == "Broad Wetland"),])


#### Read in Linda's existing geometries ####

papyrus_input <- st_read("../PapyrusData/Combined/papyrus_combined_shp.shp")
papyrus_output <- papyrus_input["geometry"] %>% 
                   st_transform(crs = 4326)
papyrus_output$Classifier <- "Papyrus"
papyrus_output$source <- "D:/MyStuff/Masters2/Research Project/PapyrusData/Combined/papyrus_combined_shp.shp" # TODO: switch this to get the path instead

broad_input <- st_read("../PapyrusData/Combined/broad_combined_shp.shp")
broad_input <- broad_input["geometry"] %>% 
               st_transform(crs = 4326)

all_input <- st_read("../PapyrusData/Combined/all_wetland_combined_shp.shp")
all_input <- all_input["geometry"] %>% 
             st_transform(crs = 4326)

# Remove areas of overlap with newer Bunyonyi dataset- we will assume the latter is more accurate
b_1 <- st_read("../PapyrusData/Bunyonyi Habitats/Bunyonyi 1/Bunyonyi 1.shp")
b_2 <- st_read("../PapyrusData/Bunyonyi Habitats/Bunyonyi 2/Bunyonyi 2.shp")
b_3 <- st_read("../PapyrusData/Bunyonyi Habitats/Bunyonyi 3/Bunyonyi 3.shp")
b_4 <- st_read("../PapyrusData/Bunyonyi Habitats/Bunyonyi 4/Bunyonyi 4.shp")
b_5 <- st_read("../PapyrusData/Bunyonyi Habitats/Bunyonyi 5/Bunyonyi 5.shp")
b_6 <- st_read("../PapyrusData/Bunyonyi Habitats/Bunyonyi 6/Bunyonyi 6.shp")
b_7 <- st_read("../PapyrusData/Bunyonyi Habitats/Bunyonyi 7/Bunyonyi 7.shp")
b_8 <- st_read("../PapyrusData/Bunyonyi Habitats/Bunyonyi 8/Bunyonyi 8.shp")
b_9 <- st_read("../PapyrusData/Bunyonyi Habitats/Bunyonyi 9/Bunyonyi 9.shp")
b_10 <- st_read("../PapyrusData/Bunyonyi Habitats/Bunyonyi 10/Bunyonyi 10.shp")

# The Merged_All_shapes is missing Bunyonyi 1's papyrus - add it back
bunyonyi_remainder <- subset(b_1, Descriptio != "Bunyonyi 1", select = c(geometry))
bunyonyi_pap <- subset(b_1, Descriptio == "Bunyonyi 1", select = c(geometry)) %>% 
                st_difference(st_union(st_combine(bunyonyi_remainder))) %>% 
                st_transform(crs = 4326)

bunyonyi_pap$Classifier <- "Papyrus"
# Fake this back to where it should have been
bunyonyi_pap$source <- "C:/Users/DAVERO~1/AppData/Local/Temp/RtmpQD5IdZ/merged_shapes_671448971c34/Bunyonyi Habitats/Bunyonyi 1/Bunyonyi 1.shp"
papyrus <- rbind(papyrus, bunyonyi_pap)

bunyonyi <- rbind(b_1, b_2, b_3, b_4, b_5, b_6, b_7, b_8, b_9, b_10) %>% 
            st_combine() %>% 
            st_union() %>% 
            st_make_valid() %>% 
            st_transform(crs = 4326)

# Remove areas covered by newer Bunyonyi data to avoid overlaps when merging later
sf_use_s2(FALSE)
all_input <- st_difference(all_input, bunyonyi)
broad_input <- st_difference(broad_input, bunyonyi)
papyrus_input <- st_difference(papyrus_input, bunyonyi)


# Create output version of 'broad' wetland, assuming this is papyrus + other wetland, to be merged
# This is assuming we don't care what type of wetland, just that it's wetland
combined_output <- broad_input
combined_output$Classifier <- "All Wetland"
combined_output$source <- "D:/MyStuff/Masters2/Research Project/PapyrusData/Combined/broad_combined_shp.shp"

#### Intersect geometries
agri_output <- st_difference(all_input, st_union(st_combine(broad_input)))
broad_output <- st_difference(broad_input, st_union(st_combine(papyrus_input)))

agri_output$Classifier <- "Agricultural Wetland"
agri_output$source <- "D:/MyStuff/Masters2/Research Project/PapyrusData/Combined/all_wetland_combined_shp.shp"
broad_output$Classifier <- "Broad Wetland"
broad_output$source <- "D:/MyStuff/Masters2/Research Project/PapyrusData/Combined/broad_combined_shp.shp"

# Merge new shapes with existing
papyrus_merged <- rbind(papyrus_output, papyrus)
broad_merged <- rbind(broad_output, other_wetland)
combined_merged <- rbind(combined_output, all_wetland)

# Output to new files
st_write(papyrus_merged, dsn = "../PapyrusData/Merged_VRC/Papyrus_merged.shp", append = F)
st_write(broad_merged, dsn = "../PapyrusData/Merged_VRC/Other_wetland_merged.shp", append = F)
st_write(combined_merged, dsn = "../PapyrusData/Merged_VRC/Both_wetland_merged.shp", append = F)
st_write(agri_output, dsn = "../PapyrusData/Merged_VRC/Agricultural_wetland.shp", append = F)
# TODO: Investigate why this gives an error on "feature 121" when writing out agri_output- it appears to be a GEOMETRYCOLLECTION where the others are all multipolygons

st_write(papyrus_merged, dsn = "../PapyrusData/Merged_VRC/All_wetlands.gpkg", layer = 'Papyrus', append = F)
st_write(broad_merged, dsn = "../PapyrusData/Merged_VRC/All_wetlands.gpkg", layer = 'Other Wetland', append = F)
st_write(combined_merged, dsn = "../PapyrusData/Merged_VRC/All_wetlands.gpkg", layer = 'Combined', append = F)
st_write(agri_output, dsn = "../PapyrusData/Merged_VRC/All_wetlands.gpkg", layer = 'Agricultural', append = F)



plot(papyrus_merged)

#### Workflow to produce classification rasters ####
prepare_geom <- function(sf){
  # Dissolve polygons, project to UTM36N for buffering, negatively buffer
  new_geom <- st_union(sf) %>% 
        st_make_valid() %>% 
        st_transform(crs = 21096) %>% 
        st_buffer(-5) %>% 
        st_make_valid() %>% 
        st_cast("POLYGON")
  
  new_sf <- st_sf(geometry = new_geom)
  # Calculate areas
  new_sf$area_m2 <- st_area(new_sf)
  
  # Sieve out the too-small areas
  min_area <- units::set_units(1000, "m^2")
  new_sf <- subset(new_sf, area_m2 > min_area)
  
  # Project back to 4326
  new_sf <- st_transform(new_sf, crs = 4326)
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

train_agriwet <- prepare_geom(agri_output)
train_papyrus <- prepare_geom(papyrus_merged)
train_broad <- prepare_geom(broad_merged)

st_write(train_agriwet, dsn = "../PapyrusData/Merged_VRC/Agri_training.shp", append = F)
st_write(train_papyrus, dsn = "../PapyrusData/Merged_VRC/Papyrus_training.shp", append = F)
st_write(train_broad, dsn = "../PapyrusData/Merged_VRC/Broad_training.shp", append = F)

# Step 2- Add classification values (as above)

# TODO: add a class name too
train_agriwet$class <- 1
train_papyrus$class <- 2
train_broad$class <- 3

# TODO: add in other classifications from manual polygons

# Step 3 - merge all shapes into 1 dataset

# TODO: Confirm this is the right sentinel to base the raster on given the cell size, CRS etc
sentinel <- rast("../PapyrusData/Sentinel_2025-08-04/S2C_MSIL2A_20250804T080631_N0511_R078_T35MRU_20250804T133415.SAFE/GRANULE/L2A_T35MRU_A004767_20250804T082807/IMG_DATA/R20m/T35MRU_20250804T080631_AOT_20m.jp2")
# TODO: We need to knit a further west raster on to capture Lake Mutanda
dataset <- rbind(train_agriwet, train_papyrus, train_broad) %>% 
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



