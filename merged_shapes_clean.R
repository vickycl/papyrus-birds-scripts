setwd("D:/MyStuff/Masters2/ResearchProject/RunThrough_Full")

library(tidyverse)
library(sf)
library(dplyr)

#===================
# Paths and other config

MERGED_SHAPES_PATH <- "Merged_All_Shapes/Merged_All_Shapes.shp"
BUNYONYI_PATH <- "Bunyonyi Habitats"
LYNDA_DATA_PATH <- "edited_lynda_data"


#===================

#### MERGED FILE CLEANING ####
# read in all the shape files
merged_shp <- st_read(MERGED_SHAPES_PATH) %>% 
              st_transform(crs = 4326)

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

# These remaining entries are duplicates of Bunyonyi 10 with no description
# TODO: (Check I did this right and the other bunyonyi 9 are still present)
merged_shp <- merged_shp[which(merged_shp$source != "C:\\Users\\DAVERO~1\\AppData\\Local\\Temp\\RtmpQD5IdZ\\merged_shapes_671448971c34/Bunyonyi Habitats/Bunyonyi 9/Bunyonyi 9.shp"),]

# For now I think we have to ignore veg code 5 and 7 as we can't be certain whether they are papyrus or other vegetation (or something else)

output <- merged_shp[which(!(merged_shp$VegCode %in% c(5, 7))), c("source", "geometry", "Classifier")] %>% 
          st_transform(crs = 4326)
papyrus <- rbind(output[which(output$Classifier == "Papyrus"),])
all_wetland <- rbind(output[which(output$Classifier %in% c("Papyrus", "Broad Wetland")),])
other_wetland <- rbind(output[which(output$Classifier == "Broad Wetland"),])


#### Read in updated versions of Lynda's geometries ####

papyrus_input <- st_read(sprintf("%s/papyrus_edited.shp", LYNDA_DATA_PATH))
papyrus_input <- papyrus_input["geometry"] %>% 
                   st_transform(crs = 4326)

other_input <- st_read(sprintf("%s/other_wetland_edited.shp", LYNDA_DATA_PATH))
other_input <- other_input["geometry"] %>% 
               st_transform(crs = 4326) %>% 
               st_make_valid()

agri_input <- st_read(sprintf("%s/agricultural_wetland_edited.shp", LYNDA_DATA_PATH))
agri_input <- agri_input["geometry"] %>% 
              st_transform(crs = 4326) %>% 
              st_make_valid()

# We need to remove areas of overlap with newer Bunyonyi dataset- we will assume the latter is more accurate
b_1 <- st_read(sprintf("%s/Bunyonyi 1/Bunyonyi 1.shp", BUNYONYI_PATH))
b_2 <- st_read(sprintf("%s/Bunyonyi 2/Bunyonyi 2.shp", BUNYONYI_PATH))
b_3 <- st_read(sprintf("%s/Bunyonyi 3/Bunyonyi 3.shp", BUNYONYI_PATH))
b_4 <- st_read(sprintf("%s/Bunyonyi 4/Bunyonyi 4.shp", BUNYONYI_PATH))
b_5 <- st_read(sprintf("%s/Bunyonyi 5/Bunyonyi 5.shp", BUNYONYI_PATH))
b_6 <- st_read(sprintf("%s/Bunyonyi 6/Bunyonyi 6.shp", BUNYONYI_PATH))
b_7 <- st_read(sprintf("%s/Bunyonyi 7/Bunyonyi 7.shp", BUNYONYI_PATH))
b_8 <- st_read(sprintf("%s/Bunyonyi 8/Bunyonyi 8.shp", BUNYONYI_PATH))
b_9 <- st_read(sprintf("%s/Bunyonyi 9/Bunyonyi 9.shp", BUNYONYI_PATH))
b_10 <- st_read(sprintf("%s/Bunyonyi 10/Bunyonyi 10.shp", BUNYONYI_PATH))

# The Merged_All_shapes is missing Bunyonyi 1's papyrus because the original data had an issue
# Work out the shape of the papyrus by taking the region described as "Bunyonyi 1" 
# and subtracting all the other described regions from it
bunyonyi_remainder <- subset(b_1, Descriptio != "Bunyonyi 1", select = c(geometry))
bunyonyi_pap <- subset(b_1, Descriptio == "Bunyonyi 1", select = c(geometry)) %>% 
                st_difference(st_union(st_combine(bunyonyi_remainder))) %>% 
                st_transform(crs = 4326)

bunyonyi_pap$Classifier <- "Papyrus"
# Fake this back to where it 'should' have been
bunyonyi_pap$source <- "C:/Users/DAVERO~1/AppData/Local/Temp/RtmpQD5IdZ/merged_shapes_671448971c34/Bunyonyi Habitats/Bunyonyi 1/Bunyonyi 1.shp"
papyrus <- rbind(papyrus, bunyonyi_pap)


# Create a single 'new' Bunyonyi to mask from the older data
# Remove areas covered by newer Bunyonyi data to avoid overlaps when merging later
bunyonyi <- rbind(b_1, b_2, b_3, b_4, b_5, b_6, b_7, b_8, b_9, b_10) %>% 
            st_combine() %>% 
            st_union() %>% 
            st_transform(crs = 4326) %>% 
            st_make_valid()

sf_use_s2(FALSE)
agri_output <- st_difference(agri_input, bunyonyi)
agri_output$Classifier <- "Agricultural Wetland"
agri_output$source <- "agricultural_wetland_edited.shp"


other_output <- st_difference(other_input, bunyonyi)
other_output$Classifier <- "Broad Wetland"
other_output$source <- "other_wetland_edited.shp"

papyrus_output <- st_difference(papyrus_input, bunyonyi)
papyrus_output$Classifier <- "Papyrus"
papyrus_output$source <- "papyrus_edited.shp"

# Merge new shapes with existing
papyrus_merged <- rbind(papyrus_output, papyrus) %>% 
                  st_cast("MULTIPOLYGON")
other_merged <- rbind(other_output, other_wetland) %>% 
                  st_cast("MULTIPOLYGON")
agri_output <- st_cast(agri_output, "MULTIPOLYGON")

# Output to new files
st_write(papyrus_merged, dsn = "Papyrus_merged.shp", append = F)
st_write(other_merged, dsn = "Other_wetland_merged.shp", append = F)
st_write(agri_output, dsn = "Agricultural_wetland.shp", append = F)
