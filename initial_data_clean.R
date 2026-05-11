setwd("D:/MyStuff/Masters2/Research Project/R Project")

library(tidyverse)
library(sf)
library(dplyr)

#### BUNYONYI RAW DATA CLEANING #####
# read in all the shape files
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

# Remove the weird overlap in 9 of 10
b_9 <- b_9[!is.na(b_9$Descriptio), ]

# Change the defunct ID field to contain the swamp ID, add a new field for the 
# swamp when they all get merged
replace_swamp_id <-  function(data, swamp_id, swamp_name)
{
  data$Id <- swamp_id
  data$Swamp <- swamp_name
  data <- filter(data, !grepl(swamp_name, Descriptio))
}

b_1 <- replace_swamp_id(b_1, 1, "Bunyonyi")
b_2 <- replace_swamp_id(b_2, 2, "Bunyonyi")
b_3 <- replace_swamp_id(b_3, 3, "Bunyonyi")
b_4 <- replace_swamp_id(b_4, 4, "Bunyonyi")
b_5 <- replace_swamp_id(b_5, 5, "Bunyonyi")
b_6 <- replace_swamp_id(b_6, 6, "Bunyonyi")
b_7 <- replace_swamp_id(b_7, 7, "Bunyonyi")
b_8 <- replace_swamp_id(b_8, 8, "Bunyonyi")
b_9 <- replace_swamp_id(b_9, 9, "Bunyonyi")
b_10 <- replace_swamp_id(b_10, 10, "Bunyonyi")

# Merge all the areas into one layer
b_merged <- rbind(b_1, b_2, b_3, b_4, b_5, b_6, b_7, b_8, b_9, b_10)

# Fix the descriptio fields
b_merged$Descriptio[which(b_merged$Descriptio %in% c("Mature papyrus", "Mature Papyrus"))] <- "Mature papyrus"
b_merged$Descriptio[which(b_merged$Descriptio %in% c("Mixed vegetation", "Mixed Vegetation"))] <- "Mixed vegetation"
b_merged$Descriptio[which(b_merged$Descriptio %in% c("Sparce papyrus"))] <- "Sparse papyrus"
b_merged$Descriptio[which(b_merged$Descriptio %in% c("Papyrus regenerating", "Regenerating papyrus", "Regenerating Papyrus"))] <- "Regenerating papyrus"

# Save out to a new gpkg
st_write(b_merged, dsn = "../PapyrusData/Bunyonyi_cleaned.gpkg", layer = 'Wetlands', append = F)

#### RUSHEBEYA CLEANING ####
# read in all the shape files
r_1 <- st_read("../PapyrusData/Rushebeya Habitats/Rushebeya 1/Rushebeya 1.shp")
r_2 <- st_read("../PapyrusData/Rushebeya Habitats/Rushebeya 2/Rushebeya 2.shp")
r_3 <- st_read("../PapyrusData/Rushebeya Habitats/Rushebeya 3/Rushebeya 3.shp")
r_4 <- st_read("../PapyrusData/Rushebeya Habitats/Rushebeya 4/Rushebeya 4.shp")
r_5 <- st_read("../PapyrusData/Rushebeya Habitats/Rushebeya 5/Rushebeya 5.shp")
r_6 <- st_read("../PapyrusData/Rushebeya Habitats/Rushebeya 6/Rushebeya 6.shp")
r_7 <- st_read("../PapyrusData/Rushebeya Habitats/Rushebeya 7/Rushebeya 7.shp")
r_8 <- st_read("../PapyrusData/Rushebeya Habitats/Rushebeya 8/Rushebeya 8.shp")
r_9 <- st_read("../PapyrusData/Rushebeya Habitats/Rushebeya 9/Rushebeya 9.shp")
r_10 <- st_read("../PapyrusData/Rushebeya Habitats/Rushebeya 10/Rushebeya 10.shp")

# Fix spelling in Rushebeya 7
r_7$Descriptio[which(r_7$Descriptio %in% c("Rushebya 7"))] <- "Rushebeya 7"

# Change the defunct ID field to contain the swamp ID
r_1 <- replace_swamp_id(r_1, 1, "Rushebeya")
r_2 <- replace_swamp_id(r_2, 2, "Rushebeya")
r_3 <- replace_swamp_id(r_3, 3, "Rushebeya")
r_4 <- replace_swamp_id(r_4, 4, "Rushebeya")
r_5 <- replace_swamp_id(r_5, 5, "Rushebeya")
r_6 <- replace_swamp_id(r_6, 6, "Rushebeya")
r_7 <- replace_swamp_id(r_7, 7, "Rushebeya")
r_8 <- replace_swamp_id(r_8, 8, "Rushebeya")
r_9 <- replace_swamp_id(r_9, 9, "Rushebeya")
r_10 <- replace_swamp_id(r_10, 10, "Rushebeya")

# Merge all the areas into one layer
r_merged <- rbind(r_1, r_2, r_3, r_4, r_5, r_6, r_7, r_8, r_9, r_10)

# Fix up discrepancies
r_merged$Descriptio[which(r_merged$Descriptio %in% c("Burnt", "Burnt papyrus"))] <- "Burnt"
r_merged$Descriptio[which(r_merged$Descriptio %in% c("Mature papyrus", "Mature Papyrus"))] <- "Mature papyrus"
r_merged$Descriptio[which(r_merged$Descriptio %in% c("Sparce papyrus", "Sparse Papyrus"))] <- "Sparse papyrus"
r_merged$Descriptio[which(r_merged$Descriptio %in% c("Shrubs"))] <- "Scrub"

# Save out to a new gpkg
st_write(r_merged, dsn = "../PapyrusData/Rushebeya_cleaned.gpkg", layer = 'Wetlands', append = F)

#### WETLAND DATA COMBINED CLEANING ####
all <- st_read("../PapyrusData/Combined/all_wetland_combined_shp.shp")

# Filter to just the columns we need
#subset_all <- all[c("Id", "patch_id1", "PYW_patch", "CC_patch", "PYW", "CC", 
#                    "FID_1", "CC_14", "CC_15", "PYW_14", "PYW_15")]

subset_all <- all[c("patch_id1", "FID_1", "CC_14", "CC_15", "PYW_14", "PYW_15")]
subset_all <- st_transform(subset_all, crs = 4326)

#### PAPYRUS DATA CLEANING ####
papyrus <- st_read("../PapyrusData/Combined/papyrus_combined_shp.shp")

# Filter to just the columns we need
#subset_pap <- papyrus[c("Id", "patch_id1", "GSW", "WWW", "PC", "FID_1",
#                        "GSW_14", "GSW_15", "WWW_14", "WWW_15", "PC_14", "PC_15")]

subset_pap <- papyrus[c("patch_id1", "FID_1", "GSW_14", "GSW_15", "WWW_14", 
                        "WWW_15", "PC_14", "PC_15")]

#### BROAD DATA CLEANING ####
broad <- st_read("../PapyrusData/Combined/broad_combined_shp.shp")

# Filter to just the columns we need
#subset_broad <- broad[c("id", "patch_id1", "PYW_patch", "CC_patch", "PYW", "CC",
#                        "FID_1", "PYW_14", "PYW_15")]

subset_broad <- broad[c("patch_id1", "FID_1", "PYW_14", "PYW_15")] # CC_14 and CC_15 seem to be missing despite CC_patch existing
subset_broad <- st_transform(subset_broad, crs = 4326)

#### Intersect geometries
sf_use_s2(FALSE)
subset_all_temp <- st_difference(subset_all, st_union(st_combine(subset_broad)))
subset_broad_temp <- st_difference(subset_broad, st_union(st_combine(subset_pap)))

#### WRITE OUT ALL THE CLEANED DATA
st_write(subset_all_temp, dsn = "../PapyrusData/Combined/all_difference_temp.gpkg", layer = "all", append = F)
st_write(subset_broad_temp, dsn = "../PapyrusData/Combined/broad_difference_temp.gpkg", layer = "broad", append = F)
st_write(subset_pap, dsn = "../PapyrusData/Combined/pap_temp.gpkg", layer = "papyrus", append = F)

#### Classify Rushebeya and export
r_merged$Descriptio[which(r_merged$Descriptio %in% c("Burnt", "Cut"))] <- "agricultural"
r_merged$Descriptio[which(r_merged$Descriptio %in% c("Mature papyrus", 
                                                     "Regenerating papyrus",
                                                     "Regenerated papyrus",
                                                     "Sparse papyrus"))] <- "papyrus"
r_merged$Descriptio[which(r_merged$Descriptio %in% c("Mixed vegetation"))] <- "broad"
r_merged$Descriptio[which(r_merged$Descriptio %in% c("Open water", "Road"))] <- NA

# Extract just papyrus
papyrus_r <- r_merged[!is.na(r_merged$Descriptio) & r_merged$Descriptio == "papyrus",] %>% 
              summarize(do_union = TRUE) %>% 
              st_transform(crs = 4326) %>% 
              st_cast("POLYGON")

# Make papyrus_r columns match subset_pap
names(subset_pap)
papyrus_r$GSW_14 <- 0
papyrus_r$GSW_15 <- 0
papyrus_r$WWW_14 <- 0
papyrus_r$WWW_15 <- 0
papyrus_r$PC_14 <- 0
papyrus_r$PC_15 <- 0
papyrus_r$patch_id1 <- "0"
papyrus_r$FID_1 <- 0
counter <- max(subset_pap$FID_1)
for (var in 1:nrow(papyrus_r))
{
  counter <- counter + 1
  papyrus_r[var, ]$FID_1 <- counter
  papyrus_r[var, ]$patch_id1 <- sprintf("P%d", counter) # this doesn't actually work- they don't always match in the originals
}
papyrus_r <- papyrus_r[, names(subset_pap), drop = FALSE]

# Combine and write out
new_papyrus_data <- rbind(subset_pap, papyrus_r)
st_write(new_papyrus_data, dsn = "../PapyrusData/Papyrus_with_Rushebeya.gpkg", layer = 'Papyrus', append = F)

# Extract broad wetlands
broad_r <- r_merged[!is.na(r_merged$Descriptio) & r_merged$Descriptio == "broad",] %>% 
  summarize(do_union = TRUE) %>% 
  st_transform(crs = 4326) %>% 
  st_cast("POLYGON")

names(subset_broad_temp)
broad_r$PYW_14 <- 0
broad_r$PYW_15 <- 0
broad_r$patch_id1 <- "0"
broad_r$FID_1 <- 0
counter <- max(subset_broad$FID_1)
for (var in 1:nrow(broad_r))
{
  counter <- counter + 1
  broad_r[var, ]$FID_1 <- counter
  broad_r[var, ]$patch_id1 <- sprintf("B%d", counter)
}
broad_r <- broad_r[, names(subset_broad), drop = FALSE]

# Combine and write out
new_broad_data <- rbind(subset_broad_temp, broad_r)
st_write(new_broad_data, dsn = "../PapyrusData/Broad_with_Rushebeya.gpkg", layer = 'Broad', append = F)

# Extract remaining agricultural wetland (?)
agri_r <- r_merged[!is.na(r_merged$Descriptio) & r_merged$Descriptio == "agricultural",] %>% 
  summarize(do_union = TRUE) %>% 
  st_transform(crs = 4326) %>% 
  st_cast("POLYGON")

names(subset_all_temp)
agri_r$PYW_14 <- 0
agri_r$PYW_15 <- 0
agri_r$CC_14 <- 0
agri_r$CC_15 <- 0
agri_r$patch_id1 <- "0"
agri_r$FID_1 <- 0
counter <- max(subset_all_temp$FID_1)
for (var in 1:nrow(agri_r))
{
  counter <- counter + 1
  agri_r[var, ]$FID_1 <- counter
  agri_r[var, ]$patch_id1 <- sprintf("B%d", counter) # Existing ones seem to have 'B' for some reason, seems 'A' would be better
}
agri_r <- agri_r[, names(subset_all_temp), drop = FALSE]

# Combine and write out
new_agri_data <- rbind(subset_all_temp, agri_r)
st_write(new_agri_data, dsn = "../PapyrusData/Agricultural_with_Rushebeya.gpkg", layer = 'Agri', append = F)
