setwd("D:/MyStuff/Masters2/Research Project/R Project")
#### NOTE: Windows users will first need to install Cygwin before using this package.
# Cygwin must be installed in the default location (C:/cygwin/bin/gawk.exe or 
# C:/cygwin64/bin/gawk.exe) in order for auk to work.

# From https://cornelllabofornithology.github.io/auk/
# See also https://ebird.github.io/ebird-best-practices/intro.html 
library(auk)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(readr)
library(sf)

convert_to_gis <- function(file_path, species_name)
{
  f_out <- "../BirdData/temp.txt"
  ebird_data <- file_path |> 
    # 1. reference file
    auk_ebd() |> 
    # 2. define filters
    # auk_species(species = species_name) |> # Not using currently
    auk_country(country = "Uganda") |> 
    # 3. run filtering
    auk_filter(file = f_out, overwrite = T) |> 
    # 4. read text file into r data frame
    read_ebd()
  
  # Filter sightings assigned to hotspots as location not accurate enough
  ebird_data <- ebird_data[ebird_data$locality_type != "H", ]
  
  layer_name <- tolower(species_name) %>% 
                stringr::str_replace_all(" ", "_")
    
  # Convert to point geometries
  data_sf <- ebird_data |> 
    select(checklist_id, latitude, longitude) |> 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  data_sf <- merge(data_sf, ebird_data, by = "checklist_id")
  
  # Write to individual layers
  st_write(data_sf, dsn = "../BirdData/Birds.gpkg", layer = layer_name, append = F)
}

convert_to_gis("../BirdData/ebd_UG_papcan1_smp_relMar-2026.txt", "Papyrus Canary")
convert_to_gis("../BirdData/ebd_UG_carcis1_smp_relMar-2026.txt", "Carruthers Cisticola")
convert_to_gis("../BirdData/ebd_UG_grswar2_smp_relMar-2026.txt", "Greater Swamp Warbler")
convert_to_gis("../BirdData/ebd_UG_papgon1_smp_relMar-2026.txt", "Papyrus Gonolek")
convert_to_gis("../BirdData/ebd_UG_papyew1_smp_relMar-2026.txt", "Papyrus Yellow Warbler")
convert_to_gis("../BirdData/ebd_UG_paywar1_smp_relMar-2026.txt", "Papyrus Yellow Warbler2") # Bit of a hack for the entries of different subspecies- should be combined
convert_to_gis("../BirdData/ebd_UG_wwswar1_smp_relMar-2026.txt", "White Winged Swamp Warbler")


#### TODO:
# Filter by swamp shape files or bounding box


#### Thoughts:
## Exclude travelling as likely not accurate enough? (observation_type == "Traveling")

## Detect duplicates eg same day? 
### - maybe not necessary as only looking at presence/ absence

## Should we only use complete checklists for presence/ absence?

#### UNUSED: Could modify this to filter to study areas
# boundary of study region, buffered by 1 km
#study_region_buffered <- read_sf("data/gis-data.gpkg", layer = "ne_states") |>
  # sf does not properly buffer complex polygons in lat/lng coordinates
  # so we temporarily project the data to a planar coordinate system
#  st_transform(crs = 8857) |>
#  filter(state_code == "US-GA") |>
#  st_buffer(dist = 1000) |> 
#  st_transform(crs = st_crs(checklists_sf))
