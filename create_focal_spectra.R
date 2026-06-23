setwd("D:/MyStuff/Masters2/ResearchProject/PapyrusData/Sentinel_2025-08-04/focal_output")

library(tidyverse)
library(sf)
library(dplyr)
library(terra)

#========================
# setup

img_path <- "../Sentinel/S2B_MSIL2A_20250730T080609_N0511_R078_T35MRU_20250730T103041.SAFE/GRANULE/L2A_T35MRU_A043862_20250730T082708/IMG_DATA/R20m"
DATED_FILE_NAME <- "T35MRU_20250730T080609"
output_path <- "focal_output"

#========================

apply_focal <- function(path, dim)
{
  spec <- rast(path)
  
  folder <- sprintf("%ix%i", dim, dim)
  filename <- sub("\\.jp2$", ".tif", basename(path))
  new_file <- file.path(output_path, folder, filename)
  
  # Compute the moving average and output to file
  focal_out <- focal(
    x = spec,
    w = dim,
    fun = "mean",
    na.policy = "omit",
    filename = new_file,
    overwrite = T
  )
  
  cat(sprintf("\n Focal raster saved to: %s\n", new_file))
  return(spec)
}


if(!dir.exists(sprintf("%s", output_path)))
  dir.create(sprintf("%s", output_path))

#==============================
# 3x3 output
#==============================
if(!dir.exists(sprintf("%s/3x3", output_path)))
  dir.create(sprintf("%s/3x3", output_path))

B01 <- apply_focal(sprintf("%s/%s_B01_20m.jp2", img_path, DATED_FILE_NAME), 3)
B02 <- apply_focal(sprintf("%s/%s_B02_20m.jp2", img_path, DATED_FILE_NAME), 3)
B03 <- apply_focal(sprintf("%s/%s_B03_20m.jp2", img_path, DATED_FILE_NAME), 3)
B04 <- apply_focal(sprintf("%s/%s_B04_20m.jp2", img_path, DATED_FILE_NAME), 3)
B05 <- apply_focal(sprintf("%s/%s_B05_20m.jp2", img_path, DATED_FILE_NAME), 3)
B06 <- apply_focal(sprintf("%s/%s_B06_20m.jp2", img_path, DATED_FILE_NAME), 3)
B07 <- apply_focal(sprintf("%s/%s_B07_20m.jp2", img_path, DATED_FILE_NAME), 3)
B08 <- apply_focal(sprintf("%s/%s_B08_20m_agg.tif", img_path, DATED_FILE_NAME), 3)
B8A <- apply_focal(sprintf("%s/%s_B8A_20m.jp2", img_path, DATED_FILE_NAME), 3)
B09 <- apply_focal(sprintf("%s/%s_B09_20m_disagg.tif", img_path, DATED_FILE_NAME), 3)
B11 <- apply_focal(sprintf("%s/%s_B11_20m.jp2", img_path, DATED_FILE_NAME), 3)
B12 <- apply_focal(sprintf("%s/%s_B12_20m.jp2", img_path, DATED_FILE_NAME), 3)

new_sentinel <- c(B01, B02, B03, B04, B05, B06, B07, B08, B8A, B09, B11, B12)

# ensure the layers have unique names for the classifier
names(new_sentinel) <- c(sprintf("mean_3x3_%s_B01_20m", DATED_FILE_NAME),
                         sprintf("mean_3x3_%s_B02_20m", DATED_FILE_NAME),
                         sprintf("mean_3x3_%s_B03_20m", DATED_FILE_NAME),
                         sprintf("mean_3x3_%s_B04_20m", DATED_FILE_NAME),
                         sprintf("mean_3x3_%s_B05_20m", DATED_FILE_NAME),
                         sprintf("mean_3x3_%s_B06_20m", DATED_FILE_NAME),
                         sprintf("mean_3x3_%s_B07_20m", DATED_FILE_NAME),
                         sprintf("mean_3x3_%s_B08_20m", DATED_FILE_NAME),
                         sprintf("mean_3x3_%s_B8A_20m", DATED_FILE_NAME),
                         sprintf("mean_3x3_%s_B09_20m", DATED_FILE_NAME),
                         sprintf("mean_3x3_%s_B11_20m", DATED_FILE_NAME),
                         sprintf("mean_3x3_%s_B12_20m", DATED_FILE_NAME)
)

writeRaster(
  new_sentinel,
  "Sentinel_3x3.tif",
  overwrite = TRUE
)

# Clean up to save memory
rm(new_sentinel)
rm(B01)
rm(B02)
rm(B03)
rm(B04)
rm(B05)
rm(B06)
rm(B07)
rm(B08)
rm(B8A)
rm(B09)
rm(B11)
rm(B12)

#==============================
# 5x5 output
#==============================

if(!dir.exists(sprintf("%s/5x5", output_path)))
  dir.create(sprintf("%s/5x5", output_path))

B01 <- apply_focal(sprintf("%s/%s_B01_20m.jp2", img_path, DATED_FILE_NAME), 5)
B02 <- apply_focal(sprintf("%s/%s_B02_20m.jp2", img_path, DATED_FILE_NAME), 5)
B03 <- apply_focal(sprintf("%s/%s_B03_20m.jp2", img_path, DATED_FILE_NAME), 5)
B04 <- apply_focal(sprintf("%s/%s_B04_20m.jp2", img_path, DATED_FILE_NAME), 5)
B05 <- apply_focal(sprintf("%s/%s_B05_20m.jp2", img_path, DATED_FILE_NAME), 5)
B06 <- apply_focal(sprintf("%s/%s_B06_20m.jp2", img_path, DATED_FILE_NAME), 5)
B07 <- apply_focal(sprintf("%s/%s_B07_20m.jp2", img_path, DATED_FILE_NAME), 5)
B08 <- apply_focal(sprintf("%s/%s_B08_20m_agg.tif", img_path, DATED_FILE_NAME), 5)
B8A <- apply_focal(sprintf("%s/%s_B8A_20m.jp2", img_path, DATED_FILE_NAME), 5)
B09 <- apply_focal(sprintf("%s/%s_B09_20m_disagg.tif", img_path, DATED_FILE_NAME), 5)
B11 <- apply_focal(sprintf("%s/%s_B11_20m.jp2", img_path, DATED_FILE_NAME), 5)
B12 <- apply_focal(sprintf("%s/%s_B12_20m.jp2", img_path, DATED_FILE_NAME), 5)

new_sentinel <- c(B01, B02, B03, B04, B05, B06, B07, B08, B8A, B09, B11, B12)

# ensure the layers have unique names for the classifier
names(new_sentinel) <- c(sprintf("mean_5x5_%s_B01_20m", DATED_FILE_NAME),
                         sprintf("mean_5x5_%s_B02_20m", DATED_FILE_NAME),
                         sprintf("mean_5x5_%s_B03_20m", DATED_FILE_NAME),
                         sprintf("mean_5x5_%s_B04_20m", DATED_FILE_NAME),
                         sprintf("mean_5x5_%s_B05_20m", DATED_FILE_NAME),
                         sprintf("mean_5x5_%s_B06_20m", DATED_FILE_NAME),
                         sprintf("mean_5x5_%s_B07_20m", DATED_FILE_NAME),
                         sprintf("mean_5x5_%s_B08_20m", DATED_FILE_NAME),
                         sprintf("mean_5x5_%s_B8A_20m", DATED_FILE_NAME),
                         sprintf("mean_5x5_%s_B09_20m", DATED_FILE_NAME),
                         sprintf("mean_5x5_%s_B11_20m", DATED_FILE_NAME),
                         sprintf("mean_5x5_%s_B12_20m", DATED_FILE_NAME)
)

writeRaster(
  new_sentinel,
  "Sentinel_5x5.tif",
  overwrite = TRUE
)

rm(new_sentinel)
rm(B01)
rm(B02)
rm(B03)
rm(B04)
rm(B05)
rm(B06)
rm(B07)
rm(B08)
rm(B8A)
rm(B09)
rm(B11)
rm(B12)

