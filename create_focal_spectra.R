setwd("D:/MyStuff/Masters2/ResearchProject/PapyrusData/Sentinel_2025-08-04/focal_output")

library(tidyverse)
library(sf)
library(dplyr)
library(terra)

img_path <- "../S2C_MSIL2A_20250804T080631_N0511_R078_T35MRU_20250804T133415.SAFE/GRANULE/L2A_T35MRU_A004767_20250804T082807/IMG_DATA/R20m"
output_path <- "D:/MyStuff/Masters2/ResearchProject/PapyrusData/Sentinel_2025-08-04/focal_output"

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
  rm(spec)
}

apply_focal(sprintf("%s/T35MRU_20250804T080631_B01_20m.jp2", img_path), 3)
apply_focal(sprintf("%s/T35MRU_20250804T080631_B02_20m.jp2", img_path), 3)
apply_focal(sprintf("%s/T35MRU_20250804T080631_B03_20m.jp2", img_path), 3)
apply_focal(sprintf("%s/T35MRU_20250804T080631_B04_20m.jp2", img_path), 3)
apply_focal(sprintf("%s/T35MRU_20250804T080631_B05_20m.jp2", img_path), 3)
apply_focal(sprintf("%s/T35MRU_20250804T080631_B06_20m.jp2", img_path), 3)
apply_focal(sprintf("%s/T35MRU_20250804T080631_B07_20m.jp2", img_path), 3)
apply_focal(sprintf("%s/T35MRU_20250804T080631_B08_20m_agg.tif", img_path), 3)
apply_focal(sprintf("%s/T35MRU_20250804T080631_B8A_20m.jp2", img_path), 3)
apply_focal(sprintf("%s/T35MRU_20250804T080631_B09_20m_disagg.tif", img_path), 3)
apply_focal(sprintf("%s/T35MRU_20250804T080631_B11_20m.jp2", img_path), 3)
apply_focal(sprintf("%s/T35MRU_20250804T080631_B12_20m.jp2", img_path), 3)


apply_focal(sprintf("%s/T35MRU_20250804T080631_B01_20m.jp2", img_path), 5)
apply_focal(sprintf("%s/T35MRU_20250804T080631_B02_20m.jp2", img_path), 5)
apply_focal(sprintf("%s/T35MRU_20250804T080631_B03_20m.jp2", img_path), 5)
apply_focal(sprintf("%s/T35MRU_20250804T080631_B04_20m.jp2", img_path), 5)
apply_focal(sprintf("%s/T35MRU_20250804T080631_B05_20m.jp2", img_path), 5)
apply_focal(sprintf("%s/T35MRU_20250804T080631_B06_20m.jp2", img_path), 5)
apply_focal(sprintf("%s/T35MRU_20250804T080631_B07_20m.jp2", img_path), 5)
apply_focal(sprintf("%s/T35MRU_20250804T080631_B08_20m_agg.tif", img_path), 5)
apply_focal(sprintf("%s/T35MRU_20250804T080631_B8A_20m.jp2", img_path), 5)
apply_focal(sprintf("%s/T35MRU_20250804T080631_B09_20m_disagg.tif", img_path), 5)
apply_focal(sprintf("%s/T35MRU_20250804T080631_B11_20m.jp2", img_path), 5)
apply_focal(sprintf("%s/T35MRU_20250804T080631_B12_20m.jp2", img_path), 5)

