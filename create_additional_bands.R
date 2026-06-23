setwd("D:/MyStuff/Masters2/ResearchProject/RunThrough_Full")

library(dplyr)
library(terra)

#========================
# Change here for different Sentinel images

IMG_DATA_PATH <- "../Sentinel/S2B_MSIL2A_20250730T080609_N0511_R078_T35MRU_20250730T103041.SAFE/GRANULE/L2A_T35MRU_A043862_20250730T082708/IMG_DATA"
DATED_FILE_NAME <- "T35MRU_20250730T080609"

#========================

#### the R20m sentinel images are missing bands, fill them in from different resolutions
sentinel_10m_B08 <- rast(sprintf("%s/R10m/%s_B08_10m.jp2", IMG_DATA_PATH, DATED_FILE_NAME))

sentinel_60m_B09 <- rast(sprintf("%s/R60m/%s_B09_60m.jp2", IMG_DATA_PATH, DATED_FILE_NAME))

sentinel_20m_B08 <- aggregate(sentinel_10m_B08, fact = 2, fun = mean)
sentinel_20m_B09 <- disagg(sentinel_60m_B09, fact = 3, method = 'bilinear')

writeRaster(
  sentinel_20m_B08,
  sprintf("%s/R20m/%s_B08_20m_agg.tif", IMG_DATA_PATH, DATED_FILE_NAME),
  overwrite = TRUE
)
writeRaster(
  sentinel_20m_B09,
  sprintf("%s/R20m/%s_B09_20m_disagg.tif", IMG_DATA_PATH, DATED_FILE_NAME),
  overwrite = TRUE
)

# Read in all bands and combine into a single file for ease of use

sentinel_20m_B01 <- rast(sprintf("%s/R20m/%s_B01_20m.jp2", IMG_DATA_PATH, DATED_FILE_NAME))
sentinel_20m_B02 <- rast(sprintf("%s/R20m/%s_B02_20m.jp2", IMG_DATA_PATH, DATED_FILE_NAME))
sentinel_20m_B03 <- rast(sprintf("%s/R20m/%s_B03_20m.jp2", IMG_DATA_PATH, DATED_FILE_NAME))
sentinel_20m_B04 <- rast(sprintf("%s/R20m/%s_B04_20m.jp2", IMG_DATA_PATH, DATED_FILE_NAME))
sentinel_20m_B05 <- rast(sprintf("%s/R20m/%s_B05_20m.jp2", IMG_DATA_PATH, DATED_FILE_NAME))
sentinel_20m_B06 <- rast(sprintf("%s/R20m/%s_B06_20m.jp2", IMG_DATA_PATH, DATED_FILE_NAME))
sentinel_20m_B07 <- rast(sprintf("%s/R20m/%s_B07_20m.jp2", IMG_DATA_PATH, DATED_FILE_NAME))
sentinel_20m_B8A <- rast(sprintf("%s/R20m/%s_B8A_20m.jp2", IMG_DATA_PATH, DATED_FILE_NAME))
sentinel_20m_B11 <- rast(sprintf("%s/R20m/%s_B11_20m.jp2", IMG_DATA_PATH, DATED_FILE_NAME))
sentinel_20m_B12 <- rast(sprintf("%s/R20m/%s_B12_20m.jp2", IMG_DATA_PATH, DATED_FILE_NAME))

new_sentinel <- c(sentinel_20m_B01,
                  sentinel_20m_B02,
                  sentinel_20m_B03,
                  sentinel_20m_B04,
                  sentinel_20m_B05,
                  sentinel_20m_B06,
                  sentinel_20m_B07,
                  sentinel_20m_B08,
                  sentinel_20m_B8A,
                  sentinel_20m_B09,
                  sentinel_20m_B11,
                  sentinel_20m_B12
                )

# ensure the layers have unique names for the classifier
names(new_sentinel) <- c(sprintf("%s_B01_20m", DATED_FILE_NAME),
                         sprintf("%s_B02_20m", DATED_FILE_NAME),
                         sprintf("%s_B03_20m", DATED_FILE_NAME),
                         sprintf("%s_B04_20m", DATED_FILE_NAME),
                         sprintf("%s_B05_20m", DATED_FILE_NAME),
                         sprintf("%s_B06_20m", DATED_FILE_NAME),
                         sprintf("%s_B07_20m", DATED_FILE_NAME),
                         sprintf("%s_B08_20m", DATED_FILE_NAME),
                         sprintf("%s_B8A_20m", DATED_FILE_NAME),
                         sprintf("%s_B09_20m", DATED_FILE_NAME),
                         sprintf("%s_B11_20m", DATED_FILE_NAME),
                         sprintf("%s_B12_20m", DATED_FILE_NAME)
)

writeRaster(
  new_sentinel,
  "Sentinel.tif",
  overwrite = TRUE
)
