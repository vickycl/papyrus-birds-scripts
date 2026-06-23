# =============================================================================
# Sentinel-2 Random Forest Classification — R / terra
# =============================================================================
# Dependencies:
#   install.packages(c("terra", "randomForest", "caret"))
#
# Usage: edit the three paths in CONFIG, then source the script.
# =============================================================================
setwd("D:/MyStuff/Masters2/ResearchProject/RunThrough_Full")

library(terra)
library(randomForest)
library(caret)
library(tidyverse)
# -----------------------------------------------------------------------------
# misc conversion code etc
# -----------------------------------------------------------------------------
#
# twi <- rast("twi.tif") %>% 
#   project(crs(image))
# twi <- resample(twi, image, method = "mean", filename = "twi_resample.tif")
#
# -----------------------------------------------------------------------------
# CONFIG — edit these
# -----------------------------------------------------------------------------
IMAGE_PATH  <- "Sentinel.tif" # Multi-band Sentinel-2 GeoTIFF
LABELS_PATH <- "Training_Raster.tif"   # Aligned single-band label raster
#TWI_PATH    <- "twi_resample.tif"      # Topographic Wetness Index as a predictor
FOCAL_PATH  <- "Sentinel_3x3.tif"    # Neighbourhood smoothing as a predictor
ADDITIONAL_FOCAL_PATH  <- "Sentinel_5x5.tif"

#   (integer class codes; 0 = no data)
OUTPUT_PATH <- "classified.tif"        # Where to write the result

N_TREES     <- 200                     # Number of Random Forest trees
MTRY        <- 7                       # Number of variables randomly sampled as candidates at each split
TEST_SPLIT  <- 0.2                     # Fraction held out for validation

# -----------------------------------------------------------------------------
# 1. Load rasters
# -----------------------------------------------------------------------------
start.time <- Sys.time()

cat("[1/5] Loading rasters...\n")
image  <- c(rast(IMAGE_PATH), rast(FOCAL_PATH), rast(ADDITIONAL_FOCAL_PATH))#, rast(TWI_PATH))

labels <- rast(LABELS_PATH)

crs(image) <- crs(labels) # Multispec doesn't assign a CRS so put it back manually

cat(sprintf("      Image bands : %d\n", nlyr(image)))
cat(sprintf("      Image size  : %d x %d\n", nrow(image), ncol(image)))


# -----------------------------------------------------------------------------
# 2. Extract training samples
# -----------------------------------------------------------------------------
cat("[2/5] Extracting training samples...\n")

# Stack image + labels into one SpatRaster for joint extraction
# i.e. removing any pixels from the stack that we don't have a training class for
stack  <- c(image, labels)
df     <- as.data.frame(stack, na.rm = TRUE)

# Last column is the label; rename for clarity
names(df)[ncol(df)] <- "class"

# Remove background (class == 0)
df <- df[df$class > 0, ]
df$class <- as.factor(df$class)

cat(sprintf("      Training pixels : %s\n", format(nrow(df), big.mark = ",")))
cat(sprintf("      Classes found   : %s\n", paste(levels(df$class), collapse = ", ")))


# -----------------------------------------------------------------------------
# 3. Train / test split & Random Forest
# -----------------------------------------------------------------------------
cat(sprintf("[3/5] Training Random Forest (ntree = %d)...\n", N_TREES))

set.seed(42)
train_idx <- createDataPartition(df$class, p = 1 - TEST_SPLIT, list = FALSE)
train_df  <- df[ train_idx, ]
test_df   <- df[-train_idx, ]

rf_model <- randomForest(
  class ~ .,
  data       = train_df,
  ntree      = N_TREES,
  mtry       = MTRY,
  importance = TRUE
)

cat(sprintf("      OOB error rate  : %.2f%%\n", rf_model$err.rate[N_TREES, "OOB"] * 100))


# -----------------------------------------------------------------------------
# 4. Validate on held-out test set
# -----------------------------------------------------------------------------
cat("[4/5] Validating...\n")

preds     <- predict(rf_model, test_df)
cm        <- confusionMatrix(preds, test_df$class)

cat(sprintf("      Overall accuracy : %.4f\n", cm$overall["Accuracy"]))
cat(sprintf("      Kappa            : %.4f\n", cm$overall["Kappa"]))
cat("\n--- Confusion Matrix ---\n")
print(cm$table)

# Feature importance plot (saved to file)
imp_path <- sub("\\.tif$", "_feature_importance.png", OUTPUT_PATH)
png(imp_path, width = 900, height = 500)
varImpPlot(rf_model, main = "Random Forest — Feature Importance")
dev.off()
cat(sprintf("      Importance plot  : %s\n", imp_path))


# -----------------------------------------------------------------------------
# 5. Classify the full image & write output
# -----------------------------------------------------------------------------
cat("[5/5] Classifying full image...\n")

classified <- predict(image, rf_model, type = "response", na.rm = TRUE)

# Write as integer GeoTIFF
writeRaster(classified, OUTPUT_PATH, datatype = "INT1U", overwrite = TRUE)
cat(sprintf("\n✓ Classified raster saved : %s\n", OUTPUT_PATH))

# Quick preview plot
preview_path <- sub("\\.tif$", "_preview.png", OUTPUT_PATH)
png(preview_path, width = 800, height = 800)
plot(classified, main = "Classified Output", col = rainbow(nlevels(df$class)))
dev.off()
cat(sprintf("✓ Preview saved           : %s\n", preview_path))

end.time <- Sys.time()
print(end.time - start.time)
cat(sprintf("\nRun complete for the following parameters:
              mtry: %i
              NTREE: %i
              %i predictor layers", 
            MTRY,
            N_TREES,
            nlyr(stack) - 1
))
cat(sprintf("\nResults:
              OOB error rate: %.2f%%
              Overall accuracy : %.4f
              Kappa: %.4f", 
            rf_model$err.rate[N_TREES, "OOB"],
            cm$overall["Accuracy"],
            cm$overall["Kappa"]
))

# next steps: generalise with ArcGIS pipeline, then 'Raster layer unique values report' toolbox in QGIS