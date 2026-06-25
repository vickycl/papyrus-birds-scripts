
setwd("~/Papyrus_Proj/RCode")

install.packages("devtools")
library(devtools)

install.packages("remotes")
library(remotes)

devtools::install_github("ilyamaclean/PopScape")
#asks to update a few packages, i only updated Rcpp (1.0.14 -> 1.1.1-1.1) [CRAN]

library(PopScape)

#=====================METAPOPULATION MODEL=========================
#load libraries 
library(PopScape)
library(terra)

# Load  new spatial dataset 
landcover <- rast("../PapyrusData/DS Classifications/FI_CS_20m_4326_echocl/FI_CS_20m_4326_echocl.tif")#crs must be in meters

summary(landcover) #want min/max 0/1, but is 1-9
print(crs(landcover, describe = TRUE)) #in WSG84

#Reproject for meters
landcover_projected <- project(landcover, "EPSG:32736", method = "bilinear")

#aggregate to larger resolution
landcover_100m <- aggregate(landcover_projected, fact = 5, fun = mean)

#Threshold (1=true, 0=false). adjust this accordingly to RandomForest hab values 
binary_patches <- landcover_100m <= 2.5 #selecting classes 1 and 2 from multispec
plot(binary_patches)
print(binary_patches)


#Add parameters  into the model (Table 2 from Donaldson et al, 2021)
gsw_simulation <- MetaPopSim( 
  habsuit = binary_patches,
  mu = 0.012,        # Greater Swamp-Warbler mu
  x = 0.864,         # Greater Swamp-Warbler x
  alpha = 0.204,     # Greater Swamp-Warbler alpha
  ygamma = 226.017,  # Greater Swamp-Warbler y
  timesteps = 100,   # 100 years 
  aos = 0.0000001,   #Area-Occupancy Scaling Coefficient (0.1)
  minden = 0.01,     #Minimum Density Threshold
  maxden = 20,       #Maximum Density Cap
  asprob = TRUE      #should gives heat map showing the likelihood of a patch remaining occupied over time.
)

#Plots 100th year 
plot(gsw_simulation, main = "Greater Swamp-Warbler Simulation")

#i'm not getting a gradient of occupancy probabilities - just showing yellow = 1

#=============================================================================================
#for time series - haven't run this yet 

library(PopScape)
library(terra)

gsw_simulation <- MetaPopSim( 
  habsuit   = #landcover_filename, 
    mu        = 0.012,        
  x         = 0.864,         
  alpha     = 0.204,     
  ygamma    = 226.017,  
  timesteps = 100, 
  aos       = 0.1,          
  minden    = 0.01,     
  maxden    = 20,       
  asprob    = FALSE           #set to get 100 individual yearly layers
)


#calculate occupied cells per year 
#Since each layer is a year of 1s and 0s, summing each layer gives total occupied cells
yearly_counts_df <- global(gsw_simulation, fun = "sum", na.rm = TRUE)

#Extract the values out of the data frame into a vector
yearly_occupied_pixels <- yearly_counts_df[, 1]

#define x-axis = match to the number of layers/timesteps in simulation
total_years <- nlyr(gsw_simulation)
timeline <- 1:total_years

#plot occupancy overtime
plot(timeline, yearly_occupied_pixels, 
     type = "l", 
     lwd = 3, 
     col = "darkgreen",
     xlab = "Simulation Year", 
     ylab = "Total Occupied Grid Cells (100m)",
     main = "Greater Swamp-Warbler 100-Year Occupancy Trend",
     cex.axis = 1.2, 
     cex.lab = 1.2)


