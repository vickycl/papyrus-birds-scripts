#installing PopScape
install.packages("devtools")
library(devtools)

install.packages("remotes")
library(remotes)

devtools::install_github("ilyamaclean/PopScape")
#asks to update a few packages, i only updated Rcpp (1.0.14 -> 1.1.1-1.1) [CRAN]

library(PopScape)

#=====================METAPOPULATION MODEL=========================
setwd("~/Papyrus_Proj/RCode")

#load libraries 
library(PopScape)
library(terra)

landcover <- rast("../PapyrusData/DS Classifications/FI_CS_20m_4326_echocl/FI_CS_20m_4326_echocl.tif") #crs must be in meters

#checks 
summary(landcover) #want min/max 0/1, but is 1-9
print(crs(landcover, describe = TRUE)) #in WSG84
freq(landcover)

#Reproject for meters
landcover_projected <- project(landcover, "EPSG:32736", method = "near")


# Creating habitat rasters for birds ====================================

# GSW and PC - papyrus only + aggregate to bigger resolution
papyrus_only <- landcover_projected == 2
papyrus_100m <- aggregate(papyrus_only, fact = 5, fun = mean)

summary(values(papyrus_100m), na.rm = TRUE)

# WWW and PYW - papyrus + broad wetland
broad_wetland <- (landcover_projected == 2 | landcover_projected == 3)
broad_wetland_100m <- aggregate(broad_wetland, fact = 5, fun = mean)

summary(values(broad_wetland_100m), na.rm = TRUE)

# CC - papyrus + broad wetland + agricultural wetland
agri_wetland <- (landcover_projected == 2 | landcover_projected == 3 | landcover_projected == 1)
agri_wetland_100m <- aggregate(agri_wetland, fact = 5, fun = mean)

summary(values(agri_wetland_100m), na.rm = TRUE)


# Non-habitat => NA (not 0)
papyrus_masked <- papyrus_100m
papyrus_masked[papyrus_masked == 0] <- NA
plot(papyrus_masked)

broad_masked <- broad_wetland_100m
broad_masked[broad_masked == 0] <- NA
plot(broad_masked)

agri_masked <- agri_wetland_100m
agri_masked[agri_masked == 0] <- NA
plot(agri_masked)


# Confirm non-zero pixels became NA
summary(values(papyrus_masked), na.rm = TRUE)
summary(values(broad_masked), na.rm = TRUE)
summary(values(agri_masked), na.rm = TRUE)
# Min should now be > 0, all zeros gone



#=============================================================================
#i just want to double check what the output is with table 2 ygamma value!
# y value for GSW => this gives an unrealistic 99.9% occupancy 
gsw_simulation <- MetaPopSim( 
  habsuit    = papyrus_masked,
  mu         = 0.012,      
  x          = 0.864,         
  alpha      = 0.204,     
  ygamma     = 226.017,     # table 2 value (i want to double check this doesnt work still)
  timesteps  = 100,   
  aos        = 0.1,   
  minden     = 0.01,     
  maxden     = 20,       
  asprob     = TRUE      
)

plot(gsw_simulation, main = "Greater Swamp-Warbler Baseline Simulation")


#calibration for ygamma GSW=========================================================

# GSW - papyrus only
# GSW target: 0.531 - 2015 observed occupancy

gamma_GSW <- c(1e5, 5e5, 1e6, 5e6, 1e7, 1e8)
gamma_GSW <- c(6e5, 7e5, 8e5, 9e5)
gamma_GSW <- c(9.0e5, 9.1e5, 9.2e5, 9.3e5)

for(g in gamma_GSW){
  occo <- MetaPopSim(papyrus_masked, 
                     mu = 0.012, x = 0.864, alpha = 0.204,
                     ygamma = g, aos = 0.1, timesteps = 100,
                     minden = 0.01, maxden = 20, asprob = TRUE)
  mean_occ <- mean(values(occo), na.rm = TRUE)
  cat("gamma =", g, "| mean occupancy =", round(mean_occ, 3), "\n")
}

# GSW baseline simulation with calibrated gamma 
gsw_simulation <- MetaPopSim( 
  habsuit    = papyrus_masked,
  mu         = 0.012,      
  x          = 0.864,         
  alpha      = 0.204,     
  ygamma     = 9e5,     # calibrated gamma for GSW
  timesteps  = 100,   
  aos        = 0.1,   
  minden     = 0.01,     
  maxden     = 20,       
  asprob     = TRUE      
) #run this later... 

plot(gsw_simulation, main = "Greater Swamp-Warbler Baseline Simulation")

mean_occ <- mean(values(gsw_simulation), na.rm = TRUE)
cat("Mean occupancy:", round(mean_occ, 3), "\n")
cat("Target occupancy:", 0.531, "\n")
cat("Difference:", round(abs(mean_occ - 0.531), 3), "\n")



#calibration for ygamma PC===================================================

# PC - papyrus only
# PC target 0.170

gamma_PC <- c(3.2e7, 3.4e7, 3.6e7, 3.8e7)

for(g in gamma_PC){
  occo <- MetaPopSim(papyrus_masked, 
                     mu = 0.012, x = 0.935, alpha = 0.190,
                     ygamma = g, aos = 0.1, timesteps = 100,
                     minden = 0.01, maxden = 20, asprob = TRUE)
  mean_occ <- mean(values(occo), na.rm = TRUE)
  cat("gamma =", g, "| mean occupancy =", round(mean_occ, 3), "\n")
}


pc_simulation <- MetaPopSim( 
  habsuit    = papyrus_masked,
  mu         = 0.012,      
  x          = 0.935,         
  alpha      = 0.190,     
  ygamma     = 3.6e+07,       # calibrated gamma for PC
  timesteps  = 100,   
  aos        = 0.1,   
  minden     = 0.01,     
  maxden     = 20,       
  asprob     = TRUE      
)

plot(pc_simulation, main = "Papyrus Canary Baseline Simulation")

mean_occ <- mean(values(pc_simulation), na.rm = TRUE)
cat("Mean occupancy:", round(mean_occ, 3), "\n")
cat("Target occupancy:", 0.170, "\n")
cat("Difference:", round(abs(mean_occ - 0.170), 3), "\n")


# WWW - Papyrus ====================================================================
# WWW target 0.223

gamma_WWW <- c(2e8, 5e8, 1e9, 2e9, 5e9)
gamma_WWW <- c(1.1e9, 1.2e9, 1.3e9, 1.4e9, 1.5e9)
gamma_WWW <- c(1e9, 1.1e9, 1.2e9, 1.3e9, 1.4e9)

for(g in gamma_WWW){
  occo <- MetaPopSim(papyrus_masked, 
                     mu = 0.059, x = 0.488, alpha = 0.021,
                     ygamma = g, aos = 0.1, timesteps = 100,
                     minden = 0.01, maxden = 20, asprob = TRUE)
  mean_occ <- mean(values(occo), na.rm = TRUE)
  cat("gamma =", g, "| mean occupancy =", round(mean_occ, 3), "\n")
}

# testing the level of stochasticity (i was getting very varied values for y here)

# Test repeatability at gamma = 1.1e9
for(i in 1:5){
  occo <- MetaPopSim(papyrus_masked, 
                     mu = 0.059, x = 0.488, alpha = 0.021,
                     ygamma = 1.1e9, aos = 0.1, timesteps = 100,
                     minden = 0.01, maxden = 20, asprob = TRUE)
  mean_occ <- mean(values(occo), na.rm = TRUE)
  cat("Run", i, "| mean occupancy =", round(mean_occ, 3), "\n")
}
#output (colonisation probability): mean of 0.212, with range of 0.202–0.231
#aim is 0.223

www_simulation <- MetaPopSim( 
  habsuit    = papyrus_masked,
  mu         = 0.059,      
  x          = 0.488,         
  alpha      = 0.021,     
  ygamma     = 1.1e9,       # calibrated gamma for WWW
  timesteps  = 100,   
  aos        = 0.1,   
  minden     = 0.01,     
  maxden     = 20,       
  asprob     = TRUE      
)

plot(www_simulation, main = "White-winged Swamp-warbler Baseline Simulation")

mean_occ <- mean(values(www_simulation), na.rm = TRUE)
cat("Mean occupancy:", round(mean_occ, 3), "\n")
cat("Target occupancy:", 0.223, "\n")
cat("Difference:", round(abs(mean_occ - 0.223), 3), "\n")


# got a super low average occupancy of 0.048
# Run WWW baseline 5 times and average
www_runs <- numeric(5)
for(i in 1:5){
  occo <- MetaPopSim(papyrus_masked,
                     mu = 0.059, x = 0.488, alpha = 0.021,
                     ygamma = 1.1e9, aos = 0.1, timesteps = 100,
                     minden = 0.01, maxden = 20, asprob = TRUE)
  www_runs[i] <- mean(values(occo), na.rm = TRUE)
  cat("Run", i, "| mean occupancy =", round(www_runs[i], 3), "\n")
}

cat("\nAverage across 5 runs:", round(mean(www_runs), 3), "\n")
cat("Target occupancy:", 0.223, "\n")
cat("Difference:", round(abs(mean(www_runs) - 0.223), 3), "\n")

#Simulated occupancy across 5 replicate runs: 0.222, 0.231, 0.230, 0.218, 0.063 (outlier - likely stochastic stronghold collapse)
#Mean (all 5 runs): 0.193, difference: 0.030
#Mean (excluding outlier): 0.225, difference: 0.002



# PYW - + broad wetland ==============================================================
# PYW target 0.196

gamma_PYW <- c(5e13, 7e13, 1e14, 1.5e14, 2e14)
gamma_PYW <- c(1.1e14, 1.2e14, 1.3e14, 1.4e14) #1.1e14, 1.2e14


for(g in gamma_PYW){
  occo <- MetaPopSim(broad_masked, mu=0.041, x=1.340, alpha=0.001,
                     ygamma=g, aos=0.1, timesteps=100,
                     minden=0.01, maxden=20, asprob=TRUE)
  mean_occ <- mean(values(occo), na.rm=TRUE)
  cat("gamma =", g, "| mean occupancy =", round(mean_occ, 3), "\n")
}


# checking if its as variable as WWW 
pyw_runs <- numeric(5)
for(i in 1:5){
  occo <- MetaPopSim(broad_masked, 
                     mu=0.041, x=1.340, alpha=0.001,
                     ygamma=1.2e14, aos=0.1, timesteps=100,
                     minden=0.01, maxden=20, asprob=TRUE)
  pyw_runs[i] <- mean(values(occo), na.rm=TRUE)
  cat("Run", i, "| mean occupancy =", round(pyw_runs[i], 3), "\n")
}

cat("\nAverage across 5 runs:", round(mean(pyw_runs), 3), "\n")
cat("Target occupancy:", 0.196, "\n")
cat("Difference:", round(abs(mean(pyw_runs) - 0.196), 3), "\n")

#Run-to-run variability: very low (0.193-0.196)

#Run PYW simulation
pyw_simulation <- MetaPopSim( 
  habsuit    = broad_masked,
  mu         = 0.041,      
  x          = 1.340,         
  alpha      = 0.001,     
  ygamma     = 1.2e14,     # calibrated gamma for PYW
  timesteps  = 100,   
  aos        = 0.1,   
  minden     = 0.01,     
  maxden     = 20,       
  asprob     = TRUE      
)

plot(pyw_simulation, main = "Papyrus Yellow Warbler Baseline Simulation")

mean_occ <- mean(values(pyw_simulation), na.rm = TRUE)
cat("Mean occupancy:", round(mean_occ, 3), "\n")
cat("Target occupancy:", 0.196, "\n")
cat("Difference:", round(abs(mean_occ - 0.196), 3), "\n")



# CC - + Broad + agri wetland ==============================================================
# CC target 0.506

Sr_CC <- calcconectivity(agri_masked, alpha = 0.070)
summary(values(Sr_CC), na.rm = TRUE)

gamma_CC <- c(6e12, 7e12, 8e12, 9e12)

for(g in gamma_CC){
  occo <- MetaPopSim(agri_masked, mu=0.061, x=0.734, alpha=0.070,
                     ygamma=g, aos=0.1, timesteps=100,
                     minden=0.01, maxden=20, asprob=TRUE)
  mean_occ <- mean(values(occo), na.rm=TRUE)
  cat("gamma =", g, "| mean occupancy =", round(mean_occ, 3), "\n")
}


cc_simulation <- MetaPopSim( 
  habsuit    = agri_masked,
  mu         = 0.061,      
  x          = 0.734,         
  alpha      = 0.070,     
  ygamma     = 9e12,    #calibrated y gamma value for CC
  timesteps  = 100,   
  aos        = 0.1,   
  minden     = 0.01,     
  maxden     = 20,       
  asprob     = TRUE      
)

plot(cc_simulation, main = "Carruthers's Cisticola Baseline Simulation")

mean_occ <- mean(values(cc_simulation), na.rm = TRUE)
cat("Mean occupancy:", round(mean_occ, 3), "\n")
cat("Target occupancy:", 0.506, "\n")
cat("Difference:", round(abs(mean_occ - 0.506), 3), "\n")



#NEXT: run models with 2025 habitat classification for updated baseline


#=============================================================================================
#for time series - trying this now :)

library(PopScape)
library(terra)


# Run simulation at different endpoints to get a trend
timestep_checkpoints <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
occupancy_trend <- numeric(length(timestep_checkpoints))

for(i in seq_along(timestep_checkpoints)){
  occo <- MetaPopSim(papyrus_masked,
                     mu = 0.012, x = 0.864, alpha = 0.204,
                     ygamma = 9e5, aos = 0.1,
                     timesteps = timestep_checkpoints[i],
                     minden = 0.01, maxden = 20, asprob = TRUE)
  occupancy_trend[i] <- mean(values(occo), na.rm = TRUE)
}

plot(timestep_checkpoints, occupancy_trend,
     type = "l", lwd = 2, col = "darkgreen",
     xlab = "Timestep", ylab = "Mean occupancy probability",
     main = "Greater Swamp-Warbler Occupancy Trend",
     ylim = c(0, 1))










