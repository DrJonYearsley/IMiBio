# Example of using terra data
# 
# Import hdf file
# Define a square in lat and long (crs=WGS84)
# Convert square to MODIS crs
# Crop the Modis data to the square
#
# Jon Yearsley
# May 2022
#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# Setup commands
rm(list=ls())
setwd('~/Research/IMiBio/')
library(terra)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Import MODIS data ---------

# File to be imported
file = 'Aqua/MYD13Q1.006/2018.10.24/MYD13Q1.A2018297.h17v03.006.2018316104437.hdf'


# Read in a modis hdf file
modis = rast(file)

# Plot the NDVI layer from the file
plot(modis$`"250m 16 days NDVI"`)







# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Define a square around Dublin ---------

# Define a square in lat and long that is over Dublin 
# (just use diagonal corners)
latitude_sq = c( 53.129, 53.491)
longitude_sq = c(-6.590,-6.000)
lonlat <- cbind(id=1, part=1, longitude_sq, latitude_sq)


# Set the CRS to WGS84
dublin_sq = vect(lonlat, type="lines", crs="epsg:4326")

# Reproject this square onto MODIS CRS
dublin_modis = project(dublin_sq, crs(modis))









# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Crop the MODIS data to the square ---------

# Crop modis data to the square around Dublin
modis_cropped = crop(modis, dublin_modis)

# Plot the cropped data for NDVI layer
plot(modis_cropped$`"250m 16 days NDVI"`)
