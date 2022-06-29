#automatic import of files
#Morgane juin 2022


rm( list=ls() )


# library(here)    # JY
library(ncdf4)
library(terra)      #JY
library(gstat)      #JY



# setwd("~/MODIS/cloud.study/datanc")
setwd('~/MEGAsync/Projects/IMiBio')    # JY

# Directory that stores the data
dataDir = "./Data"                     # JY

#define necessary files
files_names <- list.files(path=dataDir, 
                          pattern='.nc$', 
                          full.names = TRUE)  # JY
files_names
files_names[1]

#define files names                                               
nb_files <- length(files_names)
# data_names <- vector("list",length=nb_files)                   # JY
# for (i in 1:nb_files){
# data_names[i] <- strsplit(files_names[i], split=".nc")  
# }


# JY
# Create a target grid (spatial raster defining spatial resolution and extent)
# CRS is WGS84 so resolution is in degrees
# 0.1 degrees is about 10 km
target <- rast(resolution=0.01, extent=c(-55, -54, -26, -25), crs="epsg:4326")


# JY
# Create raster to hold probability of cloudiness. 
# Use target raster as the template
cloud_prob = rast(target, vals = 0)

#assign name and file
for (i in 1:nb_files) {
  f=nc_open(files_names[i])       # JY
  
  # assign(data_names[[i]] ,
  #        ncvar_get(f))
  
  # JY
  # Extract lat, long and cloud mask values
  lat = ncvar_get(f, varid="geolocation_data/latitude")
  lon = ncvar_get(f, varid="geolocation_data/longitude")
  maskint = ncvar_get(f, varid=  "geophysical_data/Integer_Cloud_Mask")
  
  # Create a lat lon object from the mask data 
  lonlat = data.frame(lon=c(lon),lat=c(lat), mask=c(maskint))
  
  # Create subset of longitudes and latitudes
  e = ext(target)  # Define an extent
  lonlat_sub = subset(lonlat, lon>e[1] & lon<e[2] & lat>e[3] & lat<e[4])
  
  # Create a model for nearest-neighbour interpolation
  gs <- gstat(formula = mask ~ 1, 
              data = lonlat_sub, 
              locations=~lon+lat,
              nmax = 1,            # Use single nearest-neighbour
              set = list(idp = 0))
  
  # Perform interpolation onto the target grid
  mask_nninterp <- interpolate(target, 
                               model=gs, 
                               xyNames=c("lon","lat"), 
                               index=1,
                               na.rm=TRUE)  
  
  # Record areas which are cloudy
  cloudy_ind = mask_nninterp <= 1    # JY (needs to be checked)
  cloud_prob[cloudy_ind] = cloud_prob[cloudy_ind] + 1
  
  nc_close(f)     # JY
}

plot(cloud_prob, type="interval")


# #stacking files
# dataset_list <- vector("list",length=nb_files)
# 
# for (i in 1:nb_files) {
#   dataset_list[[i]] <- get(data_names[[i]])
# }
# 
# complete_data <- dataset_list[[1]]
# for (i in 2:nb_files){
#   complete_data <- rbind(complete_data, dataset_list[[i]])


