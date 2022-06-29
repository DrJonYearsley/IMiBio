# Example of opening a ncdf file
#
# The script:
#     Extracts data from a netcdf file
#     Creates a set of spatial points from thie info in the file
#     Interpolates the data from the file onto a regular grid (using 
#         nearest-neighbour interpolation)
#
# Jon Yearsley
# June 2022
# +++++++++++++++++++++++++++++++++++++++

library(ncdf4)


setwd('~/Downloads/')



# Open the file
f = nc_open('CLDMSK_L2_MODIS_Aqua.A2021002.1740.001.2021003165059.nc')

# Display the name of the 1st variable (change index to see other names)
f$var[[1]]$name




# Retrieve data for specific variables from the file
lat = ncvar_get(f, varid="geolocation_data/latitude")
lon = ncvar_get(f, varid="geolocation_data/longitude")

cloud = ncvar_get(f, varid= "geophysical_data/Clear_Sky_Confidence")

mask = ncvar_get(f, varid=  "geophysical_data/Cloud_Mask")
maskint = ncvar_get(f, varid=  "geophysical_data/Integer_Cloud_Mask")
QA = ncvar_get(f, varid=  "geophysical_data/Quality_Assurance")

# Close the ncdf file
nc_close(f)



extractbit = function(x,n1) {
  # Extract the nth bit from the number x
  return(bitwAnd(bitwShiftR(x,n1),1))
}



# Extract specific data from the variables (still to be checked)
water = as.numeric(extractbit(mask,7)==0 & extractbit(mask,8)==0)


# ++++++++++++++++++++++++++++++++++++++++++++++++++++


library(terra)
library(gstat)

# Create a lat lon object from the mask data 
lonlat = data.frame(lon=c(lon),lat=c(lat), mask=c(maskint), water=c(water))

# Create subset of longitudes and latitudes
e = ext(-70, -60, -39, -30)  # Define an extent
lonlat_sub = subset(lonlat, lon>e[1] & lon<e[2] & lat>e[3] & lat<e[4])


# # create a spatial vector object from the data frame
# tmp = vect(lonlat_sub, crs="epsg:4326")
# plot(tmp, y="mask", type="classes")
# plot(tmp, y="water", type="classes")



# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Create a regular grid of values by performing nearest-neighbour interpolation

# Create a target grid (spatial raster defining spatial resolution and extent)
# CRS is WGS84 so resolution is in degrees
# 0.1 degrees is about 10 km
target <- rast(resolution=0.1, extent=e, crs="epsg:4326")

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

# plot the resulting spatial raster
plot(mask_nninterp, type="classes")

