# Prepare LANDSAT images for cloud detection and time series analysis
#
# This file:
#    1. Read in the metadata for the required landsat scenes
#    2. Creaes a list of landsat products required
#    3. Orders the products by date
#    4. Checks that the product files are available in the folder
#    5. Imports 5 bands for each landsat scene (blue, green, red, nir, swir)
#    6. Scales the reflectances and removes the bias
#    7. Writes all the bands from a scene into a single geotiff
#
# Jon Yearsley
# June 2022
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rm(list=ls())

# setwd('/media/jon/MODIS_data/Landsat/')
setwd('~/git_repos/IMiBio/Landsat/')

library(terra)
library(ggplot2)

# Lat-long coords for cropping region
e = ext(-54.9, -54.0, -25.9, -25.38)  # Define an extent around IMiBio

# e = ext(-54.2, -54, -27, -27.1)  # Another extent (not near IMiBio)


# Definition of bands for landsat 8/9, landsat 7 and landsat 4/5
landsat8_bands = list(blue="B2", green="B3", red="B4", nir="B5", swir="B6")
landsat7_bands = list(blue="B1", green="B2", red="B3", nir="B4", swir="B5")
landsat45_bands = list(blue="B1", green="B2", red="B3", nir="B4", swir="B5")


# Scaling and offset for Landsat levl 2 products
# https://www.usgs.gov/faqs/how-do-i-use-scale-factor-landsat-level-2-science-products
landsat_collection2 = list(fill=0, gain=0.0000275, offset=0)
landsat_collection1 = list(fill=-9999, gain=0.0001, offset=0)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Read metadata ------------
meta = read.csv(file="LC08_09_OT_C2_L2_METADATA_Path224_Row78.csv")

# convert dates into POSIX dates
meta$Date = as.Date(meta$Date.Acquired, format="%Y/%m/%d")



# Create subset of metadata for the files we want to process and put them in date order
meta_sub = subset(meta, Satellite==8 & format(Date,"%Y")==2019 )

# Sort into date order
meta_sub = meta_sub[order(meta_sub$Date),]


# List data to be ordered
meta_sub$Ordering.ID


# Create a file that can be used to order the landsat data using the 
# USGS system (https://earthexplorer.usgs.gov/settings?page=scenelist)

# order_list = paste0(meta_sub$Landsat.Product.Identifier.L2,'_SR_',landsat8_bands,'.TIF')
order_list = meta_sub$Landsat.Product.Identifier.L2
order_file = file("USGS_SceneList_dataset.txt", "w")
writeLines(text="#LANDSAT_8_C2|DISPLAY_ID", con=order_file)
writeLines(text=order_list, con=order_file)
close(order_file)



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Visualise metadata ------


# for all years
ggplot(data=meta,
       aes(x=Date,
           y=Land.Cloud.Cover)) +
  geom_point() + 
  theme_bw()

# Visualise metadata for one year
ggplot(data=subset(meta, format(Date,'%Y')=='2017'),
       aes(x=Date,
           y=Land.Cloud.Cover)) +
  geom_point() + 
  theme_bw()








# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Import raster data for all geotiffs in the current directory ------

# Fine raster files matching a pattern in the meta data subset
prefix = strsplit(meta_sub$Landsat.Product.Identifier.L2[1],'_',fixed=TRUE)[[1]][1]

# List all files (from band 6) that have the identifier prefix
files = list.files(path=".", 
                   pattern=paste0("^",prefix,"[[:graph:]]+_SR_B6.TIF$"),
                   full.names = FALSE)

# Create filename base by removing band 6 and .TIF 
file_base = unlist(strsplit(files, split='_SR_B6.TIF', fixed=TRUE))




# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Check all the files in the metadata subset have been found
if (all(meta_sub$Landsat.Product.Identifier.L2 %in% file_base)) {
  print('All expected files found')
} else {
  missing_files = meta_sub$Landsat.Product.Identifier.L2[!(meta_sub$Landsat.Product.Identifier.L2%in%file_base)]
  print(paste('Cannot find files for products',missing_files))
  print('These need to be downloaded into this folder')
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# If all files present import them, crop them and save them into  geotifs

# Create a polygon for croping
geom_mat = rbind(c(e[1],e[3]), c(e[1],e[4]), c(e[2],e[4]),c(e[2],e[3]))
target <- vect(x=geom_mat, type='polygons', crs="epsg:4326")



# Loop around all the files and import raster info, then save in separate files
for (f in file_base) {
  # Create list of files to import in required order
  file_import_list = paste0(f,'_SR_',landsat8_bands,'.TIF')
  
  # Import all bands (blue, green red, nir, swir)
  stacked_data = rast(x = file_import_list)
  
  # Crop the data
  cropped_stack = crop(stacked_data, project(target, stacked_data))
  
  # Scale the data
  cropped_stack[cropped_stack==landsat_collection2$fill]=NA    # Set missing value
  cropped_stack = cropped_stack*landsat_collection2$gain + landsat_collection2$offset  
  
  
  # Save the stacked raster to one file
  writeRaster(cropped_stack, file=paste0('CROPPED_',f,'_STACKED.TIF'), overwrite=TRUE)
}





# 
# extractbit = function(x,n1) {
#   # Extract the nth bit from the number x
#   return(bitwAnd(bitwShiftR(x,n1),1))
# }
# 
# 
# extractbit2 = function(x,n) {
#   floor((x-2^(n+1)*floor(x/2^(n+1)))/2^n)
# }
# 

