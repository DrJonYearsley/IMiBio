# Prepare LANDSAT images for cloud detection and time series analysis
#
# This file:
#    1. Read in the metadata for the required landsat scenes
#    2. Creates a file with sun altitude and azimuth
#    2. Creates a list of landsat products required
#    3. Orders the products by date
#    4. Checks that the product files are available in the folder
#    5. Imports 5 bands for each landsat scene (blue, green, red, nir, swir)
#    6. (disabled) Scales the reflectances and removes the bias
#    7. Writes all the bands from a scene into a single geotiff
#    8. create the water mask
#    9. create sun angle.txt
#
#
# Jon Yearsley & Morgane Beziau
# June 2022
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rm(list=ls())
setwd('~/git_repos/IMiBio/Landsat')

library(terra)
library(ggplot2)

year = 1999     # Year of interest
landsat = 5     # Either 4, 5 (landsat 4/5 TM), 7 (landsat 7 ETM), 8, 9 (landsat 8/9)

dataDir = '~/Research/IMiBio/Landsat/1999_data_path224_row78'
metadataFiles = c('./LC08_09_OT_C2_L2_METADATA_Path224_Row78.csv', 
                  './LC07_ETM_C2_L2_METADATA_Path224_Row78.csv', 
                 './LC04_05_TM_C2_LR_METADATA_Path224_Row78.csv')
outputDir = '~/Research/IMiBio/Landsat/preprocess'
prefix = 'Landsat_224078'

# Lat-long coords for cropping region
e = ext(-54.9, -54.0, -25.9, -25.38)  # Define an extent around IMiBio
# e = ext(-54.7, -54.3, -25.8, -25.5)  # A smaller extent (zoomed in to IMiBio)


# Definition of bands for landsat 8/9, landsat 7 and landsat 4/5
if (landsat%in%c(4,5)) {
  # Landsat 4-5 band definition
  landsat_bands = list(blue="B1", green="B2", red="B3", nir="B4", swir="B5")
} else if (landsat==7) {
  # Landsat 7 band definition
  landsat_bands = list(blue="B1", green="B2", red="B3", nir="B4", swir="B5")
} else if (landsat%in%c(8,9)) {
  # Landsat 8 band definition
  landsat_bands = list(blue="B2", green="B3", red="B4", nir="B5", swir="B6")
} else {
  print('landsat variable not recognised')
}



# Scaling and offset for Landsat level 2 products
# https://www.usgs.gov/faqs/how-do-i-use-scale-factor-landsat-level-2-science-products
landsat_collection2 = list(fill=0, gain=0.0000275, offset=-0.2)
# landsat_collection1 = list(fill=-9999, gain=0.0001, offset=0)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Read metadata ------------

# Read all data and then subset

# Define columns to extract
extractCols = c("Display.ID", "Ordering.ID", "Landsat.Product.Identifier.L2", 
                "Date.Acquired" , "WRS.Path" ,"WRS.Row", "Start.Time" , "Stop.Time", 
                "Day.Night.Indicator",  "Land.Cloud.Cover" , 
                 "Sun.Elevation", "Sun.Azimuth", "Satellite" )


for (f in metadataFiles) {
  tmp = read.csv(file=f)
  
  # Rename some columns
  colNames = names(tmp)
  colNames[colNames%in%c('Sun.Elevation.L1', 'Sun.Elevation.L0RA')] = 'Sun.Elevation'
  colNames[colNames%in%c('Sun.Azimuth.L1', 'Sun.Azimuth.L0RA')] = 'Sun.Azimuth'
  names(tmp) = colNames
  
  
  if (f==metadataFiles[1]) {
    meta = tmp[, extractCols]
  } else {
    meta = rbind(meta, tmp[,extractCols])
  }
}

# convert dates into POSIX dates
meta$Date = as.Date(meta$Date.Acquired, format="%Y/%m/%d")



# Create subset of metadata for the files we want to process and put them in date order
meta_sub = subset(meta,  format(Date,"%Y")==year )


# Keep only required satellite data
if (landsat%in%c(4,5)) {
  meta_sub = meta_sub[grepl("[4,5]", meta_sub$Satellite),]
} else if (landsat%in%c(7)) {
  meta_sub = meta_sub[grepl("[7]", meta_sub$Satellite),]
} else if (landsat%in%c(8,9)) {
  meta_sub = meta_sub[grepl("[8,9]", meta_sub$Satellite),]
}

# Sort into date order
meta_sub = meta_sub[order(meta_sub$Date),]


# List data to be ordered
meta_sub$Ordering.ID


# Create a file that can be used to order the landsat data using the 
# USGS system (https://earthexplorer.usgs.gov/settings?page=scenelist)

# # order_list = paste0(meta_sub$Landsat.Product.Identifier.L2,'_SR_',landsat8_bands,'.TIF')
# order_list = meta_sub$Landsat.Product.Identifier.L2
# order_file = file("USGS_SceneList_dataset.txt", "w")
# writeLines(text="#LANDSAT_8_C2|DISPLAY_ID", con=order_file)
# writeLines(text=order_list, con=order_file)
# close(order_file)









# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Add year onto prefix

prefix = paste0(prefix, '_Year',year)



# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Write sun alt and azimuth -------
sun_alt_az = data.frame(alt = 90 - meta_sub$Sun.Elevation,
                        azimuth = meta_sub$Sun.Azimuth)




sun_filename = paste0(prefix,'_sun_angle.txt')
write.table(sun_alt_az, 
            file=file.path(outputDir,sun_filename), 
            sep=' ', 
            row.names=FALSE, 
            col.names=FALSE)








# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ++++++++++++++++++++++++++++++++++++++++++++
# Write a file with auxilliary data ----------

# Create a file with cloud cover, daytime, day of year info
aux_data = data.frame(doy=format(meta_sub$Date,"%j"),
                      cloud.cover=meta_sub$Land.Cloud.Cover,
                      daytime = meta_sub$Day.Night.Indicator,
                      StartTime = meta_sub$Start.Time,
                      StopTime = meta_sub$Stop.Time)



aux_filename = paste0(prefix,'_auxillary.csv')
write.table(aux_data, 
            file=file.path(outputDir,aux_filename), 
            sep=',', 
            row.names=FALSE, 
            col.names=TRUE)














# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Import raster data for all geotiffs in the current directory ------

# Find raster files matching a pattern in the meta data subset
matchStr = substr(meta_sub$Landsat.Product.Identifier.L2[1], start = 5, stop=21)

# List all files (from band 2) that have the identifier prefix
files = list.files(path=dataDir, 
                   pattern=paste0("^[[:graph:]]{4}",matchStr,"[[:graph:]]+_SR_B2.TIF$"),
                   full.names = FALSE)

# Create filename base by removing band 2 and .TIF 
file_base = unlist(strsplit(files, split='_SR_B2.TIF', fixed=TRUE))

# Sort into date order
file_dates = unlist(lapply(strsplit(file_base, split='_',fixed=T), FUN=function(x){x[4]}))

# Convert date text into dates
file_dates = as.Date(file_dates, format='%Y%m%d')

# Make sure files are in date order
file_base = file_base[order(file_dates)]




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



# Keep only dates in meta_sub dataframe
file_base = file_base[file_base %in% meta_sub$Landsat.Product.Identifier.L2]



# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# If all files present import them, crop them and save them into  geotifs

# Create a polygon for cropping (define using WGS84)
geom_mat = rbind(c(e[1],e[3]), c(e[1],e[4]), c(e[2],e[4]),c(e[2],e[3]))
target <- vect(x=geom_mat, type='polygons', crs="epsg:4326")



# Loop around all the files and import raster info, then save in separate files
for (f in file_base) {
  
  print(f)
  
  # Create list of files to import in required order
  file_import_list = file.path(dataDir,paste0(f,'_SR_',landsat_bands,'.TIF'))


  # Import all bands (blue, green red, nir, swir)
  tmp = rast(x = file_import_list)
  names(tmp) = paste0(names(tmp),'_',c('blue','green','red','nir','swir'))

  if (f==file_base[1]) {
    target_square = project(target, tmp)
  }
  
  # Crop the data (Landsat data should be in CRS EPSG:32621 for path 224, row 78)
  tmp_cropped = crop(tmp, target_square)
  
  
  # # Scale the data
  # tmp_cropped[tmp_cropped==landsat_collection2$fill] = NA    # Set missing value
  # tmp_cropped= tmp_cropped*landsat_collection2$gain + landsat_collection2$offset  
  
  if (f==file_base[1]) {
    cropped_stacked = tmp_cropped
  } else {
    # Reset extent in case scene has corner missing
    if (ext(tmp_cropped)!=ext(cropped_stacked)) {
      tmp_cropped = extend(tmp_cropped, ext(cropped_stacked))
    }
    
    cropped_stacked = c(cropped_stacked, tmp_cropped)
  }
}


# Save the stacked raster to one file
writeRaster(cropped_stacked, 
            file=file.path(outputDir,paste0(prefix,'_CROPPED_STACKED.TIF')), 
            overwrite=TRUE,
            datatype='INT2S')








# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Write a water mask ---------


# Pick a cloud free scene
scene_id = which.min(meta_sub$Land.Cloud.Cover)

scene = cropped_stacked[[c(1:5)+(scene_id-1)*5]]
names(scene) = c('blue','green','red','nir','swir')


# Calculate NDWI (Normalized Difference Water Index )
ndwi = (scene$green- scene$nir) / (scene$green+scene$nir)

# Modified Normalized Difference Water Index 
mndwi = (scene$green- scene$swir) / (scene$green+scene$swir)


# Calculate NRCWI (None-Radiation-Calibration Water Index)
# (from paper https://doi.org/10.1080/10106049.2018.1552324).
# Values greater than 0 are water
nrcwi = (scene$green - 0.33*scene$nir  - 0.67*scene$swir) / (scene$green + scene$swir)
# plot(nrcwi)



# Create mask with 1 over land
mask = nrcwi<0
names(mask) = 'land'

# # Reclassify a land based on adjacent pixels 
# # and save to a cleaned raster (mask2)
# water = unlist(cells(mask,y=0))
# neighInd = adjacent(mask, cells=water, directions="16", include=FALSE) 
# mask2 = mask
# for (t in 1:length(water)) {
#   ind = !is.na(neighInd[t,])
#   if (sum(mask[neighInd[t,ind]]==FALSE)<=4) {
#     mask2[water[t]] = TRUE   # Set pixel to TRUE (i.e. land)
#   }
# }



# Visualise the two masks
# plot(mask2)
# plot(mask)


# Save the mask using signed 16bit integers (i.e. 2 byte integers)
writeRaster(mask, 
            filename=file.path(outputDir,paste0(prefix,'_WATERMASK.TIF')), 
            datatype='INT2S', 
            overwrite=TRUE)

