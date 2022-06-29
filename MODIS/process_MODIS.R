# Script to process MODIS data for Ireland. 
# Before running this script you must have processed the CORINE data (e.g. run the process_CORINE.R script)
#
# This script performs these tasks
#  1. read in MODIS data, 
#  2. crop to Ireland, 
#  3. rescale NDVI and EVI 
#  4. remove poor quality pixels
#  5. reproject data onto CORINE raster
#  6. select pixels correpsonding to pasture (corine code 18)
#  7. reproject onto Irish grid (TM75)
#  8. rescale extent so that grid aligns with hectads (to allow aggregation)
#  9. saves the processed raster data for NDVI, EVI and the quality data

# The final data that is saved to file will have a spatial resolution of approximately 250m 
#        (i.e. the raw MODIS resolution, albeit transformed onto Irish Grid TM75)
# Paul's edits included  (Jan 2017)
# Added year identified to analyse specific years (May 2017) 
# Set negative VI's to zero   (Aug 2017)
# Use a majority rule for aggregating CORINE classes (Aug 2017)
#
# Jon Yearsley Aug 2016

# Edited to not save the quality control flags from the analysis

library(rgdal)
library(raster)
library(gdalUtils)

rm(list=ls())
#setwd("/Libraries/DATA/")

# Directories containing the input and output MODIS data 
inputDir = '../Data/MODIS/e4ftl01.cr.usgs.gov'
outputDir = '../Data/MODIS/MODIS_v6'
outputSuffix = 'pasture'
yearStr = 'A201[0-7]'  # Some text (or reg experession) that specifies the year of the data (e.g. 'A201[0-5]' specifies years 2010-2015)
corineInclude = c(18)  # Specify corine codes to include (pasture = 18, natural grasslands=26, moors and heathland=27)
minQuality = 1 # Minimum quality to use: 0 = use only best quality pixels, 1=use reasonable pixels
scalingFactor = 0.0001 # Scale factor to apply to NDVI and EVI data from MODIS

#coastlinePath = '../Data/_Datasets covering whole island/country.shp'
coastlinePath = '../Data/country.shp'
corinePath = '../Data/CORINE_IE.grd'

# Read in CORINE data
corine = raster(corinePath)
corine.crs = crs(corine)

# Load MODIS data
regexp = paste(yearStr,'[[:graph:]]+.hdf$',sep='')
hdf.files = list.files(path=inputDir,pattern=regexp,recursive=T)

nFiles = length(hdf.files) # Calculate number of files to import

tmp=strsplit(hdf.files,'/')

#satellite = array('Aqua', dim=length(hdf.files))
#satellite[sapply(tmp, FUN=function(x){x[1]}, simplify=TRUE)=='MOLT'] = 'Terra'
satellite<-array(sapply(tmp,'[',1),dim=length(hdf.files)) ####################PAUL EDIT

dates = sapply(tmp, FUN=function(x){x[3]}, simplify=TRUE)
r.date = strptime(dates, format = "%Y.%m.%d", tz = "")

# Define extent of Irleand (roughly) in MODIS CRS
ir = extent(-7.5E5, -3.3E5,5.7E6, 6.17E6)


trim2 <- function(x,values=NA,out="matrix"){
  if(!any(out==c("matrix","raster"))) stop("output must be a matrix or raster")
  if(class(x)=="matrix" & out=="raster") stop("if you supply a matrix, you must use out='matrix'")
  if(class(x)=="RasterLayer") {
    if(out=="raster") { cres <- 0.5*res(x); crs <- projection(x); y <- x }
    x <- matrix(as.array(x),nrow=nrow(x),ncol=ncol(x))
  }
  if(class(x)!="matrix") { stop("x must be a matrix or raster")
  } else {
    r.na <- c.na <- c()
    if (is.na(values)) {
      for(i in 1:nrow(x)) r.na <- c(r.na, all(is.na(x[i,])))
      for(i in 1:ncol(x)) c.na <- c(c.na, all(is.na(x[,i])))
    } else {
      for(i in 1:nrow(x)) r.na <- c(r.na, all(x[i,]==values))
      for(i in 1:ncol(x)) c.na <- c(c.na, all(x[,i]==values))
    }
    r1 <- 1 + which(diff(which(r.na))>1)[1]; r2 <- nrow(x) -  which(diff(which(rev(r.na)))>1)[1]
    c1 <- 1 + which(diff(which(c.na))>1)[1]; c2 <- ncol(x) - which(diff(which(rev(c.na)))>1)[1]
    x <- x[r1:r2,c1:c2]
    if(out=="raster") {
      xs <- xFromCol(y,col=c(c1,c2)) + c(-1,1)*cres[1]
      ys <- yFromRow(y,row=c(r2,r1)) + c(-1,1)*cres[2]
      x <- crop(y,extent(xs,ys))
    }
  }
  return(x)
}

# Read in Ireland coastline
ie = readOGR(dsn=coastlinePath, layer='country')
ie.grid = spTransform(ie, CRS=CRS("+init=epsg:29903"))   # Transform to Irish Grid TM75

# Define bounding box of irish coastline (rounded to the nearest hectad)
ie.coords = bbox(ie.grid)/10^4
padding = 1
bb = c(1,1,1,1)
bb[c(1,3)] = (floor(ie.coords[,1])-padding)*10^4
bb[c(2,4)] = (ceiling(ie.coords[,2])+padding)*10^4

# Create a raster in Irish grid with cells that are exactly 250mx250m
ie.raster = raster(ncol=(bb[2]-bb[1])/250, nrow=(bb[4]-bb[3])/250, extent(bb), crs=CRS("+init=epsg:29903"))

for (f in 1:length(hdf.files)) {
  # Read in the MODIS data and crop to Ireland 
  sds <- get_subdatasets(paste(inputDir,hdf.files[f],sep='/'))
  
  # An alternative approach to reading in the 
  # test <- readGDAL(sds[grep("250m 16 days NDVI", sds)],as.is=T)*scalingFactor

  # These lines read in the individual wavelength bands. Used to check the scaling factor for ndvi and evi
  # ndvi is (nir-red)/(nir+red)
  #  red = crop(raster(sds[grep("250m 16 days red reflectance", sds)], as.is=T), ir)
  #  nir = crop(raster(sds[grep("250m 16 days NIR reflectance", sds)], as.is=T), ir)
  
  
  ndvi = crop(raster(sds[grep("250m 16 days NDVI", sds)], as.is=T), ir)*scalingFactor^2
  evi = crop(raster(sds[grep("250m 16 days EVI", sds)], as.is=T), ir)*scalingFactor^2
  QC = crop(raster(sds[grep("16 days pixel reliability",sds)]), ir)
  # Reliability, 0=good, 1=OK but use with care, 2=snow/icd, 3=cloudy,-1=no data 

  # More detailed quality flag
#  quality = crop(raster(sds[grep("16 days VI Quality",sds)]), ir)
  
  # Keep only good quality data (reliability=0 or 1) and reproject onto Irish grid
  ndvi[QC<0 | QC>1] <- NA 
  evi[QC<0 | QC>1] <- NA
  
  # Set negative evi & ndvi to zero
  ndvi[ndvi<0] <- 0
  evi[evi<0] <- 0
  

  evi.lc = projectRaster(evi,crs=corine.crs) # Reproject onto CRS of CORINE raster data
  ndvi.lc = projectRaster(ndvi,crs=corine.crs) # Reproject onto CRS of CORINE raster data
#  QC.lc = projectRaster(QC,crs=corine.crs) # Reproject onto CRS of CORINE raster data
  
  if (f==1) {
    # First find majority land class (aggregate by a factor of 3, which is slightly coarser than MODIS)
    corine_agg = raster::aggregate(corine, fact=3, fun=modal, na.rm=TRUE)
    # Now resample this 'majority' raster
    corine_sync = raster::resample(corine_agg,evi.lc, method='ngb', verbose=F) # Make CORINE the resolution of MODIS
    # Crop corine to have no empty rows or columns
    tmp = trim2(corine_sync, values=0, out='raster')
    crop_extent = extent(tmp)
  }
  
  # Extract cells corresponding to pasture (code=18) in CORINE
  evi_pasture = evi.lc
  evi_pasture[!(corine_sync%in%corineInclude)] <- NA
  evi_pasture = crop(evi_pasture, crop_extent)
  evi_tmp = projectRaster(evi_pasture,crs=CRS("+init=epsg:29903"))   # Transform to Irish Grid TM75
  # Sync evi.grid with the ie.raster (i.e. rounded to nearest hectad)
  evi_grid = raster::resample(evi_tmp, ie.raster, method='bilinear')
  
  ndvi_pasture = ndvi.lc
  ndvi_pasture[!(corine_sync%in%corineInclude)] <- NA
  ndvi_pasture = crop(ndvi_pasture, crop_extent)
  ndvi_tmp = projectRaster(ndvi_pasture,crs=CRS("+init=epsg:29903"))   # Transform to Irish Grid TM75
  # Sync with the ie.raster (i.e. rounded to nearest hectad)
  ndvi_grid = raster::resample(ndvi_tmp, ie.raster, method='bilinear')
  
  # QC_pasture = QC.lc
  # QC_pasture[corine_sync!=18] <- NA
  # QC_pasture = crop(QC.lc, crop_extent)
  # QC_tmp = projectRaster(QC_pasture,crs=CRS("+init=epsg:29903"))   # Transform to Irish Grid TM75
  # # Sync with the ie.raster (i.e. rounded to nearest hectad)
  # QC_grid = raster::resample(QC_tmp, ie.raster, method='bilinear')

  # Write the rasters to a new file
  fname.ndvi = paste(outputDir,'/NDVI_',outputSuffix,'_',format(r.date[f],"%Y_%m_%d"),sep='') 
  fname.evi = paste(outputDir,'/EVI_',outputSuffix,'_',format(r.date[f],"%Y_%m_%d"),sep='') 
#  fname.qc = paste(outputDir,'/QC_pasture_',format(r.date[f],"%Y_%m_%d"),sep='') 
  writeRaster(ndvi_grid,file=fname.ndvi,format='raster',overwrite=TRUE)
  writeRaster(evi_grid,file=fname.evi,format='raster',overwrite=TRUE)
#  writeRaster(QC_grid,file=fname.qc,format='raster',overwrite=TRUE)
}

