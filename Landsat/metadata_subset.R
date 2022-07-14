# Subset Landsat Bulk Meta for a particular path and row
#
# Jon Yearsley (Jon.Yearsley@ucd.ie)
# July 2022
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


rm(list=ls())


setwd('~/Research/IMiBio/Landsat/')

file = 'LANDSAT_ETM_C2_L2.csv'
fileout = 'LC07_ETM_C2_L2_METADATA_Path224_Row78.csv'

path = 224
row = 78



# Import file
meta = read.csv(file)

# Subset the metafile
meta_sub = subset(meta, WRS.Path==path & WRS.Row==row)


# Write subset to a file
write.table(meta_sub, 
            file=fileout, 
            quote=FALSE, 
            row.names=FALSE, 
            col.names=TRUE, 
            sep=',')


