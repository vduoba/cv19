### hh4_002.R
### Import geospatial DHB information
### Import DHB shape files
library(dplyr)
library(surveillance)
library(tidyverse)
library(reshape2)
library(plyr)
library(sp) # For extracting spatial polygons
library(sf) # For st_read
library(rlist) # For lis.append()
library(spdep) # For polygon adjacency measure calculation
### I downloaded a DHB .shp file from:
### https://datafinder.stats.govt.nz/layer/87883-district-health-board-2015/
### The order of DHB's in this file is not alphabetic, but geographic.
### Because the surveillance package requires column headers I decided
### that the ordering of DHB's as in this file will be called the standard
### ordering, and other data read in will have to conform. This will be made
### obvious in any code.
### There are two residual DHB's in this file as well as the main 20
path2 <- "Data\\OldData\\"
fyl3  <- "district-health-board-2015.shp"
dhb_shp<-st_read(paste0(path2,fyl3))
str(dhb_shp)   # Yuk, better to use tibble's glimpse()
#plot(dhb_shp)  # Test if everything has made sense. If plots appear, prob OK.
### List the DHB names in the shp file and get rid of blanks
dhbs_in_gis <- list()
for (ndx in 1:22) {
  # print(as.character(dhb_shp[ndx,]$DHB2015_Na))
  s=as.character(dhb_shp[ndx,]$DHB2015_Na)
  s=gsub("[^[:alnum:]]", "", s)
  dhbs_in_gis<-rlist::list.append(dhbs_in_gis, s)
}
dhbs_in_gis <- unlist(dhbs_in_gis)
print(dhbs_in_gis)
### Are the DHB names from the data file (hhh4_001.R) the same as those from
### this GIS file? Use setdiff to check if any name appears
#setdiff( dhbs_in_data, dhbs_in_gis) # Order of sets in setdiff matters
#setdiff( dhbs_in_gis, dhbs_in_data) # Order of sets in setdiff matters
### The names coincide but the last two regions "AreaoutsideDistrictHealthBoard"
### (They have the same name) are not wanted as they do not figure in the data
###
# Use sp library to get out SpatialPolygons for surveillance package
s.sp<-as(dhb_shp,"Spatial")
#class(s.sp) # "SpatialPolygonsDataFrame"
# Just for curiosity, look at the codes used for the DHB's
#as.character(s.sp@data$DHB2015_Co) 
# Funnily, the DHB2015_Co codes are 1-19, 22 then two 99's
# So, to clear out the unwanted DHB's:
map <- s.sp[s.sp@data$DHB2015_Co != "99",]
#class(map) # Check if still same "SpatialPolygonsDataFrame" class
#plot(map)  # Check if innards are still there
# Save just in case (not required so far to reload)
#saveRDS(map, "SpatialPolygonsDataFrame.RData")
#
### weserems_adjmat
### Calculate the adajacency matrix
nzrems_adjmat <- poly2adjmat(map)
### Get rid of last 2 rows and columns as they are residual "AreaoutsideDistrictHealthBoard" 
# DHB's that are not represented in the data:
# Now redundant as unwanted DHB's have already been removed.
# nzrems_adjmat <- nzrems_adjmat[1:length(dhbs_in_data),1:length(dhbs_in_data)]
# Calculate the Order matrix from the adjacency matrix
### weswerems_nbOrder
nzrems_nbOrder <- nbOrder(nzrems_adjmat,maxlag=Inf)
## Add the row and column names (remember we have dropped 2 cols/rows in nzrems_nbOrder)
length(dhbs_in_data) # Check, should be 20
colnames(nzrems_nbOrder) <- dhbs_in_gis[1:length(dhbs_in_data)]
rownames(nzrems_nbOrder) <- dhbs_in_gis[1:length(dhbs_in_data)]


colnames(nzrems_adjmat) <- dhbs_in_gis[1:length(dhbs_in_data)]
rownames(nzrems_adjmat) <- dhbs_in_gis[1:length(dhbs_in_data)]


# Now need to reorder the counts data in the data matrix nz_counts_t
# so that the order is the same as as in the nzrems_nbOrder
### Now fix up nz_counts_t hhh4_001.R so that its columns are in GIS columns order
nz_counts_t_002 <- nz_counts_t[, dhbs_in_gis[1:length(dhbs_in_data)]]
#################################################################