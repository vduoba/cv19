#This standalone script pulls the DHB Region shapefile and outputs 'Shapefile_Processed.Rdata' containing 3 objects:
# 1. map, an R spatial object
# 2. nzrems_adjmat, Adjacency Matrix
# 3. nzrems_nbOrder, nbOrder Matrix

library(dplyr)
library(stringr)
library(readxl)
library(sf)
library(surveillance)
library(spdep)


# Pull shapefile, remove Z co-ordinates, fix ordering

dhb_shp <- st_read("Data\\GeospatialData\\DHB2012\\nz-district-health-boards-2012.shp") %>%
  st_zm(.) %>%
  arrange(DHB12)

# Clean shapefile DHB names, verify

dhb_shp$NAME <- dhb_shp$NAME %>%
  str_replace_all(c(" |'" = "", "Hutt" = "HuttValley", "Midcentral" = "MidCentral"))

ExpectedDHBs <- read_excel("Data\\StandardisedNames.xlsx", sheet = "DHB") %>%
  select(DHB)

identical(dhb_shp$NAME, ExpectedDHBs$DHB)

# Create R Map

map <- as(dhb_shp, "Spatial")

row.names(map) <- pull(ExpectedDHBs)
## Optional visual check: plot(map)

# Calc adjacency matrix, nbOrder matrix

nzrems_adjmat <- poly2adjmat(map, row.names = ExpectedDHBs$DHB)
mode(nzrems_adjmat) <- "integer"
colnames(nzrems_adjmat) <- ExpectedDHBs$DHB

nzrems_nbOrder <- nbOrder(nzrems_adjmat, maxlag=Inf)

# Save Map and matrices to Rdata file for use in main script

save("map", "nzrems_adjmat", "nzrems_nbOrder", file = "Data\\GeospatialData\\DHB2012\\Shapefile_Processed.Rdata")

