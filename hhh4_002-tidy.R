# As this step is static data end to end, consider saving outputs and removing this from the standard flow.

library(dplyr)
library(readxl)
library(surveillance)
library(sf)
library(stringr)
library(spdep)

# Pull map file, remove Z co-ordinates, fix ordering

dhb_shp_2012 <- st_read("Data\\GeospatialData\\DHB2012\\nz-district-health-boards-2012.shp") %>%
  st_zm(.) %>%
  arrange(DHB12)

# Clean DHB name, verify

dhb_shp_2012$NAME <- dhb_shp_2012$NAME %>%
  str_replace_all(c(" |'" = "", "Hutt" = "HuttValley", "Midcentral" = "MidCentral"))

ExpectedDHBs <- read_excel("Data\\StandardisedNames.xlsx", sheet = "DHB") %>%
  select(DHB)

identical(dhb_shp_2012$NAME, ExpectedDHBs$DHB)

# Create R Map

map_2012 <- as(dhb_shp_2012, "Spatial")
## Optional visual check: plot(map_2012)

# Calc adjacency matrix and nbOrder matrix

nzrems_adjmat_2012 <- poly2adjmat(map_2012, row.names = ExpectedDHBs$DHB)

colnames(nzrems_adjmat_2012) <- ExpectedDHBs$DHB

nzrems_nbOrder_2012 <- nbOrder(nzrems_adjmat_2012, maxlag=Inf)

## Temporary: Checks against values from hhh4_002-CompareOutputs.R
identical(nzrems_adjmat_2012, nzrems_adjmat)
identical(nzrems_nbOrder_2012, nzrems_nbOrder)
## Haha. At least our plots will look better.

# Finalizing. Maintaining _2012 on name for now
nz_counts_t_002_2012 <- nz_counts_t %>%
  select(-seq_dayte) %>%
  select(order(ExpectedDHBs$DHB, decreasing = TRUE))
