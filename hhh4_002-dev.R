# As this step is static data end to end, consider saving outputs and removing this from the standard flow.

library(dplyr)
library(readxl)
library(surveillance)
library(sf)
library(stringr)
library(spdep)

# Pull map file, remove Z co-ordinates, fix ordering

dhb_shp <- st_read("Data\\GeospatialData\\DHB2012\\nz-district-health-boards-2012.shp") %>%
  st_zm(.) %>%
  arrange(DHB12)

# Clean DHB name, verify

dhb_shp$NAME <- dhb_shp$NAME %>%
  str_replace_all(c(" |'" = "", "Hutt" = "HuttValley", "Midcentral" = "MidCentral"))

ExpectedDHBs <- read_excel("Data\\StandardisedNames.xlsx", sheet = "DHB") %>%
  select(DHB)

identical(dhb_shp$NAME, ExpectedDHBs$DHB)

# Create R Map

map <- as(dhb_shp, "Spatial")
## Optional visual check: plot(map)

# Calc adjacency matrix and nbOrder matrix

nzrems_adjmat <- poly2adjmat(map, row.names = ExpectedDHBs$DHB)

colnames(nzrems_adjmat) <- ExpectedDHBs$DHB

nzrems_nbOrder <- nbOrder(nzrems_adjmat, maxlag=Inf)

# Finalizing
row.names(map) <- pull(ExpectedDHBs)

nz_counts_t <- nz_counts_t %>%
  select(-seq_dayte) %>%
  select(order(ExpectedDHBs$DHB, decreasing = TRUE))

nz_counts_t %>% select(ExpectedDHBs$DHB)

nz_counts_t <- nz_counts_t %>%
  select(ExpectedDHBs$DHB)



