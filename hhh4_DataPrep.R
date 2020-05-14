# Pulls latest case data as well as DHB spatial and population data. Processes it into appropiate inputs for plotting and modelling.

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(rvest)
library(httr)
library(readxl)
library(janitor)
library(surveillance)
#library(sf)
#library(spdep)


# Scrape latest DataURL and DataFilename.Verbose error checking here as data location and structure is volatile

DataPage <- read_html("https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-situation/covid-19-current-cases/covid-19-current-cases-details")

DataPageLinks <- DataPage %>%
  html_nodes("a") %>%
  html_attr("href")

DataURLSuffix <- grep("system/files/documents/pages/", DataPageLinks, value = TRUE)

DataFilename <- sub(".+/", "", DataURLSuffix)

DataURL <- paste0("https://www.health.govt.nz", DataURLSuffix)

# Error check. Passing will download new data if available, otherwise use local. Result printed

DataDir <- "D:\\StatsNZ_Work\\Covid19\\cv19\\Data\\NZCases\\"
DataPath <- paste0(DataDir, DataFilename)

if (length(DataURL) == 1 & str_sub(DataFilename, -5, -1) == ".xlsx") {
  if (any(list.files(DataDir) == DataFilename)) {
    print(paste0("Latest MoH Case Data already available locally: ", DataFilename))
  }
  else {
    print(paste0("Found new MoH Case Data: ", DataFilename))
    GET(DataURL, write_disk(paste0(DataDir, DataFilename)), progress())
  }
} else {
  print("ERROR scraping latest Data. Verify checks below:")
  length(DataURL) == 1
  str_sub(DataFilename, -5, -1) == ".xlsx"
  DataFilename <- NULL
  DataPath <- NULL
}

# Pull latest Case Data, bind sheets, clean ColNames, verify structure as expected

ExpectedSheets <- c("Confirmed", "Probable")
ExpectedCols <- c("DateOfReport", "Sex", "AgeGroup", "DHB", "OverseasTravel", "LastCountryBeforeReturn", "FlightNumber", "FlightDepartureDate", "ArrivalDate")
ExpectedDHBs <- read_excel("Data\\StandardisedNames.xlsx", sheet = "DHB") %>%
  select(DHB)

if (identical(excel_sheets(DataPath), ExpectedSheets)) {
  NZ_Covid19_Confirmed <- read_excel(DataPath, sheet = "Confirmed", skip = 3)
  NZ_Covid19_Probable <- read_excel(DataPath, sheet = "Probable", skip = 3)
} else {
  print("ERROR sheets exception. Received:")
  print(excel_sheets(DataPath))
  print("Expected:")
  print(ExpectedSheets)
}

NZ_Covid19_All <- bind_rows(NZ_Covid19_Confirmed, NZ_Covid19_Probable) %>%
  clean_names(., "upper_camel") %>%
  rename(DHB = Dhb)

if (identical(colnames(NZ_Covid19_All), ExpectedCols)) {
  print("PASSED data structure test")
} else {
  print("ERROR column names exception:")
  colnames(NZ_Covid19_All) == ExpectedCols
}

# Clean values, verify, select, aggregate, sort

NZ_Covid19_All$DHB <- gsub(" |'", "", NZ_Covid19_All$DHB)
NZ_Covid19_All$DHB <- enc2native(NZ_Covid19_All$DHB)

if (identical(sort(unique(NZ_Covid19_All$DHB)), sort(ExpectedDHBs$DHB))) {
  print("PASSED DHB names test")
} else {
  print("ERROR DHB names exception")
  sort(unique(NZ_Covid19$DHB_All)) == sort(ExpectedDHBs$DHB)
}

## Consider trimming white space

NZ_Covid19_All$DateOfReport <- dmy(NZ_Covid19_All$DateOfReport)

NZ_Covid19_Selected <- NZ_Covid19_All %>%
  select("DHB", "DateOfReport")

NZ_Covid19_Selected_Agg <- NZ_Covid19_Selected %>%
  count(DHB, DateOfReport, name = "NewCases") %>%
  arrange(DHB, DateOfReport)

# Create DateRange Tibble

## Need to discuss issues with what DateMax to use - Perhaps prudent to use cell A2 - 1 or 2 days
DateMax <- as.vector(read_excel(DataPath, sheet = "Confirmed", "A2", col_names = "Date")) %>%
  pull(., Date) %>%
  as_date(.)

if (is.Date(DateMax)) {
  print(paste0("Correctly scraped DateMax from cell A2 as: ", DateMax))
} else {
  DateMax <- max(NZ_Covid19_Selected_Agg$DateOfReport)
  print(paste0("Unable to scrape DateMax from cell A2, using max(DateOfReport) instead: ", DateMax))
}

sum(is.na(NZ_Covid19_All$DateOfReport))
DateMin <- min(NZ_Covid19_All$DateOfReport)

DateRange <- tibble(Date = seq.Date(DateMin, DateMax, by = "day"))

# Join, clean

NZ_Covid19 <- full_join(NZ_Covid19_Selected_Agg, DateRange, by = c("DateOfReport" = "Date")) %>%
  complete(DHB, nesting(DateOfReport), fill = list(NewCases = 0)) %>%
  filter(!is.na(DHB))

# Pivot Wide. Finalize.

NZ_Covid19.wide <- NZ_Covid19 %>%
  pivot_wider(., names_from = DHB, values_from = NewCases) %>%
  mutate(seq_dayte = as.numeric(DateOfReport) - 18317)


nz_counts_t <- NZ_Covid19.wide %>%
  select(ExpectedDHBs$DHB)

# Pull pop data [static], calc proportion
## To do: Deprevation data

DHB_Pop_2019 <- read_excel("Data\\DHBData\\DHBPopulation.xlsx") %>%
  mutate(PopulationProportion = Population / sum(Population))

populationFrac <- DHB_Pop_2019 %>%
  select(DHB, PopulationProportion) %>%
  pivot_wider(., names_from = DHB, values_from = PopulationProportion)

populationFracRepeated <- populationFrac %>%
  uncount(., as.integer(DateMax - DateMin + 1))

population_m <- DHB_Pop_2019 %>%
  select(DHB, Population) %>%
  pivot_wider(., names_from = DHB, values_from = Population)

population_mRepeated <- population_m %>%
  uncount(., as.integer(DateMax - DateMin + 1))

# Matricize

nz_counts_t <- as.matrix(nz_counts_t)
mode(nz_counts_t) <- "integer"
populationFracRepeated <- as.matrix(populationFracRepeated)
population_mRepeated<- as.matrix(population_mRepeated)

# Pull map data [static]

load("D:\\StatsNZ_Work\\Covid19\\cv19\\Data\\GeospatialData\\DHB2012\\Shapefile_Processed.Rdata", verbose = TRUE)

# Surveillance Time Series

covidNZ <- sts(nz_counts_t,
               start=c(lubridate::year(DateMin), yday(DateMin)),
               population=(populationFracRepeated),
               neighbourhood=nzrems_nbOrder,
               frequency=365,
               map=map)

plot(covidNZ)

## OPTIONAL: Remove unnecessary objects: rm(list = c(str_subset(objects(), "NZ_Covid19_.|Data.|Expected[C|S]")))
