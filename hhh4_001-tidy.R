# Verbose error checking as data location and structure is volatile

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(rvest)
library(httr)
library(readxl)
library(janitor)

# Scrape latest DataURL and DataFilename

DataPage <- read_html("https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-situation/covid-19-current-cases/covid-19-current-cases-details")

DataPageLinks <- DataPage %>%
  html_nodes("a") %>%
  html_attr("href")

DataURLSuffix <- grep("system/files/documents/pages/", DataPageLinks, value = TRUE)

DataFilename <- sub(".+/", "", DataURLSuffix)

DataURL <- paste0("https://www.health.govt.nz", DataURLSuffix)

# Error check. Passing will download new data if available, otherwise use local. Result printed

DataDir <- "Data\\NZCases\\"
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


# Import Data, bind sheets, clean ColNames, verify structure as expected

ExpectedSheets <- c("Confirmed", "Probable")
ExpectedCols <- c("DateOfReport", "Sex", "AgeGroup", "DHB", "OverseasTravel", "LastCountryBeforeReturn", "FlightNumber", "FlightDepartureDate", "ArrivalDate")

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

####### Consider trimming white space:

NZ_Covid19_All$DHB <- gsub(" ", "", NZ_Covid19_All$DHB)
NZ_Covid19_All$DHB <- gsub("'", "", NZ_Covid19_All$DHB)

## TO DO: make below a test
## sort(unique(NZ_Covid19$DHB)

NZ_Covid19_All$DateOfReport <- dmy(NZ_Covid19_All$DateOfReport)

NZ_Covid19_Selected <- NZ_Covid19_All %>%
  select("DHB", "DateOfReport")

NZ_Covid19_Selected_Agg <- NZ_Covid19_Selected %>%
  count(DHB, DateOfReport, name = "NewCases") %>%
  arrange(DHB, DateOfReport)

# Create DateRange Tibble

## Need to discuss issues with what DateMax to use - Perhaps prudent to use cell A2 - 1 or 2 days.
DateMax <- as.vector(read_excel(DataPath, sheet = "Confirmed", "A2", col_names = "Date")) %>%
  pull(., Date) %>%
  as_date(.)

if (is.Date(DateMax)) {
  print(paste0("Correctly scraped DateMax from cell A2 as: ", DateMax))
} else {
  DateMax <- max(NZ_Covid19_Selected_Agg$DateOfReport)
  print(paste0("Unable to scrape DateMax from cell A2, using max(DateOfReport) instead: ", DateMax))
}

DateMin <- min(NZ_Covid19_All$DateOfReport)

DateRange <- tibble(Date = seq.Date(DateMin, DateMax, by = "day"))

# Join, clean, pivot wider

NZ_Covid19 <- full_join(NZ_Covid19_Selected_Agg, DateRange, by = c("DateOfReport" = "Date")) %>%
  complete(DHB, nesting(DateOfReport), fill = list(NewCases = 0)) %>%
  filter(!is.na(DHB)) %>%
  group_by(DHB) %>%
  mutate(CumsumCases = cumsum(NewCases))

# Finalizing for compatibility hhh4_002: nz_counts_t

nz_counts_t <- NZ_Covid19 %>%
  select(-NewCases) %>%
  pivot_wider(., names_from = DHB, values_from = CumsumCases) %>%
  mutate(DateOfReport = as.numeric(DateOfReport) - 18317) %>%
  rename(seq_dayte = DateOfReport)

## Optional: clear unneeded objects: rm(list=setdiff(ls(), "nz_counts_t"))
