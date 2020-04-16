# Verbose error checking as data location and structure is volatile

library(dplyr)
library(tidyr)
library(rvest)
library(readxl)
library(httr)
library(stringr)

# Scrape latest DataURL and DataFilename

Page <- read_html("https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-situation/covid-19-current-cases/covid-19-current-cases-details")

PageLinks <- Page %>%
  html_nodes("a") %>%
  html_attr("href")

DataURLSuffix <- grep("system/files/documents/pages/", PageLinks, value = TRUE)

DataFilename <- sub(".+/", "", DataURLSuffix)

DataURL <- paste0("https://www.health.govt.nz", DataURLSuffix)

# Error check. Passing will download new data if available, otherwise use local. Result printed.

if (length(DataURL) == 1 & str_sub(DataFilename, -5, -1) == ".xlsx") {
  if (any(list.files() == DataFilename)) {
    print(paste0("Latest MoH Case Data already in current dir: ", DataFilename))
  }
  else {
    print(paste0("Found new MoH Case Data: ", DataFilename))
    GET(DataURL, write_disk(DataFilename), progress())
  }
} else {
  print("ERROR scraping latest Data. Verify checks below:")
  length(DataURL) == 1
  str_sub(DataFilename, -5, -1) == ".xlsx"
  DataFilename <- NULL
}


# Import Data, bind sheets, verify structure as expected

ExpectedSheets <- c("Confirmed", "Probable")
ExpectedCols <- c("Date of report", "Sex", "Age group", "DHB", "International travel", "Last country before return", "Flight number", "Flight departure date", "Arrival date")

if(identical(excel_sheets(DataFilename), ExpectedSheets)) {
  NZ_Covid19_Confirmed <- read_excel(DataFilename, sheet = "Confirmed", skip = 2)
  NZ_Covid19_Probable <- read_excel(DataFilename, sheet = "Probable", skip = 2)
} else {
  print("ERROR sheets exception. Received:")
  print(excel_sheets(DataFilename))
  print("Expected:")
  print(ExpectedSheets)
}

NZ_Covid19_All <- bind_rows(NZ_Covid19_Confirmed, NZ_Covid19_Probable)

if (identical(colnames(NZ_Covid19_All), ExpectedCols)) {
  print("PASSED data structure test")
} else {
  print("ERROR column names exception:")
  colnames(NZ_Covid19_all) == ExpectedCols
}


# Clean up, verify, select, aggregate sort

NZ_Covid19_All$DHB <- gsub(" ", "", NZ_Covid19_All$DHB)
NZ_Covid19_All$DHB <- gsub("'", "", NZ_Covid19_All$DHB)

## TO DO: make below a test
## sort(unique(NZ_Covid19$DHB)

NZ_Covid19_All$`Date of report` <- as.Date(NZ_Covid19_All$`Date of report`)

NZ_Covid19_Selected <- NZ_Covid19_All %>%
  select("DHB", "Date of report")

NZ_Covid19_Selected_Agg <- NZ_Covid19_Selected %>%
  dplyr::count(DHB, `Date of report`, name = "NewCases") %>%
  arrange(DHB, `Date of report`)

# Create DateRange Tibble

## Hope to eventually have a day with zero new cases. Set DateMax to data release date
DateMax <- read_excel(DataFilename, sheet = "Confirmed", "A2")
## Cheating for now, just need to format above
DateMax <- max(NZ_Covid19_All$`Date of report`)
DateMin <- min(NZ_Covid19_All$`Date of report`)

DateRange <- tibble(Date = seq.Date(DateMin, DateMax, by = "day"))

# Join, clean

NZ_Covid19 <- full_join(NZ_Covid19_Selected_Agg, DateRange, by = c("Date of report" = "Date")) %>%
  complete(DHB, nesting(`Date of report`), fill = list(NewCases = 0)) %>%
  filter(!is.na(DHB))

