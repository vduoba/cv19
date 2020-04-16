# Verbose error checking as data location and structure is volatile

library(dplyr)
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
    print(pasnte0("Found new MoH Case Data: ", DataFilename))
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

if (all(excel_sheets(DataFilename)) == all(ExpectedSheets)) {
  NZ_Covid19_Confirmed <- read_excel(DataFilename, sheet = "Confirmed", skip = 2)
  NZ_Covid19_Probable <- read_excel(DataFilename, sheet = "Probable", skip = 2)
} else {
  print("ERROR sheets exception")
  ExpectedSheets == (excel_sheets(DataFilename))
}

NZ_Covid19 <- bind_rows(NZ_Covid19_Confirmed, NZ_Covid19_Probable)

if (colnames(NZ_Covid19) == ExpectedCols) {
  print("PASSED data structure test")
} else {
  print("ERROR column names exception")
}

# Clean up, verify, sort

NZ_Covid19$DHB <- gsub(" ", "", NZ_Covid19$DHB)
NZ_Covid19$DHB <- gsub("'", "", NZ_Covid19$DHB)

## TO DO: make below a test
## sort(unique(NZ_Covid19$DHB)

NZ_Covid19$`Date of report` <- as.Date(NZ_Covid19$`Date of report`)


# Select columns
## +++ could expand these selected e.g. to capture International cases

NZ_Covid19_Selected <- NZ_Covid19 %>%
  select("DHB", "Date of report")

# Aggregate via count
NZ_Covid19_Selected_agg <- NZ_Covid19_Selected %>%
  count(DHB, `Date of report`, name = "NewCases") %>%
  arrange(DHB, `Date of report`)


# Hope to eventually have a day with zero new cases. Set DateMax to data release date

# Cheating for now, just need to format
DateMax <- as.Date("2020/04/15")
# DateMax <- read_excel(DataFilename, sheet = "Confirmed", "A2")
# DateMax #need to fix formatting
DateMin <- min(NZ_Covid19$`Date of report`)

# don't run this: DateRange <- tibble(Date = DateMin:DateMax)
# glimpse(DateRange)