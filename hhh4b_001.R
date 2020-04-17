### hhh4_001.R
### Read in Covid-19 data
### Feeds into hhh4_002.R
### Note that data coming from different sources might have DHB spelling slightly differently,
### so care is taken at various points to standardise spelling and order data into a particular
### DHB ordering.
### Read data posted on:
### https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-situation/covid-19-current-cases#dhbs
### I downloaded the data into .csv files, but might be really good to link to web url???
# [1] "Date.of.report"  "Sex"  "Age.group"    "DHB"                       
# [5] "International.travel"  "Last.country.before.return" "Flight.number" "Flight.departure.date"     
# [9] "Arrival.date"  
library(surveillance)
library(tidyverse)
library(reshape2)
library(plyr)
#
path1 <- "D:\\StatsNZ_Work\\Covid19\\cv19\\NZ COVID-19 hhh4 Model\\Data\\OldData\\"
fyl1  <- "covid-casedetails-update-6april_confirmed.csv"
data1 <- read.csv(paste(path1,fyl1,sep=""),
                   header=TRUE,
                   stringsAsFactors=TRUE)
dim(data1)   # 911   9   # ??? Old csv files, so update
names(data1) 
#
fyl2<-"covid-casedetails-update-6april_probable.csv"
data2 <- read.csv(paste(path1,fyl2,sep=""),,
                   header=TRUE,stringsAsFactors=TRUE)
dim(data2)     # 195   9  # ??? Old csv files, so update
names(data2)   # Check same as above
#
# Combine the two dataframes
nz_covid19_loaded <- rbind(data1, data2)
nz_covid19_loaded <- NZ_Covid19_All # From Henry's file
head(nz_covid19_loaded )
### Convert date string to date type and create a date sequence var
# nz_covid19_loaded$dayte     <- as.Date(nz_covid19_loaded$Date.of.report, format="%d/%m/%y")
nz_covid19_loaded$dayte     <- as.Date(nz_covid19_loaded$"Date of report", format="%y/%m/%d") # Latest format
nz_covid19_loaded$seq_dayte <- as.integer(1 + (nz_covid19_loaded$dayte - min(nz_covid19_loaded$dayte)))
min_date=min(nz_covid19_loaded$dayte)
min_doy=as.numeric(strftime(min_date, format="%j"))
### Checks:-
summary(nz_covid19_loaded$seq_dayte ) # Check
dim(nz_covid19_loaded ) # 1106    11
names(nz_covid19_loaded)
levels(nz_covid19_loaded$DHB) # 20 of these
# Rename DHB names that have spaces and apostrophes:
nz_covid19<-mutate(nz_covid19_loaded, DHB = revalue(DHB, c("Bay of Plenty" = "BayofPlenty",
                                 "Capital and Coast"  ="CapitalandCoast",
                                 "Counties Manukau"   = "CountiesManukau",
                                 "Hawke's Bay"        = "HawkesBay",
                                 "Hutt Valley"        = "HuttValley",
                                 "Nelson Marlborough" = "NelsonMarlborough",
                                 "South Canterbury"   = "SouthCanterbury",
                                 "West Coast"         = "WestCoast")))
###
dhbs_in_data <-levels(nz_covid19$DHB) # List of the DHB's with standardised text
dhbs_in_data # Check prior to ordering by date sequence
nz_covid19 <- nz_covid19[order(nz_covid19$DHB,nz_covid19$seq_dayte),] # Nice day-number order
dhbs_in_data <- levels(nz_covid19$DHB) # Paranoid about getting 
dhbs_in_data # Check again
dim(nz_covid19) # 1106    3  # Still in skinny form
#
### Retain only some vars:
nz_covid19 = nz_covid19[,c("DHB", "dayte", "seq_dayte")]
dim(nz_covid19) # 1106    3
# Summarise by day (seq_dayte) by region (DHB) and store in a dataframe
# Need to have 0's for each day and for each region # for proper time-series

nz_counts <- nz_covid19    %>% 
  group_by(DHB, seq_dayte) %>% 
  dplyr::summarize(n=n()) # tibble
###
# Some daytes have 0 reported cases
# Create dummy dataframe with all regions and all seq_daytes
df_dates<-data.frame(DHB=character(0), 
                     seq_dayte=integer(0))
for (h in unique(nz_covid19$DHB)){
  for (d in min(nz_covid19$seq_dayte):max(nz_covid19$seq_dayte)){
    df_dates<-rbind(df_dates, data.frame(DHB=h,seq_dayte=d))
  }
}
dim(df_dates) # 800   2
# Left join the df_dates with the real dataframe
df_fixed <- left_join(df_dates, nz_counts, by=c("DHB","seq_dayte"))
str(df_fixed) # Added dates have NA as counts
# Change the NA's to 0
df_fixed$n[is.na(df_fixed$n)] <- 0
dim(df_fixed)   # 800   3
names(df_fixed) # "DHB"       "seq_dayte" "n" 
# View(df_fixed) # Check
# Transpose so that each DHB is a column. dcast fn seemed easiest to use.
nz_counts_t <- dcast(df_fixed, formula=seq_dayte~DHB) # LHS stays, RHS flips
str(nz_counts_t)
# View(nz_counts_t)
############################################################################


