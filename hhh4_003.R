### hhh4_003.R
### Import population count and deprivation information.
### The DHB general population information is required for the model.
### The deprivation data is not required for the basic model, but is
### required for the model with covariates (not yet fitted)
### ??? Can we replace any of this with more recent information?
### Feeds into hhh4_004.R
### Assumes that datasets and vars from running hhh4_001-2.R
### are available in this program.
library(surveillance)
library(tidyverse)
library(reshape2)
library(plyr)
library(sp) # For extracting spatial polygons
library(sf) # For st_read
library(rlist) # For lis.append()
library(spdep) # For polygon adjacency measure calculation
# https://www.fmhs.auckland.ac.nz/en/soph/about/our-departments/epidemiology-and-biostatistics/research/hgd/research-themes/imd.html
### Downloaded csv file from above source (can we do this via the URL?)
nzdep13<-read.csv("IMD2013.csv", header=TRUE, sep=",")
dim(nzdep13) # 5958   38
summary(nzdep13)
names13 <- names(nzdep13)
names13
# [1] "DatazoneID"  "X2013URPop"  "TA2013"      "TA2013Name"  "GED2014"     "GED2014Name" "DHB12"      
# [8] "DHB2012Name" "IMDRank"     "EmpRank"     "IncRank"     "CriRank"     "HouRank"     "HlthRank"   
# [15] "EduRank"     "AccRank"     "IMDNoEmpR"   "IMDNoIncR"   "IMDNoCriR"   "IMDNoHouR"   "IMDNoHlthR" 
# [22] "IMDNoEduR"   "IMDNoAccR"   "IMDRankD"    "EmpRankD"    "IncRankD"    "CriRankD"    "HouRankD"   
# [29] "HlthRankD"   "EduRankD"    "AccRankD"    "IMDNoEmpD"   "IMDNoIncD"   "IMDNoCriD"   "IMDNoHouD"  
# [36] "IMDNoHlthD"  "IMDNoEduD"   "IMDNoAccD"
#
### Will we have aproblem with DHB names?:
levels(nzdep13$DHB2012Name)
# [1] "Auckland"           "Bay of Plenty"      "Canterbury"         "Capital and Coast"  "Counties Manukau"  
# [6] "Hawke's Bay"        "Hutt"               "Lakes"              "Midcentral"         "Nelson Marlborough"
# [11] "Northland"          "South Canterbury"   "Southern"           "Tairawhiti"         "Taranaki"          
# [16] "Waikato"            "Wairarapa"          "Waitemata"          "West Coast"         "Whanganui"  
# Subsetting of data via keeps below not retained. Might come back to it sometime
# keeps <- c("DatazoneID", "X2013URPop", "TA2013", "TA2013Name", "DHB12",
#            "DHB2012Name", "IMDRank", "EmpRank", "IncRank", "CriRank",
#            "HouRank", "HlthRank", "EduRank", "AccRank" )
# nzdep13_reduced <-nzdep13[, keeps]
# dim(nzdep13_reduced) # 5958   14
#
# View(nzdep13)
### Standardise the DHB names to those used elsewhere for this study
nzdep13<-mutate(nzdep13, DHB2012Name = revalue(DHB2012Name, c("Bay of Plenty" = "BayofPlenty",
                                                           "Capital and Coast"  ="CapitalandCoast",
                                                           "Counties Manukau"   = "CountiesManukau",
                                                           "Hawke's Bay"        = "HawkesBay",
                                                           "Midcentral"         = "MidCentral" ,
                                                           "Hutt"               = "HuttValley",
                                                           "Nelson Marlborough" = "NelsonMarlborough",
                                                           "South Canterbury"   = "SouthCanterbury",
                                                           "West Coast"         = "WestCoast")))
dhbs_dep<-levels(nzdep13$DHB2012Name) # Store the names
dhbs_dep # Check
#
# The data is at very detailed level, so summarise by DHB name
nzdep13_grouped <- nzdep13 %>% group_by(DHB2012Name)
dim(nzdep13_grouped) # 5958   38
by_dhb <- nzdep13_grouped  %>% dplyr::summarise(avg_IMDRank = mean(IMDRank), 
                                                sum_IMDRank = sum(IMDRank),
                                                popn_size   = sum(X2013URPop))
### Calculate and add on  on population proportions, as paper indicates these are used (...Frac)
### Don't know if propn_avg_IMDRank will be useful, but calc and forget
by_dhb$propn_avg_IMDRank <- by_dhb$avg_IMDRank/sum(by_dhb$avg_IMDRank)
### propn_popn is what we really want
by_dhb$propn_popn        <- by_dhb$popn_size /sum(by_dhb$popn_size )
### Put data into standard order (defined by dhbs_in_gis)
row_order <- dhbs_in_gis[1:length(dhbs_in_data)] # dhbs_in_gis list still has extra 2 residual DHB's
row_order # Check: Northland - Southern, i.e. not alphabetic
# Reorder the summary data file so that the rows are in the common DHB order:
by_dhb2 <- by_dhb %>%
          slice(match(row_order, DHB2012Name))
by_dhb2 # Check the dataframe (tibble)
#
### Prepare the matrix (1 row) of population fractions
### The pop fractions should really defined at each
### time step, but I do not think this is necessary but
### might have to repeat the row calculated to fit in
### with the German paper set-up
populationFrac=matrix(by_dhb2$propn_popn, nrow=1)
### Name the matrix columns
colnames(populationFrac) <- row_order
populationFrac # Check
#
### Prepare the matrix (1 row) of actual populations (not fractions)
popn_m <- matrix(by_dhb2$popn_size, nrow=1)
colnames(popn_m) <- row_order # Apply names
popn_m # Check
### Going back to what I said previously, the pop stuff has to
### be per time-period.
### Just repeat the single rows above.
rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
nbr_time_periods=nrow(nz_counts_t)
nbr_time_periods # Check
populationFracRepeated<-rep.row(populationFrac[1,],nbr_time_periods)
popn_mRepeated<-rep.row(popn_m[1,],nbr_time_periods)
### Add back the colnames:
colnames(populationFracRepeated) <- row_order
colnames(popn_mRepeated)         <- row_order
#
dim(nz_counts_t)       # Check
colnames(nz_counts_t)  # Check
### map has numbers as column names to fix that
### row.names(map)<-row_order
### (Perhaps check again that the data is lined up correctly)
# as.character(map@data$DHB2015_Na) # But this has the original uncompressed names! Does it matter?
map@data$DHB2015_Na <-row_order
### The spatial data structure map requires rownames:
row.names(map)     <- row_order
#
### measlesWeserEms <- sts(...)
covidNZ <- sts(nz_counts_t, 
             start=c(2020,min_doy),
             population=populationFracRepeated,
             neighbourhood=nzrems_nbOrder,
             frequency=365,
             map=map)
#
plot(covidNZ)
### Examine structure
library(tibble)
glimpse(covidNZ)
glimpse(covidNZ@map)
#
covidNZ@populationFrac # Check
################################################################




