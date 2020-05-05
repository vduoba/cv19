# Calculates deprevation weighted by population

library(dplyr)
library(readr)
library(stringr)
library(scales)

# 2013 - uses Auckland University method (NZDep IMD). Population by meshblocks

Dep2013_HighRes <- read_csv("Data/DHBData/IMD2013.csv") %>%
  select(DatazoneID, `2013URPop`, DHB2012Name, IMDRank) %>%
  group_by(DHB2012Name) %>%
  mutate(DHBPop = sum(`2013URPop`)) %>%
  mutate(DHBPopProp = `2013URPop` / DHBPop) %>%
  mutate(DHBDep = IMDRank * DHBPopProp)

Dep2013_Summary <- Dep2013_HighRes %>%
  select(DHB2012Name, `2013URPop`, DHBDep) %>%
  summarise(DHBPop = sum(`2013URPop`), DHBDep = sum(DHBDep)) %>%
  mutate(DHBDep_Scaled = rescale(DHBDep, to = c(1, 100)))

# 2018 - uses Otago Univesirty method (NZiDep). Population by SA1 (from 2018 Census on, this is the smallest area for which population data is supplied)

AreaKey <- read_tsv("Data/DHBData/Annual Areas 2018.txt") %>%
  select(SA12018_code, DHB2015_name) %>%
  distinct(.)

Dep2018_SA1 <- read_tsv("Data/DHBData/NZ Dep 2018 - OtagoUni - SA1.txt") %>%
  select(-SA22018_code, -SA22018_name) %>%
  filter(URPopnSA1_2018 != 0, !is.na(NZDep2018))

Dep2018_HighRes <- left_join(x = Dep2018_SA1, y = AreaKey) %>%
  filter(DHB2015_name != "Area outside District Health Board") %>%
  group_by(DHB2015_name) %>%
  mutate(DHBPop = sum(URPopnSA1_2018)) %>%
  mutate(DHBPopProp = URPopnSA1_2018 / DHBPop) %>%
  mutate(DHBDepScore = NZDep2018_Score * DHBPopProp)

Dep2018_Summary <- Dep2018_HighRes %>%
  select(DHB2015_name, URPopnSA1_2018, DHBDepScore) %>%
  summarise(DHBPop = sum(URPopnSA1_2018), DHBDepScore = sum(DHBDepScore)) %>%
  mutate(DHBDepScore_Scaled = rescale(DHBDepScore, to = c(1, 100)))

# Note the different methods of quantifying deprevation produce quite different outputs as below. Therefore added rescaled values [1, 100] to each *_Summary tibble

print(Dep2013_Summary)
print(Dep2018_Summary)

summary(Dep2013_Summary$DHBDep)
summary(Dep2018_Summary$DHBDepScore)
