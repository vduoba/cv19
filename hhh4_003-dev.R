library(readxl)
library(dplyr)

# Pull new pop data, calc proportion

DHB_Pop_2019 <- read_excel("Data\\DHBData\\DHBPopulation.xlsx") %>%
  mutate(PopulationProportion = Population / sum(Population))

# Matricize

populationFrac <- DHB_Pop_2019 %>%
  select(DHB, PopulationProportion) %>%
  pivot_wider(., names_from = DHB, values_from = PopulationProportion)

populationFracRepeated <- populationFrac %>%
  uncount(., as.integer(DateMax - DateMin + 1))

nz_counts_t <- as.matrix(nz_counts_t)
populationFracRepeated <- as.matrix(populationFracRepeated)

# Surveillance Time Series

covidNZ <- sts(nz_counts_t,
               start=c(lubridate::year(DateMin), yday(DateMin)),
               population=(populationFracRepeated),
               neighbourhood=nzrems_nbOrder,
               frequency=365,
               map=map)

plot(covidNZ)
