#environment setup
library(tidyverse)
library(knitr)

#I need to clean the datasets before merging them. We have SBB Data from 2012-2022, but BFS data only from 2013-2021.
#I will therefore only use the data from 2013-2021, which is the most recent data available for all datasets.
#I will also only use the data from the municipalities, not the cantons or the whole country.

#I will start with the SBB data. I will only use the data from 2013-2021.
sbb<-sbb %>% 
  filter(Jahr>=2013 & Jahr<=2021)

#topo needs a little cleaning: multiple municipalities have the same PLZ, but different bfs numbers,
#and vice versa. First, I will remove all that have the same PLZ and bfs numbers, as they are duplicates.
topo<-topo %>% 
  distinct(PLZ, BFS.Nr, .keep_all = TRUE)

#topo needs no cleaning, I can directyl join it with the sbb dataset to have the municipality information on the sbb set.
sbbtopo<-left_join(sbb, topo, by=c("PLZ"="PLZ"))
