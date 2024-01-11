#environment setup
library(tidyverse)
library(knitr)
library(stringr)
library(dplyr)

#I need to clean the datasets before merging them. We have SBB Data from 2012-2022, but BFS data only from 2013-2020.
#I will therefore only use the data from 2013-2021, which is the most recent data available for all datasets.
#I will also only use the data from the municipalities, not the cantons or the whole country.

#I will start with the SBB data. I will only use the data from 2013-2020.
sbb<-sbb %>% 
  filter(Jahr>=2013 & Jahr<=2020)

#I will now clean the bfs data. I will only use the data from 2013-2021 and remove NA rows and rows that bfs put
#there containing data about Switzerland as a whole, NA rows that are there aesthically or rows at the bottom
#containing information and copyright data about the dataset.
#the first problem I solve by taking each dataset and removing the first three rows:
bfsr13 <- bfsr13[!is.na(bfsr13[[1]]) | rowSums(!is.na(bfsr13[, -1])) == 0, ]

#the second problem I solve by removing all rows that contain NA values in all columns except the first one, which
#may or may not contain NA. This will not remove data rows with data but may contain some NAs.
bfsr13<- bfsr13[!rowSums(is.na(bfsr13[-1])) == (ncol(bfsr13) - 1), ]

#now I'll repeat this for each dataset:
bfsr14 <- bfsr14[!is.na(bfsr14[[1]]) | rowSums(!is.na(bfsr14[, -1])) == 0, ]
bfsr14<- bfsr14[!rowSums(is.na(bfsr14[-1])) == (ncol(bfsr14) - 1), ]
bfsr15<- bfsr15[!is.na(bfsr15[[1]]) | rowSums(!is.na(bfsr15[, -1])) == 0, ]
bfsr15<- bfsr15[!rowSums(is.na(bfsr15[-1])) == (ncol(bfsr15) - 1), ]
bfsr16<- bfsr16[!is.na(bfsr16[[1]]) | rowSums(!is.na(bfsr16[, -1])) == 0, ]
bfsr16<- bfsr16[!rowSums(is.na(bfsr16[-1])) == (ncol(bfsr16) - 1), ]
bfsr17<- bfsr17[!is.na(bfsr17[[1]]) | rowSums(!is.na(bfsr17[, -1])) == 0, ]
bfsr17<- bfsr17[!rowSums(is.na(bfsr17[-1])) == (ncol(bfsr17) - 1), ]
bfsr18<- bfsr18[!is.na(bfsr18[[1]]) | rowSums(!is.na(bfsr18[, -1])) == 0, ]
bfsr18<- bfsr18[!rowSums(is.na(bfsr18[-1])) == (ncol(bfsr18) - 1), ]
bfsr19<- bfsr19[!is.na(bfsr19[[1]]) | rowSums(!is.na(bfsr19[, -1])) == 0, ]
bfsr19<- bfsr19[!rowSums(is.na(bfsr19[-1])) == (ncol(bfsr19) - 1), ]
bfsr20<- bfsr20[!is.na(bfsr20[[1]]) | rowSums(!is.na(bfsr20[, -1])) == 0, ]
bfsr20<- bfsr20[!rowSums(is.na(bfsr20[-1])) == (ncol(bfsr20) - 1), ]

#Now I either need to split up the sbb dataset for each year, or add all the bfs datasets together. I will try
#the latter, but if the resulting dataset gets to big to handle, I'll change my approach.
#there are some rows in some years that have "()" instead of NA. I will replace these with NA and change the type
#of the column to numeric. Others need a additional column to be added, as they are not in the same format as the others.

# Function to replace "()" and "X" with NA
replace_with_NA <- function(df) {
  df[] <- lapply(df, function(x) ifelse(x == "X" | x == "()", NA, x))
  return(df)
}
#apply function to all datasets
bfsr13<-replace_with_NA(bfsr13)
bfsr14<-replace_with_NA(bfsr14)
bfsr15<-replace_with_NA(bfsr15)
bfsr16<-replace_with_NA(bfsr16)
bfsr17<-replace_with_NA(bfsr17)
bfsr18<-replace_with_NA(bfsr18)
bfsr19<-replace_with_NA(bfsr19)
bfsr20<-replace_with_NA(bfsr20)

#change type of column to numeric
bfsr14$Anzahl.Privathaushalte<-as.numeric(bfsr14$Anzahl.Privathaushalte)

#change name to have it harmonized with all other sets
bfsr19<-bfsr19 %>% 
  rename(Sozialhilfequote = Sozialhilfequote.3.)

#list of the columns to be converted to numeric
convert_columns<-c("Beschäftigte.total", "im.1..Sektor", "im.1..Sektor.1", "im.2..Sektor", "im.2..Sektor.1", 
                   "im.3..Sektor", "im.3..Sektor.1", "Arbeitsstätten.total", "Sozialhilfequote")

# Function to convert character columns to numeric
convert_to_numeric <- function(df, columns) {
  df %>% 
    mutate_at(vars(one_of(columns)), as.numeric)
}
#list of datasets that need their columns changed to numeric
dataset_names <- c("bfsr17", "bfsr18", "bfsr19", "bfsr20")

#apply function to all datasets in the list
for (dataset_name in dataset_names) {
  original_dataset <- get(dataset_name)
  converted_dataset <- convert_to_numeric(original_dataset, convert_columns)
  assign(dataset_name, converted_dataset)
}

#list of all datasets to be added together with year ID
year_list<-list("2013"=bfsr13, "2014"=bfsr14, "2015"=bfsr15, "2016"=bfsr16, "2017"=bfsr17, "2018"=bfsr18,
                "2019"=bfsr19, "2020"=bfsr20)

#add all datasets together
bfsr<-bind_rows(year_list, .id = "Jahr")


#topo needs also some work: multiple municipalities have the same PLZ, but different bfs numbers,
#and vice versa. First, I will remove all that have the same PLZ and bfs numbers, as they are duplicates.
topo<-topo %>% 
  mutate(unique_id = row_number()) %>% 
  distinct(PLZ, BFS.Nr, .keep_all = TRUE)

#there are still multiple occurences of either plz or bfs number. I need to create two versions of this set:
#one that has unique plz numbers, so I can merge it with the sbb dataset, and one that has unique bfs numbers,
#so I can merge it with the bfs dataset. To do this, I need to group by the two variables and set summarize functions
#on what to do with the contents of each column that gets merged.

topo_plz <- topo %>%
  group_by(PLZ) %>%
  summarize(
    Ortschaftsname = str_c(unique(na.omit(Ortschaftsname)), collapse = " / "),
    Zusatzziffer = str_c(unique(na.omit(Zusatzziffer)), collapse = " / "),
    Gemeindename = str_c(unique(na.omit(Gemeindename)), collapse = " / "),
    Kantonskürzel = first(Kantonskürzel),
    Gemeindecode = list(unique(na.omit(BFS.Nr))),
    E = mean(E, na.rm = TRUE),
    N = mean(N, na.rm = TRUE),
    Sprache = first(Sprache),
    unique_id_list = list(unique_id)
  )

topo_bfsn <- topo %>%
  group_by(BFS.Nr) %>%
  summarize(
    Ortschaftsname = str_c(unique(na.omit(Ortschaftsname)), collapse = " / "),
    Zusatzziffer = str_c(unique(na.omit(Zusatzziffer)), collapse = " / "),
    Gemeindename = str_c(unique(na.omit(Gemeindename)), collapse = " / "),
    Kantonskürzel = first(Kantonskürzel),
    PLZ = list(unique(na.omit(PLZ))),
    E = mean(E, na.rm = TRUE),
    N = mean(N, na.rm = TRUE),
    Sprache = first(Sprache),
    unique_id_list = list(unique_id)
  ) %>% 
  rename(Gemeindecode = BFS.Nr) 

#now let's check whether the new topo datasets have the same number of unique PLZs/BFS numbers as the SBB/BFS dataset:
n_distinct(topo_plz$PLZ)
n_distinct(sbb$PLZ)
#the topo_plz and sbb datasets each have 3194 unique PLZs, so I can merge them.

n_distinct(topo_bfsn$Gemeindecode)
n_distinct(bfsr$Gemeindecode)
#the topo_bfsn dataset has 2148 unique BFS numbers, but the bfsr dataset has 2440 unique one. I don't know where
#the difference comes from, but I will merge them anyway and see what happens. Maybe I can find out where the
#difference comes from later.
