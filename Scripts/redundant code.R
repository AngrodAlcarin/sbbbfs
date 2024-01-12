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




#now I'll merge the sbb_topo dataset with the bfsr_topo dataset. Before I can do that, I need to create two functions
#that will help me with the merging. The first function will check if an individual Bfs number is part of a list of codes
#in another row and group them accordingly. The second will do the same, but for the postal codes.

#function to check if a bfs number is part of a list of bfs numbers

grouping_bfs <- function(df) {
  # Create a unique identifier for each list of Gemeindecode
  df$Gemeindecode_id <- map_chr(df$Gemeindecode, ~paste(sort(.x), collapse = "-"))
  
  # Assign the same identifier to rows with individual codes that appear in these lists
  for (i in seq_along(df$Gemeindecode)) {
    if (length(df$Gemeindecode[[i]]) == 1) {
      single_code <- df$Gemeindecode[[i]]
      df$Gemeindecode_id[i] <- df$Gemeindecode_id[which(map_lgl(df$Gemeindecode, ~single_code %in% .x))]
    }
  }
  
  return(df)
}

#function to check if a postal code is part of a list of postal codes

grouping_plz <- function(df) {
  # Create a unique identifier for each list of PLZ
  df$PLZ_id <- map_chr(df$PLZ, ~paste(sort(.x), collapse = "-"))
  
  # Assign the same identifier to rows with individual codes that appear in these lists
  for (i in seq_along(df$PLZ)) {
    if (length(df$PLZ[[i]]) == 1) {
      single_code <- df$PLZ[[i]]
      df$PLZ_id[i] <- df$PLZ_id[which(map_lgl(df$PLZ, ~single_code %in% .x))]
    }
  }
  
  return(df)
}

merge_on_list_column <- function(df1, df2, list_column_name) {
  matched_rows <- vector("list", nrow(df1))
  
  for (i in seq_len(nrow(df1))) {
    matches <- sapply(df2[[list_column_name]], function(x) any(df1[[list_column_name]][[i]] %in% x))
    matched_rows[[i]] <- df2[matches, ]
  }
  
  result_df <- do.call(rbind, lapply(seq_along(matched_rows), function(i) {
    cbind(df1[i, ], matched_rows[[i]])
  }))
  
  return(result_df)
}


#now I'll merge the datasets and apply the functions.
sbbbfs<-merge_on_list_column(sbb_topo, bfsr_topo, "unique_id_list")

sbbbfs<-merge(sbb_topo, bfsr_topo, by="unique_id_list", all.x=TRUE)


bfs_summarized<-bfs_summarized %>% 
  filter(Jahr>=2013 & Jahr<=2020)
sbb_summarized<-sbb_summarized %>% 
  filter(Jahr>=2013 & Jahr<=2020)

bfssbb_sum<-bfssbb_sum %>% 
  filter(Jahr>=2013 & Jahr<=2020)
