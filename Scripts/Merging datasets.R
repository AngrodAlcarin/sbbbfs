library(purrr)
#now that I have cleaned the datasets, let's merge them. First, I'll merge the sbb dataset with the topo_plz dataset,
#since they have the same amount of unique PLZs. Then, I'll need to change the plz column to a list, since there are
#multiple PLZs for each Gemeindecode.

sbb_topo<-merge(sbb, topo_plz, by="PLZ", all.x=TRUE) %>% 
  mutate(PLZ=as.list(PLZ))

#now I'll merge the bfs dataset with the topo_bfsn dataset, but I'll have to be cautious, since they do not have the
#same amount of unique bfs numbers. I'll have to check which bfs numbers are in the bfs dataset, but not in the topo_bfsn
bfsr_topo<-merge(bfsr, topo_bfsn, by="Gemeindecode", all.x=TRUE) %>% 
  mutate(Gemeindecode=as.list(Gemeindecode))

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
