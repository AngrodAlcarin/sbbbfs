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