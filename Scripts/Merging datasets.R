#now that I have cleaned the datasets, let's merge them. First, I'll merge the sbb dataset with the topo_plz dataset,
#since they have the same amount of unique PLZs. Then, I'll need to change the plz column to a list, since there are
#multiple PLZs for each Gemeindecode.

sbb_topo<-merge(sbb, topo_plz, by="PLZ", all.x=TRUE) %>% 
  mutate(PLZ=as.list(PLZ))

#now I'll merge the bfs dataset with the topo_bfsn dataset, but I'll have to be cautious, since they do not have the
#same amount of unique bfs numbers. I'll have to check which bfs numbers are in the bfs dataset, but not in the topo_bfsn
bfsr_topo<-merge(bfsr, topo_bfsn, by="Gemeindecode", all.x=TRUE) %>% 
  mutate(Gemeindecode=as.list(Gemeindecode))


