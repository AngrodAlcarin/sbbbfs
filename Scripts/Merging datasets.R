library(purrr)

#now I'll join the topo dataset with the sbb dataset
n_distinct(topo$PLZ)
n_distinct(sbb$PLZ)

#the topo dataset has 3194 unique PLZs, same as the sbb dataset with 3194 ones. I can merge them.
sbb_topo<-merge(sbb, topo, by="PLZ", all.x=TRUE)

#now I'll try the same with the bfs dataset. I'll join it with the topo dataset
n_distinct(topo$Gemeindecode)
n_distinct(bfsr$Gemeindecode)

#the topo dataset has 2148 unique Gemeindecodes, but the bfsr dataset has 2440 unique ones. I don't know where
#the difference comes from, but I will merge them anyway and see what happens. Maybe I can find out where the
#difference comes from later.
bfsr_topo<-merge(bfsr, topo, by="Gemeindecode", all.x=TRUE)
n_distinct(bfsr_topo$Gemeindecode)

#now I'll merge the bfsr_topo dataset with the sbb_topo dataset, but I'll have to be cautious, since they do not have 
#the same amount of unique bfs numbers. 
sbb_bfs<-merge(sbb_topo, bfsr_topo, by=c("PLZ","Gemeindecode", "Jahr"), all.x=TRUE)
n_distinct(sbb_bfs$Gemeindecode)
n_distinct(sbb_bfs$PLZ)

#now I have many rows that have duplicate bfs data, but differnt sbb data because of the merge. For example,
#rows 50052 and 50053 have the same year and bfs number, but different PLZ. That means that when I want to merge them,
#I need to add together the columns of data originating from the sbb dataset, but just take the first values of the 
#columns originating from the bfs dataset. I will first define for both datasets wich columns to add together
# (which are absolut numbers), which ones to add together and divide by n (which ones are percentages), and which ones
#to take the first value (which are categorical).
sbbaddcols<-c("Generalabonnement","Halbtaxabonnement","Bevölkerung")
sbbperccols<-c("anteil_ga_besitzer","anteil_hta_besitzer")
sbbcatcols<-c("Generalabonnement.Flag","Halbtaxabonnement.Flag")

bfsaddcols<-c("Einwohner","Gesamtfläche.in.km.","Beschäftigte.total","im.1..Sektor","im.2..Sektor","im.3..Sektor",
              "Arbeitsstätten.total","im.1..Sektor.1","im.2..Sektor.1","im.3..Sektor.1","Anzahl.Privathaushalte",
              "Veränderung.in.ha","Veränderung.in.ha.1")
bfsperccols<-c("Veränderung.in..","Bevölkerungs.dichte.pro.km.","Ausländer.in..","X0.19.Jahre","X20.64.Jahre",
               "X65.Jahre.und.mehr","Rohe.Heiratsziffer","Rohe.Scheidungsziffer","Rohe.Geburtenziffer",
               "Rohe.Sterbeziffer","Durchschnittliche.Haushaltsgrösse.in.Personen","Siedlungsfläche.in..",
               "Landwirtschaftsfläche.in..","Wald.und.Gehölze.in..","Unproduktive.Fläche.in..",
               "Leerwohnungs.ziffer","Neu.gebaute.Wohnungen.pro.1000.Einwohner","Sozialhilfequote",
               "FDP.2.","CVP","SP", "SVP","EVP.CSP","GLP","BDP","PdA.Sol.","GPS","Kleine.Rechtsparteien",
               "Übrige.Parteien","gemäss.Strafgesetzbuch..StGB..","gemäss.Betäubungsmittelgesetz..BetmG.",
               "gemäss.Ausländergesetz..AuG.")

#now that I have the correct grouping of columns, I can apply different summarizations based on these
#different groupings. First, I'll group by BFS number and Jahr, and summarize the relevant sbb columns.

sbb_summarized <- sbb_bfs %>%
  group_by(Gemeindecode, Jahr) %>%
  summarize(
    across(all_of(sbbaddcols), sum, na.rm = TRUE),
    across(all_of(sbbperccols), ~ mean(., na.rm = TRUE)),
    across(all_of(sbbcatcols), first, na.rm = TRUE),
    .groups = "drop"
  )

#now I'll do the same for the bfs columns
bfs_summarized <- sbb_bfs %>%
  group_by(PLZ, Jahr) %>%
  summarize(
    across(all_of(bfsaddcols), sum, na.rm = TRUE),
    across(all_of(bfsperccols), ~ mean(., na.rm = TRUE)),
    .groups = "drop"
  )
