#environment setup
install.packages("readxl")
install.packages("xlsx")
install.packages("gdata")
library(readxl)
library(gdata)
library(xlsx)

#base dataset from sbb
sbb<-read.csv("https://data.sbb.ch/api/explore/v2.1/catalog/datasets/generalabo-halbtax-mit-bevolkerungsdaten/exports/csv?lang=de&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B",
              sep=";")
#save it as csv
setwd("C:/Users/Marius/OneDrive/Dokumente/UNI Luzern/3._Semester/Stat 101/sbbbfs/datasets")
write.csv(sbb, "generalabo_halbtax_sbb.csv", row.names = FALSE)

#swisstopo dataset containing municipalities with their PLZ code (which SBB uses) and their bfs number, which bfs uses
zipfile<-"https://data.geo.admin.ch/ch.swisstopo-vd.ortschaftenverzeichnis_plz/ortschaftenverzeichnis_plz/ortschaftenverzeichnis_plz_2056.csv.zip"
csvfile<-"PLZO_CSV_LV95/PLZO_CSV_LV95.csv"
download.file(url=zipfile, destfile="PLZO_CSV_LV95.csv.zip")
topo<-read.csv(unzip("PLZO_CSV_LV95.csv.zip", csvfile), sep=";", header=TRUE)

#bfs regional dataset 1 containing population data from 2011-2013 (realeased 2014, therefore called 2014)
download.file(url="https://dam-api.bfs.admin.ch/hub/api/dam/assets/2420591/master", destfile="bfsr13.xls",mode="wb")
bfsr13<-read.xlsx("bfsr13.xls", sheetIndex = 1, startRow = 6,header = TRUE)

#bfs regional dataset 2 containing population data from 2011-2014 (realeased 2015, therefore called 2015)
download.file(url="https://dam-api.bfs.admin.ch/hub/api/dam/assets/2420278/master", destfile="bfsr14.xls",mode="wb")
bfsr14<-read.xlsx("bfsr14.xls", sheetIndex = 1, startRow = 6,header = TRUE)

#bfs regional dataset 3 containing population data from 2013-2015 (realeased 2016, therefore called 2016)
download.file(url="https://dam-api.bfs.admin.ch/hub/api/dam/assets/328115/master", destfile="bfsr15.xls",mode="wb")
bfsr15<-read.xlsx("bfsr15.xls", sheetIndex = 1, startRow = 6,header = TRUE)

#bfs regional dataset 4 containing population data from 2014-2016 (realeased 2017, therefore called 2017)
download.file(url="https://dam-api.bfs.admin.ch/hub/api/dam/assets/2422865/master", destfile="bfsr16.xls",mode="wb")
bfsr16<-read.xlsx("bfsr16.xls", sheetIndex = 1, startRow = 6,header = TRUE)

#bfs regional dataset 5 containing population data from 2014-2017 (realeased 2018, therefore called 2018)
download.file(url="https://dam-api.bfs.admin.ch/hub/api/dam/assets/4662816/master", destfile="bfsr17.xls",mode="wb")
bfsr17<-read.xlsx("bfsr17.xls", sheetIndex = 1, startRow = 6,header = TRUE)

#bfs regional dataset 6 containing population data from 2004-2018 (realeased 2019, therefore called 2019)
download.file(url="https://dam-api.bfs.admin.ch/hub/api/dam/assets/7786544/master", destfile="bfsr18.xls",mode="wb")
bfsr18<-read.xlsx("bfsr18.xls", sheetIndex = 1, startRow = 6,header = TRUE)

#bfs regional dataset 7 containing population data from 2004-2019 (realeased 2020, therefore called 2020)
download.file(url="https://dam-api.bfs.admin.ch/hub/api/dam/assets/11587763/master", destfile="bfsr19.xls",mode="wb")
bfsr19<-read.xlsx("bfsr19.xls", sheetIndex = 1, startRow = 6,header = TRUE)

#bfs regional dataset 8 containing population data from 2004-2020 (realeased 2021, therefore called 2021)
download.file(url="https://dam-api.bfs.admin.ch/hub/api/dam/assets/15864450/master", destfile="bfsr20.xls",mode="wb")
bfsr20<-read.xlsx("bfsr20.xls", sheetIndex = 1, startRow = 6,header = TRUE)
