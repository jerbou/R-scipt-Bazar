# objectif : recuperer un fichier pour la geosirene BFC

# ==== chargement librarie ====
library(plyr)
library(installr)

# https://stackoverflow.com/questions/3053833/using-r-to-download-zipped-data-file-extract-and-import-data

# choix du wd pour stocker les archives zip
setwd("T:/Géobourgogne/DONNEES/PLATEFORME/REFERENTIELS/INSEE/geosirene/zip/")
WD <- getwd()

# ==== telechergaement des 
# temp <- tempfile()
# https://stackoverflow.com/questions/26765452/permission-denied-error-when-downloading-a-file
download.file(url = "http://212.47.238.202/geo_sirene/last/geo-sirene_21.csv.7z", destfile ="geo-sirene_21.csv.7z", mode = "wb")
download.file(url = "http://212.47.238.202/geo_sirene/last/geo-sirene_25.csv.7z", destfile ='geo-sirene_25.csv.7z', mode = "wb")
download.file(url = "http://212.47.238.202/geo_sirene/last/geo-sirene_39.csv.7z", destfile ='geo-sirene_39.csv.7z', mode = "wb")
download.file(url = "http://212.47.238.202/geo_sirene/last/geo-sirene_58.csv.7z", destfile ='geo-sirene_58.csv.7z', mode = "wb")
download.file(url = "http://212.47.238.202/geo_sirene/last/geo-sirene_70.csv.7z", destfile ='geo-sirene_70.csv.7z', mode = "wb")
download.file(url = "http://212.47.238.202/geo_sirene/last/geo-sirene_71.csv.7z", destfile ='geo-sirene_71.csv.7z', mode = "wb")
download.file(url = "http://212.47.238.202/geo_sirene/last/geo-sirene_89.csv.7z", destfile ='geo-sirene_89.csv.7z', mode = "wb")
download.file(url = "http://212.47.238.202/geo_sirene/last/geo-sirene_90.csv.7z", destfile ='geo-sirene_90.csv.7z', mode = "wb")
# d21 <- read.table(unz("geo-sirene_21.csv"), sep=',', header=T)

# changement du wd pour aller chercher les csv
setwd("T:/Géobourgogne/DONNEES/PLATEFORME/REFERENTIELS/INSEE/geosirene/entree/")


# https://stackoverflow.com/questions/3053833/using-r-to-download-zipped-data-file-extract-and-import-data
list.files()
unlink(temp)


# https://stackoverflow.com/questions/41954183/how-can-i-extract-multiple-zip-files-and-read-those-csvs-in-r
zipfil <- list.files(pattern="*.7z", full.names =FALSE)


# ldply(.data= zipfil, .fun =  unzip, "T:\\Géobourgogne\\DONNEES\\PLATEFORME\\REFERENTIELS\\INSEE\\geosirene\\entree")

WD
# In FUN(X[[i]], ...) : error 1 in extracting from zip file
# https://stackoverflow.com/questions/37221184/r-function-unzip-error-1-in-extracting-from-zip-file

# unzip("geo-sirene_70.csv.7z")

# ==== assemblage des csv ====
csvfil <- list.files(pattern="*.csv", full.names =FALSE)

#dfall <- do.call("rbind", lapply(csvfil, read.table , sep = ",", header = TRUE)) 
# dfo <- do.call(rbind, lapply(list.files(pattern = ".csv"), read.csv))
dfo <- do.call(rbind, lapply(csvfil, read.table, header= TRUE, sep=",", fileEncoding="UTF-8", stringsAsFactors=FALSE, fill= TRUE, quote="\"")) 
# 
# # https://stackoverflow.com/questions/5319839/read-multiple-csv-files-into-separate-data-frames
# tnames <-substr(csvfil,1,13)
# 
# # https://poldham.github.io/reading-csv-files-in-R/
# 
# for(i in tnames){
#   filepath <- file.path(".",paste(i,".csv",sep=""))
#   assign(i, read.delim2(filepath, header=T, sep = ";"))
# }

setwd("T:/Géobourgogne/DONNEES/PLATEFORME/REFERENTIELS/INSEE/geosirene/")

write.table(dfo, file = paste("geosirene_bfc", format(Sys.time (), "%d-%m-%Y"), ".csv",sep="_") , row.names = FALSE, sep=";", fileEncoding = "UTF-8")

dfopub <- subset(dfo, grepl("^20*", SIREN) | grepl("^21*", SIREN) | grepl("^19*", SIREN ))

write.table(dfopub, file = paste("geosirene_pub_bfc", format(Sys.time (), "%d-%m-%Y"), ".csv",sep="_") , row.names = FALSE, sep=";", fileEncoding = "UTF-8")
# r21 <- subset(r21, grepl("^21*", CODE_INSEE))

