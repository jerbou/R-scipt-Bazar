
# chargement des librairies -----------------------------------------------

library(rgdal)
library(tidyverse)
library(highcharter)

df0 <- readOGR(dsn="D:/donnees/referentiel/ign/OCSOL_BFC/donnees_derivees/ocs_ge_differntiel_commune.shp")

df1 <- as(df0, "data.frame")
rm(df0)

head(df1)
str(df1)
differentiel <- df1 %>% group_by(INSEE_COM, cs_avant,us_avant,os_avant,cs_apres,us_apres,os_apres) %>% summarise(surf_tot = sum(surface))

differentiel %>% dplyr::select(cs_avant , cs_apres, surf_tot) %>% filter(INSEE_COM=='21001')

testo <- differentiel %>% dplyr::select(cs_avant , cs_apres, surf_tot) %>% filter(INSEE_COM=='21001')

testa <- testo[c("cs_avant", "cs_apres", "surf_tot")]

testa$change <- ifelse(testa$from == testa$to, "non", "oui")

testb$weight <- round(testb$weight)

testb <- filter(testa, change=="oui")
testb$id <- paste(testb$from, testb$to)

names(testa) <- c("from", "to", "weight")
hchart(testa, "sankey", name = "couverture")

hchart(testb, "sankey", name = "couverture")


# https://rpubs.com/techanswers88/sankey


set.seed(111)

t1 <- sample(x = c("Hosp A", "Hosp B", "Hosp C") , size = 100, replace=TRUE)
t2 <- sample(x = c("Male", "Female")   , size = 100, replace=TRUE)
t3 <- sample(x = c("Survived", "Died") , size = 100, replace=TRUE)

d <- data.frame(cbind(t1,t2,t3))
names(d) <- c('Hospital', 'Gender', 'Outcome')
head(d)

data_to_sankey(d)

##   Hospital Gender  Outcome
## 1   Hosp B   Male Survived
## 2   Hosp C Female Survived
## 3   Hosp C Female     Died
## 4   Hosp C   Male     Died
## 5   Hosp A   Male     Died
## 6   Hosp C Female Survived

# First Sankey diagram
hchart(data_to_sankey(d), "sankey", name = "Hospital and Gender based Outcomes")

