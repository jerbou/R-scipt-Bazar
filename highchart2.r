# chargement de librairies ----------------------------------------------------------------------------------------------------------------------
library(readr)
library(forcats)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(ggrepel)
library(RColorBrewer)
library(sp)
library(sf)
library(rgdal)
# library(reshape2)
library(htmlwidgets)
require(highcharter)
library(classInt)  # pour discretiser

options(encoding = "UTF-8")

# chargement des data
df0<-read_delim("https://plateforme.adresse.data.gouv.fr/api/communes-summary.csv", ",", escape_double = FALSE, trim_ws = TRUE)

# https://www.data.gouv.fr/fr/datasets/etat-de-la-base-adresse-nationale-par-commune/

str(df0)

dim(df0) # 35007 communes

df_bfc <- filter(df0, region=='27')

dim(df_bfc) # 3704 communes

# jointure avec une couche de commune
# recap_nb_ad_epci <- merge(y=nb_ad_bfc , x=couch_com, by.y= "code_insee" , by.x="INSEE_COM")
merge()

table(df_bfc$type_composition)
3560+144

summary(df_bfc$analyse_adressage_ratio)

hist(df_bfc$analyse_adressage_ratio)

View(df_bfc)

summary(df_bfc$analyse_adressage_nb_adresses_attendues)

hist(df_bfc$analyse_adressage_nb_adresses_attendues)

# jenks <- classIntervals(df_bfc$analyse_adressage_nb_adresses_attendues, n = 5, style = "jenks")
# tot <- as.data.frame(jenks$var)
# jenks$brks

options(scipen=999) # pas de notation scientifique

# https://ggplot2.tidyverse.org/reference/cut_interval.html
df_bfc$class <-cut_number(df_bfc$nb_numeros, n=5, ordered_result=TRUE)
df_bfc$class <-cut_width(df_bfc$nb_numeros, width=500, center = 0, dig.lab=0)

df_bfc$class <- cut(df_bfc$nb_numeros, c(0,100,200,300,400,500,Inf))

# https://stackoverflow.com/questions/17713456/easiest-way-to-discretize-continuous-scales-for-ggplot2-color-scales
#ggplot(df, aes(x=X,y=Y,fill=cut(Val, c(0,100,200,300,400,500,Inf))))

View(df_bfc)

ggplot(df_bfc) + geom_bar(aes(x=cut(df_bfc$nb_numeros, c(0,100,200,300,400,500,Inf))))

hchart(df_bfc, type='chart', hcaes(x=class, y=))

# graphe
hchart(df_adhss, type= 'colum', hcaes(x= class, y = , group=numCollege)) %>%
  hc_tooltip( useHTML = TRUE, headerFormat = "",  pointFormat = tltip)%>% 
  hc_yAxis(title = list(text = "Cotisation en   TTC")) %>%
  hc_xAxis(title = list(text = "Nombre Habitants"), type= 'logarithmic') %>%
  hc_colors( brewer.pal(n = 6, name = "Set1"))

# pokemon%>%
#   count(type_1)%>%
#   arrange(n)%>%
#   hchart(type = "bar", hcaes(x = type_1, y = n))



# Graphe nombre numero par departement ------------------------------------
df_bfc %>%
  count(class, departement) %>%
#  arrange(n) %>% Trie par les valeurs de n pas cool dans notre exemple
  hchart(type = "column", hcaes(x = class, y = n, group= departement), stacking = "normal") %>%
  hc_xAxis(title = list(text = "Nombre de numéro")) %>%
  hc_yAxis(title = list(text = "Nombre de commune concernées")) %>%
  # Il faut connaitre au prealable le nombre de classe 
  hc_colors( brewer.pal(n = 7, name = "Set1"))


# leGENDE NE SUIT que le group et pas la color
df_bfc %>%
  count(class, departement, analyse_adressage_deficit_adresses) %>%
  hchart(type = "column", hcaes(x = class, y = n, color = departement, group= analyse_adressage_deficit_adresses))

df_bfc %>%
  count(class) %>%
  hchart(type = "item", hcaes(x = class, y=n/10 ))




# x <- diamonds$price
# hchart(x)

hchart(df_bfc$analyse_adressage_ratio)

hchart(df_bfc$nb_numeros, type='hist')

hchart(jenks)


# pie charte avec modification du tooltip
artif_pie <- recap_artif %>%
  hchart(type = "pie", hcaes(x = type , y = surface), stacking = "normal", tooltip = list(pointFormat = "{point.surface} ha")) %>%
  hc_title(text = "<b>Part d'arficialisation des ZAE</b>, données au Avril 2022",align = "center") %>%
  hc_colors(c("#EEE8CD", "#00CD00" )) %>%
  hc_credits(enabled=TRUE, text="AER BFC, 2022", style=list(fontSize = "10px")) %>%
  hc_xAxis(title = list(text = "département"), wordBreak= "break-all") 

# custmisation du tooltip
# https://stackoverflow.com/questions/46953400/r-highcharter-tooltip-customization
