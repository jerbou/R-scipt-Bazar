
# objectif : 
library("doBy", lib.loc="C:/R-3.5.0/library")
library("gridExtra", lib.loc="C:/R-3.5.0/library")
library(forcats) 

# devtools::install_github('bbc/bbplot')

# 00 charge lib -----------------------------------------------------------
library(sf)
library(spatstat)
library(sp)
library(maptools)
library(raster)
library(cartography)
library(SpatialPosition)
library(ggplot2)
library(ggplotgui)
library(doBy)
library(gghighlight)
library(dplyr)
library(rgdal)
library(remotes)
library(readr)
library("gghighlight", lib.loc="C:/R-3.5.0/library")
library(ggridges)
library("viridis", lib.loc="C:/R-3.5.0/library")
library(GGally)
library(RColorBrewer)
library(colorRamps)
library(ggrepel)
# devtools::install_github('thomasp85/gganimate')
library(gganimate)
library(ggpubr)
library(reshape2)
library(readr)
library(doBy)
library(ggplot2)
library(ggthemes)
library(viridis)
require(RPostgreSQL)
library(dplyr)
library(forcats) # pour ordonner les graphes autoamtiquement
library(waffle)
library ("FactoMineR")
library(ade4)
library(stringr)
library(tidyverse)
library(bbplot)
library(readr)

# ==== A les plateformes opendata ====
setwd("G:/07_professionnel/CNAM_certif_OD/memoire/datas_reuse")
odt <- read_csv("G:/07_professionnel/CNAM_certif_OD/memoire/datas_reuse/odt-ptf-10-2018.csv")

# autre source de donnees
# https://docs.google.com/spreadsheets/d/1yhcCDLrDsZzNxlPaIl25p_qN8WVPGCTstdIUxezDxtQ/edit#gid=707724434
library(curl)
curl("https://docs.google.com/spreadsheets/d/1yhcCDLrDsZzNxlPaIl25p_qN8WVPGCTstdIUxezDxtQ/edit#gid=707724434")

# === A1 dataviz ======
ggplot(odt) + geom_bar(aes(x=fct_infreq(regcode), fill=porteur), color="gray50") +
  labs(x="région", y="nombre")

library(reshape2)
library(factoextra)
library(FactoMineR)
odt$nb <- 1


# on ajoute le nombre de portail par region
# https://dplyr.tidyverse.org/reference/tally.html
odt <- as.data.frame(odt %>% add_count(regcode))

# nb_ty_po <- dcast(odt, regcode~type+porteur, length)
nb_ty_po <- dcast(odt, regcode~type, length)
nb_ty_po2 <- dcast(odt, regcode+n~type, length)

nb_ty_po <- as.data.frame(nb_ty_po)
# nb_ty_po <- rownames(nb_ty_po$regcode)
row.names(nb_ty_po) <- nb_ty_po$regcode

res.ca <- CA(select(nb_ty_po, -regcode))
fviz_ca_biplot (res.ca, repel = TRUE) 
ggplot(data=res.ca) + geom_point(aes(x=res.ca$col$coord))



CA (odt, ncp = 5, graph = TRUE)


dim(df0)

head(df0)


# identifier les etablissement de plus de 50 employés
str(df0)
df1 <- subset(df0, caractereemployeuretablissement=='O')
dim(df1) # 44244 etablissement employeur


dim(bfc)
bfc

df1$reg_bfc == '0'
df1$reg_bfc <- ifelse(df1$codedepetblissement =='21', "1", "0")
df1$reg_bfc <- ifelse(df1$codedepetblissement =='21' |  df1$codedepetblissement =='25' | df1$codedepetblissement =='39' | 
                        df1$codedepetblissement =='58' | df1$codedepetblissement =='70' |
                        df1$codedepetblissement =='71' | df1$codedepetblissement =='89' | df1$codedepetblissement =='90', "1", "0")

df1$od_rule == '0'
df1$od_rule <- ifelse(df1$trancheeffectifsunitelegale =='01' |  df1$trancheeffectifsunitelegale =='02' | df1$trancheeffectifsunitelegale =='03' | 
                       df1$trancheeffectifsunitelegale =='11' | df1$trancheeffectifsunitelegale =='12' |
                       df1$trancheeffectifsunitelegale =='NN' | is.na(df1$trancheeffectifsunitelegale) , "0", "1")
unique(df1$od_rule)

bfc <- subset(df1, codedepetblissement =='21' |  codedepetblissement =='25' | codedepetblissement =='39' | 
                codedepetblissement =='58' | codedepetblissement =='70' |
                codedepetblissement =='71' | codedepetblissement =='89' | codedepetblissement =='90') 


# trancheEffectifsEtablissementLibelléTranche d’effectif salarié de l’établissementDescription
# Il   s’agit   d’une   variable   statistique,   millésimée   au   31/12   d’une   année   donnée   
# (voir   variableanneeEffectifsEtablissement).Longueur: 2
# TypeListe de codesNN : Etablissement non employeur 
# (pas de salarié au cours de l'année de référence et pas d'effectif au 31/12)00 : 0 salarié (n'ayant pas d'effectif au 31/12 mais ayant employé des salariés au cours de l'année de référence) 01 : 1 ou 2 salariés 02 : 3 à 5 salariés 03 : 6 à 9 salariés 11 : 10 à 19 salariés 12 : 20 à 49 salariés 
# 21 : 50 à 99 salariés 
# 22 : 100 à 199 salariés 31 : 200 à 249 salariés 32 : 250 à 499 salariés 41 : 500 à 999 salariés 42 : 1 000 à 1 999 salariés 51 : 2 000 à 4 999 salariés 52 : 5 000 à 9 999 salariés 53 : 10 000 salariés et plus


# ==== dataviz ============

ggplot(bfc) + geom_bar(aes(x=trancheeffectifsunitelegale))
ggplot(df1) + geom_bar(aes(x=trancheeffectifsunitelegale, fill=reg_bfc)) 

ggplot(df1, aes(x=trancheeffectifsunitelegale, fill=reg_bfc)) + geom_bar() + geom_text(stat='count' , aes(label=..count..), vjust=0.5)

ggplot(bfc, aes(x=trancheeffectifsunitelegale)) + geom_bar() + geom_text(stat='count' , aes(label=..count..), position = position_stack(vjust = 0.5)) + coord_flip()
ggplot(df1, aes(x=trancheeffectifsunitelegale, fill=od_rule)) + geom_bar() + geom_text(stat='count' , aes(label=..count..), position = position_stack(vjust = 0.5)) + coord_flip()

ggplot(bfc, aes(x=trancheeffectifsunitelegale, fill=od_rule)) + geom_bar() + geom_text(stat='count' , aes(label=..count..), position = position_stack(vjust = 0.5)) + coord_flip()

ggplot(bfc, aes(x=trancheeffectifsunitelegale, fill=codedepetblissement)) + geom_bar() + geom_text(stat='count' , aes(label=..count..), position = position_stack(vjust = 0.5)) # + coord_flip()

table(bfc$od_rule) # 598 etablissement concerne avec + de 50 agents en BFC

table(bfc$od_rule, bfc$codedepetblissement)
# df.prop$colour <- ifelse(df.prop$Service == "Satisfied" & df.prop$Perc < 0.6, "orange", NA)




# ===== B 2 les organisations ayant ouvert des data =========
orga <- read_csv("G:/07_professionnel/CNAM_certif_OD/memoire/datas_reuse/odt-orga-10-2018.csv")

# https://www.datanovia.com/en/fr/blog/couleurs-ggplot-meilleures-astuces-que-vous-allez-adorer/
# ggplot(orga) + geom_bar(aes(x = fct_infreq( regcode), fill=type)) + scale_fill_viridis(discrete = TRUE, option = "D") # == graduation
ggplot(orga, aes(x = fct_infreq(regcode), fill=type)) + geom_bar(aes(x = fct_infreq( regcode), fill=type)) + scale_fill_brewer(palette = "Paired") +
  geom_text(stat='count' , aes(label=..count.., alpha=ifelse(..count.. >2, 1, 0)), position = position_stack(vjust = 0.5))
# scale_fill_brewer(palette = "Dark2")

# ====== geobs nombre de meta et part odg ===========
geobs_md <- read_delim("G:/07_professionnel/CNAM_certif_OD/memoire/datas_reuse/GEOBS_GeoCatalogue_CSW_mod.csv", 
                       ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                       trim_ws = TRUE)

melt_obs <- melt(geobs_md, id=c("IDG","ECHELON"))
melt_obs$annee = 2017
# 
# melt_obs$annee <- ifelse(melt_obs$variable=='PART_OPENDATA_2017' , '2017', '2016')
# melt_obs$annee <- ifelse(melt_obs$variable=='PART_OPENDATA_2017' , '2017', melt_obs$variable)
# melt_obs$annee <- ifelse(melt_obs$variable=='NB_METADONNEE_2016' , '2016', melt_obs$variable)
# melt_obs$annee <- ifelse(melt_obs$variable=='NB_METADONNEE_2017' , '2017', melt_obs$variable)
# 
# melt_obs$nb_meta = "nom_var"
# melt_obs$nb_meta <-ifelse(melt_obs$variable=='NB_METADONNEE_2016' , 'nb_meta', melt_obs$nb_meta)
# melt_obs$nb_meta <-ifelse(melt_obs$variable=='NB_METADONNEES_2017' , 'nb_meta', melt_obs$nb_meta)
# melt_obs$nb_meta <-ifelse(melt_obs$variable=='PART_OPENDATA_2017' , 'part_od', melt_obs$nb_meta)
# melt_obs$nb_meta <-ifelse(melt_obs$variable=='PART_OPENDATA_2016' , 'part_od', melt_obs$nb_meta)

# dcast(melt_obs, IDG+ECHELON+ annee ~ nb_meta, value.var = "value")


# ggplot(subset(geobs_md, ECHELON=='Regional')) + geom_line(aes(x=as.factor(annee), y=NB_METADONNEE_2016, group=IDG, color=IDG))

# ggplot(subset(geobs_md, ECHELON=='Regional')) + geom_line(aes(group=as.factor(annee), y=PART_OPENDATA_2016, x=IDG))

# ggplot(subset(geobs_md, ECHELON=='Regional'), aes(x=as.factor(annee), y=NB_METADONNEE_2016, fill=IDG)) + geom_bar(stat="identity", position = "dodge")
ggplot(subset(geobs_md, ECHELON=='Regional'), aes(fill=as.factor(annee), y=NB_METADONNEE_2016, x=reorder(IDG, -NB_METADONNEE_2016))) + geom_bar(stat="identity", position = "dodge")  +
  labs(x="IDG",y="Nombre de fiches")
# https://stackoverflow.com/questions/53922846/plot-line-and-bar-graph-with-secondary-axis-for-line-graph-using-ggplot

# https://whatalnk.github.io/r-tips/ggplot2-secondary-y-axis.nb.html

# graphe total
ggplot(subset(geobs_md, ECHELON=='Regional'), aes(fill=as.factor(annee), y=NB_METADONNEE_2016, x=reorder(IDG, -NB_METADONNEE_2016))) + geom_bar(stat="identity", position = "dodge")  +
  labs(x="IDG",y="Nombre de fiches") + geom_point(aes(color=as.factor(annee), y=PART_OPENDATA_2016*400, x=reorder(IDG, -PART_OPENDATA_2016)), stat="identity") +
  scale_y_continuous(  sec.axis = sec_axis(~ . / 400  ,  name = "%"))
  



g1 <- ggplot(subset(geobs_md, ECHELON=='Regional'), aes(color=as.factor(annee), y=PART_OPENDATA_2016, x=reorder(IDG, -PART_OPENDATA_2016))) + geom_point(stat="identity") +
  labs(x="IDG",y="Nombre de fiches") 

g2 <- ggplot(subset(geobs_md, ECHELON=='Regional'), aes(color=as.factor(annee), y=PART_OPENDATA_2016, x=reorder(IDG, PART_OPENDATA_2016))) + geom_linerange(ymin="PART_OPENDATA_2016", ymax="PART_OPENDATA_2016") + coord_flip() +
  labs(x="IDG",y="Nombre de fiches")




  geom_point(aes(y=PART_OPENDATA_2016, color=annee))

ggplot(subset(geobs_md, ECHELON=='Regional'))  + geom_line(aes(y=PART_OPENDATA_2016, x=IDG))

geom_line(aes(group=as.factor(annee), y=PART_OPENDATA_2016, x=reorder(IDG, NB_METADONNEE_2016)))
+ coord_flip()

# + facet_wrap(ECHELON~.)

# https://github.com/onlyphantom/safeskies
