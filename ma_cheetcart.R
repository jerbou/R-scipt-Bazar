# === Toutes les fonctions utiles dans R pour travailler ===
# 
# ==== 01: Connexion aux bdd ====
# Connexion à une bdd ACCESS
odbcCloseAll()
db <- "S:/SIG_ADMIN/MAJ/site_cadastre/Sites_cadastre.mdb"
db_TDB <- "S:/00_BASES_DE_DONNEES/BDD_Tableau_de_Bord/BD_Tableau_de_Bord.mdb"
# db_fon<-"S:/00_BASES_DE_DONNEES/BD_FONCIER/BD_Foncier.mdb"
channel <- odbcConnectAccess(db)
channel_TDB <- odbcConnectAccess(db_TDB)
# ! a vÃ©rifier les requetes
Sites<-sqlFetch(channel=channel,sqtable="RQ_Sites_S")
Sites_CEN<-sqlFetch(channel=channel,sqtable="RQ_Sites_S_No_sissonne")

# Connexion à une bdd MySQL
# On va chercher le ref faune pour faire une jointure
Connex<-odbcConnect(dsn="bd_faune_flore",uid="util",pwd="csnp")
# odbcDataSources(type = ("all"))

# Voir les tables de la connexion
sqlTables(Connex)

# Chargement plusieurs fichiers et compilation des fichiers ----------------------------------
# https://michaelinom.medium.com/how-to-combine-all-csv-files-from-the-same-folder-into-one-data-frame-automatically-with-r-1775876a876c
  setwd("V:/07_IDÃ©O/12_Donnees/demandes/gip_com_num/Fichiers contacts ComNum/Dpt 21/")
  list.files()
  # test unitaire
  # X21J <- read_excel("V:/07_IDÃ©O/12_Donnees/demandes/gip_com_num/Fichiers contacts ComNum/Dpt 21/21J.xlsx")
  # on definit la liste des fichiersa prendre
  fichiers <- Filter(function(x) grepl("21", x), list.files())
  # https://stackoverflow.com/questions/18028225/r-list-files-with-multiple-conditions/38850156
  # on rassemble le tout
  d21 <- ldply(fichiers, read_excel)
  dim(d21) # 583

  setwd("V:/07_IDÃ©O/12_Donnees/demandes/gip_com_num/Fichiers contacts ComNum/Dpt 39/")
  fichiers <- Filter(function(x) grepl("39", x), list.files())
  d39 <- ldply(fichiers, read_excel)

  setwd("V:/07_IDÃ©O/12_Donnees/demandes/gip_com_num/Fichiers contacts ComNum/Dpt 58/")
  fichiers <- Filter(function(x) grepl("58", x), list.files())
  d58 <- ldply(fichiers, read_excel)

  df_reg <- bind_rows(d21,d39,d58)
  View(df_reg)


# telechargement des donnees shp et les ouvrir -----------------------------------
# https://stackoverflow.com/questions/18967722/download-and-read-shapefile-function-in-r
dlshape=function(shploc, shpfile) {
  temp=tempfile()
  download.file(shploc, temp)
  unzip(temp)
  shp.data <- sapply(".", function(f) {
    fp <- file.path(temp, f)
    return(readOGR(".",shpfile))
  })
}

foncier = dlshape(shploc="https://trouver.ternum-bfc.fr/dataset/38548b3f-83eb-4e4f-8a1d-b11ba8334f7c/resource/68f7894e-02ec-455c-98eb-52c54e7a6585/download/r_foncier_economique_aerbfc_r27_poly_lib.zip", "r_foncier_economique_aerbfc_r27_poly_lib")



# Pour postgreSQL
# http://neocarto.hypotheses.org/1186
con <- dbConnect(dbDriver("PostgreSQL"), host="192.168.1.2", port="5432",dbname="EUPHORBE", user="user", pass="mdp")
dsn="PG:host='192.168.1.2', port='5432', dbname='EUPHORBE', user='postgres', pass='burotec'"
readOGR(dsn,"travaux_surf")

# Définition des paramètres de connexion
con <- dbConnect(dbDriver("PostgreSQL"), dbname="database",  port = 54xx, user='user', pass='mpd')
dbGetQuery(con, "SHOW SERVER_ENCODING")
df0 = dbGetQuery(con, "SELECT * FROM inpn.bdc_status_11")
# df1 = dbGetQuery(con, "SELECT cd_type_statut , count(cd_ref) FROM inpn.bdc_status_11 group by 1 ")
df1 = dbGetQuery(con, "SELECT cd_type_statut , label_statut, lb_adm_tr , count(cd_ref) FROM inpn.bdc_status_11 group by 1,2,3 ")
df2 = subset(df1, lb_adm_tr != 'France' & lb_adm_tr != 'France métropolitaine')

# postgresqlpqExec(con, "SET client_encoding = 'windows-1252'")
declare_utf8 <- function(x) {
  Encoding(x) <- "UTF-8"
  x
}
# https://stackoverflow.com/questions/44558854/wrong-text-encoding-returned-when-query-from-postgresql-database-using-rpostgres

df0 <- df0 %>% mutate_if(is.character, declare_utf8)


# lister les tables présentes dans la bdd
dbListTables(con)
readGDAL(dsn=con,"travaux_surf")

# SAuvegarder entre R et PostGreSQL
# https://claudiavitolo.com/2012/07/05/writing-tables-into-a-postgresql-database-using-r/
# http://stackoverflow.com/questions/10032390/writing-to-specific-schemas-with-rpostgresql



# ==== 02: Les REQUETES ====
# ref<-sqlFetch(Connex,sql)
sql<- "SELECT * FROM w_ref_faune WHERE w_ref_faune.famille ='Syrphidae'"
# Le rÃ©fÃ©rentiel famille syrphidae
SYRPH <- sqlQuery(Connex, sql)

# On étiquette les noms des sp
rownames(traits.PCA)<-traits[,1]

# Changer le nom d'une colonne dans un df
names(MOR)[names(MOR)=="Apb_Moreuil_14.08.2013"]<-"Apb_Moreuil_14/08/2013"

# ==== 03: Les JOINTURES ====
nodes3 <- sqldf("SELECT ID, x  FROM nodes JOIN first USING(ID)")
# http://stackoverflow.com/questions/1299871/how-to-join-merge-data-frames-inner-outer-left-right
nodes3 <- merge(x = nodes, y = first, all.x = TRUE)
# JOINTURE ATTENTION AU FORMAT DES DONNEES
# === Error in postgresqlNewConnection(drv, ...) : 
# === RS-DBI driver: (could not connect postgres@localhost on dbname "test"
# === Error in !dbPreExists : invalid argument type


# ==== 04 : Gestion d objets ====
rm(list=ls()) #will remove ALL objects

# Selection cretion d'un nouvel objet
df3<-subset(df2,select = -c(releve,ferti,fauche,MAEt))

# Selection d'un tableau avec certaine colonnes
Gbin2 <-Gbin1[c("individu","depX","depY","arX","arY","ind_pos.x","ind_pos.y")]

# selection d un dataframe a partir d un selection de valeur d un autre dataframe
    # creation du df selon de criteres, ici le plus de ligne et on prend le tom 20
top20 <- df_cat %>% group_by(organization_id, organization) %>% count() %>% arrange(-n) %>% head(20)
    # faire un liste a partir un facteur c(top20$organization_id)
df_cat20 <- df_cat %>% filter(organization_id %in% c(top20$organization_id))
                   
                     
                     
                     
# On nettoie les NA
# On nettoie
e41[is.na(e41)]<-0

# Type d'objet
typeof(a)

# Transofmation 
# Ajout d'une nouvelle colonne
BIM<-transform(BIM,"CATEGORIE"=paste(TYPE_MILIEUX_RA,variable,sep="_"))
# ajout d'un colonne avec un equivalent a left
df0<-transform(df0,"dept"=substr(CP,1,2))

# ==== 041 : transformation d'une liste en une nouvelle liste ====
# https://stackoverflow.com/questions/17866281/r-ifelse-on-string
myfun <- function(x) {
  switch(x,
         'ACTIVITES_SANITAIRES_SOCIALES_PROTECTION_CIVILE'='protection',
         'COMMUNES_3500_A_20000_HABITANTS_BOURGOGNE'='commune_35k_20k',
         'COMMUNES_MOINS_3500_HABITANTS_COTE_D_OR' = 'commune_35k',
         'ETABLISSEMENTS_DE_SANTE'= 'santé',
         'ETABLISSEMENTS_ENSEIGNEMENT_ET_RECHERCHE' = 'enseignement',
         'GROUPEMENTS_DE_COLLECTIVITES' = 'group_collectivi',
         'MEMBRES_FONDATATEURS' = 'fondateurs',
         'ORGANISMES_DIVERS' = 'divers',
         'SYNDICATS_INTERCOMMUNAUX' = 'syndicats',
         'COMMUNES_MOINS_3500_HABITANTS_NIEVRE' ='commune_35k',
         'COMMUNES_PLUS_20000_HABITANTS_BOURGOGNE'= 'commune_200k',
         'COMMUNES_MOINS_3500_HABITANTS_SAONE_LOIRE' = 'commune_35k',
         'COMMUNES_MOINS_3500_HABITANTS_YONNE' = 'commune_35k'
  )
}

df0$new_typo = sapply(df0$college, myfun)


# discretisation a la mano
# https://stackoverflow.com/questions/15497694/cut-function-in-r-labeling-without-scientific-notations-for-use-in-ggplot2
data_com_bfc$group <- cut(data_com_bfc$pop_2016, breaks=c(0, 30000, 50000, 100000,125000, 150000,max(data_com_bfc$pop_2016)), include.lowest=TRUE, dig.lab=10)
data_epci_bfc$group <- cut(data_epci_bfc$pop_2016, breaks=c(0, 30000, 50000, 100000,125000, 150000,max(data_epci_bfc$pop_2016)), include.lowest=TRUE, dig.lab=10)
melt(subset(data_com_bfc, data_com_bfc$pop_2016> 30000), id=c("group"))

sel1 <- subset(data_com_bfc, data_com_bfc$pop_2016 >= 30000)
sel2 <- subset(data_epci_bfc, data_epci_bfc$pop_2016 >= 30000)

ggplot(data=sel1) + geom_bar(aes(x=group)) + labs(y="nombre de communes", x="découpage selon population", title="Le nombre de communes \n au dessus de 30 000 habitants", caption="Source INSEE, 2018") # +
# scale_x_continuous(labels = function(x) format(group, scientific = TRUE))

ggplot(data=sel2) + geom_bar(aes(x=group)) + labs(y="nombre d'intercommunalités", x="découpage selon population", title="Le nombre d'intercommunalités \n au dessus de 30 000 habitants", caption="Source INSEE, 2018") # +
# +

res_com <- dcast(sel1, group~libelle, value.var="libelle")
resi_interco <- dcast(sel2, group~libelle, value.var="libelle")

# create a new column `x` with the three columns collapsed together
res_com$liste <- apply( res_com[ , names(res_com[2:7]) ] , 1 , paste , collapse = "," )
resi_interco$liste <- apply( resi_interco[ , names(resi_interco[2:18]) ] , 1 , paste , collapse = "," )

# https://stackoverflow.com/questions/14568662/paste-multiple-columns-together
res_com

res_com$liste <- gsub(",NA," , " ", res_com$liste, ignore.case=T)
# sub(pattern = ',NA,', replacement = ' ' ,res_com$liste )
res_com$liste <- gsub("NA\\s" , " ", res_com$liste, ignore.case=F)
res_com$liste <- gsub("*,*" , " ", res_com$liste, ignore.case=F)



# creation d objet avec expression reguliere
dftot <- within(dftot, grepl("no_*", variable)) <- 'non'


# tranformation avec une condition
V_env<-transform(V_env,"etat_num"= ifelse(Etat=="B",1,0))

# mise avec condition
# http://stackoverflow.com/questions/5545625/conditional-update-of-dataframe
dftot$geoloc <- ifelse(test = grepl("no_*", dftot$variable), yes='non_geoloc', no='geoloc')



# citation des libraries utilisees
citation(package="reshape2")
citation(package="ggplot2")
citation(package="rgdal")
citation(package="xlsx")
citation(package="plyr")


# trouver la position d'un colonne a partir de son nom
which(names(df)=="bar")



# identifier la colonne ayant la valeur max pour chaque ligne
# http://stackoverflow.com/questions/21039904/which-max-and-assign-colnames-value-to-dataframe-in-r
new$type <- names(new)[apply(new[-1], 1, which.max) + 1]


#https://www.r-bloggers.com/how-to-use-lists-in-r/
mylist<-list(x=c(1,5,7), y=c(4,2,6), z=c(0,3,4))


# ==== 05 : Gestion des valeurs ====
# On change TOUTES LES valeurs 2 en un 1
rex2[rex2=="2"]<-1

# Autre méhode avec une fonction ifelse
# http://stackoverflow.com/questions/23355806/invalid-factor-level-na-generated-r
FLORE_IG2$code <- ifelse(FLORE_IG2$taxon=="Dactylorhiza majalis (Reichenb.) P.F. Hunt et Summerh.","Dmj",0)

# Avec méthode gsub
# http://stackoverflow.com/questions/5487164/r-how-to-replace-parts-of-variable-strings-within-data-frame
CD13<-as.data.frame(sapply(CD12,gsub,pattern="/0",replacement=""))

# gestion et foramtage en date
# http://stackoverflow.com/questions/36568070/extract-year-from-date

# Pour nettoyer une colonne contenant des valeurs nulles en texte (NULL)
# https://stackoverflow.com/questions/34469178/r-convert-factor-to-numeric-and-remove-levels
df0$montant_contrat <- as.numeric(as.character(df0$montant_contrat))
df0[is.na(df0)]<-0


# Changement d'odre
# Réordonner selon plusieurs colonnes
# http://www.markhneedham.com/blog/2013/01/23/r-ordering-rows-in-a-data-frame-by-multiple-columns/
BICO<-BIC[with(BIC, order (TYPE_MILIEUX_RA,variable)),]

# On réordonne
BICO<-BIC[order
          (factor
          (BIC$variable,
          levels=c("SURF_GESTION.sum","SURF_ASSIST.sum"))),]


# Permet de créer une table avec les valeurs uniques de la colonne du df RD6
df7<-as.data.frame(unique(RD6$Parcelle))


# Compter le nombre d'occurence
# http://forums.cirad.fr/logiciel-R/viewtopic.php?t=888

# Compter en prenant compte de facteru
# http://stackoverflow.com/questions/9809166/count-number-of-rows-within-each-group
df2 <- count(df0, c('t1_sexe', 'age'))


# ==== encodage
Encoding(b$libelle_fr)<-"UTF-8"
# Réencoder la table correctement
# http://forums.cirad.fr/logiciel-R/viewtopic.php?t=5811

# ==== 06 : Sémiologie / Mise en couleur ====
# http://www.r-bloggers.com/r-using-rcolorbrewer-to-colour-your-figures-in-r/

# === AFFICHAGE ===
# découper la fenetre graphique en sous fenetres
par(mfrow = c(3,3))

# === Passage d'une matrice abondance à une matrice de presence absence
# http://stackoverflow.com/questions/14526429/turn-a-count-matrix-into-a-binary-existence-matrix
Rpres<- as.matrix((Rphy3b > 0) + 0)

# ==== 07 traitement de data ====
# === 071 :Compter les nombres de colonnes ayant des valeurs ====
# http://stackoverflow.com/questions/18862114/count-number-of-columns-by-a-condition-for-each-row
# head(subset(basc, nb_val >1))

# === Stocker en attribut le nom de la colonne ayant le max dans le dataframe
# http://stackoverflow.com/questions/17735859/for-each-row-return-the-column-name-of-the-largest-value
df$cult_maj <- colnames(df)[apply(df,1,which.max)]

# === customisation des notations ===
options(scipen=999) # pour rester sur de la notation classique et éviter la notation scientifique par défaut



# === 08 : Impresssion raster, Export sous forme d'image ici en PDF ====
pdf("dendro_releve.pdf", width = 16.53, height=11.65)
plot(hclust(vegdist(Rpres3,method="jaccard"),method="ward.D2"),cex=0.5,hang=0.25,main="CASDAR 2015 Dendrogramme \n sur relevés de Camille Delaplace",ylab="Distance de Similarité")

# === Color B brewer ===
display.brewer.pal(n = 8, name = 'Dark2')
display.brewer.all()
# http://www.stat.ubc.ca/~jenny/STAT545A/block14_colors.html



# ==== 09 : Parser un dossier contenant plusieurs xml et en faire un df ====
# objectif integrer tout les donnÃ©es des PECI a partir de la donnee **OD
# https://www.data.gouv.fr/fr/datasets/service-public-fr-annuaire-de-l-administration-base-de-donnees-locales/

# Scrpit ressource
# http://stackoverflow.com/questions/22676706/parsing-multiple-xml-files-to-a-single-dateframe-in-r

# ==== chargement des libraireis
library(dplyr)
library(XML)
library(plyr)
library(xmlparsedata)
library(data.table)
library(dtplyr)

# setwd("G:/00_data_ref/data_gouv_fr/annuaire_admini/all_latest(1).tar/all_20170218/bfc_select")
# setwd("G:/00_data_ref/data_gouv_fr/annuaire_admini/all_latest(1).tar/all_20170218/bfc_select")
# setwd("C:/COPY_data_local/all_latest(2)/BFC_select_epci")
setwd("G:/00_data_ref/data_gouv_fr/annuaire_admini/all_latest(2).tar/BFC/organismes/mairies")

rm(files)
# liste des fichiers dans le working dir
files <- list.files()# recursive = TRUE, full.names= FALSE)
# creer la fonction
parse_xml2 <-function(FileName) {
  doc1 <- xmlParse(FileName)
  doc <-xmlToList(doc1)
  doc3 <-as.data.frame(doc,stringsAsFactors=FALSE)
}

# lancer la fonction nouvellement crée pour creer le final df
rm(mydf)
dfmairie <-ldply(files,parse_xml2)



# ===== chargement de shapefiles via rgdal
require.libryr(rgdal)
shp0 <- readOGR(dsn = ".", layer = "LIVRAISON_PI_SDIS21_15012018", encoding = "UTF-8", use_iconv = TRUE)
# charge en spécificant l encodage et une conversion
head(shp0)
# on recupere la partie attributaire en dataframe, avec coord (si ponctuels, x, y...)
df0 <- as(shp0, "data.frame")



# ==== Librairie plyr ====
# Créer des sous data frame ($sp) au sein d'un data frame
testlist <- dlply(iris, .(Species))

testlist2<-dlply(FLORE_IG, .(nomsitep))

# ==== Exporter des csv autant de valeur unique, ici nomsite ====
# Fonction pour créer autant de csv que d'occurence ici, nomsitep
# https://ariel85fuentes.wordpress.com/2014/12/29/writing-multiple-csv-files-from-a-xlsx/
d_ply(FLORE_IG, .(IDENT_SITE),function(sdf) write.csv(sdf,file=paste(sdf$IDENT_SITE[[1]],".csv",sep="")))


# ==== Analyse de données ====

# AFFicher de l'ACP avec ggplot
# http://stackoverflow.com/questions/6578355/plotting-pca-biplot-with-ggplot2
# http://stackoverflow.com/questions/30056144/r-ggfortify-objects-of-type-prcomp-not-supported-by-autoplot

PCbiplot <- function(PC, x="PC1", y="PC2") {
  # PC being a prcomp object
  data <- data.frame(obsnames=row.names(PC$x), PC$x)
  plot <- ggplot(data, aes_string(x=x, y=y)) + geom_text(alpha=.4, size=3, aes(label=obsnames))
  plot <- plot + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2)
  datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
  mult <- min(
    (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
  )
  datapc <- transform(datapc,
                      v1 = .7 * mult * (get(x)),
                      v2 = .7 * mult * (get(y))
  )
  plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), size = 5, vjust=1, color="red")
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="red")
  plot
}


# boxplot SANS outliers
# http://stackoverflow.com/questions/25124895/no-outliers-in-ggplot-boxplot-with-facet-wrap

# Multiplot on ggplot2
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}






# http://www.sthda.com/english/wiki/ggplot2-multiplot-put-multiple-graphs-on-the-same-page-using-ggplot2
install_github("easyGgplot2", "kassambara")
library(easyGgplot2)

library(devtools)
install_github('sinhrks/ggfortify')
library(ggfortify)





# resume par group
# https://stackoverflow.com/questions/9847054/how-to-get-summary-statistics-by-group
# http://stat545.com/block024_group-nest-split-map.html
# tapply(df0$college, df0$cotisation_annuelle, function(x) format(summary(x), scientific = TRUE))
resume <- df0 %>%
  group_by(nature_ad) %>%
  summarize_each(funs(length, min, median, mean, max, sum), cotisation_annuelle)
names(resume) <- c("college","nombre","min","medianne","moyen","max", "somme")


# ==== RMARKDOWN ====
# source pour presentation markdonw
# http://www.hafro.is/~einarhj/education/tcrenv2016/pre/r-markdown-example.Rmd

# Ressources pour afficher des tables sous Rmarkdown / knitr
# https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html


# ==== GGPLOT ressource ====
# === Reverse order tips ====
# https://stackoverflow.com/questions/42710056/reverse-stacked-bar-order

# ==== TO DO : frise de cotisation avec colorisaiton par group / college
# https://stackoverflow.com/questions/46027258/r-graph-label-by-group

# ==== ordonner graphes
# https://stackoverflow.com/questions/46554429/ggplot2-reordering-special-variable-count
# https://stackoverflow.com/questions/5208679/order-bars-in-ggplot2-bar-graph
# http://www.reed.edu/data-at-reed/resources/R/reordering_geom_bar.html
# https://stackoverflow.com/questions/37527053/order-multiple-geom-bar-in-ggplot2-bargraph

# ==== density stacked ====
# https://stackoverflow.com/questions/12980081/create-a-stacked-density-graph-in-ggplot2


# https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2

# pour mettre plusieurs graphes
# http://www.sthda.com/french/wiki/ggplot2-combiner-plusieurs-graphiques-sur-la-m-me-page-logiciel-r-et-visualisation-de-donn-es

