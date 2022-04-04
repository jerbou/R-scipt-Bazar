library(readxl)
library(tidyverse)
# View(df_adh)
library(highcharter)
library(RColorBrewer)
library(ggplot2)
library(plotly)
require(htmlwidgets)
require(ggpubr)
library(lubridate)

library(viridisLite)



# chargement du fichier ---------------------------------------------------
df_adh <- read_excel("C:/COPY_data_local/liste globale des adhérents.xlsx")

# on ne prend que les adherents au sens stricte, sans membre fondateurs
df_adhss <- subset(df_adh, numCollege!=1)

str(df_adh)

df_adh$ttc <- as.numeric(df_adh$ttc)
summary(as.numeric(df_adh$ttc))
summary(df_adh$ttc)
# moyenne 4106

no_ttc <- subset(df_adh, is.na(ttc))

mtcars %>%
  group_by(cyl) %>%
  summarise(mean = mean(disp), n = n())

recap_ttc <-
df_adh %>%
  group_by(numCollege) %>%
  summarise(sum = sum(ttc), moyenne= mean(ttc), median=median(ttc), n = n())

recap_ttc_dep <-
  subset(df_adh, numCollege!=1) %>%
  group_by(numCollege,CodeDepartement) %>%
  summarise(sum = sum(ttc), moyenne= mean(ttc), median=median(ttc), n = n())

recap_ttc_dep$CodeDepartement <- as.factor(recap_ttc_dep$CodeDepartement)
recap_ttc_dep$numCollege <- as.factor(recap_ttc_dep$numCollege)


# regroupement en classes de cotisation -----------------------------------

summary(df_adhss$ttc)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0     0.0   110.0  1184.6   878.8 25000.0

# 0 , 500 , 750, 1000, 2500 , 5000, 10000, 15000

plot(density(df_adh$ttc))

# df$x %>% replace_na(0)
df_adh$ttc <- df_adh$ttc %>% replace_na(0)
df_adhss$ttc <- df_adhss$ttc %>% replace_na(0)

plot(density((df_adh$ttc)))
plot(density((df_adhss$ttc)))

plot((df_adhss$ttc))


# excel avec chiffres clés ------------------------------------------------
summary(df_adhss$ttc)


# cotisation moyenne 
# sortir qq onglets dans un exceln chiffres cles (min, median, moyenne, max) PAR dep, ou par College


# 0 , 500 , 750, 1000, 2500 , 5000, 10000, 15000

df_adhss <- df_adhss %>%
  mutate(
    cotisation_classes_ttc = case_when(
      ttc<=1 ~ "gratuit",
      ttc>1 & ttc <=500 ~"inférieure à 500",
      ttc>500 & ttc <=750 ~"de 500 à 750",
      ttc>750 & ttc <=1000 ~"de 750 à 1000",
      ttc>1000 & ttc <=1500 ~"de 1000 à 1500",
      ttc>1500 & ttc <=2500 ~"de 1500 à 2500",
      ttc>2500 & ttc <=5000 ~"de 2500 à 5000",
      ttc>5000 & ttc <=10000 ~"de 5000 à 10000",
      ttc>10000 & ttc <=15000 ~"de 10000 à 15000",
      ttc>15000 ~"supéreur à 15000"
        )
              )

recap_cotis <- df_adhss %>% group_by(cotisation_classes_ttc) %>% tally()

recap_cotis_coll_dep <- df_adhss %>% group_by(cotisation_classes_ttc, CodeDepartement ) %>% tally()

recap_cotis_coll <- df_adhss %>% group_by(cotisation_classes_ttc, numCollege ) %>% tally()

# definir un ordre custom
recap_cotis <- recap_cotis %>%
  arrange(factor(
    cotisation_classes_ttc, levels = c("gratuit", 
                               "inférieure à 500",
                               "de 500 à 750",
                               "de 750 à 1000",
                               "de 1000 à 1500",
                               "de 1500 à 2500",
                               "de 2500 à 5000",
                               "de 5000 à 10000", 
                               "de 10000 à 15000",
                               "supéreur à 15000")
  ))

recap_cotis_coll_dep <- recap_cotis_coll_dep %>%
  arrange(factor(
    cotisation_classes_ttc, levels = c("gratuit", 
                                       "inférieure à 500",
                                       "de 500 à 750",
                                       "de 750 à 1000",
                                       "de 1000 à 1500",
                                       "de 1500 à 2500",
                                       "de 2500 à 5000",
                                       "de 5000 à 10000", 
                                       "de 10000 à 15000",
                                       "supéreur à 15000")
  ))

recap_cotis_coll <- recap_cotis_coll %>%
  arrange(factor(
    cotisation_classes_ttc, levels = c("gratuit", 
                                       "inférieure à 500",
                                       "de 500 à 750",
                                       "de 750 à 1000",
                                       "de 1000 à 1500",
                                       "de 1500 à 2500",
                                       "de 2500 à 5000",
                                       "de 5000 à 10000", 
                                       "de 10000 à 15000",
                                       "supéreur à 15000")
  ))






# graphes interactifs -----------------------------------------------------



graph_int3 <- top10_prod %>% hchart(type= "bar", hcaes(x=reorder(PRODUCTEUR_NOM,-nb_dataset), y=nb_dataset, group=reorder(typologie,-nb_dataset)),borderColor = "black",stacking = "normal", size=30) %>%
  hc_colors(brewer.pal(n = length(unique(nb_dataset_typo$typologie)), name = "Set1")) %>%
  hc_xAxis(categories = top10_prod$PRODUCTEUR_NOM , title = list(text = "Producteur")) %>% # categories est la bonne solution pour ordonner par la ligne souhtaite et pas par le groupe
  hc_yAxis(title = list(text = "Nombre de jeux de données"))%>%
  hc_caption(text = "ARNia, Mars 2022", align="right") %>%
  hc_title(text="<b>Top 10 des producteurs \n les plus prolifiques</b>", align="center")
# ajouter de la balise categoire permet d ordonner de casser la priorite sur le groupe !!!!!!
saveWidget(graph_int3, file="graph_int3_top10_prod_2022.html") # LA BONNE SOLUTION !!




chart_cotis_dep <- recap_cotis_coll_dep %>% hchart('column',stacking = "normal", hcaes(x= cotisation_classes_ttc, y = n, group=CodeDepartement )) %>%
  hc_colors(brewer.pal(n = length(unique(recap_cotis_coll_dep$CodeDepartement)), name = "Set1")) %>%
  hc_xAxis(categories = recap_cotis$cotisation_classes_ttc)%>% 
  hc_xAxis(title = list(text = "Classes de cotisation")) %>%
  hc_yAxis(title = list(text = "Nombre d'adhérents concernés")) %>%
  hc_title(
    text = "La cotis' en dataviz selon département",
    style = list(fontWeight = "bold", fontSize = "20px"),
    align = "center")

chart_cotis_coll <- recap_cotis_coll %>% hchart('column',stacking = "normal", hcaes(x= cotisation_classes_ttc, y = n, group=numCollege )) %>%
  hc_colors(brewer.pal(n = length(unique(recap_cotis_coll$numCollege)), name = "Set1")) %>%
  hc_xAxis(categories = recap_cotis_coll$cotisation_classes_ttc)%>% 
  hc_xAxis(title = list(text = "Classes de cotisation")) %>%
  hc_yAxis(title = list(text = "Nombre d'adhérents concernés")) %>%
  hc_title(
    text = "La cotis' en dataviz selon département",
    style = list(fontWeight = "bold", fontSize = "20px"),
    align = "center")


chart_cotis <- recap_cotis %>% hchart('column', hcaes(x= cotisation_classes_ttc, y = n )) %>%
  hc_xAxis(categories = recap_cotis$cotisation_classes_ttc)%>% 
  hc_xAxis(title = list(text = "Classes de cotisation")) %>%
  hc_yAxis(title = list(text = "Nombre d'adhérents concernés")) %>%
  hc_title(
    text = "La cotis' en dataviz",
    style = list(fontWeight = "bold", fontSize = "20px"),
    align = "center")
# 
# cols <- viridis(3)
# cols <- substr(cols, 0, 7)
# # 
# # highchart() %>% 
# #   hc_add_series(data = sample(1:12)) %>% 
# #   hc_add_series(data = sample(1:12) + 10) %>% 
# #   hc_add_series(data = sample(1:12) + 20) %>% 
# #   hc_colors(cols)

x <- c("Nom Adhérent", "collège", "Cotisation", "département", "nombre habitants")
y <- sprintf("{point.%s:.2f}", c("libelle", "numCollege", "ttc", "CodeDepartement", "NbreHabitants"))

tltip <- tooltip_table(x, y)


# graphe de cotisation / nombre d'habitants
graf5 <- highchart() %>%
  hc_add_series(col2, hcaes(NbreHabitants, ttc), type = "scatter", name = "collège 2") %>% # color = brewer.pal(n = 7, name = "Set1")[3],
  hc_add_series(col3, hcaes(NbreHabitants, ttc), type = "scatter",name = "collège 3") %>%
  hc_add_series(col4, hcaes(NbreHabitants, ttc), type = "scatter",name = "collège 4") %>%
  hc_add_series(col5, hcaes(NbreHabitants, ttc), type = "scatter",name = "collège 5") %>%
  hc_add_series(col6, hcaes(NbreHabitants, ttc), type = "scatter",name = "collège 6") %>%
  hc_add_series(col7, hcaes(NbreHabitants, ttc), type = "scatter",name = "collège 7") %>%
  hc_colors( brewer.pal(n = 6, name = "Set1")) %>%
hc_tooltip( useHTML = TRUE, headerFormat = "",  pointFormat = tltip)%>% 
  hc_yAxis(title = list(text = "Cotisation en   TTC")) %>%
  hc_xAxis(title = list(text = "Nombre Habitants"))



# en cours sur ggplotly / passage en log OK, mais affichage a travailler
g1 <- ggplotly(ggplot(data=df_adhss, aes(text=libelle  , x=log(NbreHabitants), y=ttc, color=as.factor(numCollege))) + geom_point(), dynamicTicks="TRUE", tooltip = c("ttc", "libelle", "NbreHabitants"), autorange = TRUE)


# TO UPGRADE
# https://dantonnoriega.github.io/ultinomics.org/posts/2017-04-05-highcharter-explainer.html

# une bonne forme a la tidyway 
hchart(df_adhss, type= 'scatter', hcaes(x= NbreHabitants, y = ttc, group=numCollege)) %>%
  hc_tooltip( useHTML = TRUE, headerFormat = "",  pointFormat = tltip)%>% 
  hc_yAxis(title = list(text = "Cotisation en   TTC")) %>%
  hc_xAxis(title = list(text = "Nombre Habitants"), type= 'logarithmic') %>%
  hc_colors( brewer.pal(n = 6, name = "Set1"))


# data(citytemp)
# hc1 <- highchart() %>%
#   hc_chart(backgroundColor = "white") %>%
#   hc_xAxis(categories = citytemp$month) %>%
#   hc_add_series(name = "Tokyo", data = citytemp$tokyo) %>%
#   hc_add_series(name = "London", data = citytemp$london)
# hc1
# 
# citytemp2 <- citytemp %>%
#   tidyr::gather(key = city, value = temperature, tokyo, london)


# hchart(citytemp2, type = 'line', hcaes(y = temperature, group = city, x = month)) %>%
#   hc_chart(backgroundColor = "white")
# 
# hchart(df_adhss, type= 'scatter', hcaes(x= NbreHabitants, y = ttc, group=numCollege))

graf4 <- df_adhss %>% hchart('scatter', hcaes(x= NbreHabitants, y = ttc, color=numCollege, group=numCollege)) %>%
  # hc_colors(brewer.pal(n = length(unique(df_adhss$numCollege)), name = "Set1")) %>% 
  hc_tooltip(  useHTML = TRUE,  headerFormat = "",  pointFormat = tltip)

# decoupage en serie pour highcharter
col2 <- df_adhss %>% filter(numCollege == "2")
col3 <- df_adhss %>% filter(numCollege == "3")
col4 <- df_adhss %>% filter(numCollege == "4")
col5 <- df_adhss %>% filter(numCollege == "5")
col6 <- df_adhss %>% filter(numCollege == "6")
col7 <- df_adhss %>% filter(numCollege == "7")



# Durabilite de l'adhesion ------------------------------------------------

df_adhss$adh_an<-  year(now()) - year(df_adhss$datedélib)
hist(df_adhss$adh_an)

str(df_adhss)

# il faut construire la donnee de recap
recap_adh_an <- df_adhss %>% group_by(adh_an, numCollege) %>% tally()

ggplot(df_adhss) + geom_bar(aes(x=adh_an, fill=as.factor(numCollege)))

chart_adh_an <- recap_adh_an %>% hchart('column',stacking = "normal",  hcaes(x= adh_an, y = n, group=numCollege )) %>%
  # hc_xAxis(categories = recap_adh_an$cotisation_classes_ttc)%>% 
  hc_xAxis(title = list(text = "Anniversaire d'adhésion")) %>%
  hc_yAxis(title = list(text = "Nombre d'adhérents concernés")) %>%
  hc_title(
    text = "L'ancienneté d'adhésion en années et par collèges",
    style = list(fontWeight = "bold", fontSize = "20px"),
    align = "center")


# # une bonne forme a la tidyway 
# hchart(df_adhss, type= 'scatter', hcaes(x= NbreHabitants, y = ttc, group=numCollege)) %>%
#   hc_tooltip( useHTML = TRUE, headerFormat = "",  pointFormat = tltip)%>% 
#   hc_yAxis(title = list(text = "Cotisation en   TTC")) %>%
#   hc_xAxis(title = list(text = "Nombre Habitants"), type= 'logarithmic') %>%
#   hc_colors( brewer.pal(n = 6, name = "Set1"))

# sauvegarde en fichiers --------------------------------------------------
saveWidget(chart_cotis, file="graf1_cotiz.html") # LA BONNE SOLUTION !!
saveWidget(chart_cotis_dep, file="graf1_cotiz_dep.html") # LA BONNE SOLUTION !!

saveWidget(graf5, file="graf5_cotiz_hab.html") # LA BONNE SOLUTION !!
saveWidget(chart_adh_an, file="graf_adh_an.html") # LA BONNE SOLUTION !!

# 
# 
# 
# hc <- recap_ttc_dep %>%
#   hchart('column', hcaes(y = sum, x=, color=numCollege))

# recap_ttc_dep %>%
#   hchart("column", stacking = "normal", hcaes(y = sum, x=CodeDepartement, group=numCollege, color=numCollege))%>%
#   hc_colors(brewer.pal(n = length(unique(recap_ttc_dep$numCollege)), name = "Set1"))

# epaissuer du trait par variable n (nombre d adherents par departement)



# x <- c(rnorm(10000), rnorm(1000, 4, 0.5))
# hchart(x, name = "data") 
# hchart(df_adh$ttc)
# hchart(density(df_adhss$ttc), type = "area",  name = "Cotisation ttc")

# hchart(df_adhss$ttc, type = "area",  name = "Cotisation ttc")%>%
#   hc_xAxis(categories = c("0", "500", "1000", "1500", "2000", "4000", "5000", "10000"),
#            tickmarkPlacement = "on")


# df_adhss %>% hchart("scatter", hcaes(x=NbreHabitants, y=ttc, color=numCollege, group=numCollege, size=ttc),
#                     maxSize = "10%") %>%
#   hc_tooltip(
#     crosshairs = FALSE,
#     # borderWidth = 5,
#     sort = TRUE,
#     table = TRUE
#   ) 

# https://stackoverflow.com/questions/46953400/r-highcharter-tooltip-customization
# df_adhss %>% hchart("scatter", hcaes(x=NbreHabitants, y=ttc, color=numCollege, group=numCollege, size=ttc),
#                     maxSize = "10%") %>%
#   hc_tooltip(
#     crosshairs = FALSE,
#     # borderWidth = 5,
#     sort = TRUE,
#     table = TRUE
#   )

# Définir les options de highcharter
options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 0)))


# f <- wdata %>% filter(sex == "F")
# m <- wdata %>% filter(sex == "M")
# hc <- hchart(
#   density(m$weight), type = "area", 
#   color = "steelblue", name = "Male"
# ) %>%
#   hc_add_series(
#     density(f$weight), type = "area",
#     color = "#B71C1C", 
#     name = "Female"
#   )



length(unique(df_adhss$numCollege))

palette("rainbow")
display.brewer.all()

# Visualiser une seule palette RColorBrewer en spécifiant son nom
display.brewer.pal(n = 7, name = 'Set1')
# Spécification de couleur hexadécimale 
brewer.pal(n = 7, name = "Set1")

brewer.pal(n = 7, name = "Set1")[2]


data(stars)

colors <- c(
  "#FB1108", "#FD150B", "#FA7806", "#FBE426", "#FCFB8F",
  "#F3F5E7", "#C7E4EA", "#ABD6E6", "#9AD2E1"
)

stars$color <- highcharter::colorize(log(stars$temp), colors)

# x <- c("Luminosity", "Temperature", "Distance", "Radius")
# y <- sprintf("{point.%s:.2f}", c("lum", "temp", "distance", "radiussun"))


# tltip <- tooltip_table(df_adhss$libelle, df_adhss$numCollege)

# TO dO ! enlever les colleges dépourvu de population





graf3_com <- hchart(
  col2, hcaes(NbreHabitants, ttc, color=ttc), type = "scatter",
  color = brewer.pal(n = 7, name = "Set1")[1], name = "collège 2"
) %>%
  hc_add_series(
    col3, hcaes(NbreHabitants, ttc), type = "scatter",
    color = brewer.pal(n = 7, name = "Set1")[2],
    name = "collège 3"
  ) %>%
  hc_add_series(
    col4, hcaes(NbreHabitants, ttc), type = "scatter",
    color = brewer.pal(n = 7, name = "Set1")[3],
    name = "collège 4") %>%
  # hc_add_series(
  #   col5, hcaes(NbreHabitants, ttc), type = "scatter",
  #   color = brewer.pal(n = 7, name = "Set1")[4],
  #   name = "collège 5") %>%
  hc_add_series(
    col6, hcaes(NbreHabitants, ttc), type = "scatter",
    color = brewer.pal(n = 7, name = "Set1")[5],
    name = "collège 6") %>%
  hc_add_series(
    col7, hcaes(NbreHabitants, ttc), type = "scatter",
    color = brewer.pal(n = 7, name = "Set1")[6],
    name = "collège 7") %>%
  hc_tooltip(
    useHTML = TRUE,
    headerFormat = "",
    pointFormat = tltip
  )%>% 
  hc_yAxis(title = list(text = "Cotisation en   TTC")) %>%
  hc_yAxis(title = list(text = "Relation Cotisation, Nombre Habitants"))


ggplot(data = df_adhss) + geom_point(aes(x=NbreHabitants, y=ttc, fill=as.factor(numCollege)))


  # hc_colors(brewer.pal(n = length(unique(df_adhss$numCollege)), name = "Set1")) %>%
  # hc_tooltip(useHTML = TRUE,headerFormat = "",pointFormat = tltip)
  # hc_xAxis(NbreHabitants)%>% 
  # hc_yAxis(title = list(text = "Nombre d'adhérents concernés")) %>%
  # hc_title(
  #   text = "La cotis' en dataviz",
  #   style = list(fontWeight = "bold", fontSize = "20px"),
  #   align = "center")

#https://dantonnoriega.github.io/ultinomics.org/posts/2017-04-05-highcharter-explainer.html

# https://jkunst.com/highcharter/articles/showcase.html
  
hc <- hchart(
  col2, hcaes(NbreHabitants, ttc), type = "scatter",
  name = "collège 2"
) %>%
  hc_add_series(
    col3, hcaes(NbreHabitants, ttc), type = "scatter",
    name = "collège 3"
  ) %>%
  hc_add_series(
    col4, hcaes(NbreHabitants, ttc), type = "scatter",
    color = "#B71CDC",
    name = "collège 4")

# https://stackoverflow.com/questions/46953400/r-highcharter-tooltip-customization
highchart() %>%
  hc_add_series(data = df_adhss, hcaes(NbreHabitants, ttc), name = "Shipments", type = "scatter",
                tooltip = list(pointFormat = "tooltip with 2 values"))



# ggplot et plotly --------------------------------------------------------

set.seed(100)
d <- diamonds[sample(nrow(diamonds), 1000), ]

p <- ggplot(data = d, aes(x = carat, y = price)) +
  geom_point(aes(text = paste("Clarity:", clarity)), size = 4) +
  geom_smooth(aes(colour = cut, fill = cut)) + facet_wrap(~ cut)

fig <- ggplotly(p)

fig

g1 <- ggplotly(ggplot(data=df_adhss, aes(text=libelle  , x=log(NbreHabitants), y=ttc, color=as.factor(numCollege))) + geom_point(), dynamicTicks="TRUE", tooltip = c("ttc", "libelle", "NbreHabitants"), autorange = TRUE)
?ggplotly
#https://plotly.com/ggplot2/interactive-tooltip/



saveWidget(g1, file="highchart.html") # LA BONNE SOLUTION !!
