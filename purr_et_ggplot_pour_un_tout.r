

# https://thinkr.fr/comment-faire-des-boucles-en-r-ou-pas/

# Ma matrice :
my_mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
# Vecteur qui contiendra la somme de chaque ligne
vec_sum <- apply(my_mat, MARGIN = 1, FUN = sum)
# sapply pour renvoyer un vecteur :
my_list <- list(
  m1 = matrix(1:4, nrow = 2, ncol = 2),
  m2 = matrix(5:8, nrow = 2, ncol = 2)
)
sum_mat <- sapply(my_list, FUN = sum)

# faire un diagramme de sankey
sankey <- data.frame(
  source = c("a", "b", "c", "d", "c"),
  target = c("b", "c", "d", "e", "e"),
  value = ceiling(rnorm(5, 10, 1)),
  stringsAsFactors = FALSE
)


# tutoriel pour enchainer les graphes ----------------------------------------------------------------
# https://aosmith.rbind.io/2018/08/20/automating-exploratory-plots/"
# https://www.r-bloggers.com/2017/03/make-ggplot2-purrr/


library(ggplot2) # v. 3.3.3
library(purrr) # v. 0.3.4

set.seed(16)
dat = data.frame(elev = round( runif(20, 100, 500), 1),
                 resp = round( runif(20, 0, 10), 1),
                 grad = round( runif(20, 0, 1), 2),
                 slp = round( runif(20, 0, 35),1),
                 lat = runif(20, 44.5, 45),
                 long = runif(20, 122.5, 123.1),
                 nt = rpois(20, lambda = 25) )
head(dat)

response = names(dat)[1:3]
expl = names(dat)[4:7]

response = set_names(response)
response

expl = set_names(expl)
expl

scatter_fun = function(x, y) {
  ggplot(dat, aes(x = .data[[x]], y = .data[[y]]) ) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE, color = "grey74") +
    theme_bw()
}

scatter_fun = function(x, y) {
  ggplot(dat, aes_string(x = x, y = y ) ) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE, color = "grey74") +
    theme_bw() 
}

scatter_fun(x = "lat", y = "elev")

elev_plots = map(expl, ~scatter_fun(.x, "elev") )
elev_plots

all_plots = map(response,
                ~map(expl, scatter_fun, y = .x) )

all_plots2 = map(response, function(resp) {
  map(expl, function(expl) {
    scatter_fun(x = expl, y = resp)
  })
})

all_plots$grad[1:2]
all_plots$grad$long
all_plots[[3]][[3]]

resp_expl = tidyr::expand_grid(response, expl)
resp_expl

allplots2 = pmap(resp_expl, ~scatter_fun(x = .y, y = .x) )
allplots2_names = pmap(resp_expl, ~paste0(.x, "_", .y, ".png"))
allplots2_names[1:2]

pdf("all_scatterplots.pdf")
all_plots
dev.off()

iwalk(all_plots, ~{
  pdf(paste0(.y, "_scatterplots.pdf") )
  print(.x)
  dev.off()
})

plotnames = imap(all_plots, ~paste0(.y, "_", names(.x), ".png")) %>%
  flatten()
plotnames

walk2(plotnames, flatten(all_plots), ~ggsave(filename = .x, plot = .y, 
                                             height = 7, width = 7))

cowplot::plot_grid(plotlist = all_plots[[1]])

response_plots = map(all_plots, ~cowplot::plot_grid(plotlist = .x))
response_plots



sankey |> 
  e_charts() |> 
  e_sankey(source, target, value) |> 
  e_title("Sankey chart")


# adaptation a notre besoin -----------------------------------------------

# trouver une donnee a echellon communale
df0_2021 <- read_delim("G:/02_dataviz/automate_graphs/donnee-dep-data.gouv-2021-geographie2022-produit-le2022-07-27.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

# focntion graphique bidon
ggplot(df0_2021) + geom_bar(aes(x=Code.région, y=faits, fill=unité.de.compte ), stat="identity")

# creation de la fonction idoine
scatter_fun = function(x, y, z) {
  ggplot(df0_2021) + geom_bar(aes(x = .data[[x]], y = .data[[y]], fill= .data[[z]])) +
    theme_bw()
}

scatter_fun(x=Code.région, y=faits, z=unité.de.compte)


# TO DO fixer les variables, faire juste la liste sur le subset de --------

country_list <- c("France", "Germany", "United States of America", "Luxembourg", "Switzerland", "Greece")





# petit selection
# small_pwt <- pwt9.0 %>%
#   filter(country %in% country_list)

# ordonner
small_pwt <- small_pwt %>%
  mutate(country = factor(country, levels = country_list, ordered = TRUE))

plots <- df0_2021 %>%
  group_by(Code.région) %>%
  do(plot = ggplot(data = .) + 
       geom_bar(aes(x=Code.région, y=faits, fill=unité.de.compte ), stat="identity") +
       ggtitle(unique(.$Code.région)) +
       ylab("Year") +
       xlab("Average annual hours worked by persons engaged"))

names(df0_2021) <- c('classe', 'annee' , 'code_dep', 'code_reg', 'unite', 'millpop', 'milllog', 'faits', 'pop', 'log', 'taux_mille')

reg_list <- unique(df0_2021$code_reg)

df0_2021 <- df0_2021 %>% arrange(code_reg)

ggplot(df0_2021)  %>%
   group_by(code_reg) %>%
   do(plot = ggplot(data = .) + 
       geom_bar(aes(x=code_reg, y=faits, fill=unite ), stat="identity") +
       ggtitle(unique(.$code_reg)) +
       ylab("Year") +xlab("Average annual hours worked by persons engaged")
      )

file_names <- paste0(reg_list, ".pdf")
map2(file_names, plots$plot, ggsave)

graph_fonction <- graph_fonction %>% mutate(dep = factor(dep), levels=dep_list, ordered =TRUE)

