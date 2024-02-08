rm(list=ls())

library(dplyr)
library(sf)
library(mapsf)
library(classInt)
library(leaflet)
library(openxlsx)

###########
#Exercice 1
###########

#1
pop_2019 <- read.xlsx("Pop_legales_2019.xlsx")
fond <- st_read("commune_francemetro_2021.shp", options = "ENCODING=WINDOWS-1252")
Paris <- pop_2019 %>% filter(substr(COM,1,2)=='75') %>% mutate(n=sum(PMUN19))
Paris <- Paris[1,]
Paris$COM <- '75056'
Paris$NCC <- 'Paris'
Paris <- Paris[,c(1,2,4)]
names(Paris) <- c("COM", "NCC", "PMUN19")
pop_2019 <- pop_2019 %>% filter(substr(COM,1,2)!='75')
pop_2019 <- rbind(pop_2019, Paris)
fond_commune <- left_join(fond, pop_2019, by = c("code" = "COM"))
fond_commune$densite_pop <- fond_commune$PMUN19 / fond_commune$surf

#2
density(fond_commune$densite_pop)
summary(fond_commune$densite_pop)

#3
plot(fond_commune["densite_pop"], border = FALSE)

#4
plot(fond_commune["densite_pop"], border = FALSE, breaks = "quantile")  #Mieux pour moi, plus jolie
plot(fond_commune["densite_pop"], border = FALSE, breaks = "jenks")
plot(fond_commune["densite_pop"], border = FALSE, breaks = "sd")
plot(fond_commune["densite_pop"], border = FALSE, breaks = "pretty")

#5a
densite_pop_classe_quantile <- classIntervals(var = fond_commune$densite_pop, n = 5, style = "quantile")
densite_pop_classe_quantile
str(densite_pop_classe_quantile)

#5b
pal1 <- RColorBrewer::brewer.pal(n = 5, name = "YlOrRd")
plot(fond_commune["densite_pop"], border = FALSE, breaks = densite_pop_classe_quantile$brks, pal = pal1, main = "TITRE")

#5c
densite_pop_classe_jenks <- classIntervals(var = fond_commune$densite_pop, n = 5, style = "jenks")
densite_pop_classe_sd <- classIntervals(var = fond_commune$densite_pop, n = 5, style = "sd")
densite_pop_classe_pretty <- classIntervals(var = fond_commune$densite_pop, n = 5, style = "pretty")
#On en a 6, ça veut dire que pretty ne peut pas faire avec 5
plot(fond_commune["densite_pop"], border = FALSE, breaks = densite_pop_classe_jenks$brks, pal = pal1, main = "TITRE")
plot(fond_commune["densite_pop"], border = FALSE, breaks = densite_pop_classe_sd$brks, pal = pal1, main = "TITRE")
plot(fond_commune["densite_pop"], border = FALSE, breaks = densite_pop_classe_pretty$brks, pal = pal1, main = "TITRE")
#pas possible la dernière du coup avec notre palette de couleur

#5d
decoupage_main <- c(0,40,162,1000,8000,27200)
#50% de la même couleur car 40 = médiane
#30/35% dans la 2 couleur
#Le reste = proposition
densite_main <- cut(fond_commune$densite_pop, breaks = decoupage_main, right = FALSE, include.lowest = TRUE, ordered_result = TRUE)
fond_commune$densite_decoupe_main <- densite_main

#5e
table(fond_commune$densite_decoupe_main)
plot(fond_commune["densite_decoupe_main"], border = FALSE, pal = pal1, main = "TITRE")

###########
#Exercice 2
###########

rm(list=ls())

#1
fond_dep <- st_read("dep_francemetro_2021.shp", options = "ENCODING=WINDOWS-1252")
taux_pauvrete <- read.xlsx("Taux_pauvrete_2018.xlsx")
dep_pauvrete <- left_join(fond_dep, taux_pauvrete, by = c("code" = "Code"))
mf_map(x = dep_pauvrete, var = "Tx_pauvrete", breaks = "fisher", type = "choro")
mf_map(x = dep_pauvrete, var = "Tx_pauvrete", breaks = "equal", type = "choro")
mf_map(x = dep_pauvrete, var = "Tx_pauvrete", breaks = "quantile", type = "choro")

#2
decoupe_manuelle <- c(0,13,17,25,max(dep_pauvrete$Tx_pauvrete))
mf_map(x = dep_pauvrete, var = "Tx_pauvrete", breaks = decoupe_manuelle, type = "choro")
mtq_target <- dep_pauvrete %>% filter(code %in% c("75", "92", "93", "94"))
mf_inset_on(x = mtq_target, pos = "topleft", cex = .2)
mf_init(mtq_target)
mf_map(x = mtq_target, var = "Tx_pauvrete", breaks = decoupe_manuelle, type = "choro", leg_pos = NA, add = TRUE)
mf_inset_off()

#3
st_write(dep_pauvrete, "dep_pauvrete.shp")

######################
#Exercice 3 (non fini)
######################

rm(list=ls())

fond_reg <- st_read("reg_francemetro_2021.shp", options = "ENCODING=WINDOWS-1252")
pop_region <- read.xlsx("pop_region_2019.xlsx")
reg_population <- left_join(fond_reg, pop_region, by = c("code" = "reg"))
reg_population$densite <- reg_population$pop / reg_population$surf
mf_map(x = reg_population, var = "pop", type = "prop_choro")









