rm(list=ls())
library(sf)
library(dplyr)
library(ggplot2)

#1
fond <- st_read("commune_francemetro_2021.shp", 
                options = "ENCODING=WINDOWS-1252")

#2
summary(fond)

#3
View(fond[c(1:10),])

#4
st_crs(fond)  #LAMBERT 93

#5
communes_Bretagne <- fond %>% filter(reg=="53") %>% select(code, libelle, epc, dep, surf) 
#Il y a également la variable GEOMETRY

#6
str(communes_Bretagne)

#7
plot(communes_Bretagne, lwd = 1)

#8
plot(st_geometry(communes_Bretagne), lwd = 1)

#9
communes_Bretagne$surf2 <- st_area(communes_Bretagne)
str(communes_Bretagne$surf2) #m²

#10
communes_Bretagne <- communes_Bretagne %>% mutate(surf2 = st_area(geometry)) %>% mutate(surf2 = units::set_units(surf2, "km^2"))
str(communes_Bretagne$surf2) #km²

#11
# polygone simplifié par rapport au polygone utilisé pour surf.

#12
dept_bretagne <- communes_Bretagne %>% group_by(dep) %>% summarise(surf2=sum(surf2))
plot(dept_bretagne, lwd = 1)

#13
dept_bretagne_2 <- communes_Bretagne %>% group_by(dep) %>% summarise(geometry=st_union(geometry))
plot(dept_bretagne_2, lwd = 1)

#14
centroid_dept_bretagne <- dept_bretagne_2 %>% st_centroid()

#14a
str(centroid_dept_bretagne)

#14b
plot(st_geometry(dept_bretagne_2), lwd = 1, color = "black")
plot(st_geometry(centroid_dept_bretagne), lwd = 1, add = TRUE)

#14c
centroid_dept_bretagne$dep_name <- c("Côte d'Armor", "Finistère", "Ille-et-Vilaine",
                                     "Morbihan")

#14d
centroid_coords <- st_coordinates(centroid_dept_bretagne)
centroid_coords
centroid_coords <- bind_cols(centroid_coords, centroid_dept_bretagne$dep)
centroid_coords <- bind_cols(centroid_coords, centroid_dept_bretagne$dep_name)

#14e
plot(st_geometry(dept_bretagne_2), lwd = 1, color = "black")
plot(st_geometry(centroid_dept_bretagne), lwd = 1, add = TRUE)
text(x = st_coordinates(centroid_dept_bretagne)[, 1], y = st_coordinates(centroid_dept_bretagne)[, 2], 
     labels = centroid_dept_bretagne$dep_name, pos = 1, col = "red", cex = 0.7, font = 2)

#15
st_intersects(centroid_dept_bretagne, communes_Bretagne)
communes_Bretagne[c(148,476,647,1092),]

#16
st_intersection(centroid_dept_bretagne, communes_Bretagne)
intersection <- st_intersection(centroid_dept_bretagne, communes_Bretagne)
# Avant que les numéros de ligne, ici les 4 lignes concernées
st_within(centroid_dept_bretagne, communes_Bretagne)
# Comme en 15

#17
Saint_Brieuc <- communes_Bretagne %>% filter(libelle == "Saint-Brieuc")
Quimper <- communes_Bretagne %>% filter(libelle == "Quimper")
Rennes <- communes_Bretagne %>% filter(libelle == "Rennes")
Vannes <- communes_Bretagne %>% filter(libelle == "Vannes")
st_distance(centroid_dept_bretagne[1,], Saint_Brieuc)
st_distance(centroid_dept_bretagne[2,], Quimper)
st_distance(centroid_dept_bretagne[3,], Rennes)
st_distance(centroid_dept_bretagne[4,], Vannes)

#18a
st_buffer(centroid_dept_bretagne, 20000)
zone_20km <- st_buffer(centroid_dept_bretagne, 20000)

#18b
plot(st_geometry(dept_bretagne_2), lwd = 1, color = "black")
plot(st_geometry(zone_20km), add = TRUE)

#18c
commune_dans_20 <- st_intersection(zone_20km, communes_Bretagne)

#18d
commune_dans_20 %>% group_by(dep) %>% count()

#19a
communes_Bretagne_WGS84 <- st_transform(communes_Bretagne, crs = 4326)
st_crs(communes_Bretagne_WGS84)

#19b
plot(st_geometry(communes_Bretagne))
plot(st_geometry(communes_Bretagne_WGS84))

#20
communes_Bretagne_WGS84$surf3 <- st_area(communes_Bretagne_WGS84)
communes_Bretagne_WGS84 <- communes_Bretagne_WGS84 %>% mutate(surf3 = st_area(geometry)) %>% mutate(surf3 = units::set_units(surf3, "km^2"))
communes_Bretagne_WGS84$surf2 <- communes_Bretagne$surf2
# Pas les mêmes

#Bonus
Occitanie <- fond %>% filter(reg == "76")
Occitanie_dep <- Occitanie %>% group_by(dep) %>% summarise(geometry=st_union(geometry))
plot(st_geometry(Occitanie_dep))
Centroide <- Occitanie_dep %>% st_centroid()
plot(st_geometry(Centroide), add = TRUE)
Gigouzac <- Occitanie %>% filter(libelle == "Gigouzac")
Labastide <- Occitanie %>% filter(libelle == "Labastide-d'Anjou", add = TRUE)
plot(st_geometry(Gigouzac), col = "red", add = TRUE)
plot(st_geometry(Labastide), col = "red", add = TRUE)
st_distance(Gigouzac, Labastide)


