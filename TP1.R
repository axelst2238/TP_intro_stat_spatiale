rm(list=ls())
library(sf)
library(dplyr)

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
dept_bretagne <- communes_Bretagne %>% group_by(dep) %>% summarise(n=sum(surf2))
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













