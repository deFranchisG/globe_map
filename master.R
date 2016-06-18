
# Header ------------------------------------------------------------------

# Ce programme colorie les pays sur le globe terrestre tout en le faisant 
# tourner. 

# L'idée initiale était de colorier les pays si ils ont une certaine 
# propriété à partir d'une année donnée (par exemple, un pays qui
# s'est démocratisé en 1972 sera colorié pour les années après
# 1972). On fait tourner le globe en fonction des années parce que c'est 
# joli

# Sources utilisées: les travaux d'Arthur Charpentier et Ewen Gallic: 
# http://egallic.fr/maps-with-r/
# http://freakonometrics.hypotheses.org/13186

# Libraries

library(rworldmap)
library(ggplot2)
library(animation)
library(sp)


# Code --------------------------------------------------------------------
# Création de la base de données pour les graphiques qui seront utilisés ====

# Carte du monde
worldMap <- getMap()
world.points <- fortify(worldMap)
world.points$region <- world.points$id


# La base de données
world.df <- world.points[,c("long","lat","group", "region")]
world.df$ordre <- c(1:nrow(world.df))

# On rajoute l'année à partir de laquelle chaque pays doit être colorié
# ici c'est la date de l'entrée en vigueur du prélèvement à la source (PAS)
nregion <- length(unique(world.df$region))
annee.pas <- c(1917,1925,1941,1942,1943,1944,1960,1962,1967,1970,1973,1979,1989,2018)
pays <- c("Canada", "Allemagne", "Pays-bas", "Australie", "Etats-Unis","Royaume-uni","Irlande","Belgique","Luxembourg","Danemark", "Italie", "Espagne","Nouvelle-Zelande", "France")
region <- c("Canada", "Germany", "Netherlands", "Australia", "United States of America","United Kingdom","Ireland","Belgium","Luxembourg","Denmark", "Italy", "Spain","New Zealand", "France")
df.pas <- data.frame(annee.pas,pays,region)
df.pas$annee.pas <- as.numeric(as.character(df.pas$annee.pas))

# On intègre ça dans la base de données
world.df <- merge(world.df, df.pas, by = "region",all.x = TRUE)
world.df <- world.df[order(world.df$ordre),]
# les pays qui n'ont pas encore le prélèvementà la source ont une date d'entrée du PAS qui vaut 9999
world.df$annee.pas[is.na(world.df$annee.pas)==TRUE] <- 9999

# Colorier ou non (0 ou 1)
world.df$PAS <- 0
world.df$PAS[which(world.df$annee.pas < year)] <- 1
world.df$PAS <- factor(world.df$PAS)

# ajout des centroides de la région principale de chaque pays à la base de données ayant adopté le PAS
world_data <- cbind(worldMap@data,getSpPPolygonsLabptSlots(worldMap))
world_data_pas <- merge(world_data,df.pas,by.x = c("ADMIN"),by.y=c("region"))
world_data_pas <- world_data_pas[order(world_data_pas$annee.pas),]


# Fonction produisant le graphique ====
# La fonction qui produit le graphique centré sur une coordonnée passée en argument
# les pays qui ont le prélèvement à la source 
rotateMap <- function(coord){
  # Le graphique. C'est un ggplot classique mais la fonction "coord_map" fait
  # la projection
  p <- ggplot() 
  p <- p + theme_bw()
  p <- p + geom_polygon(data = world.df, aes(x = long, y = lat, group = group, fill = PAS))
  p <- p + scale_y_continuous(breaks = (-2:2) * 20) 
  p <- p + scale_x_continuous(breaks = (-4:4) * 15) 
  p <- p + coord_map("ortho", orientation=c(coord[1], coord[2], 0)) 
  p <- p + scale_fill_manual(breaks = c(0, 1),values = c("grey80",  "red"),guide = FALSE)
  p <- p + theme(panel.grid.major = element_line(colour = "black"),panel.grid.minor = element_line(colour = "black"))
  p
}


# dessin d'une carte par pays ayant adopté le PAS, par ordre d'adoption du PAS

coord <- world_data[3,c(50,51)]
rotateMap(coord)
# Création de l'animation avec le package animation ====

# # Choix des années et des rotations
# years <- seq(1900, 2020, by = 20)
# nyears <- length(years)
# angles <- seq(0, 360, length = nyears)
# 
# # Création du fichier HTML en faisant une boucle produisant les graphiques
# saveHTML({
#   ani.options(nmax = 360)
#   for(i in 1:nyears){
#     print(rotateMap(angle = angles[i], year = years[i]))
#   }
# }, interval = 1, outdir=getwd(), movie.name = "PAStimeline2.html")
# 
