library(maptools)
library(rworldmap)
library(dplyr)
library(ggplot2)
library(geosphere)
library(gpclib)
library(mapproj)
library(animation)

# World map
worldMap <- getMap()
world.points <- fortify(worldMap)
world.points$region <- world.points$id


# La base de données
world.df <- world.points[,c("long","lat","group", "region")]

# ON crée des années aléatoires d'instauration du PAS
nregion <- length(unique(world.df$region))
df.pas <- cbind(unique(world.df$region), sample(1900:2020, nregion, replace = T))
df.pas <- data.frame(df.pas)
names(df.pas) <- c("region", "annee.pas")
df.pas$annee.pas <- as.numeric(as.character(df.pas$annee.pas))
world.df <- merge(world.df, df.pas, by = "region")

# La fonction qui produit le graphique avec un angle donné, et colorie
# les pays qui ont le prélèvement à la source 

rotateMap <- function(angle, year){
  
  world.df$PAS <- 0
  world.df$PAS[which(world.df$annee.pas < year)] <- 1
  world.df$PAS <- factor(world.df$PAS)
  p <- ggplot() + 
    geom_polygon(data = world.df, aes(x = long, y = lat, group = group, fill = PAS)) +
    scale_y_continuous(breaks = (-2:2) * 30) +
    scale_x_continuous(breaks = (-4:4) * 45) +
    coord_map("ortho", orientation=c(61, angle, 0)) +
    scale_fill_manual(breaks = c(0, 1),
                      values = c("grey80",  "red"),
                      guide = FALSE)
  p
}

# Création de l'animation
years <- seq(1900, 2020, by = 5)
nyears <- length(years)
angles <- seq(0, 360, length = nyears)

saveHTML({
  ani.options(nmax = 360)
  for(i in 1:nyears){
    print(rotateMap(angle = angles[i], year = years[i]))
  }
}, interval = 1, outdir=getwd(), movie.name = "PAStimeline2.html")


