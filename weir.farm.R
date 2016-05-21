### Weir Farm Bioblitz Bacteria
install.packages("ggplot2")
install.packages("vegan")
install.packages("ecodist")
install.packages("gridExtra")
install.packages("RColorBrewer")
library(ggplot2)
library(vegan)
library(ecodist)
library(gridExtra)
library(RColorBrewer)

# Read in data

# Alpha diversity
alpha <- read.csv(file="weir.farm.alpha.csv", header=T)

# Beta diversity
tyc <- read.csv(file="weir.farm.tyc3.csv", header=T, row.names = 1)
jc <- read.csv(file="weir.farm.j3.csv", header=T)

# Environmental data
env <- read.csv(file="weir.farm.env.csv", header=T)

# Join data


###
# alpha diversity (diversity within a sample)
species.site <- ggplot(data=alpha, (aes(x=env$Site, y=alpha$sobs)))+
    geom_boxplot()+
    theme_bw()+
    ggtitle("Bacterial Species by Site")
species.site
ggsave(file="species.site.jpg")

species.area <- ggplot(data=alpha, (aes(x=env$Area, y=alpha$sobs)))+
    geom_boxplot()+
    theme_bw()+
    ggtitle("Bacterial Species by Sampling area")
species.area
ggsave(file="species.area.jpg")

species.type <- ggplot(data=alpha, (aes(x=env$Type, y=alpha$sobs)))+
    geom_boxplot()+
    theme_bw()+
    ggtitle("Bacterial Species by Sample type")
species.type
ggsave(file="species.type.jpg")

# grid.arrange(species.site, species.area, species.type, ncol=2, main="Bacterial Species Richness")

invsimp.site <- ggplot(data=alpha, (aes(x=env$Site, y=alpha$invsimpson)))+
    geom_boxplot()+
    theme_bw()+
    ggtitle("Bacterial Diversity by Site")
invsimp.site
ggsave(file="invsimp.site.jpg")

invsimp.area <- ggplot(data=alpha, (aes(x=env$Area, y=alpha$invsimpson)))+
    geom_boxplot()+
    theme_bw()+
    ggtitle("Bacterial Diversity by Sampling area")
invsimp.area
ggsave(file="invsimp.area.jpg")


invsimp.type <- ggplot(data=alpha, (aes(x=env$Type, y=alpha$invsimpson)))+
    geom_boxplot()+
    theme_bw()+
    ggtitle("Bacterial Diversity by Sample type")
invsimp.type
ggsave(file="invsimp.type.jpg")



###
# Beta diversity (how samples relate to each other)

tyc.nms <- metaMDS(as.dist(tyc), k=2, trymin=50, trymax=250, wascores=F)
ordiplot(tyc.nms)
tyc.points <- data.frame(tyc.nms$points)
ggplot(tyc.points, aes(x=MDS1, y=MDS2))+
    geom_point(aes(color=factor(env$Area), shape=factor(env$Site), size=3))+
    theme_bw()+
    scale_color_brewer(palette="Spectral")
ggsave(file="Bacterial_Community.jpg")
