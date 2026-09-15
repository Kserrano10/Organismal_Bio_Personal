### my comments preceded by `#CPK:`
## Load library
library(tidyverse)

## Set directory
setwd("~/Documents/Assignments/Organismal Biology/Module1_Team8/Scales")#CPK: be careful, I don't have this directory path.


## Read data
dat <- read.csv("scales.csv")
#CPK: Be sure to include your data with submission

#CPK: for me
dat <- read_csv("https://bcorgbio.github.io/class/scales.csv")

## Class of each column
sapply(dat,class)

## Dimensions
dim(dat)

# Levels by species
dat$species <- as.factor(dat$species)
species <- levels(dat$species)

## Number of punctures per species
species.n <- dat %>%
  group_by(species) %>%
  summarise(n = n())
species.n

## Number of specimens per species
dat %>%
  count(species,specimen) %>%
  print() %>%
  count(species,name = "n.specimens")

## Saving figures
pdf("Kayla_Serrano_species_quadrant.pdf")
for(i in species){
  p <-  dat %>%
    filter(species == i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()
list.files(pattern=".pdf")






###CPK: Excellent work, Kayla! Just be sure to include your data with your submission.




#Total points: 25

