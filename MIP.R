install.packages("dplyr")
install.packages("ggplot2")
install.packages("reshape2")

library(dplyr) #Load readxl package
library(ggplot2) #Load readxl package
library(reshape2) #Load readxl package
library(readr)

# chargement données 2019-2020
brk20 <- read_csv("brk20.csv")
View(brk20)
brk19 <- read_csv("brk19.csv")
View(brk19)
no20 <- read_csv("no20.csv")
View(no20)
lal19 <- read_csv("lal19.csv")
View(lal19)

## Extraction nom
first = function(str){
    str = str[[1]]
    parts = strsplit(str,'\\\\')[[1]]
    first_part = parts[1]
    if(length(parts) >= 2)
      print(sprintf(' - Il y a plusieurs parties dans "%s", ne gardons que %s.',paste(parts,collapse=""),first_part))  
    return(first_part)
  }



# Correctif nom joueur
brk20['X2'] = apply(brk20['X2'], 1, first) 
brk19['X2'] = apply(brk19['X2'], 1, first) 
no20['X2'] = apply(no20['X2'], 1, first) 
lal19['X2'] = apply(lal19['X2'], 1, first) 

# Fonction calcul stat par minutes
per_min = function(x,y){
  return(x/y)
}


# liste stats à corriger
list<-c("3P","3PA","2P","2PA","FT","FTA","ORB","DRB","TRB","AST","STL","BLK","TOV","PF","PTS")

# Application fonction à liste variables
brk20[list]<-per_min(brk20[list],brk20$MP)
brk19[list]<-per_min(brk19[list],brk19$MP)
no20[list]<-per_min(no20[list],no20$MP)
lal19[list]<-per_min(lal19[list],lal19$MP)

# Normalisation
brk20[c(3:28)] <- scale(brk20[c(3:28)])
brk19[c(3:28)] <- scale(brk19[c(3:28)])
no20[c(3:28)] <- scale(no20[c(3:28)])
lal19[c(3:28)] <- scale(lal19[c(3:28)])

# Conservation Dinwiddie
Dinwiddie20<-brk20[brk20$X2=="Spencer Dinwiddie",]
Dinwiddie20<-Dinwiddie20[2:28]

Dinwiddie19<-brk19[brk19$X2=="Spencer Dinwiddie",]
Dinwiddie19<-Dinwiddie19[2:28]

# Conservation Ingram
Ingram20<-no20[no20$X2=="Brandon Ingram",]
Ingram20<-Ingram20[2:28]

Ingram19<-lal19[lal19$X2=="Brandon Ingram",]
Ingram19<-Ingram19[2:28]


# Retrait age 
Dinwiddie20 <- Dinwiddie20[-2]
Dinwiddie19 <- Dinwiddie19[-2]
Ingram20 <- Ingram20[-2]
Ingram19 <- Ingram19[-2]

# Changement format données
Dinwiddie20<-melt(Dinwiddie20,  id="X2")
Dinwiddie19<-melt(Dinwiddie19,  id="X2")
Ingram20<-melt(Ingram20,  id="X2")
Ingram19<-melt(Ingram19,  id="X2")

#Merge
Dinwiddie<-merge(Dinwiddie19,Dinwiddie20,by=c("X2","variable"))
Dinwiddie$diff<-Dinwiddie$value.y-Dinwiddie$value.x
Ingram<-merge(Ingram19,Ingram20,by=c("X2","variable"))
Ingram$diff<-Ingram$value.y-Ingram$value.x

#Jointure
Dinwiddie_Ingram<-rbind(Dinwiddie,Ingram)

#Graphique comparaison des 2 joueurs
ggplot(Dinwiddie_Ingram,aes(x = Dinwiddie_Ingram$variable, y = Dinwiddie_Ingram$diff))+
  geom_bar(aes(fill = Dinwiddie_Ingram$diff < 0), stat = "identity")+coord_flip()+
  scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c("green", "gray"))+
  theme_classic()+ xlab("Stat en relation avec l'équipe")+ylab("Différence 2019-2020")+
  facet_wrap(~X2)

# Somme diff

sum(Dinwiddie_Ingram$diff[Dinwiddie_Ingram$X2=="Spencer Dinwiddie"]) #12.39
sum(Dinwiddie_Ingram$diff[Dinwiddie_Ingram$X2=="Brandon Ingram"]) #10.61

mean(Dinwiddie_Ingram$diff[Dinwiddie_Ingram$X2=="Spencer Dinwiddie"]) #0.4956364
mean(Dinwiddie_Ingram$diff[Dinwiddie_Ingram$X2=="Brandon Ingram"]) #0.4244306
