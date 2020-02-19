# Import package ggplot2
library(ggplot2)

# Import de données

proa1415 <- read.csv("https://raw.githubusercontent.com/xavierbarbier/basketball/master/proa1415.csv")
proa1516 <- read.csv("https://raw.githubusercontent.com/xavierbarbier/basketball/master/proa1516.csv")
proa1617 <- read.csv("https://raw.githubusercontent.com/xavierbarbier/basketball/master/proa1617.csv")
proa1718 <- read.csv("https://raw.githubusercontent.com/xavierbarbier/basketball/master/proa1718.csv")
proa1819 <- read.csv("https://raw.githubusercontent.com/xavierbarbier/basketball/master/proa1819.csv")


head(proa1415)
names(proa1415)


# VOP saison 1415
proa1415$POS<-proa1415$FGA-proa1415$ORB+proa1415$TOV+(.44*proa1415$FTA)
proa1415$VOP<-proa1415$PTS/proa1415$POS

# VOP saison 1516
proa1516$POS<-proa1516$FGA-proa1516$ORB+proa1516$TOV+(.44*proa1516$FTA)
proa1516$VOP<-proa1516$PTS/proa1516$POS

# VOP saison 1617
proa1617$POS<-proa1617$FGA-proa1617$ORB+proa1617$TOV+(.44*proa1617$FTA)
proa1617$VOP<-proa1617$PTS/proa1617$POS

# VOP saison 1718
proa1718$POS<-proa1718$FGA-proa1718$ORB+proa1718$TOV+(.44*proa1718$FTA)
proa1718$VOP<-proa1718$PTS/proa1718$POS

# VOP saison 1819
proa1819$POS<-proa1819$FGA-proa1819$ORB+proa1819$TOV+(.44*proa1819$FTA)
proa1819$VOP<-proa1819$PTS/proa1819$POS

#Creation data frame VOP entre 2014 et 2019
VOP1419<-data.frame(proa1415$VOP,proa1516$VOP,proa1617$VOP,proa1718$VOP,proa1819$VOP)

# Rangement des données  
Saison1415 = c(VOP1419$proa1415.VOP)
Saison1516 = c(VOP1419$proa1516.VOP)
Saison1617 = c(VOP1419$proa1617.VOP)
Saison1718 = c(VOP1419$proa1718.VOP)
Saison1819 = c(VOP1419$proa1819.VOP)
VOP1419 = data.frame(cbind(Saison1415,Saison1516,Saison1617,Saison1718,Saison1819))
VOP1419<-stack(VOP1419)
names(VOP1419)[names(VOP1419) == "ind"] <- "Saison"
names(VOP1419)[names(VOP1419) == "values"] <- "VOP"

# graphique VOP par saison
boxplot(VOP1419$VOP~VOP1419$Saison,main="VOP par saison", xlab = "Saison", ylab= "VOP")
abline(h=median(proa1415$VOP),col = "red")

# ANOVA VOP par saison
lmVOP1419<-aov(VOP1419$VOP~VOP1419$Saison)
summary(lmVOP1419)
TukeyHSD(lmVOP1419)


# regroupement data frame dans une seule
VOP_RK <- merge(merge(merge(merge(
  proa1415,
  proa1516, all = TRUE),
  proa1617, all = TRUE),
  proa1718, all = TRUE),
  proa1819, all = TRUE)
head(VOP_RK)

# graphique VOP - classement
plot(x=VOP_RK$VOP, y=VOP_RK$Rk,main="Correlation VOP et Classement final PROA saisons 14-19",xlab = "VOP", ylab= "Classement")
abline(lm(VOP_RK$Rk~VOP_RK$VOP),col = "red")

# correlation VOP et classement
summary(lm(VOP_RK$Rk~VOP_RK$VOP))


# graphique comparaison regression linéaire et non lineaire
ggplot(data=VOP_RK, aes(VOP,Rk))+geom_point()+
  geom_smooth(method = lm,formula = y ~ splines::bs(x, 3), se = FALSE)+
  geom_smooth(method = lm, color = "red",se = FALSE)+
  theme_classic()+
  ggtitle("Correlation VOP et Classement final PROA saisons 14-19") +
  xlab("VOP")+ylab("Classement")




