#TELECHARGEMENT DES LIBRAIRIES
library(readr)
library(ggplot2)
library(labelled)
library(dplyr)
install.packages("RColorBrewer")
library(RColorBrewer)

#IMPORTATION DES BASES DE DONNEES
maths <- read_csv("Maths.csv")
Portuguese <- read_csv("Portuguese.csv")

####I. Introduction####

#Ces donnÃ©es portent sur les rÃ©sultats des Ã©lÃ¨ves dans l'enseignement 
#secondaire de deux Ã©coles portugaises. Les attributs des donnÃ©es 
#comprennent les notes des Ã©lÃ¨ves, les caractÃ©ristiques dÃ©mographiques, 
#sociales et scolaires, et ont Ã©tÃ© collectÃ©s Ã  l'aide de rapports et de 
#questionnaires scolaires. Deux ensembles de donnÃ©es sont fournis concernant
#les performances dans deux matiÃ¨res distinctes : Les mathÃ©matiques (mat) et
#la langue portugaise (por). 

#A partir de l'analyse du lien qui pourrait exister entre la consommation d'alcool et les résultats
#scolaires, l'idée est de déterminer plus généralement quels pourraient être les facteurs affectant la
#réussite scolaire dans le contexte des jeux de données dont nous disposons.

N1 <- nrow(maths)
N2 <- nrow(Portuguese)
#Pour la base de donnee maths, il y a 395 observations et 33 variables.
#Pour la base de donnee portuguese, il y a 649 observations et 33 variables identiques avec la 1ere bdd. 

fulldt<-bind_rows(maths, Portuguese)
nrow(fulldt)
str(fulldt)
#Avec str, on visualiser les premieres donnees des variables, et on regarde si ce sont des variables qualitative ou quantitatives


#### II. Visualisation des donnees####

# a) Ecole
ggplot(as.data.frame(table(fulldt$school))) +
     geom_bar(aes(x = Var1, y = Freq, fill = Var1), 
                           stat = 'identity') +
     scale_fill_manual(values=palette) +
     ggtitle("Répartition de l'école") +
     xlab("Ecole") +
     ylab("Effectifs") +
     theme_bw() +
     theme(axis.text.x = element_text(face = 'bold', size = 10),
                     axis.text.y = element_text(face = 'bold', size = 10))

#Pour fulldt, il y a 772 eleves scolarises a Gabriel Pereira et 272  a Mousinho da Silveira.

#b) sexe en fonction de l'école
ggplot(fulldt) +
  aes(x = sex, fill = school) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Sexe",
    y = "Effectifs",
    title = "Répartition du sexe en fonction de l'école"
  ) +
  theme_minimal()

#Il y a 591 filles et 453 garcons au sein des 2 écoles


#c)Age en fonction du sexe
ggplot(fulldt) +
  aes(x = age, fill = sex) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Age",
    y = "Effectifs",
    title = "Répartition de l'âge en fonction du sexe"
  ) +
  theme_minimal()

#d) Adresse
#Adresse en fonction de école
ggplot(fulldt) +
  aes(x = address, fill = school) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Adresse",
    y = "Effectif",
    title = "Répartition de l'adresse (U ou R) en fonction de l'école"
  ) +
  theme_minimal()

#Visualisation seulement adresse
as.data.frame(table(fulldt$address))
ggplot(as.data.frame(table(fulldt$address))) +
  geom_bar(aes(x = Var1, y = Freq,  fill = Var1), 
           stat = 'identity') +
  scale_fill_manual(values=c( '#2B9E67', '#FFA44B')) +
  ggtitle("Basic Bar Plot") +
  xlab("Adresse") +
  ylab("Effectifs") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#Emploi pere

#e) Emploi père

as.data.frame(table(fulldt$Fjob))
ggplot(as.data.frame(table(fulldt$Fjob))) +
  geom_bar(aes(x = Var1, y = Freq, fill = Var1), 
           stat = 'identity') +
  scale_fill_manual(values=palette) +
  ggtitle("Basic Bar Plot") +
  xlab("Job du pere") +
  ylab("Effectifs") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))
#Interprétation: Nous observons que la catégorie "les autres" se démarque des 3 autres catégories (à la maison, santé et professeurs)


#f) Emploi de la mere
as.data.frame(table(fulldt$Mjob))
ggplot(as.data.frame(table(fulldt$Mjob))) +
  geom_bar(aes(x = Var1, y = Freq, fill = Var1), 
           stat = 'identity') +
  scale_fill_manual(values=palette) +
  ggtitle("Basic Bar Plot") +
  xlab("Job de la mere") +
  ylab("Effectifs") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#Ici aussi, other se démarque des autres, suivi de services







#Partie Juliette qui ne marche pas! 
## Essais Juliette
# Croisement de certaines variables :
# Alcool / sexe

install.packages("esquisses")
library(esquisse)
library(ggplot2)

# La consommation d'alcool la semaine selon le sexe

ggplot(fulldt) +
  aes(x = Dalc, fill = sex) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  labs(x = "Consommation d'alcool en semaine (1 Ã©tant le plus faible)", y = "Nombre d'Ã©lÃ¨ves",
    title = "Consommation d'alcool en semaine selon le sexe ",
    fill = "sexe"
  labs(x = "Consommation d'alcool en semaine (1 étant le plus faible)", y = "Nombre d'élèves",
       title = "Consommation d'alcool en semaine selon le sexe ",
       fill = "sexe"
  ) +
  theme_minimal()

# La consommation d'alcool le week-end selon le sexe

ggplot(fulldt) +
  aes(x = Walc, fill = sex) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  labs(x = "Consommation d'alcool pendant le week-end (1 Ã©tant le plus faible)", y = "Nombre d'Ã©lÃ¨ves", 
       title = "Consommation d'alcool le week-end chez les Ã©lÃ¨ves selon le sexe ", 

  labs(x = "Consommation d'alcool pendant le week-end (1 étant le plus faible)", y = "Nombre d'élèves", 
       title = "Consommation d'alcool le week-end chez les élèves selon le sexe ", 
       fill = "sexe") +
  theme_minimal()

# Consommation d'alcool semaine et week-end confondus :

alc <- c(fulldt$Dalc, fulldt$Walc)
view(alc)

ggplot(fulldt) +
  aes(x = alc, fill = sex) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  labs(x = "DegrÃ© de consommation d'alcool (1 Ã©tant le plus faible)", y = "Nombre d'Ã©lÃ¨ves", 
       title = "Consommation d'alcool chez les Ã©lÃ¨ves selon le sexe ", 
       fill = "sexe") +
  theme_minimal()

## -> Ã§a marche pas! 
  
  labs(x = "Degré de consommation d'alcool (1 étant le plus faible)", y = "Nombre d'élèves", 
       title = "Consommation d'alcool chez les élèves selon le sexe ", 
       fill = "sexe") +
  theme_minimal()
## -> ça marche pas!

# La consommation d'alcool la semaine selon l'environnement (rural/urbain)

ggplot(fulldt) +
 aes(x = Walc, fill = address) +
 geom_histogram(bins = 30L) +
 scale_fill_hue(direction = 1) +
 labs(x = "Consommation d'alcool pendant leweek-end ", y = "ElÃ¨ves dÃ©clarant boire le week-end", 
 title = "Consommation d'alcool le week-end chez les Ã©lÃ¨ves selon l'environnement ", subtitle = "Tous niveaux confondus", 
 fill = "Cadre de vie (R = rural, U = Urbain)") +
 theme_minimal()


  aes(x = Walc, fill = address) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  labs(x = "Consommation d'alcool pendant leweek-end ", y = "Elèves déclarant boire le week-end", 
       title = "Consommation d'alcool le week-end chez les élèves selon l'environnement ", subtitle = "Tous niveaux confondus", 
       fill = "Cadre de vie (R = rural, U = Urbain)") +
  theme_minimal()
