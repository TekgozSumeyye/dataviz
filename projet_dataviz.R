#TELECHARGEMENT DES LIBRAIRIES
library(readr)
library(coefplot)
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

# b) sexe en fonction de l'école
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


# c)Age en fonction du sexe
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

# d) Adresse
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

#e) Emploi pere

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

#Peut etre rajouter d'autres graphiques pour la visualisation? demander au socio

#### III. Visualision des données afin d'établir une éventuelle corrélation entre la consommation d'alcool et les résultats scolaires

  #a)profil general des consommateurs d'alcool, afin d'établir une première typologie général :

  #Visualisation des consommations d'alcool weekend et semaine

table(fulldt$Walc)

ggplot(fulldt) +
  aes(x = Walc) +
  geom_histogram(bins = 30L, fill = "#112446") +
  theme_minimal()

table(fulldt$Dalc)

ggplot(fulldt) +
  aes(x = Dalc) +
  geom_histogram(bins = 30L, fill = "#112446") +
  theme_minimal()
#Bcp plus de consommation le weekend ->faire analyse approfondie
#Consommation modéré en semaine alors que wk il a plus de répartition pour la consommation

#Impact alcool sur G3
cor1 <- G3 ~ Walc + Dalc
lm1<-lm(cor1 , data = fulldt)
coefplot(lm1 , outerCI = 1.96 , intercept = FALSE)

#La consommation d'alcool en semaine a plus d'impact négative sur les résultats G3 que la consommation le weekend
#Dalc= -0.39 et walc -0.17

cor2 <- age ~ Walc + Dalc
lm2<-lm(cor2 , data = fulldt)
coefplot(lm2 , outerCI = 1.96 , intercept = FALSE)
#La consommation d'alcool en semaine dépend plus de l'age que la consommation le wk

ggplot(fulldt, aes(x = Walc, y = Dalc , color = sex, size = freetime))+   geom_jitter(position=position_jitter(0.2))
#Analuse Aurore
#Les femmes ont une consommation plutot modérer que les hommes, on retrouve plus les H dans cat 5

ggplot(fulldt, aes(x = Walc, y = Dalc , color = address, size = freetime))+ geom_jitter(position=position_jitter(0.2))
#Analyse Doriane
#j'ai moi même du mal à interpréter ca
ggplot(fulldt, aes(x = Walc, y = Dalc , shape = sex, color = sex, size = freetime))+ geom_boxplot()

#A corriger 
ggplot(fulldt) +
  aes(x = Dalc, y = Walc, colour = age, size = age) +
  geom_point(shape = "circle") +
  scale_color_distiller(palette = "Reds", direction = 1) +
  labs(
    x = "Consommation alcool semaine",
    y = "Consommation alcool weekend",
    title = "Nuage de point consommation alcool en fonction du sexe"
  ) +
  theme_bw()



#Consommation d'alcool selon job mere
ggplot(fulldt) +
  aes(x = Dalc, y = Walc, colour = Mjob, size = Mjob) +
  geom_jitter() +
  scale_color_viridis_d(option = "plasma", direction = 1) +
  labs(
    x = "Consommation alcool semaine",
    y = "Consommation alcool weekend",
    title = "Nuage de point consommation alcool en fonction du job de la mere"
  ) +
  theme_bw()
#Analyse Hugo

#Notes G3
ggplot(fulldt) +
  aes(x = Dalc, y = Walc, colour = school, size = G3) +
  geom_jitter() +
  scale_color_hue(direction = 1) +
  labs(
    x = "Consommation alcool semaine",
    y = "Consommation alcool weekend",
    title = "Nuage de point consommation alcool en fonction G3 et école"
  ) +
  theme_bw()
#Analyse Juliette

table(fulldt$G3)

#Sumeyye
ggplot(fulldt) +
  aes(x = G3, y = sex) +
  geom_boxplot(fill = "#BDD3E8") +
  labs(x = "Notes G3", y = "Sexe", title = "Boxplot") +
  theme_bw()

ggplot(fulldt) +
  aes(x = G3, y = Mjob, fill = Mjob) +
  geom_boxplot() +
  scale_fill_brewer(palette = "OrRd", direction = 1) +
  labs(x = "Notes G3", y = "Job mere", title = "Boxplot") +
  theme_bw()

#Aurore
ggplot(fulldt) +
  aes(x = G3, y = Fjob, fill = Fjob) +
  geom_boxplot() +
  scale_fill_brewer(palette = "OrRd", direction = 1) +
  labs(x = "Notes G3", y = "Job mere", title = "Boxplot") +
  theme_bw()

#Faire boxplot pour etudes pere et mere 
#b)Recherche plus pouss�e des raisons d'une consommation d'alcool �excessive� :








#Partie Juliette

#Consommation d'alcool selon la taille de la famille

ggplot(fulldt) +
  aes(x = Dalc, y = Walc, colour = famsize, size = famsize) +
  geom_jitter() +
  scale_color_viridis_d(option = "plasma", direction = 1) +
  labs(
    x = "Consommation alcool semaine",
    y = "Consommation alcool weekend",
    title = "Nuage de point consommation alcool en fonction du job de la mere"
  ) +
  theme_bw()


## Encore essais Juliette
# Croisement de certaines variables :
# Alcool / sexe

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
       

# Soutien scolaire et consommation d'alcool en semaine

ggplot(fulldt) +
  aes(x = Dalc, fill = schoolsup) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Quantité d'alcool consommée en semaine (1 étant le plus faible)",
    y = "nombre d'élèves",
    title = "Soutien scoalire et consommation d'alcool le week-end",
    fill = "Bénéficie de soutien scolaire"
  ) +
  theme_minimal()

##=> Les élèves qui déclarent consommer peu d'alcool en semaine, sont plus nombreux à bénéficier de soutien scolaire
