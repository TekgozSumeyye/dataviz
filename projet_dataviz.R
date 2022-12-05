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

#Ces donnÃƒÂ©es portent sur les rÃƒÂ©sultats des ÃƒÂ©lÃƒÂ¨ves dans l'enseignement 
#secondaire de deux ÃƒÂ©coles portugaises. Les attributs des donnÃƒÂ©es 
#comprennent les notes des ÃƒÂ©lÃƒÂ¨ves, les caractÃƒÂ©ristiques dÃƒÂ©mographiques, 
#sociales et scolaires, et ont ÃƒÂ©tÃƒÂ© collectÃƒÂ©s ÃƒÂ  l'aide de rapports et de 
#questionnaires scolaires. Deux ensembles de donnÃƒÂ©es sont fournis concernant
#les performances dans deux matiÃƒÂ¨res distinctes : Les mathÃƒÂ©matiques (mat) et
#la langue portugaise (por). 

#A partir de l'analyse du lien qui pourrait exister entre la consommation d'alcool et les rÃ©sultats
#scolaires, l'idÃ©e est de dÃ©terminer plus gÃ©nÃ©ralement quels pourraient Ãªtre les facteurs affectant la
#rÃ©ussite scolaire dans le contexte des jeux de donnÃ©es dont nous disposons.

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
     ggtitle("RÃ©partition de l'Ã©cole") +
     xlab("Ecole") +
     ylab("Effectifs") +
     theme_bw() +
     theme(axis.text.x = element_text(face = 'bold', size = 10),
                     axis.text.y = element_text(face = 'bold', size = 10))

#Pour fulldt, il y a 772 eleves scolarises a Gabriel Pereira et 272  a Mousinho da Silveira.

# b) sexe en fonction de l'Ã©cole
ggplot(fulldt) +
  aes(x = sex, fill = school) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Sexe",
    y = "Effectifs",
    title = "RÃ©partition du sexe en fonction de l'Ã©cole"
  ) +
  theme_minimal()

#Il y a 591 filles et 453 garcons au sein des 2 Ã©coles


# c)Age en fonction du sexe
ggplot(fulldt) +
  aes(x = age, fill = sex) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Age",
    y = "Effectifs",
    title = "RÃ©partition de l'Ã¢ge en fonction du sexe"
  ) +
  theme_minimal()

# d) Adresse
#Adresse en fonction de Ã©cole
ggplot(fulldt) +
  aes(x = address, fill = school) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Adresse",
    y = "Effectif",
    title = "RÃ©partition de l'adresse (U ou R) en fonction de l'Ã©cole"
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
#InterprÃ©tation: Nous observons que la catÃ©gorie "les autres" se dÃ©marque des 3 autres catÃ©gories (Ã  la maison, santÃ© et professeurs)


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

#Ici aussi, other se dÃ©marque des autres, suivi de services

#Peut etre rajouter d'autres graphiques pour la visualisation? demander au socio

#### III. Visualision des donnÃ©es afin d'Ã©tablir une Ã©ventuelle corrÃ©lation entre la consommation d'alcool et les rÃ©sultats scolaires

  #a)profil general des consommateurs d'alcool, afin d'Ã©tablir une premiÃ¨re typologie gÃ©nÃ©ral :

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
#Consommation modÃ©rÃ© en semaine alors que wk il a plus de rÃ©partition pour la consommation

#Impact alcool sur G3
cor1 <- G3 ~ Walc + Dalc
lm1<-lm(cor1 , data = fulldt)
coefplot(lm1 , outerCI = 1.96 , intercept = FALSE)

#La consommation d'alcool en semaine a plus d'impact nÃ©gative sur les rÃ©sultats G3 que la consommation le weekend
#Dalc= -0.39 et walc -0.17

cor2 <- age ~ Walc + Dalc
lm2<-lm(cor2 , data = fulldt)
coefplot(lm2 , outerCI = 1.96 , intercept = FALSE)
#La consommation d'alcool en semaine dÃ©pend plus de l'age que la consommation le wk

ggplot(fulldt, aes(x = Walc, y = Dalc , color = sex, size = freetime))+   geom_jitter(position=position_jitter(0.2))
#Analuse Aurore
#Les femmes ont une consommation plutot modÃ©rer que les hommes, on retrouve plus les H dans cat 5

ggplot(fulldt, aes(x = Walc, y = Dalc , color = address, size = freetime))+ geom_jitter(position=position_jitter(0.2))
#Analyse Doriane
#j'ai moi mÃªme du mal Ã  interprÃ©ter ca
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
    title = "Nuage de point consommation alcool en fonction G3 et Ã©cole"
  ) +
  theme_bw()

# Ici on utilise le nuage de point pour montrer la différence entre école et l'influence de la consommation d'alcool sur les notes aux examens finaux (G3) 
# car la fonction nuage de point nous permet ici de facilement montrer (ou non) la disparité entre les deux écoles et surtout de faire ressortir les tendances statistiques 
# sur la consommation d'alcool des élèves et peut être d'établir une influence sur les résultats finaux aux examens des élèves.
# Le graphique se lit comme suit : les points en haut à droite sont ceux qui consomment le plus d'alcool à la fois en weekend et en semaine (ceux se rapprochant le plus du 5 sur l'abscisse et l'ordonnée).
# A l'inverse, ceux étant le plus en bas à gauche (les plus proches de 1) sont ceux consommant le moins, voir pas d'alcool. En haut à gauche, se situent les personnes consommant exclusivement le weekend (1 à l'abcisse et 5 à l'ordonnée),
# et ceux étant le plus en bas à droite sont ceux qui consomment de l'alcool exclusivement en semaine (5 à l'abscisse et 1 à l'ordonnée).
# Le premier résultat que l'on remarque est que la tendance à la consommation d'alcool est beaucoup plus importante durant le weekend que durant la semaine malgrès quelques exceptions.
# On peut expliquer cette tendance par le fait que la consommation d'alcool à l'adolescence et pour les jeunes adultes soient surtout liée à des moments sociabilités entre groupes de pair.
# Les étudiants ayant cours en semaine, la plupart des activités sociales sont donc organisées en fin de semaine, en weekend et ces activités sont le moment propices à la consommation d'alcool en vue de sociabiliser (sorties en bars, boites, soirée chez quelqu'un).
# Le deuxième résultat que l'on peut noter est que les élèves de Gabirel Pereira (en rouge) sont plus nombreux à consommer de l'alcool que ceux de Mousinho da Silveira (en bleu).
# Les élèves de GP sont aussi surtout beaucou plus représenter dans des consommations intensives et notamment celles faites en semaines.
# Difficile de dire si la différence est assez grande pour être significative. Un début d'analyse pour expliquer cette différence serait que Gabriel Pereira
# est une école publique et donc que les élèves aient une plus grande liberté et moins d'attente au niveau du corps enseignant.
# Le troisième résultat est que l'on remarque que les résultats les plus bas (0, 5, 10) ont tendance à augmenter avec la consommation d'alcool
# et surtout avec la consommation d'alcool en semaine. On ne peut pas affirmer que ce résultat soit significatif au vu de la présence de très nombreux non-buveurs ou buveurs occasionnelle.
# Cependant on peut imaginer une hypothèse qui est que ceux qui consomment en semaine sont probablement victime d'addiction, d'alcoolisme
# étant donné que l'addiction à l'alcool se caractèrise par une consommation presque journalière. On manque d'élément pour approuver la validation d'une tel hypothèse,
# mais tout comment l'addiction à la marijuana a des conséquences sur les notes, l'addiction à l'alcool pourrait entrainer des conséquences sur les notes finales comme le montre le graphique.
# Cependant, tout comme l'addiction à la marijuana, il faut voir si c'est l'addiction à l'alcool en elle même qui cause la baisse des notes ou un environnement social particulièrement précaire
# qui causerait cette baisse des notes et cette addiction à la fois. C'est pour celà que nous analyseront la consommation en fonction de l'origine sociale, de la structure familiale et de l'accompagnement des élèves.


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

ggplot(fulldt) +
  aes(x = G3, y = guardian, fill = guardian) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Notes G3",
    y = "Tuteur",
    title = "Boxplot impact tuteur eleve sur G3"
  ) +
  theme_minimal()
#Analyser Sumeyye

#b)Recherche plus poussée des raisons d'une consommation d'alcool «excessive» :








#Partie Juliette

library(ggplot2)

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

# La consommation d'alcool la semaine selon le sexe

ggplot(fulldt) +
  aes(x = Dalc, fill = sex) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  labs(x = "Consommation d'alcool en semaine (1 ÃƒÂ©tant le plus faible)", y = "Nombre d'ÃƒÂ©lÃƒÂ¨ves",
    title = "Consommation d'alcool en semaine selon le sexe ",
    fill = "sexe"
  labs(x = "Consommation d'alcool en semaine (1 Ã©tant le plus faible)", y = "Nombre d'Ã©lÃ¨ves",
       title = "Consommation d'alcool en semaine selon le sexe ",
       fill = "sexe"
  ) +
  theme_minimal()

# La consommation d'alcool le week-end selon le sexe

ggplot(fulldt) +
  aes(x = Walc, fill = sex) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  labs(x = "Consommation d'alcool pendant le week-end (1 ÃƒÂ©tant le plus faible)", y = "Nombre d'ÃƒÂ©lÃƒÂ¨ves", 
       title = "Consommation d'alcool le week-end chez les ÃƒÂ©lÃƒÂ¨ves selon le sexe ", 

  labs(x = "Consommation d'alcool pendant le week-end (1 Ã©tant le plus faible)", y = "Nombre d'Ã©lÃ¨ves", 
       title = "Consommation d'alcool le week-end chez les Ã©lÃ¨ves selon le sexe ", 
       fill = "sexe") +
  theme_minimal()

# Consommation d'alcool semaine et week-end confondus :

alc <- c(fulldt$Dalc, fulldt$Walc)
view(alc)

ggplot(fulldt) +
  aes(x = alc, fill = sex) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  labs(x = "DegrÃƒÂ© de consommation d'alcool (1 ÃƒÂ©tant le plus faible)", y = "Nombre d'ÃƒÂ©lÃƒÂ¨ves", 
       title = "Consommation d'alcool chez les ÃƒÂ©lÃƒÂ¨ves selon le sexe ", 
       fill = "sexe") +
  theme_minimal()

## -> ÃƒÂ§a marche pas! 
  
  labs(x = "DegrÃ© de consommation d'alcool (1 Ã©tant le plus faible)", y = "Nombre d'Ã©lÃ¨ves", 
       title = "Consommation d'alcool chez les Ã©lÃ¨ves selon le sexe ", 
       fill = "sexe") +
  theme_minimal()
## -> Ã§a marche pas!

# La consommation d'alcool la semaine selon l'environnement (rural/urbain)

ggplot(fulldt) +
 aes(x = Walc, fill = address) +
 geom_histogram(bins = 30L) +
 scale_fill_hue(direction = 1) +
<<<<<<< Updated upstream
 labs(x = "Consommation d'alcool pendant leweek-end ", y = "ElÃƒÂ¨ves dÃƒÂ©clarant boire le week-end", 
 title = "Consommation d'alcool le week-end chez les ÃƒÂ©lÃƒÂ¨ves selon l'environnement ", subtitle = "Tous niveaux confondus", 
 fill = "Cadre de vie (R = rural, U = Urbain)") +
 theme_minimal()
=======
 labs(x = "Consommation d'alcool pendant leweek-end ", y = "ElÃ¨ves dÃ©clarant boire le week-end", 
 title = "Consommation d'alcool le week-end chez les Ã©lÃ¨ves selon l'environnement ", subtitle = "Tous niveaux confondus", 
 fill = "Cadre de vie (R = rural, U = Urbain)") + theme_minimal()
>>>>>>> Stashed changes


  aes(x = Walc, fill = address) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
<<<<<<< Updated upstream
  labs(x = "Consommation d'alcool pendant leweek-end ", y = "ElÃ¨ves dÃ©clarant boire le week-end", 
       title = "Consommation d'alcool le week-end chez les Ã©lÃ¨ves selon l'environnement ", subtitle = "Tous niveaux confondus", 
       fill = "Cadre de vie (R = rural, U = Urbain)") +
  theme_minimal()
=======
  labs(x = "Consommation d'alcool pendant leweek-end ", y = "Elèves déclarant boire le week-end", 
       title = "Consommation d'alcool le week-end chez les élèves selon l'environnement ", subtitle = "Tous niveaux confondus", 
       fill = "Cadre de vie (R = rural, U = Urbain)") + theme_minimal()
>>>>>>> Stashed changes
       

# Soutien scolaire et consommation d'alcool en semaine

ggplot(fulldt) +
  aes(x = Dalc, fill = schoolsup) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  labs(
    x = "QuantitÃ© d'alcool consommÃ©e en semaine (1 Ã©tant le plus faible)",
    y = "nombre d'Ã©lÃ¨ves",
    title = "Soutien scoalire et consommation d'alcool le week-end",
    fill = "BÃ©nÃ©ficie de soutien scolaire"
  ) +
  theme_minimal()

##=> Les Ã©lÃ¨ves qui dÃ©clarent consommer peu d'alcool en semaine, sont plus nombreux Ã  bÃ©nÃ©ficier de soutien scolaire
       
       
        # Essaie de calcul des variables famsup et paidclass. 
              
              # Consomation d'alcool excessive selon le soutien scolaire familial : variable famsup :
              
              gglot(fulldt) +
                aes(x = Dalc + Walk, fill = famsup) +
                geom_histogram(bins = 30L) +
                scale_fill_hue(direction = 1) +
                labs (   
                  x = "consomation d'alcool en semaine et en week end = consomation d'alcool excessive"
                  y = "Nombres d'eleves"
                  title = "Le soutien scolaire familial et la consomation excessive d'alcool",
                  fill = "Beneficie de soutien scolaire familial"
                ) +
                theme_minimal()
              
              
              # Consomation d'alcool excessive selon les cours supplémentaire payant : varable paidclass :
              
              gglot(fulldt) +
                aes(x = Dalc + Walk, fill = paidclass) +
                geom_histogram(bins = 30L) +
                scale_fill_hue(direction = 1) +
                labs (   
                  x = "consomation d'alcool en semaine et en week end = consomation d'alcool excessive"
                  y = "Nombres d'eleves"
                  title = "Le soutien scolaire familial et la consomation excessive d'alcool",
                  fill = "Beneficie de soutien scolaire familial"
                ) +
                theme_minimal()
              
              # Ne marche pas sur mon ordinateur car pas tout les paquets qui s'installent à voir si marche de manière générale. 

              
              
#Test Hugo
              
              