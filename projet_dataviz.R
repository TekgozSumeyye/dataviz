#Chemin des bases de donnees
path <- file.path("C:", "Users", "tekgo", "Documents", "GitHub", "dataviz", fsep="\\")
setwd(path)

setwd("C:/Users/Aurore/OneDrive/Bureau/M2 ASC/Premier Semestre/Outil et logiciel d'analyse/Outils et analyse de données")

#R version 4.1.3 (2022-03-10)

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

#Ces donnees portent sur les resultats des eleves dans l'enseignement 
#secondaire de deux ecoles portugaises. Les attributs des donnees 
#comprennent les notes des eleves, les caracteristiques demographiques, 
#sociales et scolaires, et ont ete collectes a l'aide de rapports et de 
#questionnaires scolaires. Deux ensembles de donnees sont fournis concernant
#les performances dans deux matieres distinctes : Les mathematiques (mat) et
#la langue portugaise (por). 

#A partir de l'analyse du lien qui pourrait exister entre la consommation d'alcool et les resultats
#scolaires, l'idee est de determiner plus geralement quels pourraient etre les facteurs affectant la
#reussite scolaire dans le contexte des jeux de donnees dont nous disposons.

N1 <- nrow(maths)
N2 <- nrow(Portuguese)
#Pour la base de donnee maths, il y a 395 observations et 33 variables.
#Pour la base de donnee portuguese, il y a 649 observations et 33 variables identiques avec la 1ere bdd. 

fulldt<-bind_rows(maths, Portuguese)
nrow(fulldt)
str(fulldt)
#Avec str, on visualise les premieres donnees des variables, et on regarde si ce sont des variables qualitative ou quantitatives


#### II. Visualisation des donnees####

# a) Ecole
ggplot(as.data.frame(table(fulldt$school))) +
     geom_bar(aes(x = Var1, y = Freq, fill = Var1), 
                           stat = 'identity') +
     scale_fill_manual(values=c("#56B4E9", "#E69F00")) +
     ggtitle("Repartition de l'ecole") +
     xlab("Ecole") +
     ylab("Effectifs") +
     theme_bw() +
     theme(axis.text.x = element_text(face = 'bold', size = 10),
                     axis.text.y = element_text(face = 'bold', size = 10))

#Pour fulldt, il y a 772 eleves scolarises a Gabriel Pereira et 272  a Mousinho da Silveira. 

# b) sexe en fonction de l'ecole
ggplot(fulldt) +
  aes(x = school, fill = sex) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Ecole",
    y = "Effectifs",
    title = "Repartition du sexe en fonction de l'ecole"
  ) +
  theme_minimal()

#Il y a 591 filles et 453 garcons au sein des 2 ecoles
#La proportion de filles est plus importantes au sein de l'ecole MS. 


# c)Age en fonction du sexe
ggplot(fulldt) +
  aes(x = age, fill = sex) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Age",
    y = "Effectifs",
    title = "Repartition de l'age en fonction du sexe"
  ) +
  theme_minimal()
table(fulldt$age)
#Il y a tres peu d'eleves au dela de 19 ans, les eleves au dela de cette age sont surement du a des redoublements au cours de leurs scolarites.

# d) Adresse
#Adresse en fonction de ecole
ggplot(fulldt) +
  aes(x = address, fill = school) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Adresse",
    y = "Effectif",
    title = "Repartition de l'adresse (U ou R) en fonction de l'ecole"
  ) +
  theme_minimal()
#Pour l'ecole MS, il y a une repartition approximative des eleves vivant dans une zone urbaine ou rurale alors que au contraire pour l'ecole GP, une grande partie vivent dans une zone urbaine. 

#Visualisation seulement adresse
as.data.frame(table(fulldt$address))
ggplot(as.data.frame(table(fulldt$address))) +
  geom_bar(aes(x = Var1, y = Freq,  fill = Var1), 
           stat = 'identity') +
  scale_fill_manual(values=c( '#2B9E67', '#FFA44B')) +
  ggtitle("Repartition de l'adresse") +
  xlab("Adresse") +
  ylab("Effectifs") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))
table(fulldt$address)
#Il y a 285 eleves vivant dans une zone rurale et 759 dans une zone urbaine

#e) Emploi pere

as.data.frame(table(fulldt$Fjob))
ggplot(as.data.frame(table(fulldt$Fjob))) +
  geom_bar(aes(x = Var1, y = Freq, fill = Var1), 
           stat = 'identity') +
  scale_fill_manual(values=palette) +
  ggtitle("Distribution de metier du pere") +
  xlab("Job du pere") +
  ylab("Effectifs") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))
#Nous observons que la categorie "les autres" se demarque des 3 autres categories (a la maison, sante et professeurs)


#f) Emploi de la mere
as.data.frame(table(fulldt$Mjob))
ggplot(as.data.frame(table(fulldt$Mjob))) +
  geom_bar(aes(x = Var1, y = Freq, fill = Var1), 
           stat = 'identity') +
  scale_fill_manual(values=palette) +
  ggtitle("Distribution du metier de la mere") +
  xlab("Job de la mere") +
  ylab("Effectifs") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#Ici aussi, other se demarque des autres, suivi de services
#Contrairement aux meres, les peres travaillent beaucoup plus dans des metiers de categories "autres" ou services, alors qu'il y a une part importante de mere travaillant dans leducation, sante ou des meres aux foyers.



#### III. Visualision des donnees afin d'etablir une eventuelle correlation entre la consommation d'alcool et les resultats scolaires

  #a)profil general des consommateurs d'alcool, afin d'etablir une premiere typologie general :

  #Visualisation des consommations d'alcool weekend et semaine
#Consommation semaine
table(fulldt$Dalc)

ggplot(fulldt) +
  aes(x = Dalc) +
  geom_histogram(bins = 30L, fill = "#112446") +
  theme_minimal()
#En semaine, la consommation d'alcool est plutot moderer (tres peu de consommation)

table(fulldt$Walc)

ggplot(fulldt) +
  aes(x = Walc) +
  geom_histogram(bins = 30L, fill = "#112446") +
  theme_minimal()
#Il y a beaucoup plus de consommation le weekend, etant donne qu'il n'y a pas cours, plus de personnes consomment excessifement de l'alcool. 

#Impact alcool sur G3
cor1 <- G3 ~ Walc + Dalc
lm1<-lm(cor1 , data = fulldt)
coefplot(lm1 , outerCI = 1.96 , intercept = FALSE)
#La consommation d'alcool en semaine a plus d'impact negatif sur les resultats scolaires G3 que la consommation le weekend. 
#Les deux coefficient est negative, mais l'intervalle de confiance de Walc couvre 0, donc le coefficient de Walc n'est pas significatif. 
#Cela peut etre du au fait qu'en semaine, les personnes consommant de l'alcool ne se concentre pas aux revisions donc leurs notes sont impact?s. 
#Dalc= -0.39 et walc -0.17

cor2 <- age ~ Walc + Dalc
lm2<-lm(cor2 , data = fulldt)
coefplot(lm2 , outerCI = 1.96 , intercept = FALSE)
#Ici, nous chercons ? comprendre si la consommations d'alcool en semaine ou en weekend depend de l'age
#Les deux coefficient sont positive, mais etant donne que l'intervalle de confiance de Walc couvre 0, le coefficent n'est pas significative contrairement a dalc. 
#La consommation d'alcool en semaine depend plus de l'age que la consommation le weekend, c'est a dire que par exemple, en fonction de l'age, les personnes font plus attention ? leurs consommations d'alcool en semaine que en weekend.  

ggplot(fulldt, aes(x = Walc, y = Dalc , color = sex, size = freetime))+   geom_jitter(position=position_jitter(0.2))
#Analyse Aurore

#Les femmes ont une consommation plutot moderer que les hommes, on retrouve plus les H dans cat 5

ggplot(fulldt, aes(x = Walc, y = Dalc , color = address, size = freetime))+ geom_jitter(position=position_jitter(0.2))

# Analyse de la consommation d’alcool durant le travail en semaine (Dalc) et la consommation d’alcool durant le week-end
# (Walc) selon le genre : Valeurs numérique : 1 – Equivaut a tres faible et 5 – equivaut à tres elevee. 
#A travers le nuage de points ce que l'on peut voir selon la consommation d'alcool pendant le travail en semaine 
#(Dalc) et en week-end (Walc)  c'est qu'il y a peu de consommation d’alcool que ce soit pour les homme ou pour les femmes,
#les points étant principalement concentrer entre le nombre 1 et 1,5. 
#Les femmes à part quelques occurrences (certain point compris au-delà de 5), boivent moins que les hommes qui sont
#plus nombreux à boire durant le travail en semaine (une importante répartition de points entre le 4 et le 5). 

#Pour la consommation d’alcool durant le week-end (Walc), il y a également peu de consommation 
#(majorité étant compris dans le 1) que ce soit pour les hommes ou pour les femmes . Il y a plus de consommation de la part
#des hommes durant le week end les points bleu étant compris entre le 4 et le 5. Il y a une augmentation de la consommation d’alcool pour les hommes le week-end (points bleu compris entre le 4 et le 5). 

#Selon notre nuages de points les hommes consomme plus d’alcool le week-end (la majorité étant 
#comprises dans le 4) que la semaine (la majorité étant comprises entre le 3 et 4). 

#Les femmes consommerai plus d’alcool durant le week-end (entre le 2 et le 3) qu’en semaine (majorité en 1). 





#Faire boxplot pour etudes pere et mere 


# Analyse des notes (compris entre 0 et 20) selon la profession de la mère (Mjob) compris entre les catégories : teacher, services, other, healt et at home.

#On observe selon le boxplot que les élèves ayant une mère qui est soit enseignante, soit dans la santé ou à la maison ont leur notes minimums comprise entre 5 et 7. 
#La majorité des éléves ont des notes tournant autour de la moyenne (la majorité des premier quartiles dépassant la note 10), ceux qui ont les notes les plus éléves sont ceux dont leur mères est enseignante (médiane supérieure aux autres ainsi que le troisième quartile). Les élèves qui ont leur mères qui est « autres » (other) ont leur notes minimum qui est plus large (4,5 environ), sûrement liées au fait que cette catégorie est très large (beaucoup de qualificatif large à l’intérieur). 
#Les éléves ayant une mère enseignante (teacher) ont de meilleures notes (ayant leur médiane se rapprochant plus de 15 et                                                                       leur 3 eme quartile dépassant les 15).
#Ceux qui ont une mère à la maison  (at_home) en majorité ont des notes comprise entre 10 et 14 (la médiane etant environ à 11).
#On peut conclure alors que la profession de la mère à une influence sur les résultats scolaire de l’éleve. 
#Cela peut s’expliquer notamment par le temps disponible liées à la profession mais aussi les aides qui peuvent être apporter
#(par exemple le fait d’être enseignant est plus susceptible d’aider).


#b)Recherche plus poussée des raisons d'une consommation d'alcool «excessive» :

# Consomation "excessive" d'alcool selon le soutien scolaire : variable schoolsup :
gglot(fulldt) +
  aes(x = Dalc + Walk, fill = schoolsup ) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  labs (   
    x = "consomation d'alcool en semaine et en week end = consomation d'alcool excessive" ,
    y = "Nombres d'eleves" ,
    title = "Le soutien scolaire et la consomation excessive d'alcool",
    fill = "Beneficie de soutien scolaire"
  ) +
  theme_minimal()

# Essaie de calcul des variables famsup et paidclass. 

# Consomation d'alcool excessive selon le soutien scolaire familial : variable famsup :

gglot(fulldt) +
  aes(x = Dalc + Walk, fill = famsup) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  labs (   
    x = "consomation d'alcool en semaine et en week end = consomation d'alcool excessive" ,
    y = "Nombres d'eleves" ,
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
    x = "consomation d'alcool en semaine et en week end = consomation d'alcool excessive" ,
    y = "Nombres d'eleves" ,
    title = "Le soutien scolaire familial et la consomation excessive d'alcool",
    fill = "Beneficie de soutien scolaire familial"
  ) +
  theme_minimal()
# Après visualisation des résultats nous concluons qu'ils ne sont pas pertinent pour notre analyse car sont trop proche et ne sont pas signifiant.
# Ne marche pas sur mon ordinateur car pas tout les paquets qui s'installent à voir si marche de manière générale.



#Analyse Doriane
#


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
    title = "Nuage de point consommation alcool en fonction G3 et ecole"
  ) +
  theme_bw()

# Ici on utilise le nuage de point pour montrer la difference entre école et l'influence de la consommation d'alcool sur les notes aux examens finaux (G3) 
# car la fonction nuage de point nous permet ici de facilement montrer (ou non) la disparite entre les deux ecoles et surtout de faire ressortir les tendances statistiques 
# sur la consommation d'alcool des eleves et peut etre d'etablir une influence sur les resultats finaux aux examens des eleves.
# Le graphique se lit comme suit : les points en haut a droite sont ceux qui consomment le plus d'alcool a la fois en weekend et en semaine (ceux se rapprochant le plus du 5 sur l'abscisse et l'ordonnee).
# A l'inverse, ceux etant le plus en bas a gauche (les plus proches de 1) sont ceux consommant le moins, voir pas d'alcool. En haut a gauche, se situent les personnes consommant exclusivement le weekend (1 a l'abcisse et 5 a l'ordonnee),
# et ceux etant le plus en bas a droite sont ceux qui consomment de l'alcool exclusivement en semaine (5 a l'abscisse et 1 a l'ordonne).
# Le premier resultat que l'on remarque est que la tendance a la consommation d'alcool est beaucoup plus importante durant le weekend que durant la semaine malgres quelques exceptions.
# On peut expliquer cette tendance par le fait que la consommation d'alcool a l'adolescence et pour les jeunes adultes soient surtout liee a des moments sociabilites entre groupes de pair.
# Les etudiants ayant cours en semaine, la plupart des activites sociales sont donc organisees en fin de semaine, en weekend et ces activites sont le moment propices a la consommation d'alcool en vue de sociabiliser (sorties en bars, boites, soiree chez quelqu'un).
# Le deuxieme resultat que l'on peut noter est que les eleves de Gabirel Pereira (en rouge) sont plus nombreux a consommer de l'alcool que ceux de Mousinho da Silveira (en bleu).
# Les eleves de GP sont aussi surtout beaucou plus representer dans des consommations intensives et notamment celles faites en semaines.
# Difficile de dire si la difference est assez grande pour etre significative. Un debut d'analyse pour expliquer cette difference serait que Gabriel Pereira
# est une ecole publique et donc que les eleves aient une plus grande liberte et moins d'attente au niveau du corps enseignant.
# Le troisieme resultat est que l'on remarque que les resultats les plus bas (0, 5, 10) ont tendance a augmenter avec la consommation d'alcool
# et surtout avec la consommation d'alcool en semaine. On ne peut pas affirmer que ce resultat soit significatif au vu de la presence de tres nombreux non-buveurs ou buveurs occasionnelle.
# Cependant on peut imaginer une hypothese qui est que ceux qui consomment en semaine sont probablement victime d'addiction, d'alcoolisme
# etant donne que l'addiction a l'alcool se caracterise par une consommation presque journaliere. On manque d'element pour approuver la validation d'une tel hypothese,
# mais tout comment l'addiction a la marijuana a des consequences sur les notes, l'addiction a l'alcool pourrait entrainer des consequences sur les notes finales comme le montre le graphique.
# Cependant, tout comme l'addiction a la marijuana, il faut voir si c'est l'addiction a l'alcool en elle meme qui cause la baisse des notes ou un environnement social particulierement precaire
# qui causerait cette baisse des notes et cette addiction a la fois. C'est pour cela que nous analyseront la consommation en fonction de l'origine sociale, de la structure familiale et de l'accompagnement des eleves.


#Analyse Juliette

table(fulldt$G3)

#Sumeyye
ggplot(fulldt) +
  aes(x = G3, y = sex) +
  geom_boxplot(fill = "#BDD3E8") +
  labs(x = "Notes G3", y = "Sexe", title = "Boites � moustaches du sexe sur les notes G3") +
  theme_bw()
#Pour le sexe masculin, il y a 2 valeurs aberrantes et pour le sexe feminin seulement 1.
#Les notes medians sont plus eleves pour les filles avec environ 12 et 11 pour les garcons. 
#Par contre, le minimum est plus eleve chez les garcon, le maximum aussi est plus eleve pour les garcons. 
#Cela s'explique que les notes des filles sont similaires et que globalement les filles ont des notes elevees alors que chez les garcons, malgre quelques bonnes notes, ils ont plutot des notes moyennes. 

ggplot(fulldt) +
  aes(x = G3, y = Mjob, fill = Mjob) +
  geom_boxplot() +
  scale_fill_brewer(palette = "OrRd", direction = 1) +
  labs(x = "Notes G3", y = "Job mere", title = "Boites � moustaches du job de la mere sur les notes G3 ") +
  theme_bw()
��#Pour les boites a moustache du metier de la mere, il y a 6 valeurs aberrantes pour la categorie "autres", ce sont des valeurs qui sont sup?rieures ou inf?rieures aux limites d?finies par les moustaches. 
#On remarque que les eleves de mere travaillant dans la sante ou les eleves de meres professeurs ont des meilleurs notes (m?diant) que les meres aux foyers et "autres". 
#Le minimum de mere-sante est de 7 et le maximum est de 20 contrairement au mere-maison ou mere-service qui est de 5 et 18,5 respectivement. 
#Par contre, l'icart interquartile est vraiment important pour les meres-sante, ce qui signifie que les notes sont variables. 


#Aurore

# Essaie de calcul des variables famsup et paidclass. 

# Consomation d'alcool excessive selon le soutien scolaire familial : variable famsup :
#ne marche pas
gglot(fulldt) +
  aes(x = Dalc + Walk, fill = famsup) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  labs (   
    x = "consomation d'alcool en semaine et en week end = consomation d'alcool excessive" ,
    y = "Nombres d'eleves" , 
    title = "Le soutien scolaire familial et la consomation excessive d'alcool",
    fill = "Beneficie de soutien scolaire familial"
  ) +
  theme_minimal()


# Consomation d'alcool excessive selon les cours supplémentaire payant : varable paidclass :
#ne marche pas
gglot(fulldt) +
  aes(x = Dalc + Walk, fill = paidclass) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  labs (   
    x = "consomation d'alcool en semaine et en week end = consomation d'alcool excessive",
    y = "Nombres d'eleves" , 
    title = "Le soutien scolaire familial et la consomation excessive d'alcool",
    fill = "Beneficie de soutien scolaire familial"
  ) +
  theme_minimal()

# Ne marche pas sur mon ordinateur car pas tout les paquets qui s'installent à voir si marche de manière générale.
# Nous n'allons pas garder ces calculs car les résultats ne sont pas vraiment significatif ou intéressant dans ce que nous cherchons 
# a voir cela était alors des essais non concluant. 















ggplot(fulldt) +
  aes(x = G3, y = Fjob, fill = Fjob) +
  geom_boxplot() +
  scale_fill_brewer(palette = "OrRd", direction = 1) +
  labs(x = "Notes G3", y = "Job mere", title = "Boxplot") +
  theme_bw()

# Analyse des notes (compris entre 0 et 20) selon la profession de la mère (Mjob) compris entre les catégories : teacher, services, other, healt et at home.

#On observe selon le boxplot que les élèves ayant une mère qui est soit enseignante, soit dans la santé ou à la maison ont leur notes minimums comprise entre 5 et 7. 
#La majorité des éléves ont des notes tournant autour de la moyenne (la majorité des premier quartiles dépassant la note 10), ceux qui ont les notes les plus éléves sont ceux dont leur mères est enseignante (médiane supérieure aux autres ainsi que le troisième quartile). Les élèves qui ont leur mères qui est « autres » (other) ont leur notes minimum qui est plus large (4,5 environ), sûrement liées au fait que cette catégorie est très large (beaucoup de qualificatif large à l’intérieur). 
#Les éléves ayant une mère enseignante (teacher) ont de meilleures notes (ayant leur médiane se rapprochant plus de 15 et                                                                       leur 3 eme quartile dépassant les 15).
#Ceux qui ont une mère à la maison  (at_home) en majorité ont des notes comprise entre 10 et 14 (la médiane etant environ à 11).
#On peut conclure alors que la profession de la mère à une influence sur les résultats scolaire de l’éleve. 
#Cela peut s’expliquer notamment par le temps disponible liées à la profession mais aussi les aides qui peuvent être apporter
#(par exemple le fait d’être enseignant est plus susceptible d’aider).

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


# Analyse de la consommation d’alcool durant le travail en semaine (Dalc) et la consommation d’alcool durant le week-end
# (Walc) selon le genre : Valeurs numérique : 1 – Equivaut a tres faible et 5 – equivaut à tres elevee. 
#A travers le nuage de points ce que l'on peut voir selon la consommation d'alcool pendant le travail en semaine 
#(Dalc) et en week-end (Walc)  c’est qu’il y a peu de consommation d’alcool que ce soit pour les homme ou pour les femmes,
#les points étant principalement concentrer entre le nombre 1 et 1,5. 
#Les femmes à part quelques occurrences (certain point compris au-delà de 5), boivent moins que les hommes qui sont
#plus nombreux à boire durant le travail en semaine (une importante répartition de points entre le 4 et le 5). 

#Pour la consommation d’alcool durant le week-end (Walc), il y a également peu de consommation 
#(majorité étant compris dans le 1) que ce soit pour les hommes ou pour les femmes . Il y a plus de consommation de la part
#des hommes durant le week end les points bleu étant compris entre le 4 et le 5. Il y a une augmentation de la consommation d’alcool pour les hommes le week-end (points bleu compris entre le 4 et le 5). 

#Selon notre nuages de points les hommes consomme plus d’alcool le week-end (la majorité étant 
#comprises dans le 4) que la semaine (la majorité étant comprises entre le 3 et 4). 

#Les femmes consommerai plus d’alcool durant le week-end (entre le 2 et le 3) qu’en semaine (majorité en 1). 


#Faire boxplot pour etudes pere et mere 


# Analyse des notes (compris entre 0 et 20) selon la profession de la mère (Mjob) compris entre les catégories : teacher, services, other, healt et at home.

#On observe selon le boxplot que les élèves ayant une mère qui est soit enseignante, soit dans la santé ou à la maison ont leur notes minimums comprise entre 5 et 7. 
#La majorité des éléves ont des notes tournant autour de la moyenne (la majorité des premier quartiles dépassant la note 10), ceux qui ont les notes les plus éléves sont ceux dont leur mères est enseignante (médiane supérieure aux autres ainsi que le troisième quartile). Les élèves qui ont leur mères qui est « autres » (other) ont leur notes minimum qui est plus large (4,5 environ), sûrement liées au fait que cette catégorie est très large (beaucoup de qualificatif large à l’intérieur). 
#Les éléves ayant une mère enseignante (teacher) ont de meilleures notes (ayant leur médiane se rapprochant plus de 15 et                                                                       leur 3 eme quartile dépassant les 15).
#Ceux qui ont une mère à la maison  (at_home) en majorité ont des notes comprise entre 10 et 14 (la médiane etant environ à 11).
#On peut conclure alors que la profession de la mère à une influence sur les résultats scolaire de l’éleve. 
#Cela peut s’expliquer notamment par le temps disponible liées à la profession mais aussi les aides qui peuvent être apporter
#(par exemple le fait d’être enseignant est plus susceptible d’aider).
#Analyse Sumeyye
#median tuteur-pere=12,5, median other et mere = identiques

#b)Recherche plus poussee des raisons d'une consommation d'alcool ?excessive? :
#...


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


# La consommation d'alcool la semaine selon l'environnement (rural/urbain)

ggplot(fulldt) +
 aes(x = Walc, fill = address) +
 geom_histogram(bins = 30L) +
 scale_fill_hue(direction = 1) +
 labs(x = "Consommation d'alcool pendant leweek-end ", y = "Eleves declarant boire le week-end", 
 title = "Consommation d'alcool le week-end chez les eleves selon l'environnement ", subtitle = "Tous niveaux confondus", 
 fill = "Cadre de vie (R = rural, U = Urbain)") +
 theme_minimal()

 labs(x = "Consommation d'alcool pendant le week-end ", y = "Eleves declarant boire le week-end", 
 title = "Consommation d'alcool le week-end chez les eleves selon l'environnement ", subtitle = "Tous niveaux confondus", 
 fill = "Cadre de vie (R = rural, U = Urbain)") + theme_minimal()


 aes(x = Walc, fill = address) +
   geom_histogram(bins = 30L) +
   scale_fill_hue(direction = 1) +
  labs(x = "Consommation d'alcool pendant leweek-end ", y = "Eleves declarant boire le week-end", 
       title = "Consommation d'alcool le week-end chez les eleves selon l'environnement ", subtitle = "Tous niveaux confondus", 
       fill = "Cadre de vie (R = rural, U = Urbain)") +
  theme_minimal()
#JSP si pour vous ca marche, mais en tout cas ca ne marche pas pour moi, tu peux reverifier Juliette?

  labs(x = "Consommation d'alcool pendant leweek-end ", y = "Eleves declarant boire le week-end", 
       title = "Consommation d'alcool le week-end chez les eleves selon l'environnement ", subtitle = "Tous niveaux confondus", 
       fill = "Cadre de vie (R = rural, U = Urbain)") + theme_minimal()
#Pareil
       

# Soutien scolaire et consommation d'alcool en semaine

ggplot(fulldt) +
  aes(x = Dalc, fill = schoolsup) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Quantite d'alcool consommee en semaine (1 etant le plus faible)",
    y = "nombre d'eleves",
    title = "Soutien scoalire et consommation d'alcool le week-end",
    fill = "Beneficie de soutien scolaire") + theme_minimal()

##=> Les eleves qui declarent consommer peu d'alcool en semaine, sont plus nombreux a beneficier de soutien scolaire
       
       

              # Ne marche pas sur mon ordinateur car pas tout les paquets qui s'installent a voir si marche de manière générale. 
#cest normale que ca ne marche pas, tu peux pas avoir 2 variables dans x (sauf erreur de ma part) et la 2eme variable sappelle paid pas paidclass, je te l'aisse corriger sinon je vais le faire a ta place? 
              
#Test Hugo
              #Structure Familiale
              
              #Travail sur la cohabitation des parents
             
              table(fulldt$Pstatus)
              mode(fulldt$Pstatus) 
              
              ggplot(fulldt)+
                aes(x = Dalc,y = Pstatus, fill = Pstatus)+
                scale_fill_hue(direction=1)+
                geom_boxplot()+
                labs(x = "Consommation en semaine", y = "Cohabitation parentale", title = "Lien entre consommation en semaine et parents vivant ensemble")+
                theme_bw()
              
              ggplot(fulldt)+
                aes(x = Walc,y = Pstatus, fill = Pstatus)+
                scale_fill_hue(direction=1)+
                geom_boxplot()+
                labs(x = "Consommation durant le weekend", y = "Cohabitation parentale", title = "Lien entre consommation le weekend et parents vivant ensemble")+
                theme_bw()
              
#Je ne sais pas si il y n'a aucune différence entre les élèves avec des parents séparés et ceux avec des parents habitant ensemble ou si mon code ne marche juste pas. 
              
              #Travail sur la qualité des relations familiales
              table(fulldt$famrel)
              
              ggplot(fulldt)+
                aes(x = Dalc, y = famrel, fill=famrel)+
                scale_fill_hue(direction = 1)+
                geom_boxplot()+
                labs(x = "Consommation d alcool en semaine", y = "qualite des relations familiales", title="Lien entre conso d alcool en semaine et relation familiale")+
                theme_bw()
              # Ca marche pas et je comprends pas pourquoi, j ai peut etre mal choisi la mauvaise fonction pour montrer ce que je veux montrer
              
              ggplot(fulldt)+
                aes(x=Dalc, y=Walc, colour=famrel)+
                scale_fill_continuous()+
                geom_jitter()+
                labs(x="Consommation d alcool en semaine", y = "Consommation d alcool en weekend", title="Lien entre consommation d alcool et relation familiale")+
                theme_bw()
              #C est peut etre mieux avec les nuages de points, je me demande si y a pas moyen de garder juste les extremes pour "famrel" mais je sais pas si c'est possible
            
# En observant et en se concentrant sur les extremes (les points les plus clairs et ceux les plus sombres)
# on peut remarquer qu'il n'y pas une si grande disparite que ça et que la qualité des relations familiales est distribuee
# de facon assez uniforme a travers tout le nuage de point. Les eleves ont en général une bonne relation avec leur famille et il y en a peu
# avec une mauvaise ou tres mauvaise relation familiale (1, 2) et meme en se concentrant sur ceux la, ils se repartissent de facon assez
# equitable meme si ils ont l air d etre un peu plus présent parmis ceux qui boivent plus frequemment.
              
              #Travail sur le responsable legal de l eleve
              
              table(fulldt$guardian)
              
              ggplot(fulldt)+
                aes(x = guardian, fill = guardian)+
                geom_histogram()+
                scale_fill_hue(direction = 1)+
                labs(x = "responsable legal", title = "test")+
                theme_bw()
              #Je voulais juste voir la répartion du père ou de la mère mais la fonction table a suffit pour montrer 

              #la difference de 25 - 75%

              # La consommation d'alcool le week-end selon l'origine sociale 
              
              
              ggplot(fulldt) +
                
                aes(x = Dalc, y = Medu, fill = Medu) +
                geom_boxplot(fill = "#BDD3E8") +
                scale_fill_brewer(palette = "OrRd", direction = 1) +
                labs(x = "Conso alc semaine", y = "etudes mere", title = "Boxplot") +
                theme_bw()
              
              ggplot(fulldt) +
                aes(x = Dalc, y = Fedu, fill = Fedu) +
                geom_boxplot(fill = "#BDD3E8") +
                scale_fill_brewer(palette = "OrRd", direction = 1) +
                labs(x = "Conso alc semaine", y = "etudes pere", title = "Boxplot") +
                theme_bw()
              
              
                            #la difference de 25 - 75%
