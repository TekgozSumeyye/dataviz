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
  scale_fill_manual(values=c("#ff8000", "#FF0000", "#1884F7", "#23BB66", "#1EC7DE")) +
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
  scale_fill_manual(values=c("#ff8000", "#FF0000", "#1884F7", "#23BB66", "#1EC7DE")) +
  ggtitle("Distribution du metier de la mere") +
  xlab("Job de la mere") +
  ylab("Effectifs") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#Ici aussi, la cat?gorie "autres" se d?marque des autres, suivi de services.
#Contrairement aux m?res, les p?res travaillent beaucoup plus dans des m?tiers
#de cat?gories "autres" ou services, alors qu'il y a une part importante de m?re
#travaillant dans ?ducation, sant? ou des m?res aux foyers.


#### III. Visualision des donnees afin d'etablir une eventuelle correlation entre la consommation d'alcool et les resultats scolaires

  #a)profil general des consommateurs d'alcool, afin d'etablir une premiere typologie general :

#Visualisation des consommations d'alcool weekend et semaine
#Consommation semaine
table(fulldt$Dalc)

ggplot(fulldt) +
  aes(x = Dalc) +
  geom_histogram(bins = 30L, fill = "#112446") +
  theme_minimal()
#En semaine, la consommation d'alcool est plutot moderer (tres faible consommation)

table(fulldt$Walc)

ggplot(fulldt) +
  aes(x = Walc) +
  geom_histogram(bins = 30L, fill = "#112446") +
  theme_minimal()
#Il y a beaucoup plus de consommation le weekend, etant donne qu'il n'y a pas cours, plus de personnes consomment excessivement de l'alcool. 

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
#Ici, nous cherchons a comprendre si la consommations d'alcool en semaine ou en weekend depend de l'age
#Les deux coefficient sont positive, mais etant donne que l'intervalle de confiance de Walc couvre 0, le coefficent n'est pas significative contrairement a dalc. 
#La consommation d'alcool en semaine depend plus de l'age que la consommation le weekend, c'est a dire que par exemple, en fonction de l'age, les personnes font plus attention ? leurs consommations d'alcool en semaine que en weekend.  

ggplot(fulldt, aes(x = Walc, y = Dalc , color = sex))+   geom_jitter(position=position_jitter(0.2))
# Analyse de la consommation d’alcool durant le travail en semaine (Dalc) et la consommation d’alcool durant le week-end
# (Walc) selon le genre : Valeurs numérique : 1 – Equivaut a tres faible et 5 – equivaut à tres elevee. 
#A travers le nuage de points ce que l'on peut voir selon la consommation d'alcool pendant le travail en semaine 
#(Dalc) et en week-end (Walc)  c'est qu'il y a peu de consommation excesive d’alcool que ce soit pour les homme ou pour les femmes,
#les points étant principalement concentrer entre le nombre 1 et 1,5. 
#Les femmes à part quelques occurrences (certain point compris au-delà de 5), boivent moins que les hommes qui sont
#plus nombreux à boire durant le travail en semaine (une importante répartition de points entre le 4 et le 5). 

#Pour la consommation d’alcool durant le week-end (Walc), il y a également peu de consommation 
#(majorité étant compris dans le 1) que ce soit pour les hommes ou pour les femmes . Il y a plus de consommation de la part
#des hommes durant le week end les points bleu étant compris entre le 4 et le 5. Il y a une augmentation de la consommation d’alcool pour les hommes le week-end (points bleu compris entre le 4 et le 5). 

#Selon notre nuages de points les hommes consomme plus d’alcool le week-end (la majorité étant 
#comprises dans le 4) que la semaine (la majorité étant comprises entre le 3 et 4). 

#Les femmes consommerai plus d’alcool durant le week-end (entre le 2 et le 3) qu’en semaine (majorité en 1). 

# La consommation d'alcool la semaine selon l'environnement (rural/urbain)

ggplot(fulldt, aes(x = Walc, y = Dalc , color = address))+ geom_jitter(position=position_jitter(0.2))

ggplot(fulldt) +
  aes(x = Walc, fill = address) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  labs(x = "Consommation d'alcool pendant leweek-end ", y = "Eleves declarant boire le week-end", 
       title = "Consommation d'alcool le week-end chez les eleves selon l'environnement ", subtitle = "Tous niveaux confondus", 
       fill = "Cadre de vie (R = rural, U = Urbain)") +
  theme_minimal()


ggplot(fulldt) +
  aes(x = Dalc, y = G3, colour = Dalc) +
  geom_jitter(size = 1.5) +
  scale_color_viridis_c(option = "viridis", direction = 1) +
  labs(
    x = "Consommation alcool semaine",
    y = "Notes G3",
    title = "Conso alcool semaine notes"
  ) +
  theme_minimal()
#Manque analyse






#b)Recherche plus poussée des raisons d'une consommation d'alcool «excessive» :




# Consomation "excessive" d'alcool selon le soutien scolaire : variable schoolsup :
ggplot(fulldt) +
  aes(x = Dalc, fill = schoolsup ) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  labs (   
    x = "consomation d'alcool en semaine" ,
    y = "Nombres d'eleves" ,
    title = "Le soutien scolaire et la consomation d'alcool",
    fill = "Beneficie de soutien scolaire"
  ) +
  theme_minimal()


# Consomation d'alcool excessive selon le soutien scolaire familial : variable famsup :

ggplot(fulldt) +
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

ggplot(fulldt) +
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









#Consommation d'alcool selon job mere
ggplot(fulldt) +
  aes(x = Mjob, fill = Mjob) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(
    x = "M?tiers de la m?re",
    y = "Effectifs",
    title = "Consommation d'alcool des ?l?ves selon le m?tiers de la m?re "
  ) +
  theme_minimal() +
  facet_wrap(vars(Dalc))

#Consommation d'alcool selon job du pere
ggplot(fulldt) +
  aes(x = Fjob, fill = Fjob) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(
    x = "M?tiers du p?re",
    y = "Effectifs",
    title = "Consommation d'alcool des ?l?ves selon le m?tiers du p?re "
  ) +
  theme_minimal() +
  facet_wrap(vars(Dalc))

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



table(fulldt$G3)
ggplot(fulldt) +
  aes(x = G3, y = sex) +
  geom_boxplot(fill = "#BDD3E8") +
  labs(x = "Notes G3", y = "Sexe", title = "Boites ? moustaches du sexe sur les notes G3") +
  theme_bw()
#Pour le sexe masculin, il y a 2 valeurs aberrantes et pour le sexe feminin seulement 1.
#Les notes medians sont plus eleves pour les filles avec environ 12 et 11 pour les garcons. 
#Par contre, le minimum est plus eleve chez les garcon, le maximum aussi est plus eleve pour les garcons. 
#Cela s'explique que les notes des filles sont similaires et que globalement les filles ont des notes elevees alors que chez les garcons, malgre quelques bonnes notes, ils ont plutot des notes moyennes. 


#BOXPLOT FJOB, MJOB, FEDU, MEDU:

ggplot(fulldt) +
  aes(x = G3, y = Mjob, fill = Mjob) +
  geom_boxplot() +
  scale_fill_brewer(palette = "OrRd", direction = 1) +
  labs(x = "Notes G3", y = "Job mere", title = "Boites ? moustaches du job de la mere sur les notes G3 ") +
  theme_bw()
#Pour les boites a moustache du metier de la mere, il y a 6 valeurs aberrantes pour la categorie "autres", ce sont des valeurs qui sont sup?rieures ou inf?rieures aux limites d?finies par les moustaches. 
#On observe selon le boxplot que les eleves ayant une mere qui est soit enseignante, soit dans la sante ou a la maison ont leur notes minimums comprise entre 5 et 7. 
#ceux qui ont les notes les plus eleves sont ceux dont leur meres est enseignante (mediane superieure aux autres ainsi que le troisieme quartile). Les eleves qui ont leur mères qui est "autre" (other) 
#ont leur notes minimum qui est plus large (4,5 environ), sûrement liées au fait que cette categorie est tres large (beaucoup de qualificatif large a linterieur). 
#Les eleves ayant une mere enseignante (teacher) ont de meilleures notes (ayant leur médiane se rapprochant plus de 15 et                                                                       leur 3 eme quartile dépassant les 15).
#Ceux qui ont une mere a la maison  (at_home) en majorite ont des notes comprise entre 10 et 14 (la médiane etant environ a 11).
#On peut conclure alors que la profession de la mere a une influence sur les resultats scolaire de leleve. 
#Cela peut sexpliquer notamment par le temps disponible liees a la profession mais aussi les aides qui peuvent etre apporter
#(par exemple le fait dautre enseignant est plus susceptible daider).
#Le minimum de mere-sante est de 7 et le maximum est de 20 contrairement au mere-maison ou mere-service qui est de 5 et 18,5 respectivement. 
#Par contre, l'ecart interquartile est vraiment important pour les meres-sante, ce qui signifie que les notes sont variables. 

ggplot(fulldt) +
  aes(x = G3, y = Fjob, fill = Fjob) +
  geom_boxplot() +
  scale_fill_brewer(palette = "OrRd", direction = 1) +
  labs(x = "Notes G3", y = "Job mere", title = "Boxplot") +
  theme_bw()

#MANQUE MEDU ET FEDU BOXPLOT


# Consomation d'alcool excessive en semaine selon le soutien scolaire familial : variable famsup :
ggplot(fulldt) +
  aes(x = Dalc, fill = famsup) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  labs (   
    x = "consomation d'alcool en semaine" ,
    y = "Nombres d'eleves" , 
    title = "Le soutien scolaire familial et la consomation d'alcool en semaine",
    fill = "Beneficie de soutien scolaire familial"
  ) +
  theme_minimal()


# Consomation d'alcool excessive selon les cours supplémentaire payant : varable paidclass :

ggplot(fulldt) +
  aes(x = Dalc, fill = paid) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  labs (   
    x = "consomation d'alcool en semaine",
    y = "Nombres d'eleves" , 
    title = "Le soutien scolaire familial et la consomation excessive d'alcool",
    fill = "Beneficie de soutien scolaire familial"
  ) +
  theme_minimal()
#Les eleves qui declarent consommer peu d'alcool en semaine, sont plus nombreux a beneficier de soutien scolaire
     

#FAIRE ANALYSE TUTEUR SUR G3
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




#b)Recherche plus poussee des raisons d'une consommation d'alcool ?excessive? :
#...


#Consommation d'alcool selon la taille de la famille

ggplot(fulldt) +
  aes(x = Dalc, y = Walc, colour = famsize) +
  geom_jitter() +
  scale_color_viridis_d(option = "plasma", direction = 1) +
  labs(
    x = "Consommation alcool semaine",
    y = "Consommation alcool weekend",
    title = "Nuage de point consommation alcool en fonction du job de la mere"
  ) +
  theme_bw()




#Structure Familiale
#Travail sur la cohabitation des parents
                 
table(fulldt$Pstatus)
mode(fulldt$Pstatus) 
                    
ggplot(fulldt) +
  aes(x = Dalc, fill = Pstatus, colour = Pstatus) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Consommation en semaine",
    y = "Effectifs",
    title = "Consommation d'alcool en semaine selon la cohabitation parentale"
  ) +
  theme_minimal()
                    
ggplot(fulldt) +
  aes(x = Walc, fill = Pstatus, colour = Pstatus) +
  geom_histogram(bins = 30L) +
  scale_fill_hue(direction = 1) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Consommation le weekend",
    y = "Effectifs",
    title = "Consommation d'alcool le weekend selon la cohabitation parentale"
  ) +
  theme_minimal()
                    

#Travail sur la qualité des relations familiales
table(fulldt$famrel)
 
ggplot(fulldt)+
  aes(x=famrel, y=Walc, colour=famrel)+
  scale_fill_continuous()+
  geom_jitter()+
  labs(x="Relation familiale", y = "Consommation d alcool en semaine", title="Lien entre consommation d alcool en semaine et relation familiale")+
  theme_bw()

      # Concernant les deux tableaux sur le lien entre relation familiale et consommation d'alcool (un pour la semaine et un pour le week end).
      # On remarque une tendance generale sur les deux tableaux, etant que les reponses sont distribuees de facon assez uniforme a travers le nuage de points.
      # Si les relations familiales impactaient la consommation d'alcool, on aurait du voir une concentration de point
      # en haut à gauche et en bas a droite des tableaux. Au lieu de ca, les points sont distribues de facon similaire
      # entre ceux ayant les pires relations et ceux ayant les meilleures relations familiales. La seule difference est que l'on remarque
      # que les gens ont tendance a plus consommer en week end qu'en semaine comme le confirme un de nos tableaux precedents.
                    
                    #Travail sur le responsable legal de l eleve
                    
                    table(fulldt$guardian)
                    
                    ggplot(fulldt)+
                      aes(x = guardian, fill = guardian)+
                      geom_histogram()+
                      scale_fill_hue(direction = 1)+
                      labs(x = "responsable legal", title = "test")+
                      theme_bw()
                    #Je voulais juste voir la répartion du père ou de la mère mais la fonction table a suffit pour montrer 
      

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
           
