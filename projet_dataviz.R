#Chemin importation des bases de donn?es
path <- file.path("C:", "Users", "tekgo", "Documents", "GitHub", "dataviz", fsep="\\")
setwd(path)

#Version de R : R version 4.1.3 (2022-03-10)
#La r?partition du travail est disponible sur GitHub

#T?l?chargement des librairies 
library(readr)
library(coefplot)
library(ggplot2)
library(labelled)
library(dplyr)
library(RColorBrewer)
library(GGally)
library(scales)

#Importation de la base de donn?es 
maths <- read_csv("Maths.csv")
Portuguese <- read_csv("Portuguese.csv")

# I- Introduction 

#Ces donn?es portent sur les r?sultats des ?l?ves dans l'enseignement secondaire de deux ?coles portugaises. Les attributs des donn?es comprennent les notes des ?l?ves, les caract?ristiques d?mographiques, sociales et scolaires, et ont ?t? collect?s a l'aide de rapports et de questionnaires scolaires. Deux ensembles de donn?es sont fournis concernant les performances dans deux mati?res distinctes : Les math?matiques (mat) et la langue portugaise (por).
#A partir de l'analyse du lien qui pourrait exister entre la consommation d'alcool et les r?sultats scolaires, l'id?e est de d?terminer plus g?n?ralement quels pourraient ?tre les facteurs affectant la r?ussite scolaire dans le contexte des jeux de donn?es dont nous disposons.
N1 <- nrow(maths)
N2 <- nrow(Portuguese)

#Pour la base de donn?e maths, il y a 395 observations et 33 variables. Pour la base de donn?e portuguese, il y a 649 observations et 33 variables identiques avec la premi?re base de donn?e.

#Nous avons d?cid? de rassembler les deux bases de donn?es Avec str, on visualise les premi?res donn?es des variables, et on regarde si ce sont des variables qualitative ou quantitatives

fulldt<-bind_rows(maths, Portuguese)
nrow(fulldt)
str(fulldt)

# II- Visualisation des donn?es 
## a) Ecole

ggplot(as.data.frame(table(fulldt$school))) +
  geom_bar(aes(x = Var1, y = Freq, fill = Var1), 
           stat = 'identity') +
  scale_fill_manual(values=c("#56B4E9", "#E69F00")) +
  ggtitle("R?partition de l'?cole") +
  xlab("Ecole") +
  ylab("Effectifs") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#Pour la base fulldt, il y a 772 ?l?ves scolarises ? Gabriel Pereira et 272 ? Mousinho da Silveira.

## b) Le sexe en fonction de l'?cole
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

#Il y a 591 filles et 453 gar?ons au sein des 2 ?coles.
#La proportion de fille est plus importante au sein de l'?cole Mousinho da Silveira.

## c) Age en fonction du sexe
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

#Il y a tr?s peu d'?l?ves au del? de 19 ans, les ?l?ves au del? de cette ?ge sont s?rement d? ? des redoublements au cours de leurs scolarit?s.

## d) Adresse

### Adresse en fonction de l'?cole

ggplot(fulldt) +
  aes(x = address, fill = school) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Adresse",
    y = "Effectif",
    title = "Repartition de l'adresse (U ou R) en fonction de l'?cole"
  ) +
  theme_minimal()

#Pour l'?cole MS, il y a une r?partition approximative des ?l?ves vivant dans une zone urbaine ou rurale alors que au contraire pour l'?cole GP, une grande partie vivent dans une zone urbaine.

### Seulement adresse
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

#Il y a 285 ?l?ves vivant dans une zone rurale et 759 dans une zone urbaine.


## e) Emploi du p?re
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

#Nous observons que la cat?gorie "les autres" se d?marque des 3 autres cat?gories (? la maison, sant? et professeurs)

## f) Emploi de la m?re

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

#Ici aussi, la cat?gorie "autres" se d?marque des autres, suivi de services. Contrairement aux m?res, les p?res travaillent beaucoup plus dans des m?tiers de cat?gories "autres" ou services, alors qu'il y a une part importante de m?re travaillant dans ?ducation, sant? ou des m?res aux foyers.

## h) Maitrice de corr?lation entre variables quantitatives

ggcorr(fulldt)

#Gr?ce ? cette matrice de corr?lation, nous pouvons apercevoir les variables quantitatives corr?l?es, par exemple, il y a une forte corr?lation entre les notes G1, G2 et G3. La consommation d'alcool en semaine et le weekend a une corr?lation positive d'environ 0.5. 
#Au contraire, il n'y a pas de corr?lation entre les variables age et le temps libres par exemples. 
#Il y a aussi une corr?lation n?gative entre la variables ?checs scolaires et les notes G1, G2 et G3, qui montre que l'?chec scolaires impactes n?gativement les notes. 

# III. Visualision des donn?es afin d'?tablir une eventuelle corr?lation entre la consommation d'alcool et les r?sultats scolaires

## Profil g?n?ral des consommateurs d'alcool, afin d'?tablir une premi?    re typologie general :

### 1. Visualisation des consommations d'alcool weekend et semaine

##### Consommation d'alcool en semaine
table(fulldt$Dalc)

ggplot(fulldt) +
  aes(x = Dalc) +
  geom_histogram(bins = 30L, fill = "#112446") +
  theme_minimal()

#En semaine, la consommation d'alcool est plut?t mod?rer (tr?s faible consommation).

##### Consommation d'alcool le weekend 
table(fulldt$Walc)

ggplot(fulldt) +
  aes(x = Walc) +
  geom_histogram(bins = 30L, fill = "#112446") +
  theme_minimal()

#Il y a beaucoup plus de consommation le weekend, ?tant donn? qu'il n'y a pas cours, plus de personnes consomment excessivement de l'alcool. 


##### Impact de l'alcool sur les notes G3
ggplot(fulldt) +
  aes(x = Dalc, y = G3, colour = Dalc) +
  geom_jitter(size = 1.5) +
  scale_color_viridis_c(option = "viridis", direction = 1) +
  labs(
    x = "Consommation alcool semaine",
    y = "Notes G3",
    title = "Les notes G3 en fonction de la consommation d'alcool"
  ) +
  theme_minimal()

#Grace au nuage de point, on remarque que les ?l?ves qui ont obtenue de mauvaises notes (0) sont g?n?ralement des ?l?ves consommant tr?s peu d'alcool, on ne retrouve pas d'?l?ves consommant beaucoup d'alcool dans les mauvaises notes. De plus, tr?s peu d'?l?ves consomment excessivement de l'alcool en semaine, et ces ?l?ves ont des notes obtenue entre 5 et 15. De plus, les consommateurs mod?r?s, ont g?n?ralement des notes compris entre 7.5 et 17.5, quelques personnes se d?marques en ayant obtenue de tr?s bonne note, ce sont souvent des ?l?ves consommant tr?s peu d'alcool. 

cor1 <- G3 ~ Walc + Dalc
lm1<-lm(cor1 , data = fulldt)
coefplot(lm1 , outerCI = 1.96 , intercept = FALSE)

#La consommation d'alcool en semaine a plus d'impact n?gatif sur les r?sultats scolaires G3 que la consommation le weekend. 
#Les deux coefficients est n?gative, mais l'intervalle de confiance de Walc couvre 0, donc le coefficient de Walc n'est pas significatif. 
#Cela peut ?tre d? au fait qu'en semaine, les personnes consommant de l'alcool ne se concentre pas aux r?visions donc leurs notes sont impact?s. 

##### Impact de l'?ge sur la consommation de l'alcool
cor2 <- age ~ Walc + Dalc
lm2<-lm(cor2 , data = fulldt)
coefplot(lm2 , outerCI = 1.96 , intercept = FALSE)

#Ici, nous cherchons ? comprendre si la consommations d'alcool en semaine ou le weekend d?pend de l'?ge.
#Les deux coefficient sont positifs, mais ?tant donn? que l'intervalle de confiance de Walc couvre 0, le coefficent n'est pas significative contrairement a Dalc. 
#La consommation d'alcool en semaine d?pend plus de l'?ge que la consommation le weekend, c'est ? dire que par exemple, en fonction de l'age, les personnes font plus attention ? leurs consommations d'alcool en semaine que le weekend.

##### Impact du sexe sur la consommation de l'alcool
ggplot(fulldt, aes(x = Walc, y = Dalc , color = sex))+   geom_jitter(position=position_jitter(0.2))

#Analyse de la consommation d'alcool durant le travail en semaine (Dalc) et la consommation d'alcool durant le week-end
#(Walc) selon le genre : Valeurs num?rique: 1 est ?quivaut a tr?s faible et 5 est ?quivaut ? tr?s ?lev?. 
#A travers le nuage de points ce que l'on peut voir selon la consommation d'alcool pendant le travail en semaine (Dalc) et en week-end (Walc) c'est qu'il y a peu de consommation excessive d'alcool que ce soit pour les hommes ou pour les femmes,
#les points ?tant principalement concentrer entre le nombre 1 dalc et 3 walc.
#Les femmes ? part quelques occurrences (certain point compris au-dela de 5), boivent moins que les hommes qui sont plus nombreux ? boire en semaine (une importante r?partition de points entre le 4 et le 5). 

#Pour la consommation d'alcool durant le week-end (Walc), il y a ?galement peu de consommation 
#(majorit? ?tant compris dans le 1) que ce soit pour les hommes ou pour les femmes . Il y a plus de consommation de la part
#des hommes durant le week end les points bleu ?tant compris entre le 4 et le 5. Il y a une augmentation de la consommation d'alcool pour les hommes le week-end (points bleu compris entre le 4 et le 5). 

#Selon notre nuages de points les hommes consomme plus d'alcool le week-end (la majorit? ?tant 
#comprises dans le 4) que la semaine (la majorit? ?tant comprises entre le 3 et 4). 

#Les femmes consommerai plus d'alcool durant le week-end (entre le 2 et le 3) qu'en semaine (majorit? en 1). 

##### Impact de l'adresse sur la consommation de l'alcool
table(fulldt$address, fulldt$Dalc)
table(fulldt$address, fulldt$Walc)
Dalc<-c(table(fulldt$Dalc))
Walc<-c(table(fulldt$Walc))
data.frame(Dalc, Walc)

ggplot(fulldt, aes(x = Walc, y = Dalc , color = address))+ geom_jitter(position=position_jitter(0.2))

ggplot(fulldt) +
  aes(x = Dalc, fill = address) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  labs(x = "Consommation d'alcool en semaine ", y = "Pourcentage d'?l?ve buvant en semaine", 
       title = "Consommation d'alcool en semaine chez les ?l?ves selon l'environnement ", fill = "Cadre de vie (R = rural, U = Urbain)") +
  scale_y_continuous(labels = percent) +
  theme_minimal()

#Le pourcentage d'?l?ves consommant de l'alcool en semaine est plus ?lev? pour les ?l?ves habitant dans une zone urbaine (environ 75% pour les cat?gories 1, 2, 4 et 5). 

ggplot(fulldt) +
  aes(x = Walc, fill = address) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  labs(x = "Consommation d'alcool le week-end ", y = "Pourcentage d'?l?ve buvant le week-end", 
       title = "Consommation d'alcool le week-end chez les ?l?ves selon l'environnement ", fill = "Cadre de vie (R = rural, U = Urbain)") +
  scale_y_continuous(labels = percent) +
  theme_minimal()

#Le pourcentage d'?l?ves consommant de l'alcool le weekend est presque identique que le pourcentage d'?l?ves consomment de l'alcool en semaine, ici aussi, les ?l?ves sont plus nombreux ? d?clarer habiter dans une zone urbaine. 

##### La consommation d'alcool le week-end selon l'origine sociale 

#Origine sociale (Medu / Fedu / Mjob / Fjob)

###### Mjob (geom_bar, geom_jitter)
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

#Etant donn? que les ?l?ves d?clarant boire de l'alcool est plus ?l?ves que les autres, nous avons meilleur graphique. Nous remarquons que les m?res travaillant dans "les autres", services ou encore des m?res ? la maison ont des enfants qui consomment beaucoup plus que les enfants ayant une m?re travaillant dans la sant? ou professeur. 

#Consommation d'alcool selon job mere
ggplot(fulldt) +
  aes(x = Dalc, y = Walc, colour = Mjob) +
  geom_jitter() +
  scale_color_viridis_d(option = "plasma", direction = 1) +
  labs(
    x = "Consommation alcool semaine",
    y = "Consommation alcool weekend",
    title = "Nuage de point consommation alcool en fonction du job de la mere"
  ) +
  theme_bw()

#Comme analyser pr?c?demment, si on observe dans ce nuage de point la partie consommant beaucoup d'alcool, on retrouve surtout des m?res travaillant dans les services, autres ou des m?res au foyer. De plus, nous observons qu'il y a un regroupement de points pour les ?chelles de consommation de le weekend et principalement en 1 pour la consommation en semaine.

###### Fjob (geom_bar)
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

#Nous observons que comme pour le graphique du m?tier de la m?re vu pr?c?demment, on a un graphique distincte pour les ?l?ves consommant tr?s peu d'alcool. Pour les p?res travaillant dans la cat?gories "autres", nous observons que leur enfants sont plus nombreux ? boire de l'alcool que les autres m?tiers, suivi de services. 


#la structure familiale (Pstatus / famsize / famrel / guardian / )
table(fulldt$Pstatus)

#923 ?l?ves ont des parents vivant toujours ensemble et 121 ont des parents s?par?s. 

ggplot(fulldt) +
  aes(x = Dalc, fill = Pstatus, colour = Pstatus) +
  geom_histogram(position = "fill") +
  scale_fill_hue(direction = 1) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Consommation en semaine",
    y = "Effectifs",
    title = "Pourcentage de la consommation d'alcool en semaine selon la cohabitation parentale"
  ) +
  scale_y_continuous(labels = percent) +
  theme_minimal()

#On ne peut pas conclure qu'avoir des parents s?par?s influent la consommation d'alcool en semaine, ?tant donn? qu'environ 85% ont des parents vivant ensemble. 

ggplot(fulldt) +
  aes(x = Walc, fill = Pstatus, colour = Pstatus) +
  geom_histogram(position = "fill") +
  scale_fill_hue(direction = 1) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Consommation le weekend",
    y = "Effectifs",
    title = "Pourcentage de la consommation d'alcool le weekend selon la cohabitation parentale"
  ) +
  scale_y_continuous(labels = percent) +
  theme_minimal()

#La conclusion est identique pour la consommation d'alcool le weekend, nous pouvons affirmer que le fait d'avoir des parents s?par?s n'affecte pas la consommation d'alcool en g?n?rale. 

ggplot(fulldt) +
  aes(x = Dalc, y = Walc, colour = famsize) +
  geom_jitter() +
  scale_color_viridis_d(option = "plasma", direction = 1) +
  labs(
    x = "Consommation alcool semaine",
    y = "Consommation alcool weekend",
    title = "Nuage de point de la consommation d'alcool en fonction de la taille de la famille "
  ) +
  theme_bw()

#Nous observons que pour la consommation d'alcool excessive, nous retrouvons majoritairement des points bleus, c'est ? dire des ?l?ves ayant une famille de taille sup?rieur ? 3. 

ggplot(fulldt) +
  aes(x = Dalc, fill = famsize, colour = famsize) +
  geom_histogram(position = "fill") +
  scale_fill_hue(direction = 1) +
  scale_color_hue(direction = 1) +
  labs(
    x = "Consommation en semaine",
    y = "Effectifs",
    title = "Pourcentage de la consommation d'alcool en semaine selon la taille de la famille"
  ) +
  scale_y_continuous(labels = percent) +
  theme_minimal()

#Grace ? cet histogramme, on observe que g?n?ralement plus de la moiti? des consommateurs sont issues de familles nombreuses.
#75% des personnes d?clarant boire tr?s peu ou beaucoup d'alcool sont issues de famille nombreuses. 

ggplot(fulldt) +
  aes(x = famrel, y = Dalc, colour = famrel) +
  geom_jitter(size = 1.5) +
  scale_color_gradient() +
  labs(
    x = "relation familiale",
    y = "Consommation d'alcool semaine",
    title = "Consommation d'alcool en fonction des relations familiales"
  ) +
  theme_minimal()

#Nous observons que les personnes consomment tr?s peu d'alcool en semaine ont une tr?s bonne relation familiale majoritairement. 
#G?n?ralement, les nuages de points pour les relations familiales mauvaises se regroupent pour la consommation d'alcool tr?s faible.
#Nous pouvons conclure que les relations familiale n'est peu ?tre pas forcement un facteur de la consommation d'alcool chez les ?l?ves, ?tant donn? qu'il n'y a que 30 ?l?ves d?clarant avoir des relations mauvaises, nous pouvons pas faire une conclusion.  

ggplot(fulldt) +
  aes(x = famrel, y = Walc, colour = famrel) +
  geom_jitter(size = 1.5) +
  scale_color_gradient() +
  labs(
    x = "relation familiale",
    y = "Consommation d'alcool le weekend",
    title = "Consommation d'alcool le weekend en fonction des relations familiales"
  ) +
  theme_minimal()

#Identique ? la pr?c?dente interpr?tation, ?tant donn? qu'il y a tr?s peu d'?l?ves d?clarant avoir une mauvaise relation familiale, le nuage de point n'est pas forcement appropri? pour savoir si les relations familiale est un facteur de la consommation d'alcool.

ggplot(fulldt) +
  aes(x = guardian, fill = guardian, weight = Dalc) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Tuteur",
    y = "Effectifs",
    title = "Consommation d'alcool en semaine selon le tuteur de l'?l?ve"
  ) +
  theme_minimal() +
  facet_wrap(vars(Dalc))

#G?n?ralement, les ?l?ves ayant comme tuteur leur m?re consomment plus d'alcool, suivi du tuteur p?re. Dans la cat?gorie autres tr?s peu consomment d'alcool. 

ggplot(fulldt) +
  aes(x = guardian, fill = guardian, weight = Walc) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Tuteur",
    y = "Effectifs",
    title = "Consommation d'alcool le weekend selon le tuteur de l'?l?ve"
  ) +
  theme_minimal() +
  facet_wrap(vars(Walc))

#Ce graphique est un peu pr?s identique que la consommation en semaine, sauf que les effectifs sont beaucoup plus important. 

#L'accompagnement (variables schoolsup / famsup / paid)

ggplot(fulldt) +
  aes(x = Dalc, fill = famsup) +
  geom_histogram(position = "fill", bins = 30) +
  scale_fill_hue(direction = 1) +
  labs (   
    x = "consomation d'alcool en semaine" ,
    y = "Nombres d'eleves" , 
    title = "Le soutien scolaire familial et la consomation d'alcool en semaine",
    fill = "Beneficie de soutien scolaire familial"
  ) +
  scale_y_continuous(labels = percent) +
  theme_minimal()

#Les ?l?ves consommant beaucoup d'alcool en semaine ont environ 80% de soutiens familiale, contre 65% pour des ?l?ves consommant tr?s peu d'alcool. Pour les personnes ayant une consommation ayant une consommation normale (3), environ 55% ne b?n?ficie pas de soutient familiale. 

ggplot(fulldt) +
  aes(x = Dalc, fill = paid) +
  geom_histogram(position = "fill", bins = 30L) +
  scale_fill_hue(direction = 1) +
  labs (   
    x = "consomation d'alcool en semaine",
    y = "Nombres d'eleves" , 
    title = "Les cours suppl?mentaires pay?s et la consomation d'alcool",
    fill = "Cours suppl?mentaire"
  ) +
  scale_y_continuous(labels = percent) +
  theme_minimal()

#Les ?l?ves b?n?ficiant du soutient scolaire consomme moins d'alcool que les ?l?ves ne b?n?ficiant pas de soutient scolaire, en effet, les ?l?ves d?clarant consomm?s tr?s peu d'alcool est de 20%, et de d'environ 35% pour les ?l?ves consommant beaucoup d'alcool. 

ggplot(fulldt) +
  aes(x = Dalc, fill = schoolsup ) +
  geom_histogram(position = "fill", bins = 30L) +
  scale_fill_hue(direction = 1) +
  labs (   
    x = "consomation d'alcool en semaine" ,
    y = "Nombres d'eleves" ,
    title = "Le soutien scolaire et la consomation d'alcool",
    fill = "Beneficie de soutien scolaire"
  ) +
  scale_y_continuous(labels = percent) +
  theme_minimal()

#Les personnes qui b?n?ficie d'un soutient scolaire ne boivent presque pas d'alcool en semaine, seulement 12,5% consomment tr?s peu et 15% consomment beaucoup d'alcool en semaine. 


### 2. Visualisation des facteurs impactant les notes G3

##### Impact de l'?cole sur les notes G3
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

#Ici on utilise le nuage de point pour montrer la diff?rence entre ?cole et l'influence de la consommation d'alcool sur les notes aux examens finaux (G3) car la fonction nuage de point nous permet ici de facilement montrer (ou non) la disparit? entre les deux ?coles et surtout de faire ressortir les tendances statistiques sur la consommation d'alcool des ?l?ves et peut ?tre d'?tablir une influence sur les r?sultats finaux aux examens des ?l?ves. 
#Le graphique se lit comme suit : les points en haut a droite sont ceux qui consomment le plus d'alcool a la fois en weekend et en semaine (ceux se rapprochant le plus du 5 sur l'abscisse et l'ordonn?e).
#A l'inverse, ceux ?tant le plus en bas a gauche (les plus proches de 1) sont ceux consommant le moins, voir pas d'alcool. En haut a gauche, se situent les personnes consommant exclusivement le weekend (1 a l'abscisse et 5 a l'ordonn?e), et ceux ?tant le plus en bas a droite sont ceux qui consomment de l'alcool exclusivement en semaine (5 a l'abscisse et 1 a l'ordonne).
#Le premier r?sultat que l'on remarque est que la tendance a la consommation d'alcool est beaucoup plus importante durant le weekend que durant la semaine malgr? quelques exceptions.
#On peut expliquer cette tendance par le fait que la consommation d'alcool a l'adolescence et pour les jeunes adultes soient surtout li?e a des moments sociabilit?s entre groupes de pair.
#Les ?tudiants ayant cours en semaine, la plupart des activit?s sociales sont donc organis?es en fin de semaine, en weekend et ces activit?s sont le moment propices a la consommation d'alcool en vue de sociabiliser (sorties en bars, boites, soir?e chez quelqu'un).
#Le deuxi?me r?sultat que l'on peut noter est que les ?l?ves de Gabirel Pereira (en rouge) sont plus nombreux a consommer de l'alcool que ceux de Mousinho da Silveira (en bleu).
#Les ?l?ves de GP sont aussi surtout beaucoup plus repr?senter dans des consommations intensives et notamment celles faites en semaines.
#Difficile de dire si la diff?rence est assez grande pour ?tre significative. Un d?but d'analyse pour expliquer cette diff?rence serait que Gabriel Pereira est une ?cole publique et donc que les ?l?ves aient une plus grande libert? et moins d'attente au niveau du corps enseignant.
#Le troisi?me r?sultat est que l'on remarque que les r?sultats les plus bas (0, 5, 10) ont tendance a augmenter avec la consommation d'alcool et surtout avec la consommation d'alcool en semaine. On ne peut pas affirmer que ce r?sultat soit significatif au vu de la pr?sence de tr?s nombreux non-buveurs ou buveurs occasionnelle.
#Cependant on peut imaginer une hypoth?se qui est que ceux qui consomment en semaine sont probablement victime d'addiction, d'alcoolisme ?tant donne que l'addiction a l'alcool se caract?rise par une consommation presque journali?re. On manque d'?l?ment pour approuver la validation d'une tel hypoth?se, mais tout comment l'addiction a la marijuana a des cons?quences sur les notes, l'addiction a l'alcool pourrait entra?ner des cons?quences sur les notes finales comme le montre le graphique.
#Cependant, tout comme l'addiction a la marijuana, il faut voir si c'est l'addiction a l'alcool en elle m?me qui cause la baisse des notes ou un environnement social particuli?rement pr?caire qui causerait cette baisse des notes et cette addiction a la fois. C'est pour cela que nous analyseront la consommation en fonction de l'origine sociale, de la structure familiale et de l'accompagnement des ?l?ves.

##### Impact du sexe sur les notes G3
table(fulldt$G3)
ggplot(fulldt) +
  aes(x = G3, y = sex) +
  geom_boxplot(fill = "#BDD3E8") +
  labs(x = "Notes G3", y = "Sexe", title = "Boites ? moustaches du sexe sur les notes G3") +
  theme_bw()

#Pour le sexe masculin, il y a 2 valeurs aberrantes et pour le sexe f?minin seulement 1.
#Les notes m?dians sont plus ?l?ves pour les filles avec environ 12 et 11 pour les gar?ons. 
#Par contre, le minimum est plus ?lev? chez les gar?on, le maximum aussi est plus ?lev? pour les gar?ons. 
#Cela s'explique que les notes des filles sont similaires et que globalement les filles ont des notes ?lev?es alors que chez les gar?ons, malgr? quelques bonnes notes, ils ont plut?t des notes moyennes. 

##### Impact du m?tier de la m?re sur les notes G3
ggplot(fulldt) +
  aes(x = G3, y = Mjob, fill = Mjob) +
  geom_boxplot() +
  scale_fill_brewer(palette = "OrRd", direction = 1) +
  labs(x = "Notes G3", y = "Job mere", title = "Boites ? moustaches du job de la mere sur les notes G3 ") +
  theme_bw()

#Pour les boites ? moustache du m?tier de la m?re, il y a 6 valeurs aberrantes pour la cat?gorie "autres", ce sont des valeurs qui sont sup?rieures ou inf?rieures aux limites d?finies par les moustaches. 
#On observe selon la boite ? moustache que les ?l?ves ayant une m?re qui est soit enseignante, soit dans la sante ou ? la maison ont leur notes minimums comprise entre 5 et 7. Ceux qui ont les notes les plus ?l?ves sont ceux dont leur m?res est enseignante (m?diane sup?rieure aux autres ainsi que le troisi?me quartile). Les ?l?ves qui ont leur m?res qui est "autre" (other) ont leur notes minimum qui est plus large (4,5 environ), surement li?es au fait que cette cat?gorie est tr?s large (beaucoup de qualificatif large ? lint?rieur). 
#Les ?l?ves ayant une m?re enseignante (teacher) ont de meilleures notes (ayant leur m?diane se rapprochant plus de 15 et leur 3?me quartile d?passant les 15).
#Ceux qui ont une m?re ? la maison  (at_home) en majorit? ont des notes comprise entre 10 et 14 (la m?diane ?tant environ ? 11).
#On peut conclure alors que la profession de la m?re ? une influence sur les r?sultats scolaires de l'?l?ve. 
#Cela peut s'expliquer notamment par le temps disponible li?es ? la profession mais aussi les aides qui peuvent ?tre apporter
#(par exemple le fait d'autre enseignant est plus susceptible d'aider).
#Le minimum de m?re-sante est de 7 et le maximum est de 20 contrairement aux m?res-maison ou m?res-service qui est de 5 et 18,5 respectivement. 
#Par contre, l'?cart interquartile est vraiment important pour les m?res-sante, ce qui signifie que les notes sont variables. 

##### Impact du m?tier du p?re sur les notes G3
ggplot(fulldt) +
  aes(x = G3, y = Fjob, fill = Fjob) +
  geom_boxplot() +
  scale_fill_brewer(palette = "OrRd", direction = 1) +
  labs(x = "Notes G3", y = "Job mere", title = "Boxplot") +
  theme_bw()
#Pour les boites ? moustache du m?tier du p?re, il y a 2 valeurs aberrantes pour la cat?gorie "services", ce sont des valeurs qui sont sup?rieures ou inf?rieures aux limites d?finies par les moustaches. 
#On observe selon la boite ? moustache que les ?l?ves ayant un p?re
#travaillant dans les cat?gories services, ? la maison ou autres ont des m?dians similaire ?gales ? environ 11. 
#Le maximum le plus ?lev? appartient ? un ?l?ve qui a un p?re travaillant dans les services.  
#La boites ? moustache de p?re-professeur est particuli?rement diff?rentes des autres, car le premier et troisi?me quartile sont sup?rieur aux autres, les notes sont g?n?ralement compris entre 11 et 16. Egalement les p?res-sant? ont un m?dian l?g?rement sup?rieur aux 3 autres cat?gories, le fait de faire des ?tudes peut donc impact?s les notes de leurs enfants. Les notes minimum de autres, ? la maison et professeurs sont identiques. 

##### Impact du tuteur sur les notes G3
ggplot(fulldt) +
  aes(x = G3, y = guardian, fill = guardian) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  labs(
    x = "Notes G3",
    y = "Tuteur",
    title = "Boites ? moustache de l'impact du tuteur de l'?l?ve sur les notes G3"
  ) +
  theme_minimal()

#Les ?l?ves ayant comme p?re et m?re tuteur sont identiques au niveau des r?partitions de notes, la note m?dian des ?l?ves ayant un p?re tuteur est de 12 et celui des m?res tuteur et autres sont de 1.
#Le minimum de tuteur autre est de 7 alors que pour m?re et p?re tuteur celui ci est ?gale ou inf?rieur ? 5. Par contre, le maximum est de 16 alors que celui du tuteur m?re est de 20 et tuteur p?re est de 18. La boite ? moustache de autre tuteur est plus petit, ce qui signifie que les notes se concentre g?n?ralement autour de 9 et 12 alors que les 2 autres sont autour de 10 et 14. 

table(fulldt$famrel, fulldt$Dalc)

ggplot(fulldt)+
  aes(x=famrel, y=Walc, colour=famrel)+
  scale_fill_continuous()+
  geom_jitter()+
  labs(x="Relation familiale", y = "Consommation d alcool en semaine", title="Lien entre consommation d alcool en semaine et relation familiale")+
  theme_bw()

#Concernant les deux tableaux sur le lien entre relation familiale et consommation d'alcool (un pour la semaine et un pour le week-end).
#On remarque une tendance g?n?rale sur les deux tableaux, ?tant que les r?ponses sont distribu?es de fa?on assez uniforme a travers le nuage de points. Si les relations familiales impactaient la consommation d'alcool, on aurait du voir une concentration de point en haut ? gauche et en bas a droite des tableaux. Au lieu de ca, les points sont distribues de fa?on similaire entre ceux ayant les pires relations et ceux ayant les meilleures relations familiales. La seule diff?rence est que l'on remarque que les gens ont tendance a plus consommer en week-end qu'en semaine comme le confirme un de nos tableaux pr?c?dents.
