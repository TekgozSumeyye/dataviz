library(readr)
library(ggplot2)
library(labelled)

maths <- read_csv("Maths.csv")
Portuguese <- read_csv("Portuguese.csv")

    ####Introduction####

#Ces données portent sur les résultats des élèves dans l'enseignement 
#secondaire de deux écoles portugaises. Les attributs des données 
#comprennent les notes des élèves, les caractéristiques démographiques, 
#sociales et scolaires, et ont été collectés à l'aide de rapports et de 
#questionnaires scolaires. Deux ensembles de données sont fournis concernant
#les performances dans deux matières distinctes : Les mathématiques (mat) et
#la langue portugaise (por). 

N1 <- nrow(maths)
N2 <- nrow(Portuguese)
#Pour la base de donnee maths, il y a 395 observations et 33 variables.
#Pour la base de donnee portuguese, il y a 649 observations et 33 variables identiques avec la 1ere bdd. 

fulldt<-bind_rows(maths, Portuguese)
nrow(fulldt)
str(fulldt)
#

    #### I. Visualisation des donnees####

#FAIRE PRESENTATION GENERALE DES VARIABLES AVEC GRAPH

    # a) Ecole
aggregate (x = maths$school,
           by = list(maths$school),
           FUN = length
)

    #b) sexe
maths_sex_n <- aggregate (x = maths$sex,
           by = list(maths$sex),
           FUN = length
)


summary(maths)
summary(Portuguese)
#Faire description des variables 

table(maths$sex)

#Savoir nombre de résultat par variable, ex sexe =2 (h et f)
#...

ggplot(data=maths, aes(x=sex)) + geom_bar()


table(maths$school)
table(Portuguese$school)

#PARTIE VISUALISATION
#barre, barplot, plot

table(fulldt$sex, fulldt$age)
ggplot(fulldt, aes(fulldt$age)) + 
  geom_bar()

ggplot(fulldt, aes(fulldt$sex),) + 
  geom_bar()



# Analyse globale de la variable sex 

summary(studentmat)
table(fulldt$sex)
summary(fulldt$sex)
hist(table(fulldt$sex))

hist(table(fulldt$sex))
hist(fulldt$sex)


# Graphique de la variable sex

ggplot(fulldt, aes(fulldt$sex))+geom_bar()


#Analyse globale de la variable age :

summary(fulldt$age)
#Graphique de la variable age :

ggplot(fulldt,aes(fulldt$age))+geom_bar()

#Minimum de la variable age :
min(fulldt$age)

#Maximum de la variable age :
max(fulldt$age)
range(fulldt$age)

#Moyenne de l'age

mean(fulldt$age)

#Mediane de l age

median(fulldt$age)

#Variance de la variable age

var(fulldt$age)

#Ecart type de la variable age

sd(fulldt$age)

#Quartille de la variable age

quantile(fulldt$age, probs = 0,25)
quantile(fulldt$age, probs = 0,75)

# Representation en graphique de la variable age

dotchart(table(fulldt$age))
dotchart (sort(table(fulldt$age)))

# Analyse globale de la variable school

summary(fulldt$school)
hist(table(fulldt$school))

hist(table(fulldt$school))
hist(fulldt$school)

#Graphique de la variable school :

ggplot(fulldt,aes(fulldt$school))+geom_bar()

#Moyenne de school:

mean(fulldt$school)

#Mediane de school :

median(fulldt$school)

# Representation en graphique de la variable school :

dotchart(table(fulldt$school))
dotchart (sort(table(fulldt$school)))

