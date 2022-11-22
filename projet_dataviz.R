library(readr)
library(ggplot2)
library(labelled)
library(dplyr)
install.packages("RColorBrewer")
library(RColorBrewer)

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
#Avec str, on visualiser les premieres donnees des variables, et on regarde si ce sont des variables qualitative ou quantitatives

#Je pense qu'il est plus interessant de creer un bdd avec uniquement les variables qu'on avait choisit 
data<- fulldt[, c(1:4, 9:10, 22, 24:25, 29:30)]
data_notes<-fulldt[, c(31:33)]

#### I. Visualisation des donnees####
summary(maths)
summary(Portuguese)
#FAIRE PRESENTATION GENERALE DES VARIABLES AVEC GRAPH

# a) Ecole
aggregate (x = maths$school,
           by = list(maths$school),
           FUN = length
)
#Pour fulldt, il y a 772 eleves scolarises a Gabriel Pereira et 272  a Mousinho da Silveira.
barplot(table(fulldt$school), beside=TRUE,legend.text = TRUE,col=brewer.pal(n = 2, name = "Set1"),ylim=c(0,800),main="Repartition de l'ecole",xlab="ecole",ylab="Effectifs")


#b) sexe
maths_sex_n <- aggregate (x = maths$sex,
                          by = list(maths$sex),
                          FUN = length
)
barplot(table(fulldt$sex), beside=TRUE,legend.text = TRUE,col=brewer.pal(n = 2, name = "Set1"),ylim=c(0,600),main="Repartition dU sexe pour les 2 écoles",xlab="Sexe",ylab="Effectifs")
#Il y a 591 filles et 453 garcons au sein des 2 écoles
#Graphique sexe a GP
table(fulldt$sex, fulldt$school)
barplot(table(fulldt$sex, fulldt$school), beside=TRUE,legend.text = TRUE,col=brewer.pal(n = 4, name = "Set1"),ylim=c(0,600),main="Repartition dU sexe pour les 2 écoles",xlab="Sexe",ylab="Effectifs")

ggplot(fulldt, aes(x = sex, y = school))+
  geom_col(aes(fill = school), width = 0.7)
#Demander comment faire  geom_text(aes(y = school, label = sex, group =sex), color = "white")

table(maths$sex)
ggplot(data=maths, aes(x=sex)) + geom_bar()


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


#Partie sumeyye
#ADAPTER LES COULEURS
#Table Adresse
table(fulldt$address)
table(maths$address)
table(Portuguese$address)


as.data.frame(table(fulldt$address))
ggplot(as.data.frame(table(fulldt$address))) +
  geom_bar(aes(x = Var1, y = Freq), 
           stat = 'identity', fill = 'darkgreen') +
  ggtitle("Basic Bar Plot") +
  xlab("Adresse") +
  ylab("Effectifs") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#Emploi père
#Table Adresse


as.data.frame(table(fulldt$Fjob))
ggplot(as.data.frame(table(fulldt$Fjob))) +
  geom_bar(aes(x = Var1, y = Freq), 
           stat = 'identity', fill = 'darkgreen') +
  ggtitle("Basic Bar Plot") +
  xlab("Job du pere") +
  ylab("Effectifs") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
        axis.text.y = element_text(face = 'bold', size = 10))

#Emploi de la mere
#Table Adresse


as.data.frame(table(fulldt$Mjob))
ggplot(as.data.frame(table(fulldt$Mjob))) +
  geom_bar(aes(x = Var1, y = Freq), 
           stat = 'identity', fill = 'darkgreen') +
  ggtitle("Basic Bar Plot") +
  xlab("Job de la mere") +
  ylab("Effectifs") +
  theme_bw() +
  theme(axis.text.x = element_text(face = 'bold', size = 10),
