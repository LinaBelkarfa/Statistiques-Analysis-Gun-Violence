setwd("D:/Cours/M1 MIAGE/Maths pour big data/groupe-BZEZ-20/Projet_Gun")
library(xlsx2dfs)

########################### QUESTION 2 #####################################################################################
#Prenez les données de la période entre le 1er janvier 2013 et le 31 décembre 2017 pour calculer les quantités statistiques
#(moyenne, écart type, etc) et considérez les données entre le 1 janvier 2018 et le 31 mars 2018 comme un échantillon. 
#Est-ce que l'on peut dire que les valeurs moyennes des variables dans V ont significativement changé par rapport au passé ?

######## ETAPE 1 : ajouter les dates à nos données ######################################################################

#Données pas encodé (utile pour représentation spatiale après pour Steven car j'ajoute aussi la localisation lat et lng)
DATA<-read.xlsx("Gun_Without_Na.xlsx") #Les données avec state pas encodé (sans NA)

#Données encodées 
EncodingData<-read.xlsx("One_Hot_DataGun.xlsx")# Les données avec state encodé (sans NA)


#La base de données de départ qui contient les dates (j'en profite pour ajouter les lat et long pour plus tard)
DataDepart<-read.xlsx('Gun.xlsx')

#Les indices des crimes contenu dans mes bases de données (ceux que j'ai gardé sans NA)
IndicesDATA<-DATA[,1]

#Les données que je veux ajouter à ma base de données grâce aux indices de crimes 
DateLngLat<-DataDepart[,c(1,2,15,17)]

# Je cré une matrice composé des lignes dont les indices correspond à la matrice que j'ai contruite auparavant
Matrice <- DateLngLat[DateLngLat[,1] %in% IndicesDATA,]
View(Matrice)

View(DATA)

DATA_Loc_Date<-cbind(Matrice[,-1],EncodingData)
View(DATA_Loc_Date)

write.xlsx(DATA_Loc_Date,"DataGun_Date_Loc.xlsx")


######## ETAPE 2 : formaliser les dates ######################################################################
library(lubridate)
DATA_Loc_Date<-read.xlsx("DataGun_Date_Loc.xlsx")

DATA_Loc_Date$date=as.Date(DATA_Loc_Date$date, format="%Y-%m-%d")

#Visualisation rapide de l'évolution du nombre de crimes aux états unis dans le temps 
#attention : nous avions retirer les NA donc il manque une partie des observation (nous en avons actuellement 214 814)
png(file="Evolution_Crime_Temps.png", width=1000, height=1000)
barplot(table(DATA_Loc_Date$date),main="Nombre de crimes avec arme aux Etats Unis de 2013 à 2018",
        xlab = "Dates",
        ylab = "Nombre de crimes",
        cex.main=2,
        cex.lab=1.7)
dev.off()

#création de la colonne mois : 
Mois<-months(DATA_Loc_Date$date)
Annees<-year(DATA_Loc_Date$date)

DATA_Loc_Date<- cbind(Mois,DATA_Loc_Date)
DATA_Loc_Date<-cbind(Annees,DATA_Loc_Date)
View(DATA_Loc_Date)

#On a désormais les mois et années, on peut écraser l'ancien fichier et le remplacer par celui ci
write.xlsx(DATA_Loc_Date,"DataGun_Date_Loc.xlsx")

#Observation rapide du nombre d'observations par mois
png(file="Evolution_Crime_ParAnnee.png", width=1000, height=1000)
barplot(table(DATA_Loc_Date$Annees),main="Nombre de crimes avec arme aux Etats Unis par années",
        xlab = "Années",
        ylab = "Nombre de crimes",
        cex.main=2,
        cex.lab=1.7)
dev.off()

###################### ETAPE 3 : REPONSE A LA QUESTION ##########################################################

#Prenez les données de la période :
#   - entre le 1er janvier 2013 et le 31 décembre 2017 pour calculer les quantités statistiques (moyenne, écart type, etc)
#   - considérez les données entre le 1 janvier 2018 et le 31 mars 2018 comme un échantillon.

Statistiques2013_2017<- DATA_Loc_Date[DATA_Loc_Date$Annees %in% c(2013,2014,2015,2016,2017),]
Echantillon2018<-DATA_Loc_Date[DATA_Loc_Date$Annees %in% c(2018),]
View(Echantillon2018)

Moyennes<-colMeans(as.matrix(Statistiques2013_2017[,-c(1,2,3,4,5,6)]))
Moyennes
EcartType<-apply(as.matrix(Statistiques2013_2017[,-c(1,2,3,4,5,6)]),2,sd)
EcartType

MatriceStatistiques<-rbind(Moyennes,EcartType)
View(MatriceStatistiques)

#Est-ce que l'on peut dire que les valeurs moyennes des variables dans V ont significativement changé par rapport au passé ?

Moyennes2018<-colMeans(as.matrix(Echantillon2018[,-c(1,2,3,4,5,6)]))
Moyennes2018
EcartType2018<-apply(as.matrix(Echantillon2018[,-c(1,2,3,4,5,6)]),2,sd)
EcartType2018

MatriceStatistiques2018<-rbind(Moyennes2018,EcartType2018)
View(MatriceStatistiques2018)

#Nous allons effectuer des test de student pour établir si les moyennes statistiques ont changés de manière significative en 2018

################## TEST SUR LES MOYENNES ############################################################################
#Dans un premier temps, nous allons traiter uniquement les moyennes que nous avons calculé pour chaque variables :

# Moyennes de 2013 à 2017
x<-Moyennes
# Moyennes de 2018
y<-Moyennes2018

#res le test statistiques de student 
res<-t.test(x, y, paired=TRUE)  
res

#Paired t-test

#data:  x and y
#t = -0.3366, df = 56, p-value = 0.7377
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#        -0.002812166  0.002003063
#sample estimates:
#        mean of the differences 
#-0.0004045515 

#Dans le résultat ci-dessus : t est la statistique de Student (t= -0.3366), 
#df est le degré de liberté (df= 56), p-value est le degré de significativité du test (p-value=0.7377). 
#L'intervalle de confiance de la différence des moyennes à 95% est également montrée
#et enfin, on a la valeur moyenne de la différence des deux séries de moyennes (-0.0004045515).

#La p-value du test est de 0.7377
#Ce qui est largement supérieur à 0.05. 
#On conclut que les valeurs des moyennes des variables de 2013 à 2017 ne sont pas significativement différentes de celles de 2018

################## TEST SUR LES ECART TYPES ############################################################################

# Ecart type de 2013 à 2017
z<-EcartType
# Ecart type  de 2018
w<-EcartType2018

#res le test statistiques de student 
res<-t.test(z, w, paired=TRUE) 
res

#La p-value du test est de 0.9259
#Ce qui est largement supérieur à 0.05. 
#On conclut que les valeurs des écarts types des variables de 2013 à 2017 ne sont pas significativement différentes de celles de 2018

############## TEST par colonnes ################################################################################

x<-Statistiques2013_2017[,-c(1,2,3,4,5,6)]
y<-Echantillon2018[,-c(1,2,3,4,5,6)]

TestStudent<-function(x,y){
        MatricePvalue<<-c()
        pvalue=0
        for (i in 1:ncol(x)){
            
                Test<-t.test(x[,i], y[,i], paired=FALSE) #paired=FALSE car ici nous avons des échantillons de taille différentes
                pvalue=Test$p.value
                MatricePvalue<<-append(MatricePvalue,as.numeric(pvalue))
        }
        variable<-colnames(x)
        MatricePvalue<<-cbind(variable,MatricePvalue)
}
TestStudent(x,y)
as.numeric(MatricePvalue[,2])

Indice<-which(as.numeric(MatricePvalue[,2])<0.05)
MatricePvalue[Indice,]




