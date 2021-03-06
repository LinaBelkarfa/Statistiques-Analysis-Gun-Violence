setwd("D:/Cours/M1 MIAGE/Maths pour big data/groupe-BZEZ-20/Projet_Gun")
library(xlsx2dfs)

########################### QUESTION 2 #####################################################################################
#Prenez les donn�es de la p�riode entre le 1er janvier 2013 et le 31 d�cembre 2017 pour calculer les quantit�s statistiques
#(moyenne, �cart type, etc) et consid�rez les donn�es entre le 1 janvier 2018 et le 31 mars 2018 comme un �chantillon. 
#Est-ce que l'on peut dire que les valeurs moyennes des variables dans V ont significativement chang� par rapport au pass� ?

######## ETAPE 1 : ajouter les dates � nos donn�es ######################################################################

#Donn�es pas encod� (utile pour repr�sentation spatiale apr�s pour Steven car j'ajoute aussi la localisation lat et lng)
DATA<-read.xlsx("Gun_Without_Na.xlsx") #Les donn�es avec state pas encod� (sans NA)

#Donn�es encod�es 
EncodingData<-read.xlsx("One_Hot_DataGun.xlsx")# Les donn�es avec state encod� (sans NA)


#La base de donn�es de d�part qui contient les dates (j'en profite pour ajouter les lat et long pour plus tard)
DataDepart<-read.xlsx('Gun.xlsx')

#Les indices des crimes contenu dans mes bases de donn�es (ceux que j'ai gard� sans NA)
IndicesDATA<-DATA[,1]

#Les donn�es que je veux ajouter � ma base de donn�es gr�ce aux indices de crimes 
DateLngLat<-DataDepart[,c(1,2,15,17)]

# Je cr� une matrice compos� des lignes dont les indices correspond � la matrice que j'ai contruite auparavant
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

#Visualisation rapide de l'�volution du nombre de crimes aux �tats unis dans le temps 
#attention : nous avions retirer les NA donc il manque une partie des observation (nous en avons actuellement 214 814)
png(file="Evolution_Crime_Temps.png", width=1000, height=1000)
barplot(table(DATA_Loc_Date$date),main="Nombre de crimes avec arme aux Etats Unis de 2013 � 2018",
        xlab = "Dates",
        ylab = "Nombre de crimes",
        cex.main=2,
        cex.lab=1.7)
dev.off()

#cr�ation de la colonne mois : 
Mois<-months(DATA_Loc_Date$date)
Annees<-year(DATA_Loc_Date$date)

DATA_Loc_Date<- cbind(Mois,DATA_Loc_Date)
DATA_Loc_Date<-cbind(Annees,DATA_Loc_Date)
View(DATA_Loc_Date)

#On a d�sormais les mois et ann�es, on peut �craser l'ancien fichier et le remplacer par celui ci
write.xlsx(DATA_Loc_Date,"DataGun_Date_Loc.xlsx")

#Observation rapide du nombre d'observations par mois
png(file="Evolution_Crime_ParAnnee.png", width=1000, height=1000)
barplot(table(DATA_Loc_Date$Annees),main="Nombre de crimes avec arme aux Etats Unis par ann�es",
        xlab = "Ann�es",
        ylab = "Nombre de crimes",
        cex.main=2,
        cex.lab=1.7)
dev.off()

###################### ETAPE 3 : REPONSE A LA QUESTION ##########################################################

#Prenez les donn�es de la p�riode :
#   - entre le 1er janvier 2013 et le 31 d�cembre 2017 pour calculer les quantit�s statistiques (moyenne, �cart type, etc)
#   - consid�rez les donn�es entre le 1 janvier 2018 et le 31 mars 2018 comme un �chantillon.

Statistiques2013_2017<- DATA_Loc_Date[DATA_Loc_Date$Annees %in% c(2013,2014,2015,2016,2017),]
Echantillon2018<-DATA_Loc_Date[DATA_Loc_Date$Annees %in% c(2018),]
View(Echantillon2018)

Moyennes<-colMeans(as.matrix(Statistiques2013_2017[,-c(1,2,3,4,5,6)]))
Moyennes
EcartType<-apply(as.matrix(Statistiques2013_2017[,-c(1,2,3,4,5,6)]),2,sd)
EcartType

MatriceStatistiques<-rbind(Moyennes,EcartType)
View(MatriceStatistiques)

#Est-ce que l'on peut dire que les valeurs moyennes des variables dans V ont significativement chang� par rapport au pass� ?

Moyennes2018<-colMeans(as.matrix(Echantillon2018[,-c(1,2,3,4,5,6)]))
Moyennes2018
EcartType2018<-apply(as.matrix(Echantillon2018[,-c(1,2,3,4,5,6)]),2,sd)
EcartType2018

MatriceStatistiques2018<-rbind(Moyennes2018,EcartType2018)
View(MatriceStatistiques2018)

#Nous allons effectuer des test de student pour �tablir si les moyennes statistiques ont chang�s de mani�re significative en 2018

################## TEST SUR LES MOYENNES ############################################################################
#Dans un premier temps, nous allons traiter uniquement les moyennes que nous avons calcul� pour chaque variables :

# Moyennes de 2013 � 2017
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

#Dans le r�sultat ci-dessus : t est la statistique de Student (t= -0.3366), 
#df est le degr� de libert� (df= 56), p-value est le degr� de significativit� du test (p-value=0.7377). 
#L'intervalle de confiance de la diff�rence des moyennes � 95% est �galement montr�e
#et enfin, on a la valeur moyenne de la diff�rence des deux s�ries de moyennes (-0.0004045515).

#La p-value du test est de 0.7377
#Ce qui est largement sup�rieur � 0.05. 
#On conclut que les valeurs des moyennes des variables de 2013 � 2017 ne sont pas significativement diff�rentes de celles de 2018

################## TEST SUR LES ECART TYPES ############################################################################

# Ecart type de 2013 � 2017
z<-EcartType
# Ecart type  de 2018
w<-EcartType2018

#res le test statistiques de student 
res<-t.test(z, w, paired=TRUE) 
res

#La p-value du test est de 0.9259
#Ce qui est largement sup�rieur � 0.05. 
#On conclut que les valeurs des �carts types des variables de 2013 � 2017 ne sont pas significativement diff�rentes de celles de 2018

############## TEST par colonnes ################################################################################

x<-Statistiques2013_2017[,-c(1,2,3,4,5,6)]
y<-Echantillon2018[,-c(1,2,3,4,5,6)]

TestStudent<-function(x,y){
        MatricePvalue<<-c()
        pvalue=0
        for (i in 1:ncol(x)){
            
                Test<-t.test(x[,i], y[,i], paired=FALSE) #paired=FALSE car ici nous avons des �chantillons de taille diff�rentes
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




