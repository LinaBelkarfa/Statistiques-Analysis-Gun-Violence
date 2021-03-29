setwd("D:/Cours/M1 MIAGE/Maths pour big data/groupe-BZEZ-20/Projet_Gun")
library(xlsx2dfs)

########################### QUESTION 3 #####################################################################################

# Si l'on prend en compte aussi le mois de l'année dans lequel le fait divers a été commis, est-ce qu'il y a 
#une forte corrélation entre le nombre de faits divers et le mois de l'année ? Quelles conclusions en tirez-vous ?

################ Chargement des données ###################################################################################
DATA_Loc_Date<-read.xlsx("DataGun_Date_Loc.xlsx")


################## Définition de mois et années comme facteurs ###########################################################

DATA_Loc_Date$Mois<-factor(DATA_Loc_Date$Mois,levels = c("janvier","février","mars","avril","mai","juin","juillet","août","septembre","octobre","novembre","décembre"))
DATA_Loc_Date$Annees<-factor(DATA_Loc_Date$Annees,levels = c("2013","2014","2015","2016","2017","2018"))

########################## Observation du cumul des crimes ###############################################################

barplot(table(DATA_Loc_Date$Mois),main="Cumul des crimes avec arme aux Etats Unis par mois de 2013 à Mars 2018",
        xlab = "Mois",
        ylab = "Nombre de crimes",
        cex.main=2,
        cex.lab=1.7)


######################### Observation par années #########################################################################

Indice2013<-c(as.numeric(which(DATA_Loc_Date$Annees==2013)))
mois<-DATA_Loc_Date$Mois
barplot(table(mois[Indice2013]),main="Nombre de crimes avec armes à feu aux Etats Unis par mois en 2013",
xlab = "Mois",
ylab = "Nombre de crimes",
las=2,
beside = F,
cex.main=2,
cex.lab=2)


Indice2014<-c(as.numeric(which(DATA_Loc_Date$Annees==2014)))
mois<-DATA_Loc_Date$Mois
barplot(table(mois[Indice2014]),main="Nombre de crimes avec armes à feu aux Etats Unis par mois en 2014",
        xlab = "Mois",
        ylab = "Nombre de crimes",
        las=2,
        beside = F,
        cex.main=2,
        cex.lab=2)


Indice2015<-c(as.numeric(which(DATA_Loc_Date$Annees==2015)))
mois<-DATA_Loc_Date$Mois
barplot(table(mois[Indice2015]),main="Nombre de crimes avec armes à feu aux Etats Unis par mois en 2015",
        xlab = "Mois",
        ylab = "Nombre de crimes",
        las=2,
        beside = F,
        cex.main=2,
        cex.lab=2)

Indice2016<-c(as.numeric(which(DATA_Loc_Date$Annees==2016)))
mois<-DATA_Loc_Date$Mois
barplot(table(mois[Indice2016]),main="Nombre de crimes avec armes à feu aux Etats Unis par mois en 2016",
        xlab = "Mois",
        ylab = "Nombre de crimes",
        las=2,
        beside = F,
        cex.main=2,
        cex.lab=2)

Indice2017<-c(as.numeric(which(DATA_Loc_Date$Annees==2017)))
mois<-DATA_Loc_Date$Mois
barplot(table(mois[Indice2017]),main="Nombre de crimes avec armes à feu aux Etats Unis par mois en 2017",
        xlab = "Mois",
        ylab = "Nombre de crimes",
        las=2,
        beside = F,
        cex.main=2,
        cex.lab=2)


Indice2016<-c(as.numeric(which(DATA_Loc_Date$Annees==2018)))
mois<-DATA_Loc_Date$Mois
barplot(table(mois[Indice2018]),main="Nombre de crimes avec armes à feu aux Etats Unis par mois en 2018",
        xlab = "Mois",
        ylab = "Nombre de crimes",
        las=2,
        beside = F,
        cex.main=2,
        cex.lab=2)



################################ Lecture des données (créer à partir du fichier VisualisationDesDonnées.R partie Question 3) ####################

DataByMounth<-read.xlsx("NombreCrime_ParMois_EtAnnees.xlsx")
Annees<-c("2014","2015","2016","2017","2018")
DataByMounth<-cbind(Annees,DataByMounth)
DataByMounth<-DataByMounth[,-1]

########################## Ajout des sommes (nb de crimes) par années ###################################################
Somme<-c(sum(DataByMounth[1,]),sum(DataByMounth[2,]),sum(DataByMounth[3,]),sum(DataByMounth[4,]),sum(DataByMounth[5,]))
DataByMounth<-cbind(DataByMounth,Somme)


########################## Observation des corrélations ##################################################################
MatCor <- cor(DataByMounth[-5,-1], method = "pearson") #On retire la 5ème ligne car 2018 n'a que 3 mois
View(MatCor)                                            #et on retire la première colonne qui définie quelle est l'année
library(corrplot)
corrplot(MatCor, type="upper", order="hclust", tl.col="black", tl.srt=45,method="shade",outline = FALSE,addCoef.col = "black")

library(Hmisc)
Pvalue<-rcorr(as.matrix(MatCor))
corrplot(MatCor, type="upper", order="hclust", tl.col="black",method="shade",outline = FALSE,addCoef.col = "black",p.mat = Pvalue$P, sig.level =0.01)




