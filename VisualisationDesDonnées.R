

############################# FICHIER DE VISUALISATION RAPIDE DES GRAPHIQUE ET DONNEES #################################################

setwd("D:/Cours/M1 MIAGE/Maths pour big data/groupe-BZEZ-20/Projet_Gun")


################################## ETAPE 1 : chargement et observations des données de départ ################################################################################

DATA<-read.csv('Gun.csv',sep=',',header = TRUE, na.strings = c(" ","","NA"))
View(DATA)
ncol(DATA)
nrow(DATA)
summary(DATA)

################################## ETAPE 2 : chargement et observations des données contenant que les colonnes voulues ################################################################################

library(xlsx2dfs)
DATA<-read.xlsx('Gun_Good_Columns.xlsx')
View(DATA)

######################## QUESTION 1 - CORRELATION LINEAIRE ENTRE TOUTES LES PAIRES DE VARIABLES VUE GLOBALE ###########################################


setwd("D:/Cours/M1 MIAGE/Maths pour big data/groupe-BZEZ-20/Projet_Gun")
library(xlsx2dfs)

EncodingData<-read.xlsx("One_Hot_DataGun.xlsx")# Les données avec state encodé (sans NA)
MatCor <- cor(EncodingData[,-1], method = "pearson")

#Création d'un png qui contient la matrice de toutes les corrélations
library(corrplot)
png(file="Corrélation_Linéaire.png", width=4000, height=4000)
corrplot(MatCor, type="upper", order="hclust", tl.col="black", tl.srt=45,method="shade",outline = FALSE,addCoef.col = "black")
dev.off()

#Création d'un png qui contient la matrice de toutes les corrélations avec les non significatifs barrés
library(Hmisc)
Pvalue<-rcorr(as.matrix(MatCor))

png(file="Corrélation_Significativite_0,01.png", width=4000, height=4000)
corrplot(MatCor, type="upper", order="hclust", tl.col="black",method="shade",outline = FALSE,addCoef.col = "black",p.mat = Pvalue$P, sig.level =0.01)
dev.off()


#Affichage de la matrice de correlation et de la matrice des pvalues des variables corréllées dont les
#corrélation sont significative au niveau p<0.01
MatCor[c(53,54,55,56),c(53,54,55,56)]
Pvalue$P[c(53,54,55,56),c(53,54,55,56)]


######################## QUESTION 2 - EVOLUTION SIGNIFICATIVE  ###########################################


setwd("D:/Cours/M1 MIAGE/Maths pour big data/groupe-BZEZ-20/Projet_Gun")
library(xlsx2dfs)

DataWithDate<-read.xlsx("DataGun_Date_Loc.xlsx")
View(DataWithDate)


################## QUESTION 3 ###########################################################################
library(xlsx2dfs)
DATA_Loc_Date<-read.xlsx("DataGun_Date_Loc.xlsx")

library(tidyverse)
annees = c(2014,2015,2016,2017,2018)
mois<-DATA_Loc_Date$Mois


Indice2014<-c(as.numeric(which(DATA_Loc_Date$Annees==2014)))
Indice2015<-c(as.numeric(which(DATA_Loc_Date$Annees==2015)))
Indice2016<-c(as.numeric(which(DATA_Loc_Date$Annees==2016)))
Indice2017<-c(as.numeric(which(DATA_Loc_Date$Annees==2017)))
Indice2018<-c(as.numeric(which(DATA_Loc_Date$Annees==2018)))



janvier<- c(sum(str_count(DATA_Loc_Date$Mois[Indice2014],"janvier")),
            sum(str_count(DATA_Loc_Date$Mois[Indice2015],"janvier")),
            sum(str_count(DATA_Loc_Date$Mois[Indice2016],"janvier")),
            sum(str_count(DATA_Loc_Date$Mois[Indice2017],"janvier")),
            sum(str_count(DATA_Loc_Date$Mois[Indice2018],"janvier"))
            )

fevrier<-c(sum(str_count(DATA_Loc_Date$Mois[Indice2014],"février")),
           sum(str_count(DATA_Loc_Date$Mois[Indice2015],"février")),
           sum(str_count(DATA_Loc_Date$Mois[Indice2016],"février")),
           sum(str_count(DATA_Loc_Date$Mois[Indice2017],"février")),
           sum(str_count(DATA_Loc_Date$Mois[Indice2018],"février"))
)


mars<-c(sum(str_count(DATA_Loc_Date$Mois[Indice2014],"mars")),
        sum(str_count(DATA_Loc_Date$Mois[Indice2015],"mars")),
        sum(str_count(DATA_Loc_Date$Mois[Indice2016],"mars")),
        sum(str_count(DATA_Loc_Date$Mois[Indice2017],"mars")),
        sum(str_count(DATA_Loc_Date$Mois[Indice2018],"mars"))
)

avril<-c(sum(str_count(DATA_Loc_Date$Mois[Indice2014],"avril")),
         sum(str_count(DATA_Loc_Date$Mois[Indice2015],"avril")),
         sum(str_count(DATA_Loc_Date$Mois[Indice2016],"avril")),
         sum(str_count(DATA_Loc_Date$Mois[Indice2017],"avril")),
         sum(str_count(DATA_Loc_Date$Mois[Indice2018],"avril"))
)

mai<-c(sum(str_count(DATA_Loc_Date$Mois[Indice2014],"mai")),
       sum(str_count(DATA_Loc_Date$Mois[Indice2015],"mai")),
       sum(str_count(DATA_Loc_Date$Mois[Indice2016],"mai")),
       sum(str_count(DATA_Loc_Date$Mois[Indice2017],"mai")),
       sum(str_count(DATA_Loc_Date$Mois[Indice2018],"mai"))
)

juin<-c(sum(str_count(DATA_Loc_Date$Mois[Indice2014],"juin")),
        sum(str_count(DATA_Loc_Date$Mois[Indice2015],"juin")),
        sum(str_count(DATA_Loc_Date$Mois[Indice2016],"juin")),
        sum(str_count(DATA_Loc_Date$Mois[Indice2017],"juin")),
        sum(str_count(DATA_Loc_Date$Mois[Indice2018],"juin"))
)

juillet<-c(sum(str_count(DATA_Loc_Date$Mois[Indice2014],"juillet")),
           sum(str_count(DATA_Loc_Date$Mois[Indice2015],"juillet")),
           sum(str_count(DATA_Loc_Date$Mois[Indice2016],"juillet")),
           sum(str_count(DATA_Loc_Date$Mois[Indice2017],"juillet")),
           sum(str_count(DATA_Loc_Date$Mois[Indice2018],"juillet"))
)

aout<-c(sum(str_count(DATA_Loc_Date$Mois[Indice2014],"août")),
        sum(str_count(DATA_Loc_Date$Mois[Indice2015],"août")),
        sum(str_count(DATA_Loc_Date$Mois[Indice2016],"août")),
        sum(str_count(DATA_Loc_Date$Mois[Indice2017],"août")),
        sum(str_count(DATA_Loc_Date$Mois[Indice2018],"août"))
)

septembre<-c(sum(str_count(DATA_Loc_Date$Mois[Indice2014],"septembre")),
             sum(str_count(DATA_Loc_Date$Mois[Indice2015],"septembre")),
             sum(str_count(DATA_Loc_Date$Mois[Indice2016],"septembre")),
             sum(str_count(DATA_Loc_Date$Mois[Indice2017],"septembre")),
             sum(str_count(DATA_Loc_Date$Mois[Indice2018],"septembre"))
)

octobre<-c(sum(str_count(DATA_Loc_Date$Mois[Indice2014],"octobre")),
           sum(str_count(DATA_Loc_Date$Mois[Indice2015],"octobre")),
           sum(str_count(DATA_Loc_Date$Mois[Indice2016],"octobre")),
           sum(str_count(DATA_Loc_Date$Mois[Indice2017],"octobre")),
           sum(str_count(DATA_Loc_Date$Mois[Indice2018],"octobre"))
)

novembre<-c(sum(str_count(DATA_Loc_Date$Mois[Indice2014],"novembre")),
            sum(str_count(DATA_Loc_Date$Mois[Indice2015],"novembre")),
            sum(str_count(DATA_Loc_Date$Mois[Indice2016],"novembre")),
            sum(str_count(DATA_Loc_Date$Mois[Indice2017],"novembre")),
            sum(str_count(DATA_Loc_Date$Mois[Indice2018],"novembre"))
)

decembre<-c(sum(str_count(DATA_Loc_Date$Mois[Indice2014],"décembre")),
            sum(str_count(DATA_Loc_Date$Mois[Indice2015],"décembre")),
            sum(str_count(DATA_Loc_Date$Mois[Indice2016],"décembre")),
            sum(str_count(DATA_Loc_Date$Mois[Indice2017],"décembre")),
            sum(str_count(DATA_Loc_Date$Mois[Indice2018],"décembre"))
)
data<-cbind(janvier,fevrier,mars,avril,mai,juin,juillet,aout,septembre,octobre,novembre,decembre)
data
write.xlsx(data, "NombreCrime_ParMois_EtAnnees.xlsx")


bp<-barplot(data,beside=F,col=c("blue","pink","yellow","red","orange"),ylab="Nombre de crimes par arme à feu",names=colnames(data),las=2,horiz=F,space=0.2)

legend(x="topright", legend=c(annees), cex=0.8,fill=c("blue","pink","yellow","red","orange"))


############### Corrélation entre nb de crime et mois de l'année

DataByMounth<-read.xlsx("NombreCrime_ParMois_EtAnnees.xlsx")
Annees<-c("2014","2015","2016","2017","2018")
DataByMounth<-cbind(Annees,DataByMounth)
DataByMounth<-DataByMounth[,-1]

Somme<-c(sum(DataByMounth[1,]),sum(DataByMounth[2,]),sum(DataByMounth[3,]),sum(DataByMounth[4,]),sum(DataByMounth[5,]))
DataByMounth<-cbind(DataByMounth,Somme)

MatCor <- cor(DataByMounth[-5,-1], method = "pearson")
View(MatCor)
library(corrplot)
corrplot(MatCor, type="upper", order="hclust", tl.col="black", tl.srt=45,method="shade",outline = FALSE,addCoef.col = "black")

library(Hmisc)
Pvalue<-rcorr(as.matrix(MatCor))
corrplot(MatCor, type="upper", order="hclust", tl.col="black",method="shade",outline = FALSE,addCoef.col = "black",p.mat = Pvalue$P, sig.level =0.01)

