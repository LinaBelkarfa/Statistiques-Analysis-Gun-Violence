setwd("D:/Cours/M1 MIAGE/Maths pour big data/groupe-BZEZ-20/Projet_Gun")

#Nous devons d�sormais formaliser les donn�es, c'est � dire faire en sorte que l'on puisse calculer dessus
#Pour se faire nous allons tout d'abord mettre nos variables sous la bonne forme : Modifier les types de variables 

#Chargement de la base de donn�es que nous avons fait en pr�traitement 
library(xlsx2dfs)
DATA<-read.xlsx("Gun_Good_Columns.xlsx")

################################################## ETAPE 1 : FAIRE UN POINT DES TYPES DE VARIABLES A MODIFIER ##############################################
summary(DATA)
#  incident_id         state              n_killed         n_injured      ColonneNbMalfaiteur     Adult              Teen              Child         
#Min.   :  92114   Length:239677      Min.   : 0.0000   Min.   : 0.000   Min.   : 0.000      Min.   : 0.0000   Min.   : 0.00000   Min.   :0.000000  
#1st Qu.: 308545   Class :character   1st Qu.: 0.0000   1st Qu.: 0.000   1st Qu.: 0.000      1st Qu.: 0.0000   1st Qu.: 0.00000   1st Qu.:0.000000  
#Median : 543587   Mode  :character   Median : 0.0000   Median : 0.000   Median : 1.000      Median : 0.0000   Median : 0.00000   Median :0.000000  
#Mean   : 559334                      Mean   : 0.2523   Mean   : 0.494   Mean   : 0.928      Mean   : 0.6115   Mean   : 0.05242   Mean   :0.002403  
#3rd Qu.: 817228                      3rd Qu.: 0.0000   3rd Qu.: 1.000   3rd Qu.: 1.000      3rd Qu.: 1.0000   3rd Qu.: 0.00000   3rd Qu.:0.000000  
#Max.   :1083472                      Max.   :50.0000   Max.   :53.000   Max.   :63.000      Max.   :63.0000   Max.   :10.00000   Max.   :3.000000

#Nous devons modifier :
  #La colonne incident_id en variable cat�gorielle (un seul de chaque �videmment)
  #La colonne state en variable cat�gorielle (chaque �tat est une cat�gorie)

DATA$state<-factor(DATA$state)
DATA$incident_id<-factor(DATA$incident_id)

summary(DATA)
#incident_id            state           n_killed         n_injured      ColonneNbMalfaiteur     Adult              Teen              Child         
#92114  :     1   Illinois  : 17556   Min.   : 0.0000   Min.   : 0.000   Min.   : 0.000      Min.   : 0.0000   Min.   : 0.00000   Min.   :0.000000  
#92117  :     1   California: 16306   1st Qu.: 0.0000   1st Qu.: 0.000   1st Qu.: 0.000      1st Qu.: 0.0000   1st Qu.: 0.00000   1st Qu.:0.000000  
#92119  :     1   Florida   : 15029   Median : 0.0000   Median : 0.000   Median : 1.000      Median : 0.0000   Median : 0.00000   Median :0.000000  
#92122  :     1   Texas     : 13577   Mean   : 0.2523   Mean   : 0.494   Mean   : 0.928      Mean   : 0.6115   Mean   : 0.05242   Mean   :0.002403  
#92125  :     1   Ohio      : 10244   3rd Qu.: 0.0000   3rd Qu.: 1.000   3rd Qu.: 1.000      3rd Qu.: 1.0000   3rd Qu.: 0.00000   3rd Qu.:0.000000  
#92129  :     1   New York  :  9712   Max.   :50.0000   Max.   :53.000   Max.   :63.000      Max.   :63.0000   Max.   :10.00000   Max.   :3.000000  

################################################# ETAPE 2 : TRAITEMENT DES NA ###############################################################################

#Nous avons mainenant nos donn�es bien formalis�s en type (m�me si les pays devront �tre mit en onehot encoding plus tard )
#Nous devons maintenant voir ce que nous faisons des NA
#Dans les questions pos�es sur cette �tude, nous devons �tudier les corr�lations entre les variables dans un premier temps
#Cependant, si nous rempla�ons les NA ( par des moyennes, m�dianes, valeur la plus courante ou plus proche voisin), nous allons influencer les r�sultats des donn�es
#Nous pourrions faire une �tude � la fin dans laquelle nous remplacerions les valeurs manquantes (les NA)
#Mais pour le moment nous devons retirer les lignes contenant des NA 

#Nous faisons une boucle qui compte les NA par ligne (r�sultat stock� dans NaRow)
NaRow<-c()
for(i in 1:nrow(DATA)){
  acc=0
  for (j in 1:ncol(DATA)){
    if (is.na(DATA[i,j])==TRUE){
      acc=acc+1
    }
  }
  NaRow<-append(NaRow,acc)
}
View(NaRow)

#Ensuite on r�cup�re les indices de ceux ayant des NA donc �tant diff�rent de zero dans NaRow
acc<-c()
for(i in 1:length(NaRow)){
  if(NaRow[i]!=0){
    acc<-append(acc,i)
  }
}
acc
#On cr� une nouvelle base qui sera la base sans NA et on l'�crit dans un fichier xlsx pour �viter d'avoir � refaire tout cela
#en effet, le traitement prend quelques minutes car il y a 239677 ligne 
#apr�s retrait des NA on a 214814 observations, ce qui est largement suffisant

DataWithoutNa<-DATA[-acc,]
View(DataWithoutNa)
write.xlsx(DataWithoutNa,"Gun_Without_Na.xlsx")

##################################### ETAPE 3 : ONE HOT ENCODING DE STATE ########################################################
install.packages("phonTools")
library(xlsx2dfs)
DATA<-read.xlsx("Gun_Without_Na.xlsx")
library('mltools')
library('data.table')

#On fait un summary(DATA) pour v�rifier que les donn�es sont toujours sous la bonne forme
#Nous devons remettre state en factor avant de l'encoder
DATA$state<-as.factor(DATA$state)
DATA<-data.table(DATA)

#Encodage : cols est la colonne qu'on encode 
#           
EncodingData<-one_hot(dt=DATA, cols = "state" ,sparsifyNAs = FALSE, naCols = FALSE, dropCols = TRUE, dropUnusedLevels = FALSE)
View(EncodingData)


#On �crit dans un xlsx la base encod�e pour ne pas avoir � run � chaque fois 
write.xlsx(EncodingData,"One_Hot_DataGun.xlsx")


##################################### ETAPE 4 : state en categorielle ########################################################

DATA$state<-as.factor(DATA$state)





