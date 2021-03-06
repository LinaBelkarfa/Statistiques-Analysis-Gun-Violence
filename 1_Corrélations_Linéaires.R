
setwd("D:/Cours/M1 MIAGE/Maths pour big data/groupe-BZEZ-20/Projet_Gun")
library(xlsx2dfs)

#################################################### CHARGEMENT DES DONNEES ########################################################
DATA<-read.xlsx("Gun_Without_Na.xlsx") #Les donn�es avec state pas encod� (sans NA)
EncodingData<-read.xlsx("One_Hot_DataGun.xlsx")# Les donn�es avec state encod� (sans NA)

########################### QUESTION 1 : Est-ce qu'il y a des corr�lations (lin�aires) entre les variables de V ? ################
View(DATA)

#Nous allons utiliser la m�thode "cor()" pour r�pondre � cette question : cor(data, method = c("pearson", "kendall", "spearman"))
#Cette fonction nous permet d'obtenir la matrice des coefficients de corr�lation sont entre les diff�rentes paires possibles de variables.
#Nous choisirons d'utiliser le param�tre "Pearson"
#Le coefficient de corr�lation de pearson mesure une corr�lation lin�aire entre deux variables

# La matrice de correlation de la base de donn�e encod� � 57 colonnes (donc 57 variables) car on retire les ID
#Cela est probl�matique car on ne pourra pas afficher dans le plot de Rstudio la matrice de corr�lation

MatCor <- cor(EncodingData[,-1], method = "pearson")
View(MatCor)

#Nous avons alors choisi de dessiner la mtrice de corr�lation en grande dimension dans un png 
#Les corr�lations positives sont affich�es en bleu et les corr�lations n�gatives en rouge. 
#L'intensit� de la couleur est proportionnelle aux coefficients de corr�lation. 
#A droite du corr�logramme, la l�gende de couleurs montre les coefficients de corr�lation et les couleurs correspondantes.
library(corrplot)
png(file="Corr�lation_Lin�aire.png", width=4000, height=4000)
corrplot(MatCor, type="upper", order="hclust", tl.col="black", tl.srt=45,method="shade",outline = FALSE,addCoef.col = "black")
dev.off()

#Malheureusement, cette fonction n'affiche pas la significativit� de la corr�lation (p-value). 
#Il nous faut calculer la p-value de chaque corr�lation.

#La fonction rcorr() du package Hmisc peut �tre utilis�e pour calculer le niveau de significativit� 
#pour les corr�lations de pearson
library(Hmisc)
Pvalue<-rcorr(as.matrix(MatCor))
##En utilisant cette fonction le coefficient de corr�lation r de Pearson est calculer pour toutes les paires de variables possibles dans la table de donn�e.
#Comme r�sultat, la fonction rcorr() renvoie une liste avec les �l�ments suivants : 
#- r : la matrice de corr�lation. 
#- n : La matrice du nombre d'observations utilis� dans l'analyse de chaque paire de variables. 
#- P : les p-values correspondant aux niveaux de significativit� des corr�lations, c'est ce qui nous int�resse
View(Pvalue$P)
Pvalue

# Indication des corr�lations non significatives 
# par une croix
library(corrplot)

png(file="Corr�lation_Significativite_0,01.png", width=4000, height=4000)
corrplot(MatCor, type="upper", order="hclust", tl.col="black",method="shade",outline = FALSE,addCoef.col = "black",p.mat = Pvalue$P, sig.level =0.01)
dev.off()

#Nous pouvons d�sormais voir les corr�lations suivantes( significatives au niveau p<0,01 -> g�n�ralement consid�r�s comme �tant statistiquement significatifs)

# -Corr�lation positive entre les variables Adult et ColonneNbMalfaiteur -> 0,76
# -Corr�lation positive entre les variables Teen et ColonneNbMalfaiteur -> 0,26
# -Corr�lation n�gative entre les variables Adult et n_injured -> -0.19
# -Corr�lation n�gative entre les variables ColonneNbMalfaiteur et n_injured -> -0.18


MatCor[c(53,54,55,56),c(53,54,55,56)]
Pvalue$P[c(53,54,55,56),c(53,54,55,56)]

#Test de corr�lation avec intervalle de confiance � 95%

cor.test(DATA$Adult,DATA$ColonneNbMalfaiteur)



