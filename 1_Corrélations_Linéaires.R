
setwd("D:/Cours/M1 MIAGE/Maths pour big data/groupe-BZEZ-20/Projet_Gun")
library(xlsx2dfs)

#################################################### CHARGEMENT DES DONNEES ########################################################
DATA<-read.xlsx("Gun_Without_Na.xlsx") #Les données avec state pas encodé (sans NA)
EncodingData<-read.xlsx("One_Hot_DataGun.xlsx")# Les données avec state encodé (sans NA)

########################### QUESTION 1 : Est-ce qu'il y a des corrélations (linéaires) entre les variables de V ? ################
View(DATA)

#Nous allons utiliser la méthode "cor()" pour répondre à cette question : cor(data, method = c("pearson", "kendall", "spearman"))
#Cette fonction nous permet d'obtenir la matrice des coefficients de corrélation sont entre les différentes paires possibles de variables.
#Nous choisirons d'utiliser le paramètre "Pearson"
#Le coefficient de corrélation de pearson mesure une corrélation linéaire entre deux variables

# La matrice de correlation de la base de donnée encodé à 57 colonnes (donc 57 variables) car on retire les ID
#Cela est problématique car on ne pourra pas afficher dans le plot de Rstudio la matrice de corrélation

MatCor <- cor(EncodingData[,-1], method = "pearson")
View(MatCor)

#Nous avons alors choisi de dessiner la mtrice de corrélation en grande dimension dans un png 
#Les corrélations positives sont affichées en bleu et les corrélations négatives en rouge. 
#L'intensité de la couleur est proportionnelle aux coefficients de corrélation. 
#A droite du corrélogramme, la légende de couleurs montre les coefficients de corrélation et les couleurs correspondantes.
library(corrplot)
png(file="Corrélation_Linéaire.png", width=4000, height=4000)
corrplot(MatCor, type="upper", order="hclust", tl.col="black", tl.srt=45,method="shade",outline = FALSE,addCoef.col = "black")
dev.off()

#Malheureusement, cette fonction n'affiche pas la significativité de la corrélation (p-value). 
#Il nous faut calculer la p-value de chaque corrélation.

#La fonction rcorr() du package Hmisc peut être utilisée pour calculer le niveau de significativité 
#pour les corrélations de pearson
library(Hmisc)
Pvalue<-rcorr(as.matrix(MatCor))
##En utilisant cette fonction le coefficient de corrélation r de Pearson est calculer pour toutes les paires de variables possibles dans la table de donnée.
#Comme résultat, la fonction rcorr() renvoie une liste avec les éléments suivants : 
#- r : la matrice de corrélation. 
#- n : La matrice du nombre d'observations utilisé dans l'analyse de chaque paire de variables. 
#- P : les p-values correspondant aux niveaux de significativité des corrélations, c'est ce qui nous intéresse
View(Pvalue$P)
Pvalue

# Indication des corrélations non significatives 
# par une croix
library(corrplot)

png(file="Corrélation_Significativite_0,01.png", width=4000, height=4000)
corrplot(MatCor, type="upper", order="hclust", tl.col="black",method="shade",outline = FALSE,addCoef.col = "black",p.mat = Pvalue$P, sig.level =0.01)
dev.off()

#Nous pouvons désormais voir les corrélations suivantes( significatives au niveau p<0,01 -> généralement considérés comme étant statistiquement significatifs)

# -Corrélation positive entre les variables Adult et ColonneNbMalfaiteur -> 0,76
# -Corrélation positive entre les variables Teen et ColonneNbMalfaiteur -> 0,26
# -Corrélation négative entre les variables Adult et n_injured -> -0.19
# -Corrélation négative entre les variables ColonneNbMalfaiteur et n_injured -> -0.18


MatCor[c(53,54,55,56),c(53,54,55,56)]
Pvalue$P[c(53,54,55,56),c(53,54,55,56)]

#Test de corrélation avec intervalle de confiance à 95%

cor.test(DATA$Adult,DATA$ColonneNbMalfaiteur)



