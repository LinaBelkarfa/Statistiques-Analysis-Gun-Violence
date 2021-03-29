setwd("D:/Cours/M1 MIAGE/Maths pour big data/groupe-BZEZ-20/Projet_Gun")

################################## ETAPE 1 : chargement et observations des données ################################################################################

DATA<-read.csv('Gun.csv',sep=',',header = TRUE, na.strings = c(" ","","NA"))
View(DATA)

#Je réécris mon fichier en format xlsx, cela me facilitera les prochains traitements
library(xlsx2dfs)
write.xlsx(DATA,'Gun.xlsx')

summary(DATA)

#Données à garder pour le Projet : 
  # nombre de blessées ;colonne 7
  # nombre de morts ;colonne 6
  # nombre de malfaiteurs ; a créer à partir de participant_type
  # âge des malfaiteurs ; a créer a partir de participant_type et age ou catégorie d'age 
  # état dans lequel le fait divers a eu lieu : colonne 3 

################################# ETAPE 2 : Sélection des colonnes utiles ################################################################################

#Nous avons du effectué un traitement particulier sur excel, car dans certaines colonnes dont nous avons besoin (type, age, catégorie d'age), les séparateurs étaient des "||"
#Le problème des "||", c'est qu'il est difficile de boucler dessus pour extraire des string, car R considère se signe comme un opérateur logique (logical OR)
#Nous choisissons de le remplacer par des "et", ce qui est peut conventionnel mais plus sûre qu'un charactère spécial, car notre base de données en contient beaucoup

#Nous utilisons pour la suite ce nouveau fichier plus propre en format xlsx
DATA<-read.xlsx('Gun.xlsx')
#On garde les colonnes utile pour notre études, on garde les index au cas où (on les retirera lorsque nécessaire)
DATABase<-DATA[,c(1,3,6,7,20,21,26)]
View(DATABase)

################################# ETAPE 3 : Création de la colonne NOMBRE DE MALFAITEURS ################################################################################

DATABase$participant_type
#Nous allons devoir compter le nombre de "subject suspect" par ligne (donc par crime) pour créer la colonne NOMBRE DE MALFAITEUR 
#Durant la prochaine étape, nous devrons utiliser les ID de chacun des malfaiteurs afin de récupérer également leur age dans DATABase$participant_age ou DATABase$participant_age_group

#On charge la librairie Tidyverse qui contient la fonction str_count()
library(tidyverse)

#On utilise la fonction str_count qui permet de compter le nombre d'occurence d'une string dans une autre string
ColonneNbMalfaiteur<-str_count(DATABase[,7], "Subject")
View(ColonneNbMalfaiteur)

#On ajoute cette colonne à la base de données 
DATABase<-cbind(DATABase,ColonneNbMalfaiteur)

View(DATABase)
#Or nous voulons également l'age des malfaiteurs, donc nous avons besoin de reconnaitre a qui appartient quel age dans la base de donnée
#Avant de continuer, J'enregistre dans un fichier xlsx ma nouvelle base de donnée qui contient désormais le nombre de malfaiteur 
write.xlsx(DATABase,'BasededonnéeNbMalfaiteur.xlsx')

############################################################### ETAPE 4 : Création de la colonne age (moyen) des malfaiteurs ################################################################################

setwd("D:/Cours/M1 MIAGE/Maths pour big data/groupe-BZEZ-20/Projet_Gun")


library(xlsx2dfs)
DATA<-read.xlsx('BasededonnéeNbMalfaiteur.xlsx')
View(DATA)

#Pour créer la colonne d'age des malfaiteurs, je vais donc avoir besoin de deux colonnes uniquement : la colonne age et la colonne type 
#Le but est de prendre les ID (0,1,2,3,4...) des malfaiteurs et de les croiser avec l'age de ceux ci

ParticipantsType<-DATA$participant_type
ParticipantsAgeGroup<-DATA$participant_age_group
Age_Type<-cbind(ParticipantsType,ParticipantsAgeGroup)
View(Age_Type)

#Nous chargeons la library tidyverse afin de pouvoir utiliser la fonction str_split
#cette fonction permet de couper une string en un vecteur de string en choisissant le séparateur qui sera retiré lors du découpage
#Ici, notre séparateur est "et" (en effet nous avions essayé au préalable avce "||" mais il était indétéctable par les algorithmes )
library(tidyverse)

#Voici un exemple de ce que fais str_split : 
  #Si nous renvoyons le premier élément de notre base :
    Age_Type[1,1]
  #Cela nous retoune une string comme ceci : 
    #ParticipantsType 
    #"0::Victimet1::Victimet2::Victimet3::Victimet4::Subject-Suspect" 
  #Or lorsque je fais str_split en précisant le "et"
    str_split(Age_Type[1,1],pattern = "et",simplify = TRUE)
  #Cela nous renvoie un vecteur ou chaque élément est bien distinct 
    #     [,1]        [,2]        [,3]        [,4]        [,5]                
    #[1,] "0::Victim" "1::Victim" "2::Victim" "3::Victim" "4::Subject-Suspect"

    
#Type est la matrice contenant par colonne le statut de victim ou suspect 
#La colonne 1 correspond au participant 0, la deux au participants 1 etc...
Type<-str_split(Age_Type[,1],pattern = "et",simplify = TRUE)


#Age est la matrice contenant par colonne la catégorie d'age du participant
#La colonne 1 correspond au participant 0, la deux au participants 1 etc...
Age<-str_split(Age_Type[,2],pattern = "et",simplify = TRUE)

#La fonction grep va nous être utile pour la suite, elle permet de détécter les indices contenant une string choisie
#Par exemple lorsque je regarde la première ligne et que je veux voir quels éléments contiennent le mot "Adult" :
  grep(Age[1,],pattern = "Adult")
#J'obtiens alors un vecteur contenant les indices des colonnes contenant ce mot : 
  #[1] 1 2 3 4 5
#Cela me permet ici de voir que les participants 0,1,2,3 et 4 de la premiere ligne (du crime) sont des adultes  

#Je peux faire de même pour voir qui sont les suspects   
  grep(Type[1,],pattern = "Suspect")
  #[1] 5


#Nous avons choisi d'utiliser les catégories d'age, car cela sera plus réaliste que de faire une moyenne d'age des malfaiteurs pour chaque crime
#En effet, la moyenne d'age pourrait être biaisée s'il y a des valeurs extrêmes
#Ce qui peut cependant être plus intéressant, c'est de faire 3 colonne de catégorie d'age Adult, Teen, Child, afin de mettre le nombre de malfaiteurs
#appartenant à chacune de ces catégories  
#Nous faison alors une boucle pour obtenir un vecteur nombre adult, un nombre teen, et un nombre child 
  
  
Adult<-c()
Teen<-c()
Child<-c()
for(i in 1:nrow(Type)){        #Pour chaque ligne (chaque crime)
  nbAdult=0
  nbTeen=0
  nbChild=0
  
  suspect<-grep(Type[i,],pattern = "Suspect")  #je prend toutes les colonnes contenant le mot suspect (donc les id des supects dans la base )
  
  for(j in suspect){   
                                              #pour chacun de ces id 
    if(isTRUE(str_detect(Age[i,j],"Adult"))){     #je regarde si l'élément de cette ligne et de cette colonne est adult teen ou child
      nbAdult=nbAdult+1
    }
    
    else if(isTRUE(str_detect(Age[i,j],"Teen"))){
      nbTeen=nbTeen+1
    }
    
    else if(isTRUE(str_detect(Age[i,j],"Child"))){
      nbChild=nbChild+1
    }
    
    else{
      nbAdult=nbAdult+0    #si on a un NA ou un nul on met 0 (car nous retirerons plus tard les lignes ayant des NA)
      nbTeen=nbTeen+0
      nbChild=nbChild+0
    }
    
  }
  
  Adult<-append(Adult,nbAdult)
  Teen<-append(Teen,nbTeen)
  Child<-append(Child,nbChild)
}
#On combine tout afin d'avoir pour chaque ligne le nombre d'adult, ados ou enfants
AgeGroupeSuspect<-cbind(Adult,Teen,Child) 
#On observe la justesse de nos réultats
View(AgeGroupeSuspect)

#Les résultats étant juste, on peut les ajouter à notre base de données et retirer les colonnes plus nécessaires 
DATA<-cbind(DATA,AgeGroupeSuspect)
DATA<-DATA[,-c(5,6,7)]
View(DATA)
#Notre base est enfin prête; il ne reste qu'à modifier les types de variables pour commencer l'étude
write.xlsx(DATA,"Gun_Good_Columns.xlsx")


