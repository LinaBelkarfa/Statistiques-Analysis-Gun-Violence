setwd("D:/Cours/M1 MIAGE/Maths pour big data/groupe-BZEZ-20/Projet_Gun")

################################## ETAPE 1 : chargement et observations des donn�es ################################################################################

DATA<-read.csv('Gun.csv',sep=',',header = TRUE, na.strings = c(" ","","NA"))
View(DATA)

#Je r��cris mon fichier en format xlsx, cela me facilitera les prochains traitements
library(xlsx2dfs)
write.xlsx(DATA,'Gun.xlsx')

summary(DATA)

#Donn�es � garder pour le Projet : 
  # nombre de bless�es ;colonne 7
  # nombre de morts ;colonne 6
  # nombre de malfaiteurs ; a cr�er � partir de participant_type
  # �ge des malfaiteurs ; a cr�er a partir de participant_type et age ou cat�gorie d'age 
  # �tat dans lequel le fait divers a eu lieu : colonne 3 

################################# ETAPE 2 : S�lection des colonnes utiles ################################################################################

#Nous avons du effectu� un traitement particulier sur excel, car dans certaines colonnes dont nous avons besoin (type, age, cat�gorie d'age), les s�parateurs �taient des "||"
#Le probl�me des "||", c'est qu'il est difficile de boucler dessus pour extraire des string, car R consid�re se signe comme un op�rateur logique (logical OR)
#Nous choisissons de le remplacer par des "et", ce qui est peut conventionnel mais plus s�re qu'un charact�re sp�cial, car notre base de donn�es en contient beaucoup

#Nous utilisons pour la suite ce nouveau fichier plus propre en format xlsx
DATA<-read.xlsx('Gun.xlsx')
#On garde les colonnes utile pour notre �tudes, on garde les index au cas o� (on les retirera lorsque n�cessaire)
DATABase<-DATA[,c(1,3,6,7,20,21,26)]
View(DATABase)

################################# ETAPE 3 : Cr�ation de la colonne NOMBRE DE MALFAITEURS ################################################################################

DATABase$participant_type
#Nous allons devoir compter le nombre de "subject suspect" par ligne (donc par crime) pour cr�er la colonne NOMBRE DE MALFAITEUR 
#Durant la prochaine �tape, nous devrons utiliser les ID de chacun des malfaiteurs afin de r�cup�rer �galement leur age dans DATABase$participant_age ou DATABase$participant_age_group

#On charge la librairie Tidyverse qui contient la fonction str_count()
library(tidyverse)

#On utilise la fonction str_count qui permet de compter le nombre d'occurence d'une string dans une autre string
ColonneNbMalfaiteur<-str_count(DATABase[,7], "Subject")
View(ColonneNbMalfaiteur)

#On ajoute cette colonne � la base de donn�es 
DATABase<-cbind(DATABase,ColonneNbMalfaiteur)

View(DATABase)
#Or nous voulons �galement l'age des malfaiteurs, donc nous avons besoin de reconnaitre a qui appartient quel age dans la base de donn�e
#Avant de continuer, J'enregistre dans un fichier xlsx ma nouvelle base de donn�e qui contient d�sormais le nombre de malfaiteur 
write.xlsx(DATABase,'Basededonn�eNbMalfaiteur.xlsx')

############################################################### ETAPE 4 : Cr�ation de la colonne age (moyen) des malfaiteurs ################################################################################

setwd("D:/Cours/M1 MIAGE/Maths pour big data/groupe-BZEZ-20/Projet_Gun")


library(xlsx2dfs)
DATA<-read.xlsx('Basededonn�eNbMalfaiteur.xlsx')
View(DATA)

#Pour cr�er la colonne d'age des malfaiteurs, je vais donc avoir besoin de deux colonnes uniquement : la colonne age et la colonne type 
#Le but est de prendre les ID (0,1,2,3,4...) des malfaiteurs et de les croiser avec l'age de ceux ci

ParticipantsType<-DATA$participant_type
ParticipantsAgeGroup<-DATA$participant_age_group
Age_Type<-cbind(ParticipantsType,ParticipantsAgeGroup)
View(Age_Type)

#Nous chargeons la library tidyverse afin de pouvoir utiliser la fonction str_split
#cette fonction permet de couper une string en un vecteur de string en choisissant le s�parateur qui sera retir� lors du d�coupage
#Ici, notre s�parateur est "et" (en effet nous avions essay� au pr�alable avce "||" mais il �tait ind�t�ctable par les algorithmes )
library(tidyverse)

#Voici un exemple de ce que fais str_split : 
  #Si nous renvoyons le premier �l�ment de notre base :
    Age_Type[1,1]
  #Cela nous retoune une string comme ceci : 
    #ParticipantsType 
    #"0::Victimet1::Victimet2::Victimet3::Victimet4::Subject-Suspect" 
  #Or lorsque je fais str_split en pr�cisant le "et"
    str_split(Age_Type[1,1],pattern = "et",simplify = TRUE)
  #Cela nous renvoie un vecteur ou chaque �l�ment est bien distinct 
    #     [,1]        [,2]        [,3]        [,4]        [,5]                
    #[1,] "0::Victim" "1::Victim" "2::Victim" "3::Victim" "4::Subject-Suspect"

    
#Type est la matrice contenant par colonne le statut de victim ou suspect 
#La colonne 1 correspond au participant 0, la deux au participants 1 etc...
Type<-str_split(Age_Type[,1],pattern = "et",simplify = TRUE)


#Age est la matrice contenant par colonne la cat�gorie d'age du participant
#La colonne 1 correspond au participant 0, la deux au participants 1 etc...
Age<-str_split(Age_Type[,2],pattern = "et",simplify = TRUE)

#La fonction grep va nous �tre utile pour la suite, elle permet de d�t�cter les indices contenant une string choisie
#Par exemple lorsque je regarde la premi�re ligne et que je veux voir quels �l�ments contiennent le mot "Adult" :
  grep(Age[1,],pattern = "Adult")
#J'obtiens alors un vecteur contenant les indices des colonnes contenant ce mot : 
  #[1] 1 2 3 4 5
#Cela me permet ici de voir que les participants 0,1,2,3 et 4 de la premiere ligne (du crime) sont des adultes  

#Je peux faire de m�me pour voir qui sont les suspects   
  grep(Type[1,],pattern = "Suspect")
  #[1] 5


#Nous avons choisi d'utiliser les cat�gories d'age, car cela sera plus r�aliste que de faire une moyenne d'age des malfaiteurs pour chaque crime
#En effet, la moyenne d'age pourrait �tre biais�e s'il y a des valeurs extr�mes
#Ce qui peut cependant �tre plus int�ressant, c'est de faire 3 colonne de cat�gorie d'age Adult, Teen, Child, afin de mettre le nombre de malfaiteurs
#appartenant � chacune de ces cat�gories  
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
    if(isTRUE(str_detect(Age[i,j],"Adult"))){     #je regarde si l'�l�ment de cette ligne et de cette colonne est adult teen ou child
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
#On observe la justesse de nos r�ultats
View(AgeGroupeSuspect)

#Les r�sultats �tant juste, on peut les ajouter � notre base de donn�es et retirer les colonnes plus n�cessaires 
DATA<-cbind(DATA,AgeGroupeSuspect)
DATA<-DATA[,-c(5,6,7)]
View(DATA)
#Notre base est enfin pr�te; il ne reste qu'� modifier les types de variables pour commencer l'�tude
write.xlsx(DATA,"Gun_Good_Columns.xlsx")


