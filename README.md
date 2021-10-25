# Statistiques-Analysis-Gun-Violence

<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
    </li>
    <li>
    <a href="#questions">Questions</a>
    </li>
    <li>
    <a href="#finalités-du-sujet">Finalités du sujet</a>
    </li>
    <li>
    <a href="#les-fichiers">Les fichiers</a>
    </li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About The Project
Il s’agira de mener une série de statistiques sur l’archive du même intitulé qui se trouve                               
ici.  
Cet archive (au format CSV) contient des entrées relatives aux faits divers causés                         
par des armes à feu qui ont eu lieu aux Etats Unis entre le 1er janvier 2013 et le                                     
31 mars 2018. Il s’agira de montrer l'évolution d’un certain nombre de variables                         
pendant la période concernée, d’en calculer la moyenne et l'écart type (quand cela                         
est possible). L’ensemble V des variables qui nous intéressent contient : 
* nombre de blessées ; 
* nombre de morts ; 
* nombre de malfaiteurs ; 
* âge des malfaiteurs ; 
* état dans lequel le fait divers a eu lieu. 
 
### Questions  

*1. Est-ce qu’il y a des corrélations (linéaires) entre les variables de V ? 
 
*2. Prenez les données de la période entre le 1er janvier 2013 et le 31 décembre                             
2017 pour calculer les quantités statistiques (moyenne, écart type, etc) et                     
considérez les données entre le 1 janvier 2018 et le 31 mars 2018 comme un                             
échantillon. Est-ce que l’on peut dire que les valeurs moyennes des variables                       
dans V ont significativement changé par rapport au passé ? 
 
*3. Si l’on prend en compte aussi le mois de l’année dans lequel le fait divers a                               
été commis, est-ce qu’il y a une forte corrélation entre le nombre de faits                           
divers et le mois de l'année ? Quelles conclusions en tirez-vous ?


### Finalités du sujet   
- Traitement des données afin qu’elles soient exploitables 
- Création des colonnes manquantes (si nécessaire) 
- Etude des données en répondant aux problématiques posées 
- Conclusion à tirer de cette étude et informations supplémentaires  
 
### Les fichiers
- PRE est le dossier contenant les fichiers de scripts de pré-traitement des                       
données suivant : 
* 1_PréTraitementDeLaBase.R 
* 2_ModificationTypeVariables.R 
- R est le dossier contenant les fichiers de scripts d’études statistiques (répondant                       
au question) :  
* 1_Corrélations_Linéaires.R 
* 2 Changements Significatifs.R 
* 3_Crimes_et_mois.R 
- FIG contient les scripts R pour générer les graphiques contenu dans le rapport et                           
des fichiers image: 
* VisualisationDesDonnées.R 
* Des fichiers png des graphiques générés 
- REP le rapport final au format html 
