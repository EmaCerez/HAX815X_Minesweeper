# HAX815X_Minesweeper / Des mines et des chiffres
An R library allowing the user to play a game of Minesweeper.  
Une librairie R permettant à l'utilisateur de jouer une partie de Démineur.

## Installation and Use of the Graphical Game

Due to some problems with the pictures, the game is not complete in the package form.  
So please, download the Zip File below and decompress it in a folder.

En raison de certains problèmes avec les images, le jeu n'est pas complet en package.  
Veuillez télécharger le fichier Zip suivant et décompressez-le dans un dossier:

  https://github.com/EmaCerez/HAX815X_Minesweeper/archive/refs/heads/main.zip

Once un-Zip, open "HAX815X_Minesweeper.Rproj" and enter in the terminal:  
Une fois dé-Zippé, ouvrez "HAX815X_Minesweeper.Rproj" et rentrez dans le terminal:

```
  library(shiny)
  runApp('RR/ManualGame.R')
```  

## Installation of the terminal Game from Git_Hub

If you already download it from the previous part, this part is optionnal:  
Si vous avez déjà téléchargé le Zip à la partie précédente, cette partie est optionnelle:

```
  devtools::install_github("EmaCerez/HAX815X_Minesweeper")
  library(HAX815X)
  jouer_partie()
``` 

If you already have it:  
Si vous l'avez déjà:

```
  source("RR/sources/Demine.R") #Useless if you've already played once in the other game mode. // Si vous avez déjà joué avec l'interface, cette commande n'est pas néceissaire.
  jouer_partie()
``` 



## Règles

Come on. Mines on grid. Numbers say how many mines around. Good luck.  
Tout est dans le titre.
