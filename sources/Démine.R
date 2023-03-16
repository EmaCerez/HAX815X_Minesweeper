# Fonctions du démineur

## Fonction pour afficher la grille
afficher_grille <- function(grille, visible, drapeaux) {
  for (i in 1:nrow(grille)) {
    for (j in 1:ncol(grille)) {
      if (visible[i,j]) {
        if (grille[i,j] == -2) {
          cat("F ")
        } else if (grille[i,j] == -1) {
          cat("* ")
        } else {
          cat(grille[i,j], "")
        }
      } else if (drapeaux[i,j]) {
        cat("D ")
      } else {
        cat("# ")
      }
    }
    cat("\n")
  }
}

###Print ne fonctionne pas pour afficher.

## Fonction pour calculer le nombre de mines adjacentes
calculer_mines_adjacentes <- function(grille, i, j) {
  nb_mines_adjacentes <- 0
  for (k in (i-1):(i+1)) {
    for (l in (j-1):(j+1)) {
      if (k >= 1 & k <= nrow(grille) & l >= 1 & l <= ncol(grille)) {
        if (grille[k,l] == -1) {
          nb_mines_adjacentes <- nb_mines_adjacentes + 1
        }
      }
    }
  }
  return(nb_mines_adjacentes)
}

## Fonction pour révéler les cases adjacentes vides
reveler_cases_adjacentes <- function(grille, visible, i, j) {
  if (visible[i,j]) {
    return(visible)
  }
  visible[i,j] <- TRUE
  if (grille[i,j] != 0) {
    return(visible)
  }
  for (k in (i-1):(i+1)) {
    for (l in (j-1):(j+1)) {
      if (k >= 1 & k <= nrow(grille) & l >= 1 & l <= ncol(grille)) {
        visible <- reveler_cases_adjacentes(grille, visible, k, l)
      }
    }
  }
  return(visible)
}

# Données


## Définition de la taille de la grille
taille_grille <- 10

## Définition du nombre de mines
nb_mines <- 10

# Jouer

jouer_partie <- function() {
  
  choix2 <- readline("Choisissez une taille de grille et le nombre de mines (par exemple, 10 10): ")
  choix2 <- strsplit(choix2, " ")[[1]]
  taille_grille <- as.integer(choix2[1])
  nb_mines <- as.integer(choix2[2])
  
  ## Création de la grille vide
  grille <- matrix(0, nrow = taille_grille, ncol = taille_grille)
  
  ## Placement des bombes de manière aléatoire
  mines <- sample(1:(taille_grille*taille_grille), nb_mines)
  grille[mines] <- -1
  
  visible <- matrix(FALSE, nrow = nrow(grille), ncol = ncol(grille))
  drapeaux <- matrix(FALSE, nrow = nrow(grille), ncol = ncol(grille))
  
  afficher_grille(grille, visible, drapeaux)
  choix <- readline("Choisissez une case (par exemple, 3 4): ")
  choix <- strsplit(choix, " ")[[1]]
  i <- as.integer(choix[1])
  j <- as.integer(choix[2])
  d <- choix[3]
  if (is.na(d)){
    A=TRUE
    if (grille[i,j] ==-1){
      A=FALSE
    }
  
    while (!A) {
      grille <- matrix(0, nrow = taille_grille, ncol = taille_grille)
      mines <- sample(1:(taille_grille*taille_grille), nb_mines)
      grille[mines] <- -1
      if (grille[i,j] !=-1){
        A=TRUE
      }
    }
  
    for (ii in 1:nrow(grille)) {
      for (jj in 1:ncol(grille)) {
        if (grille[ii,jj]!= -1) {
          grille[ii,jj] <- calculer_mines_adjacentes(grille, ii, jj)
        }
      }
    }
  
    if (is.na(d)){
      visible <- reveler_cases_adjacentes(grille,visible,i,j)
    } else {drapeaux[i,j] <- !drapeaux[i,j]}
  } else {while (!is.na(d)){
    drapeaux[i,j] <- !drapeaux[i,j]
    afficher_grille(grille, visible, drapeaux)
    d=NA
    choix <- readline("Choisissez une case (par exemple, 3 4): ")
    choix <- strsplit(choix, " ")[[1]]
    i <- as.integer(choix[1])
    j <- as.integer(choix[2])
    d <- choix[3]
  }
    A=TRUE
    if (grille[i,j] ==-1){
      A=FALSE
    }
    
    while (!A) {
      grille <- matrix(0, nrow = taille_grille, ncol = taille_grille)
      mines <- sample(1:(taille_grille*taille_grille), nb_mines)
      grille[mines] <- -1
      if (grille[i,j] !=-1){
        A=TRUE
      }
    }
    
    for (ii in 1:nrow(grille)) {
      for (jj in 1:ncol(grille)) {
        if (grille[ii,jj]!= -1) {
          grille[ii,jj] <- calculer_mines_adjacentes(grille, ii, jj)
        }
      }
    }
    visible <- reveler_cases_adjacentes(grille,visible,i,j)
    
  }
  

  ### On fait une première boucle à part pour ne pas perdre au premier coup
  
  partie_terminee <- FALSE
  
  while (!partie_terminee) {
    ptm <- proc.time()
    afficher_grille(grille, visible, drapeaux)
    nb_drapeaux=nb_mines-sum(drapeaux)
    if (sum(visible)==taille_grille^2-nb_mines) {
      cat("Gagné !")
      cat("Vous avez gagné en: ")
      cat((proc.time()-ptm)[3])
      cat(" s")
      partie_terminee <- TRUE
    } else {
      choix <- readline("Choisissez une case (par exemple, 3 4): ")
      choix <- strsplit(choix, " ")[[1]]
      i <- as.integer(choix[1])
      j <- as.integer(choix[2])
      d <- choix[3]
      if (!is.na(d)){
        if (sum(drapeaux)==nb_mines){
          cat("Vous n'avez plus de drapeaux...")
          cat("\n")
        } else if (visible[i,j]) {
          cat("Vous avez déjà creusez ici...")
          cat("\n")
        } else {
          drapeaux[i,j] <- !drapeaux[i,j]
        }
      } else if(drapeaux[i,j]){
        cat("Il y a un drapeau ici")
        cat("\n")
      } else if (grille[i,j] == -1) {
        cat("Perdu !")
        cat("\n")
        cat("Vous avez perdu en: ")
        cat((proc.time()-ptm)[3])
        cat(" s")
        cat("\n")  
        visible <- matrix(TRUE, nrow = nrow(grille), ncol = ncol(grille))
        grille[i,j]=-2
        afficher_grille(grille, visible ,drapeaux)
        partie_terminee <- TRUE
      } else {
        visible <- reveler_cases_adjacentes(grille,visible,i,j)
      }}}}

### readline est indispensable, on ne peut pas mettre print, ni cat

# à rajouter

#Drapeaux (Fait)
#Quand on perd, mettre la bombe explosée et afficher les autres (Fait)

#Après l'interface graphique:
#-Timer (Fait)
#-Nombre de Drapeaux (Fait)
#-Choix de la difficulté (Fait)
#-etc.

#A rajouter, le nombre de drapeaux au début, avant la regénération de la grille

jouer_partie()
