# Fonctions du démineur

## Fonction pour afficher la grille
afficher_grille <- function(grille, visible = NULL) {
  if (is.null(visible)) {
    visible <- matrix(FALSE, nrow = nrow(grille), ncol = ncol(grille))
  }
  for (i in 1:nrow(grille)) {
    for (j in 1:ncol(grille)) {
      if (visible[i,j]) {
        if (grille[i,j] == -1) {
          cat("* ")
        } else {
          cat(grille[i,j], "")
        }
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
  
  ## Création de la grille vide
  grille <- matrix(0, nrow = taille_grille, ncol = taille_grille)
  
  ## Placement des bombes de manière aléatoire
  mines <- sample(1:(taille_grille*taille_grille), nb_mines)
  grille[mines] <- -1
  
  visible <- matrix(FALSE, nrow = nrow(grille), ncol = ncol(grille))
  
  afficher_grille(grille, visible)
  choix <- readline("Choisissez une case (par exemple, 3 4): ")
  choix <- strsplit(choix, " ")[[1]]
  i <- as.integer(choix[1])
  j <- as.integer(choix[2])
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
  
  ### On fait une première boucle à part pour ne pas perdre au premier coup
  
  partie_terminee <- FALSE
  
  while (!partie_terminee) {
    afficher_grille(grille, visible)
    if (sum(visible)==taille_grille^2-nb_mines) {
      afficher_grille(grille,visible)
      cat("Gagné")
      partie_terminee <- TRUE
    } else {
      choix <- readline("Choisissez une case (par exemple, 3 4): ")
      choix <- strsplit(choix, " ")[[1]]
      i <- as.integer(choix[1])
      j <- as.integer(choix[2])
      if (grille[i,j] == -1) {
        afficher_grille(grille,visible)
        cat("Perdu !")
        partie_terminee <- TRUE
      } else {
        visible <- reveler_cases_adjacentes(grille,visible,i,j)}}}}

### readline est indispensable, on ne peut pas mettre print, ni cat

# à rajouter

#Drapeaux
#Quand on perd, mettre la bombe explosée et afficher les autres

#Après l'interface graphique:
#-Timer
#-Nombre de Drapeaux
#-Choix de la difficulté
#-etc.

jouer_partie()
