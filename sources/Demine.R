# Fonctions du démineur

## Fonction pour afficher la grille
afficher_grille <- function(grille, visible, drapeaux) {
  for (i in 1:nrow(grille)) {
    for (j in 1:ncol(grille)) {
      if (visible[i, j]) {
        if (grille[i, j] == -2) {
          cat("F ")
        } else if (grille[i, j] == -1) {
          cat("* ")
        } else {
          cat(grille[i, j], "")
        }
      } else if (drapeaux[i, j]) {
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
  for (k in (i - 1):(i + 1)) {
    for (l in (j - 1):(j + 1)) {
      if (k >= 1 && k <= nrow(grille) && l >= 1 && l <= ncol(grille)) {
        if (grille[k, l] == -1) {
          nb_mines_adjacentes <- nb_mines_adjacentes + 1
        }
      }
    }
  }
  return(nb_mines_adjacentes)
}

## Fonction pour révéler les cases adjacentes vides
reveler_cases_adjacentes <- function(grille, visible, i, j) {
  if (visible[i, j]) {
    return(visible)
  }
  visible[i, j] <- TRUE
  if (grille[i, j] != 0) {
    return(visible)
  }
  for (k in (i - 1):(i + 1)) {
    for (l in (j - 1):(j + 1)) {
      if (k >= 1 && k <= nrow(grille) && l >= 1 && l <= ncol(grille)) {
        visible <- reveler_cases_adjacentes(grille, visible, k, l)
      }
    }
  }
  return(visible)
}

## Fonction pour les points

calcul_points <- function(points, temps_depart, nb_cases, nb_nouvelles_cases) {
  points <- points + (nb_nouvelles_cases - nb_cases)^2 * max(1, floor(100 - (proc.time()[3] - temps_depart)))
  return(points)
}

#On récompense les joueurs rapides et coups qui révélent beaucoup de cases d'un coup.

# Jouer

jouer_partie <- function() {

  choix2 <- readline("Choisissez une taille de grille et le nombre de mines (par ex, 10espace5espace10): ")
  choix2 <- strsplit(choix2, " ")[[1]]
  taille_grille1 <- as.integer(choix2[1])
  taille_grille2 <- as.integer(choix2[2])
  nb_mines <- as.integer(choix2[3])

  ## Création de la grille vide
  grille <- matrix(0, nrow = taille_grille1, ncol = taille_grille2)

  ## Placement des bombes de manière aléatoire
  mines <- sample(1:(taille_grille1 * taille_grille2), nb_mines)
  grille[mines] <- -1

  visible <- matrix(FALSE, nrow = nrow(grille), ncol = ncol(grille))
  drapeaux <- matrix(FALSE, nrow = nrow(grille), ncol = ncol(grille))

  afficher_grille(grille, visible, drapeaux)
  choix <- readline("Choisissez une case (par exemple, 4espace2): ")
  choix <- strsplit(choix, " ")[[1]]
  i <- as.integer(choix[1])
  j <- as.integer(choix[2])
  d <- choix[3]
  ptm <- proc.time()[3]
  if (is.na(d)) {
    test <- TRUE
    if (grille[i, j] == -1) {
      test <- FALSE
    }

    while (!test) {
      grille <- matrix(0, nrow = taille_grille1, ncol = taille_grille2)
      mines <- sample(1:(taille_grille1 * taille_grille2), nb_mines)
      grille[mines] <- -1
      if (grille[i, j] != -1) {
        test <- TRUE
      }
    }

    for (ii in 1:nrow(grille)) {
      for (jj in 1:ncol(grille)) {
        if (grille[ii, jj] != -1) {
          grille[ii, jj] <- calculer_mines_adjacentes(grille, ii, jj)
        }
      }
    }
    visible <- reveler_cases_adjacentes(grille, visible, i, j)
    points <- calcul_points(0, ptm, 0, sum(visible))

  } else {
    drapeaux[i, j] <- !drapeaux[i, j]
    while (!is.na(d)) {
      nb_drapeaux <- nb_mines - sum(drapeaux)
      afficher_grille(grille, visible, drapeaux)
      d <- NA
      choix <- readline("Choisissez une case (par exemple, 4espace2): ")
      choix <- strsplit(choix, " ")[[1]]
      i <- as.integer(choix[1])
      j <- as.integer(choix[2])
      d <- choix[3]
      if (!is.na(d)) {
        if (nb_drapeaux == 0) {
          cat("Vous n'avez plus de drapeaux...")
          cat("\n")
      } else {
        drapeaux[i, j] <- !drapeaux[i, j]
      }
    } else if (drapeaux[i, j]) {
        cat("Il y a un drapeau ici")
        cat("\n")
        d <- 5
      }
  }

    test <- TRUE
    if (grille[i, j] == -1) {
      test <- FALSE
    }

    while (!test) {
      grille <- matrix(0, nrow = taille_grille1, ncol = taille_grille2)
      mines <- sample(1:(taille_grille1 * taille_grille2), nb_mines)
      grille[mines] <- -1
      if (grille[i, j] != -1) {
        test <- TRUE
      }
    }

    for (ii in 1:nrow(grille)) {
      for (jj in 1:ncol(grille)) {
        if (grille[ii, jj] != -1) {
          grille[ii, jj] <- calculer_mines_adjacentes(grille, ii, jj)
        }
      }
    }
    visible <- reveler_cases_adjacentes(grille, visible, i, j)
    points <- calcul_points(0, ptm, 0, sum(visible))
  }

  ### On fait une première boucle à part pour ne pas perdre au premier coup

  partie_terminee <- FALSE
  while (!partie_terminee) {
    cases <- sum(visible)
    afficher_grille(grille, visible, drapeaux)
    nb_drapeaux <- nb_mines - sum(drapeaux)
    if (sum(visible) == taille_grille1 * taille_grille2 - nb_mines) {
      cat("Gagné ! ")
      cat("\n")
      cat("Vous avez gagné en: ")
      cat((proc.time()[3] - ptm))
      cat(" s")
      cat("\n")
      cat("Vous avez eu: ", points, " points. ")
      cat("\n")
      partie_terminee <- TRUE
    } else {
      choix <- readline("Choisissez une case (par exemple, 4espace2): ")
      choix <- strsplit(choix, " ")[[1]]
      i <- as.integer(choix[1])
      j <- as.integer(choix[2])
      d <- choix[3]
      if (!is.na(d)) {
        if (nb_drapeaux == 0) {
          cat("Vous n'avez plus de drapeaux...")
          cat("\n")
        } else if (visible[i, j]) {
          cat("Vous avez déjà creusez ici...")
          cat("\n")
        } else {
          drapeaux[i, j] <- !drapeaux[i, j]
        }
      } else if (drapeaux[i, j]) {
        cat("Il y a un drapeau ici")
        cat("\n")
      } else if (grille[i, j] == -1) {
        cat("Perdu !")
        cat("\n")
        cat("Vous avez perdu en: ")
        cat((proc.time()[3] - ptm))
        cat(" s")
        cat("\n")
        visible <- matrix(TRUE, nrow = nrow(grille), ncol = ncol(grille))
        grille[i, j] <- -2
        afficher_grille(grille, visible, drapeaux)
        partie_terminee <- TRUE
      } else {
        visible <- reveler_cases_adjacentes(grille, visible, i, j)
        points <- calcul_points(points, ptm, cases, sum(visible))
      }
    }
  }
}

### readline est indispensable, on ne peut pas mettre print, ni cat

# à rajouter

#Drapeaux (Fait)
#Quand on perd, mettre la bombe explosée et afficher les autres (Fait)

#Après l'interface graphique:
#-Timer (Fait)
#-Nombre de Drapeaux (Fait)
#-Choix de la difficulté (Fait)
#-etc.

#A rajouter, le nombre de drapeaux au début, avant la regénération de la grille (fait)

jouer_partie()
