

asc <- function(x) { strtoi(charToRaw(x),16L) - 96 }

chr <- function(n) { rawToChar(as.raw(n + 96)) }

initGrid <- function(rows, columns, value){
  grid <- matrix(value, nrow = rows, ncol = columns)
  return(grid)
}


generateGrid <- function(grid, rows, columns, bombs){
  mines <- sample(1:(rows * columns), bombs)
  grid[mines] <- -1
  for(i in 1:rows) {
    for(j in 1:columns){
      nb_mines <- ifelse(grid[i, j] != -1, 
                         calculer_mines_adjacentes(grid, i, j), 
                         -1)
      grid[i, j] <- nb_mines
    }
  }
  return(grid)
} 


generateButtons <- function(gridValues, gridHidden, gridFlags, rows, columns){
  grid <- initGrid(value=0, rows=rows, columns=columns)
  for (i in 1:rows) {
    for (j in 1:columns) {
      if (gridFlags[i, j]) {
        grid[i, j] <- 10
      } 
      else if (gridHidden[i, j]){
        grid[i, j] <- 11
      }
      else {
        grid[i, j] <- gridValues[i, j]
      }
    }
  }
  return(grid)
}


updateButton <- function(i, j, gridValues){
  picture <- ""
  num <- gridValues[i, j] + 2
  if(j %% 2 == 1 & i %% 2 == 0){
    picture <- switch(num, 
                      "db", "dr", "d1", "d2", "d3", 
                      "d4", "d5", "d6", "d7", "d8",
                      "de", "df", "dg") 
  }
  else if(j %% 2 == 1 & i %% 2 == 1){
    picture <- switch(num, 
                      "lb", "lr", "l1", "l2", "l3", 
                      "l4", "l5", "l6", "l7", "l8",
                      "le", "lf", "lg") 
  }
  else if(j %% 2 == 0 & i %% 2 == 1){
    picture <- switch(num, 
                      "db", "dr", "d1", "d2", "d3", 
                      "d4", "d5", "d6", "d7", "d8",
                      "de", "df", "dg")
  } 
  else {
    picture <- switch(num, 
                      "lb", "lr", "l1", "l2", "l3", 
                      "l4", "l5", "l6", "l7", "l8",
                      "le", "lf", "lg")  
  }
  return(picture)
}



convertGrid <- function(grid, rows, columns) {
  for(i in 1:rows) {
    for(j in 1:columns){
      picture <- ""
      num <- as.integer(grid[i, j]) + 2
      if(j %% 2 == 1 & i %% 2 == 0){
        picture <- switch(num, 
                          "db", "dr", "d1", "d2", "d3", 
                          "d4", "d5", "d6", "d7", "d8",
                          "de", "df", "dg") 
      }
      else if(j %% 2 == 1 & i %% 2 == 1){
        picture <- switch(num, 
                          "lb", "lr", "l1", "l2", "l3", 
                          "l4", "l5", "l6", "l7", "l8",
                          "le", "lf", "lg") 
      }
      else if(j %% 2 == 0 & i %% 2 == 1){
        picture <- switch(num, 
                          "db", "dr", "d1", "d2", "d3", 
                          "d4", "d5", "d6", "d7", "d8",
                          "de", "df", "dg")
      } 
      else {
        picture <- switch(num, 
                          "lb", "lr", "l1", "l2", "l3", 
                          "l4", "l5", "l6", "l7", "l8",
                          "le", "lf", "lg")  
      }
      if (!is.null(picture)) {grid[i, j] <- picture}
    }
  }
  return(grid)
}


revealBlock <-  function(grille, visible, i, j) {
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
        visible <- revealBlock(grille, visible, k, l)
      }
    }
  }
  return(visible)
}


to_coordinates <- function(matrice) {
  r <- nrow(matrice)
  c <- ncol(matrice)
  coordinates <- matrix(c(0,0), nrow = 1, ncol = 2)
  for (i in 1:r) {
    for (j in 1:c){
      if (matrice[i, j]){
        coordinates <- rbind(coordinates, c(i, j))
      }
    }
  }
  return(coordinates)
}


endgame <- function(){
  
}