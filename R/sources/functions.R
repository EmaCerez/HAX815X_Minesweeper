

asc <- function(x) {
  strtoi(charToRaw(x), 16L) - 96
}

chr <- function(n) {
  rawToChar(as.raw(n + 96))
}

init_grid <- function(rows, columns, value) {
  grid <- matrix(value, nrow = rows, ncol = columns)
  return(grid)
}


generate_grid <- function(grid, rows, columns, bombs) {
  mines <- sample(1:(rows * columns), bombs)
  grid[mines] <- -1
  for (i in 1:rows) {
    for (j in 1:columns){
      nb_mines <- ifelse(grid[i, j] != -1,
                         calculer_mines_adjacentes(grid, i, j),
                         -1)
      grid[i, j] <- nb_mines
    }
  }
  return(grid)
}


generate_buttons <- function(grid_values, grid_hidden, grid_flags, rows, columns) {
  grid <- init_grid(value = 0, rows = rows, columns = columns)
  for (i in 1:rows) {
    for (j in 1:columns) {
      if (grid_flags[i, j]) {
        grid[i, j] <- 10
      } else if (grid_hidden[i, j]) {
        grid[i, j] <- 11
      } else {
        grid[i, j] <- grid_values[i, j]
      }
    }
  }
  return(grid)
}


update_button <- function(i, j, grid_values) {
  picture <- ""
  num <- grid_values[i, j] + 2
  if ((j %% 2 == 1 && i %% 2 == 0)||(j %% 2 == 0 && i %% 2 == 1)) {
    picture <- switch(num,
                      "db", "dr", "d1", "d2", "d3",
                      "d4", "d5", "d6", "d7", "d8",
                      "de", "df", "dg")
  } else {
    picture <- switch(num,
                      "lb", "lr", "l1", "l2", "l3",
                      "l4", "l5", "l6", "l7", "l8",
                      "le", "lf", "lg")
  }
  return(picture)
}



convert_grid <- function(grid, rows, columns) {
  for (i in 1:rows) {
    for (j in 1:columns){
      picture <- ""
      num <- as.integer(grid[i, j]) + 2
      if ((j %% 2 == 1 && i %% 2 == 0)||(j %% 2 == 0 && i %% 2 == 1)) {
        picture <- switch(num,
                          "db", "dr", "d1", "d2", "d3",
                          "d4", "d5", "d6", "d7", "d8",
                          "de", "df", "dg")
      } else {
        picture <- switch(num,
                          "lb", "lr", "l1", "l2", "l3",
                          "l4", "l5", "l6", "l7", "l8",
                          "le", "lf", "lg")
      }
      if (!is.null(picture)) {
        grid[i, j] <- picture
      }
    }
  }
  return(grid)
}


reveal_block <-  function(grille, visible, i, j) {
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
        visible <- reveal_block(grille, visible, k, l)
      }
    }
  }
  return(visible)
}


to_coordinates <- function(matrice) {
  r <- nrow(matrice)
  c <- ncol(matrice)
  coordinates <- matrix(c(0, 0), nrow = 1, ncol = 2)
  for (i in 1:r) {
    for (j in 1:c){
      if (matrice[i, j]) {
        coordinates <- rbind(coordinates, c(i, j))
      }
    }
  }
  return(coordinates)
}
