

# Packages

library("shiny")
library("png")
library("shinyjs")
library("shinyWidgets")


# Modules

source("modules/time-module.R")
source("modules/welcome-module.R")


# Functions

#source("sources/Démine.R")
#source("sources/functions.R")


# Difficulté


flags_left <- 99

#flags_left <- reactive({
#  flags_left <- bombs[difficulty()]
#})


points <- 1800                    # Faire que ça augmente automatiquement

mine_logo <- img(src="images/tiles/dark_brown_bomb.png", height=32, width=32)
no_logo <- img(src="images/tiles/dark_green.png", height=32, width=32) 

n_tile <- 4 # Nombre de tiles dans www/images/tiles
