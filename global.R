

# Packages

library("shiny")
library("png")
library("shinyjs")
library("shinyWidgets")
library("beepr")


# Modules

source("modules/time-module.R")
source("modules/welcome-module.R")
source("modules/endgame-module.R")
source("modules/win-module.R")


# Functions

#source("sources/Démine.R")
#source("sources/functions.R")


variable_glo <- reactiveVal(FALSE)

colorbl <- reactiveVal(FALSE)

cells_revealed <- reactiveVal(0)

flags_left <- reactiveVal(0)

points <- reactiveVal(0)

new_cells <- reactiveVal(0)