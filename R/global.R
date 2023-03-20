

# Packages

library("shiny")
library("png")
library("shinyjs")
library("shinyWidgets")
library("beepr")


# Modules

source("R/modules/time-module.R")
source("R/modules/welcome-module.R")
source("R/modules/endgame-module.R")
source("R/modules/win-module.R")


# Functions

source("R/sources/demine.R")
source("R/sources/functions.R")


variable_glo <- reactiveVal(FALSE)

colorbl <- reactiveVal(FALSE)

cells_revealed <- reactiveVal(0)

bombs_total <- reactiveVal(0)

flags_left <- reactiveVal(0)

points <- reactiveVal(0)

new_cells <- reactiveVal(0)
