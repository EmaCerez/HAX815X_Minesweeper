# Packages

library("shiny")
library("png")
library("shinyjs")
library("shinyWidgets")
library("beepr")


# Modules

source("R/time-module.R")
source("R/welcome-module.R")
source("R/endgame-module.R")
source("R/win-module.R")


# Functions

source("R/demine.R")
source("R/functions.R")


variable_glo <- reactiveVal(FALSE)

colorbl <- reactiveVal(FALSE)

cells_revealed <- reactiveVal(0)

bombs_total <- reactiveVal(0)

flags_left <- reactiveVal(0)

points <- reactiveVal(0)

new_cells <- reactiveVal(0)
