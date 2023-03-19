

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


variable_glo <- reactiveVal(FALSE)

flags_left <- reactiveVal(0)