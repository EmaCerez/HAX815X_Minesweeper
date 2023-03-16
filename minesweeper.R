
#  ------------------------------------------------------------------------
#
# Title : Minesweeper
#    By : Ema Cerezo and Paul Crespin
#  Date : 2023-03-20
#    
#  ------------------------------------------------------------------------


# Packages ----------------------------------------------------------------

library("shiny")
library("png")


# Modules -----------------------------------------------------------------

source("modules/time-module.R")
source("modules/case-module.R")
source("modules/welcome-module.R")


# Functions ---------------------------------------------------------------

#source("sources/Démine.R")
#source("sources/functions.R")


# Global ------------------------------------------------------------------

mine_logo <- readPNG("images/tiles/dark_brown_bomb.png")

n_rows <- 5
n_tile <- 4


# RShiny App --------------------------------------------------------------


ui <- fluidPage(
  
  tags$head(
    tags$link(href="styles.css", rel="stylesheet", type="text/css")
  ),
  tags$div(
    class = "title-app",
    tags$h1("Minesweeper"),
    tags$h4("Find all the bombs!")
  ),
  
  tags$br(),
  
  tags$div(
    style = "width: 650px; margin: auto;",
    time_UI("timer"),
    tags$br(),
    lapply(
      X = seq_len(n_rows * 2),
      FUN = function(x) {
        case_UI(id = paste0("modules", x))
      }
    )
  )
)



server <- function(input, output, session) {
  
  start <- callModule(module = welcome, id = "welcome")
  timer <- callModule(module = time, id = "timer", start = start)
  
  tiles_png <- sample(list.files(path = "images/tiles/", pattern = "png$"), n_tile)
  tiles_png <- sample(rep(tiles_png, 2))
  
  results_mods <- reactiveValues()
  results_mods_parse <- reactiveValues(all = NULL, show1 = NULL, show2 = NULL, show3 = NULL)
  reset <- reactiveValues(x = NULL)
  block <- reactiveValues(x = NULL)
  
  lapply(
    X = seq_len(n_tile * 2),
    FUN = function(x) {
      results_mods[[paste0("module", x)]] <- callModule(
        module = case,
        id = paste0("module", x),
        mine_logo = mine_logo,
        reset = reset,
        block = block
      )
    }
  )
}



shinyApp(ui = ui, server = server)