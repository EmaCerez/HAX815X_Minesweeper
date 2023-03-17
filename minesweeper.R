
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
library("shinyjs")


# Modules -----------------------------------------------------------------

source("modules/time-module.R")
source("modules/case-module.R")
source("modules/welcome-module.R")
#source("sources/cells.R")


# Functions ---------------------------------------------------------------

#source("sources/Démine.R")
#source("sources/functions.R")


# Global ------------------------------------------------------------------

mine_logo <- img(src="images/tiles/dark_brown_bomb.png")
no_logo <- img(src="images/tiles/dark_green.png")


n_rows <- 5
n_tile <- 4


bombs <- c(10, 40, 99)

layout <- matrix(c(8, 10, 14, 18, 20, 24), ncol=3, nrow=2)

difficulty <- 3

boutons <- rep("no_logo", layout[2, difficulty])

matrice_boutons <- matrix(rep(boutons, layout[1, difficulty]), 
                          ncol=layout[2, difficulty], 
                          nrow=layout[1, difficulty], 
                          byrow=FALSE)


# RShiny App --------------------------------------------------------------


ui <- fluidPage(
  
  tags$head(
    tags$link(href="styles.css", rel="stylesheet", type="text/css"),
    tags$script(src = "message-handler.js"),
#    tags$style(type="text/css", ".btn {padding-right: 0px; padding-left: 0px;}") # Autour du texte dans le btn
    tags$style(type="text/css", "div {white-space: nowrap;}")
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
  ),

  
  lapply(1:length(boutons), function(i) {
    actionButton(inputId = paste0("button", i),
                 label = get(boutons[i]),
                 style = "padding: 0px; margin-right: -5px; margin-bottom: -1px;"
    )
  }),
  
  tags$br()

)



server <- function(input, output, session) {
  
  start <- callModule(module = welcome, id = "welcome")
  timer <- callModule(module = time, id = "timer", start = start)
  
#  tiles_png <- sample(list.files(path = "images/tiles/", pattern = "png$"), n_tile)
#  tiles_png <- sample(rep(tiles_png, 2))
  
#  results_mods <- reactiveValues()
#  results_mods_parse <- reactiveValues(all = NULL, show1 = NULL, show2 = NULL, show3 = NULL)
#  reset <- reactiveValues(x = NULL)
#  block <- reactiveValues(x = NULL)
  
#  lapply(
#    X = seq_len(n_tile * 2),
#    FUN = function(x) {
#      results_mods[[paste0("module", x)]] <- callModule(
#        module = case,
#        id = paste0("module", x),
#        mine_logo = mine_logo,
#        reset = reset,
#        block = block
#      )
#    }
#  )
  
  observeEvent(input$do, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Thank you for clicking')
  })
}



shinyApp(ui = ui, server = server)