
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
library("shinyWidgets")


# Modules -----------------------------------------------------------------

source("modules/time-module.R")
source("modules/welcome-module.R")


# Functions ---------------------------------------------------------------

#source("sources/Démine.R")
#source("sources/functions.R")


# Global ------------------------------------------------------------------

mine_logo <- img(src="images/tiles/dark_brown_bomb.png", height=32, width=32)
no_logo <- img(src="images/tiles/dark_green.png", height=32, width=32)


n_tile <- 4


bombs <- c(10, 40, 99)

layout <- matrix(c(8, 10, 14, 18, 20, 24), ncol=3, nrow=2)

difficulty <- 3

boutons <- rep("no_logo", layout[2, difficulty])

matrice_boutons <- matrix(rep(boutons, layout[1, difficulty]), 
                          ncol=layout[2, difficulty], 
                          nrow=layout[1, difficulty], 
                          byrow=FALSE)

len_mat_jumps <- length(matrice_boutons) + layout[1, difficulty]

flags_left <- bombs[difficulty]

points <- 1800


# RShiny App --------------------------------------------------------------


# UI --------------------------------------------------------------

ui <- fluidPage(
  
  tags$head(
    tags$link(href="styles.css", rel="stylesheet", type="text/css"),
    tags$script(src = "message-handler.js"),
    tags$style(type="text/css", "div {white-space: nowrap;}")
  ),
  
  
  tags$div(
    
    # ------------- Titre ------------------------------------------------------
    
    class = "title-app",
    tags$h1("Minesweeper"),
    tags$h4("Find all the bombs!"),
    
    tags$br(style = "display: block; content: ''; margin-top: 40px;"),
    
    tags$div(
      class = "top-container",
      
      tags$div(
        style = "width: 100%; text-align: center; font-size: 166%; font-weight: bold; width: 33%;",
        tags$style(".fa-crown {color: #40DFEF; font-size: 83%}"),
        icon("crown", lib="font-awesome"),
        points
      ),
      
      tags$div(
        style = "width: 100%; text-align: center; font-size: 166%; font-weight: bold; width: 33%;",
        tags$style(".fa-flag {color: #DF2E38; font-size: 83%}"),
        icon("flag", lib="font-awesome"),
        flags_left
        
      ),
      
      tags$div(
        style = "text-align: center; width: 33%;",
        time_UI("timer"),
      ),
    ),
    
    
    
    # -------------- Jeu -------------------------------------------------------
    
    
    tags$div(
      class = "bottom-container",
      tags$div(
        style = ("outline: 10px rgba(69, 35, 17, 1);"),
        lapply(X = 1:len_mat_jumps,
               FUN = function(i) {
                 if (i %% (layout[2, difficulty] + 1) == 0) {tags$br()} 
                 else {
                   actionButton(
                     inputId = paste0("button", i - (i %/% (layout[1, difficulty] + 1))),
                     label = get(matrice_boutons[i - (i %/% (layout[1, difficulty] + 1))]),
                     style = "padding: 0px;
                            background-size: cover;
                            border: none;
                            margin-right: -5px; 
                            margin-bottom: -1px;")
                 }
               }
        ),
      )
    ),
  ),
  
  tags$br()
)


# Server --------------------------------------------------------------

server <- function(input, output, session) {
  
  start <- callModule(module = welcome, id = "welcome")
  timer <- callModule(module = time, id = "timer", start = start)
  
  observeEvent(input$do, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Thank you for clicking')
  })
}



shinyApp(ui = ui, server = server)