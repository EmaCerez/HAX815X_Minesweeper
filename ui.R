#  ------------------------------------------------------------------------
#
# Title : Minesweeper Hex - UI
#    By : Ema Cerezo and Paul Crespin
#  Date : 2023-03-20
#    
#  ------------------------------------------------------------------------

library("shiny")

fluidPage(
  
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
      X = seq_len(n_hex * 2),
      FUN = function(x) {
        case_UI(id = paste0("modules", x))
      }
    )
  )
)