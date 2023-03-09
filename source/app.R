

library(shiny)
library(shinyWidgets)    


sizes <- c(1, 2, 3)
difficulties <- c(1, 2, 3)

size <- 2
difficulty <- 1


if (interactive()) 
{
  size = 8 * difficulty
  
  ui <- fluidPage(
    tags$h1("Minesweeper"),
    checkboxGroupButtons(
      inputId = "cases",
      choices = rep("x", size * size),
      width = 33 * size,
    ),
  )
  
  server <- function(input, output) {
  }
  
  shinyApp(ui, server)
}