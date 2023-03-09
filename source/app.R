
library(shiny)
library(shinyWidgets)    


sizes <- c(1, 2, 3)
difficulties <- c(1, 2, 3)

size <- 2
difficulty <- 1

bomb_icon <- icon(name="bomb", lib="font-awesome")
flag_icon <- icon(name="flag", lib="font-awesome")
default_icon <- icon(name="check-square", lib="font-awesome")


if (interactive()) 
{
  size = 8 * size
  
  ui <- fluidPage(
    tags$h1("Minesweeper"),
    checkboxGroupButtons(
      inputId = "cases",      
      checkIcon = list(
        yes = bomb_icon,
        no = flag_icon
      ),
      choices = rep("", size * size),
      width = 40 * size,
    ),
  )
  
  server <- function(input, output) {
  }
  
  shinyApp(ui, server)
}