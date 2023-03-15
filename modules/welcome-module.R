
# Welcome message ----------------------------------------------------------


welcome_UI <- function(id) {
  ns <- NS(id)
  modalDialog(
    title = tags$h1(
      style = "text-align: center;",
      "Welcome to Minesweeper !"
    ),
    tags$div(
      style = "text-align: center;",
      tags$p("Find all the bombs as soon as possible!"),
      tags$p("Left click on a case to reveal what's underneath"),
      tags$p("Numbers on cases indicate how many bombs there are around it"),
      tags$p("When you think you've found a bomb, right click on the case to plant a flag"),
      tags$p("When you're ready, click button below to play !")
    ), 
    footer = actionButton(
      inputId = ns("play"),
      label = "Play !",
      icon = icon("play"),
      style = "width: 100%"
    )
  )
}

welcome <- function(input, output, session) {
  
  id <- gsub("-$", "", session$ns(""))
  showModal(ui = welcome_UI(id))
  
  observeEvent(input$play, {
    removeModal()
  })
  
  return(reactive(input$play))
}