
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
      tags$p("When you're ready, click button below to select difficulty !")
    ), 
    
    tags$br(),
    
    h3("Difficulty"),
    
    tagList(
      tags$style(type = 'text/css', '#big_slider .irs-grid-text {font-size: 15px;}'), 
      div(id = 'big_slider',
          sliderTextInput(
            inputId = "diff",
            label = NULL, 
            grid = T,
            width = 400,
            hide_min_max = T,
            force_edges = F,
            choices = c("Rookie", "Deminer", "Wizard", "All-seeing")
          )
      )
    ),
    
    hr(),
    
    uiOutput(outputId="colorblindness"),
    
    fluidRow(verbatimTextOutput("value")),
    
    footer = actionButton(
      inputId = ns("play"),
      icon = icon("play"),
      label = h3("Play !"),
      style = "width: 100%; color: #ffffff; background-color: #ADE792; float: right;"
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