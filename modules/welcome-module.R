
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
      tags$div(
        style = "text-align: center; display: flex;",
        tags$div(
          style = "width: 20%; text-align:right;",
          img(src = "images/bomb_wake.png", align = "center"),
        ),
        tags$div(
          style = "width: 60%; padding-top: 20px; font-weight: bold;",
          tags$p("Reveal all the cells without exploding!"),
        ),
        tags$div(
          style = "width: 20%; text-align:left;",
          img(src = "images/bomb_wake.png", align = "center")
        ),
      ),
      tags$p("Left click on a cell to reveal what's underneath"),
      tags$p("The number on a cell indicate how many bombs there are around it"),
      tags$p("You may plant flags on suspicious-looking cells"),
      tags$p("When you're ready, pick a difficulty and click the button below to play the game!")
    ),

    tags$br(),

    h3("Difficulty"),

    tagList(
      tags$style(type = "text/css", "#big_slider .irs-grid-text {font-size: 15px;}"),
      div(id = "big_slider",
          sliderTextInput(
            inputId = "diff",
            label = NULL,
            grid = TRUE,
            width = 400,
            hide_min_max = TRUE,
            force_edges = FALSE,
            choices = c("Rookie", "Deminer", "Wizard", "All-seeing")
          )
      )
    ),

    hr(),

    uiOutput(outputId = "colorblindness"),

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