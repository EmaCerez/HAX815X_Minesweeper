
# Victory pop-up ----------------------------------------------------------


win_UI <- function(id, score, time) {
  ns <- NS(id)
  modalDialog(
    title = tags$h1(
      style = "text-align: center;",
      "You win!"
    ),

    tags$div(
      class = "top-container;",
      style = "outline: 0;",
      tags$div(
        style = "width: 50%; 
                  text-align: center; 
                  font-size: 250%; 
                  font-weight: bold;
                  float: left;
                   display: inline-block;",

        tags$style(".fa-crown {color: #40DFEF; font-size: 83%}"),
        icon("crown", lib = "font-awesome"),
        score
      ),

      tags$div(
        style = "width: 50%; text-align: center; font-size: 250%; float: right; display: inline-block;",
        tags$style(".fa-clock {color:#F7C04A}"),
        icon("clock", lib = "font-awesome"),
        " ",
        time,
        "s"
      )
    ),

    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),

    tags$div(
      style = "text-align: center;",
      img(src = "images/bomb_sleep.png", align = "center")
    ),

    hr(),

    actionButton(
      inputId = ns("refresh2"),
      label = h3("Play Again"),
      style = "width: 100%; color: #ffffff; background-color: #ADE792; float: right;"
    )
  )
}

win <- function(input, output, session, score, time) {
  id <- gsub("-$", "", session$ns(""))
  showModal(ui = win_UI(id, score = score, time = time))

  observeEvent(input$refresh2, {
    refresh()
  })

  return(reactive(input$refresh2))
}