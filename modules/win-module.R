
# Victory pop-up ----------------------------------------------------------


win_UI <- function(id) {
  ns <- NS(id)
  modalDialog(
    title = tags$h1(
      style = "text-align: center;",
      "Victory"
    ),
    
    actionButton(
      inputId = ns("refresh"),
      label = h3("Try Again"),
      style = "width: 100%; color: #ffffff; background-color: #19A7CE; float: right;"
    )
  )
}

win <- function(input, output, session) {
  id <- gsub("-$", "", session$ns(""))
  showModal(ui = win_UI(id))
  
  observeEvent(input$refresh, {
    refresh()
  })
  
  return(reactive(input$refresh))
}