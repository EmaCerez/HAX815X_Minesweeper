

# Time -------------------------------------------------------------


time_UI <- function(id) {
  ns <- NS(id)
  tags$div(
    style = "width: 100%; text-align: center; font-size: 150%;",
    tags$style(".fa-clock {color:#F7C04A}"),
    icon("clock", lib = "font-awesome"),
    " ",
    uiOutput(outputId = ns("timer_ui"), style = "font-size: 120%; font-weight: bold;", inline = TRUE),
    "s"
  )
}

time <- function(input, output, session, start = reactive(0)) {

  time_r <- reactiveVal(value = 0)
  started <- reactiveVal(value = FALSE)

  observeEvent(start(), {
    time_r(0)
    started(TRUE)
  }, ignoreInit = TRUE)

  observe({
    if (started()) {
      invalidateLater(1000, session)
      isolate({
        new_time <- time_r() + 1
        time_r(new_time)
      })
    }
  })

  output$timer_ui <- renderUI({
    as.character(time_r())
  })

  return(time_r)
}
