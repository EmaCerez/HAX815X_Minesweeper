
# Cases --------------------------------------------------------


case_UI <- function(id) {
  ns <- NS(id)
  tagList(
    imageOutput(
      outputId = ns("case"), 
      click = clickOpts(id = ns("hex_click"), clip = FALSE),
      width = 120, 
      height = 139, 
      inline = TRUE
    )
  )
}

case <- function(input, output, session, mine_logo, reset = reactiveValues(x = NULL), block = reactiveValues(x = NULL)) {
  
  click_status <- reactiveValues(show = FALSE, hex = mine_logo, ts = Sys.time(), found = FALSE)
  
  
  return(click_status)
}
