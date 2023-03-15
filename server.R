#  ------------------------------------------------------------------------
#
# Title : Minesweeper Hex - UI
#    By : Ema Cerezo and Paul Crespin
#  Date : 2023-03-20
#    
#  ------------------------------------------------------------------------


library("shiny")


function(input, output, session) {
  
  start <- callModule(module = welcome, id = "welcome")
  timer <- callModule(module = time, id = "timer", start = start)
  
  tiles_png <- sample(list.files(path = "images/tiles/", pattern = "png$"), n_tile)
  tiles_png <- sample(rep(tiles_png, 2))
  
  results_mods <- reactiveValues()
  results_mods_parse <- reactiveValues(all = NULL, show1 = NULL, show2 = NULL, show3 = NULL)
  reset <- reactiveValues(x = NULL)
  block <- reactiveValues(x = NULL)
  
  lapply(
    X = seq_len(n_tile * 2),
    FUN = function(x) {
      results_mods[[paste0("module", x)]] <- callModule(
        module = cases,
        id = paste0("module", x),
        hex_logo = hex_png[x],
        reset = reset,
        block = block
      )
    }
  )
}