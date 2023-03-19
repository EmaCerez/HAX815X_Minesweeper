
#  -----------------------------------------------------------------------------
#
# Title : Minesweeper
#    By : Ema Cerezo and Paul Crespin
#  Date : 2023-03-20
#    
#  -----------------------------------------------------------------------------


source("global.R")


# RShiny App -------------------------------------------------------------------


# Serveur

server <- function(input, output, session) {
  
  # Appels
  start <- callModule(module = welcome, id = "welcome")
  timer <- callModule(module = time, id = "timer", start = start)
  
  # Boutons
  #observeEvent(input$do, {
  #  session$sendCustomMessage(type = 'testmessage',
  #                            message = 'Thank you for clicking')
  #})
  
  
  # -------------- Jeu ---------------------------------------------------------
  
  # Difficulté
  output$results <- renderText(input$diff)
  difficulty <- reactiveVal(1)
  observe({
    if (!is.null(input$diff)) {
      difficulty(switch(input$diff, 
                        "Rookie" = 1, 
                        "Deminer" = 2, 
                        "Wizard" = 3, 
                        "All-seeing" = 4, 
                        1))}
  })
  
  
  # Bombes, disposition, score et drapeaux
  bombs <- c(10, 40, 99, 120)
  layout <- matrix(c(8, 10, 14, 18, 20, 24, 20, 24), ncol=4, nrow=2)
  
  column_number <- reactiveVal(8)
  row_number <- reactiveVal(10)
  
  observe({
    column_number(layout[2, difficulty()])
    row_number(layout[1, difficulty()])
  })
 
  len_mat_jumps <- reactiveVal(90)
  
  flags_left <- reactiveVal(0)
  points <- reactiveVal(0)
  
  
  # Début de réactivité
  observe({
    
    flags_left(bombs[difficulty()])
  
#    boutons_l_paire <- rep(c("lg", "dg"), column_number()/2)
#    boutons_l_impai <- rep(c("dg", "lg"), column_number()/2)
    
    matrice_valeurs <- initGrid(value=0, rows=row_number(), columns=column_number())
    matrice_valeurs <- generateGrid(matrice_valeurs, 
                                   rows=row_number(),
                                   columns=column_number(),
                                   bombs=bombs[difficulty()])
    
    matrice_cache <- initGrid(value=TRUE, rows=row_number(), columns=column_number())
    matrice_drapeaux <- initGrid(value=FALSE, rows=row_number(), columns=column_number())
    
    matrice_jeu <- generateButtons(gridValues=matrice_valeurs,
                                   gridFlags=matrice_drapeaux,
                                   gridHidden=matrice_cache,
                                   rows=row_number(),
                                   columns=column_number())
    
    matrice_boutons <- convertGrid(grid=matrice_jeu, 
                                   rows=row_number(), 
                                   columns=column_number())
    
    boutons <- reactiveValues(matrice_boutons = matrice_boutons)
    
    len_mat_jumps <- length(matrice_boutons) + row_number()
    
    # Boutons ?
    
    lignes <- rep(1:row_number(), each=column_number())
    buttons_ids <- c(paste0(lignes, "_", 1:column_number()))
    #buttons_ids <- matrix(buttons_ids, nrow=row_number(), ncol=column_number(), byrow=TRUE)
    
    print(buttons_ids)
    
    lapply(X = buttons_ids, function(x){
      observeEvent(input[[x]], {
        coordinates <- as.integer(unlist(strsplit(x=x, "_")))
        o <- coordinates[1]
        p <- coordinates[2]
        
        update <- updateButton(i=o, j=p, gridValues=matrice_valeurs)
        
        if(update == "dr" | update == "lr"){
          matrice_temp <- initGrid(value=FALSE, rows=row_number(), columns=column_number())
          to_update <- revealBlock(i=o, j=p, grille=matrice_valeurs, visible=matrice_temp)
          to_update <- to_coordinates(to_update)
          for (i in 2:nrow(to_update)){
            m <- to_update[i, 1]
            n <- to_update[i, 2]
            boutons$matrice_boutons[m, n] <- updateButton(i=m, j=n, gridValues=matrice_valeurs)
          }
        } else if (update == "db" | update == "lb"){
          boutons$matrice_boutons[o, p] <- paste0(substr(boutons$matrice_boutons[o, p], 1, 1), "b")
          endgame()
        } else {
          boutons$matrice_boutons[o, p] <- update
        }
        
          
        #runjs(paste0("$('label[for=\"x\"]').text('",TextVariable,"')"))
        
        #print(drapeaux$matrice_drapeaux[x, y])
        #drapeaux$matrice_drapeaux[x, y] <- TRUE
        #jeu$matrice_jeu[x, y] <- updateButton(i=x, j=y, gridFlags=drapeaux$matrice_drapeaux, gridValues = matrice_valeurs, gridHidden=matrice_cache)
        #print(drapeaux$matrice_drapeaux[x, y])
      })
    })
    
    
    # -------------- UI -------------------------------------------------------
    
    # Informations menu
    output$informations <- renderUI(
      tags$div(
        class = "top-container",
        
        # Score
        tags$div(
          style = "width: 100%; 
                  text-align: center; 
                  font-size: 166%; 
                  font-weight: bold; 
                  width: 33%;",
          tags$style(".fa-crown {color: #40DFEF; font-size: 83%}"),
          icon("crown", lib="font-awesome"),
          points()
        ),
        
        # Drapeaux restants
        tags$div(
          style = "width: 100%; 
                  text-align: center; 
                  font-size: 166%; 
                  font-weight: bold; 
                  width: 33%;",
          tags$style(".fa-flag {color: #DF2E38; font-size: 83%}"),
          icon("flag", lib="font-awesome"),
          flags_left()
        ),
        
        # Temps
        tags$div(
          style = "text-align: center; width: 33%;",
          time_UI("timer"),
        ),
      ),
    )
    
    
    # Grille de jeu
    output$game <- renderUI(
      
      tags$div(
        class = "bottom-container",
        
        # Grille
        tags$div(
          style = ("outline: 10px rgba(69, 35, 17, 1);"),
          lapply(X = 1:len_mat_jumps,
                 FUN = function(i) {
                   if (i %% (column_number() + 1) == 0) {tags$br()} 
                   else {
                     actionButton(
                       inputId = paste0(i %/% (column_number() + 1) + 1, 
                                        "_",
                                        i %% (column_number() + 1)),
                       label = get(boutons$matrice_boutons[i %/% (column_number() + 1) + 1, i %% (column_number() + 1)]),
                       style = "padding: 0px;
                            background-size: cover;
                            border: none;
                            margin-right: -5px; 
                            margin-bottom: -1px;")
                   } # else
                 } # FUN
          ) # lapply
        ) # div grille
      ) # div bottom container
    ) # renderUI output$game
  }) # observe
  
  # musique
  observe({
    req(input$sound_seek)
    if (round(input$sound_seek) == 10) {
      pauseHowl("Main theme")
    }
  })
    
  
} # server



# UI

ui <- fluidPage(
  
  useShinyjs(),
  
  tags$head(
    tags$link(href="styles.css", rel="stylesheet", type="text/css"),
    tags$script(src = "message-handler.js"),
    tags$style(type="text/css", "div {white-space: nowrap;}")
  ),
  
  tags$div(
    style = "float: right; background: none;",
    tags$audio(
      src = "sound/main_theme.mp3",
      autoplay = TRUE,
      controls = FALSE,
      loop = TRUE,
      preload = "auto",
      controlslist = "nodownload"      
    )
  ),
  
  tags$br(),
  
  tags$div(
    # Titre
    class = "title-app",
    tags$h1("Minesweeper"),
    tags$h4("Can you find all the bombs?"),
    
    tags$br(style = "display: block; content: ''; margin-top: 40px;"),
    
    # Informations sur la partie
    uiOutput("informations"),
    
    # Jeu
    uiOutput("game")
  ),
  
  tags$br(),
  
)


# Lancement --------------------------------------------------------------------

shinyApp(ui = ui, server = server)