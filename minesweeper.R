
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
  bombs <- c(10, 30, 70, 110)
  layout <- matrix(c(8, 10, 14, 18, 20, 24, 20, 24), ncol=4, nrow=2)
  
  column_number <- reactiveVal(8)
  row_number <- reactiveVal(10)
  
  observe({
    column_number(layout[2, difficulty()])
    row_number(layout[1, difficulty()])
  })
 
  len_mat_jumps <- reactiveVal(90)
  
  points <- reactiveVal(0)
  
  #var <- reactiveVal(FALSE)
  
  #observeEvent(input$flagMode, {
  #  var(!var())
  #  output$flagM <- var()
  #})
  
  # Début de réactivité
  observe({
    observeEvent(input$flagMode, {
      variable_glo(input$flagMode)
    })
    
    observeEvent(input$colorblind, {
      colorbl(input$colorblind)
    })
    
    # Images -------------------------------------------------------------------
    
    n_tiles <- 26
    
    path <- "images/tiles"
    
    if (colorbl()) {
      path <- paste0(path, "D")
    } 
    
    df <- img(src=paste0(path, "/df.png"), height=32, width=32)
    lf <- img(src=paste0(path, "/lf.png"), height=32, width=32)
    
    db <- img(src=paste0(path, "/db.png"), height=32, width=32)
    lb <- img(src=paste0(path, "/lb.png"), height=32, width=32)
    
    de <- img(src=paste0(path, "/de.png"), height=32, width=32)
    le <- img(src=paste0(path, "/le.png"), height=32, width=32)
    
    dg <- img(src=paste0(path, "/dg.png"), height=32, width=32)
    lg <- img(src=paste0(path, "/lg.png"), height=32, width=32)
    
    dr <- img(src=paste0(path, "/dr.png"), height=32, width=32)
    lr <- img(src=paste0(path, "/lr.png"), height=32, width=32)
    
    dn <- c()
    ln <- c()
    
    for (i in 1:8){
      dn[i] <- paste0(path, "/d", i, ".png")
      ln[i] <- paste0(path, "/l", i, ".png")
    }
    
    d1 <- img(src=dn[1], height=32, width=32)
    d2 <- img(src=dn[2], height=32, width=32)
    d3 <- img(src=dn[3], height=32, width=32)
    d4 <- img(src=dn[4], height=32, width=32)
    d5 <- img(src=dn[5], height=32, width=32)
    d6 <- img(src=dn[6], height=32, width=32)
    d7 <- img(src=dn[7], height=32, width=32)
    d8 <- img(src=dn[8], height=32, width=32)
    
    l1 <- img(src=ln[1], height=32, width=32)
    l2 <- img(src=ln[2], height=32, width=32)
    l3 <- img(src=ln[3], height=32, width=32)
    l4 <- img(src=ln[4], height=32, width=32)
    l5 <- img(src=ln[5], height=32, width=32)
    l6 <- img(src=ln[6], height=32, width=32)
    l7 <- img(src=ln[7], height=32, width=32)
    l8 <- img(src=ln[8], height=32, width=32)
    
    
    
    # Difficulté ---------------------------------------------------------------
    
    flags_left(bombs[difficulty()])
  
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
    
    
    # Boutons
    
    lignes <- rep(1:row_number(), each=column_number())
    buttons_ids <- c(paste0(lignes, "_", 1:column_number()))
    
    lapply(X = buttons_ids, function(x){
      
      observeEvent(input[[x]], {
        
        coordinates <- as.integer(unlist(strsplit(x=x, "_")))
        o <- coordinates[1]
        p <- coordinates[2]
        
        update <- updateButton(i=o, j=p, gridValues=matrice_valeurs)
        
        if(variable_glo()){
          if (boutons$matrice_boutons[o, p] == "df" | boutons$matrice_boutons[o, p] == "lf"){
            #nouvelleValeur <- as.integer(flags_left()) + 1
            #flags_left(nouvelleValeur)
            boutons$matrice_boutons[o, p] <- paste0(substr(boutons$matrice_boutons[o, p], 1, 1), "g")
          } else {
            #nouvelleValeur <- as.integer(flags_left()) + 1
            #flags_left(nouvelleValeur)
            boutons$matrice_boutons[o, p] <- paste0(substr(boutons$matrice_boutons[o, p], 1, 1), "f")
          }
        } else {
          if (boutons$matrice_boutons[o, p] == "df" | boutons$matrice_boutons[o, p] == "lf"){
            
          } else if(update == "dr" | update == "lr"){
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
            delay(ms=500, expr={
              boutons$matrice_boutons[o, p] <- paste0(substr(boutons$matrice_boutons[o, p], 1, 1), "e")
              delay(ms=1000, expr={
                for (i in 1:row_number()) {
                  for (j in 1:column_number()) {
                    if (i != o | j != p) {
                      if (boutons$matrice_boutons[i, j] != "df" & boutons$matrice_boutons[i, j] != "lf")
                        boutons$matrice_boutons[i, j] <- updateButton(i=i, j=j, gridValues=matrice_valeurs)
                    }
                  }
                }
                lose <- callModule(module = endgame, id = "endgame")
              })
            })
            
          } else {
            boutons$matrice_boutons[o, p] <- update
          }
        } # else
      }) # observeEvent
    }) # lapply
    
    
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
          icon("bomb", lib="font-awesome"),
          flags_left()
        ),
        
        # Temps
        tags$div(
          style = "text-align: center; width: 33%;",
          time_UI("timer"),
        ),
      ),
    )
    
    output$parameters <- renderUI(
      
      tags$div(
        class = "top-container",
        style = "margin-top: 0;
                 border-top-left-radius: 0;
                 border-top-right-radius: 0;
                 border-top: 0;
                 background-color: #dadada;",
          
        tags$div(
          style = "width: 50%;"
        ),
        
        tags$div(
          style = "width: 50%;
                   text-align: right;
                   padding-right: 32px;
                   font-weight: bold;",
          switchInput(
            inputId = "flagMode",
            label = paste0(icon("flag", lib="font-awesome"), "  Flag"),
            value = variable_glo(),
            handleWidth = 80, 
            labelWidth = 80,
            width = "200px",
            size = "mini",
            onStatus = "success"
          )
        )
      )
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
    
    
    output$colorblindness <- renderUI(
      tags$div(
        style = "width: 100%;
                   text-align: left;
                   padding-left: 210px;
                   font-weight: bold;",
        
        materialSwitch(
          inputId = "colorblind",
          label = "Colorblind", 
          status = "success",
          value = colorbl()
        ),
      ),
    ) # renderUI output$colorblindness
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
    
    # Paramètres
    uiOutput("parameters"),
    
    # Jeu
    uiOutput("game")
  ),
  
  tags$br(),
  
)


# Lancement --------------------------------------------------------------------

shinyApp(ui = ui, server = server)