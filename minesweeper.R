
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
  
    boutons_l_paire <- rep(c("lg", "dg"), column_number()/2)
    boutons_l_impai <- rep(c("dg", "lg"), column_number()/2)
    
    matrice_boutons <- matrix(rep(c(boutons_l_paire, boutons_l_impai), row_number()/2), 
                              ncol=column_number(), 
                              nrow=row_number(), 
                              byrow=TRUE)
    
    print(matrice_boutons)
    
    len_mat_jumps <- length(matrice_boutons) + row_number()
    
    
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
                                        "-",
                                        i %% (column_number() + 1)),
                       label = get(matrice_boutons[i %/% (column_number() + 1) + 1, i %% (column_number() + 1)]),
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
} # server



# UI

ui <- fluidPage(
  
  tags$head(
    tags$link(href="styles.css", rel="stylesheet", type="text/css"),
    tags$script(src = "message-handler.js"),
    tags$style(type="text/css", "div {white-space: nowrap;}")
  ),
  
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
  
  tags$br()
)


# Lancement --------------------------------------------------------------------

shinyApp(ui = ui, server = server)