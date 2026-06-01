# ============================================================================
# baobabStats - Application Shiny unifiee
# Recensements & enquetes en Afrique : collecte -> diffusion
# ============================================================================

library(shiny)
suppressWarnings({
  has_dash <- requireNamespace("shinydashboard", quietly = TRUE)
  has_dt   <- requireNamespace("DT", quietly = TRUE)
})
if (has_dash) library(shinydashboard)
if (has_dt)   library(DT)

# Charge le package s'il est installe ; sinon mode developpement (source des R/)
if (!requireNamespace("baobabStats", quietly = TRUE)) {
  rfiles <- list.files(file.path("..", "..", "R"), pattern = "\\.R$", full.names = TRUE)
  invisible(lapply(rfiles, function(f) try(source(f), silent = TRUE)))
} else {
  library(baobabStats)
}

# ----------------------------------------------------------------------------
# UI
# ----------------------------------------------------------------------------
sidebar <- dashboardSidebar(
  div(class = "bs-sidebar-logo",
      tags$img(src = "logo_emblem.png", alt = "baobabStats"),
      div(class = "bs-tagline", "Tools for Data — Rooted in Africa")),
  sidebarMenu(
    id = "onglet",
    menuItem("Accueil", tabName = "accueil", icon = icon("house")),
    menuItem("1. Collecte", tabName = "collecte", icon = icon("file-import")),
    menuItem("2. Traitement", tabName = "traitement", icon = icon("broom")),
    menuItem("3. Qualite", tabName = "qualite", icon = icon("circle-check")),
    menuItem("4. Indicateurs & tableaux", tabName = "analyse", icon = icon("table")),
    menuItem("5. Projections", tabName = "projection", icon = icon("chart-line")),
    menuItem("6. Visualisation", tabName = "viz", icon = icon("chart-column")),
    menuItem("7. Diffusion", tabName = "diffusion", icon = icon("share-nodes")),
    menuItem("Interpretation & IA", tabName = "interpretation", icon = icon("wand-magic-sparkles")),
    menuItem("Pilotage Excel", tabName = "config", icon = icon("file-excel"))
  )
)

body <- dashboardBody(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  tabItems(
    tabItem("accueil",
      div(class = "bs-hero",
        h1("baobabStats"),
        h4("Suite integree R pour les recensements et enquetes en Afrique"),
        p("Tools for Data \u2014 Rooted in Africa. De la collecte de terrain a la diffusion des resultats."),
        fluidRow(
          valueBox("3 -> 1", "Moteurs unifies", icon = icon("layer-group"), color = "teal"),
          valueBox("7", "Etapes du cycle", icon = icon("route"), color = "olive"),
          valueBox("Excel", "Pilotage sans code", icon = icon("file-excel"), color = "orange")
        )),
      box(width = 12, title = "Demarrage", status = "primary", solidHeader = TRUE,
        tags$ol(
          tags$li("Importez vos donnees dans l'onglet Collecte."),
          tags$li("Lancez le controle qualite (intrinseque et a posteriori)."),
          tags$li("Calculez indicateurs, tableaux et projections."),
          tags$li("Visualisez, interpretez puis diffusez."),
          tags$li("Ou pilotez tout via un fichier Excel (onglet Pilotage)."))
      )
    ),

    tabItem("collecte",
      box(width = 4, title = "Importer des donnees", status = "primary", solidHeader = TRUE,
        selectInput("src", "Source", c("Fichier (csv/xlsx/sav/dta)" = "fichier",
                                       "CSPro" = "cspro", "KoboToolbox" = "kobo", "ODK" = "odk")),
        fileInput("fichier", "Fichier de donnees", accept = c(".csv", ".xlsx", ".sav", ".dta", ".json")),
        actionButton("btn_import", "Importer", icon = icon("upload"), class = "btn-bs")),
      box(width = 8, title = "Apercu et diagnostic des manquants", status = "info",
        if (has_dt) DT::DTOutput("apercu") else tableOutput("apercu_simple"),
        hr(), h5("Valeurs manquantes (> seuil)"),
        if (has_dt) DT::DTOutput("na_tab") else tableOutput("na_simple"))
    ),

    tabItem("traitement",
      box(width = 4, title = "Options de traitement", status = "primary", solidHeader = TRUE,
        textInput("pays", "Code pays", value = "CM"),
        checkboxInput("harm", "Harmoniser les regions", TRUE),
        checkboxInput("imp", "Imputer les manquants", TRUE),
        checkboxInput("dup", "Detecter les doublons", TRUE),
        actionButton("btn_trait", "Lancer le traitement", icon = icon("play"), class = "btn-bs")),
      box(width = 8, title = "Journal des transformations", status = "info",
        verbatimTextOutput("journal"))
    ),

    tabItem("qualite",
      tabBox(width = 12,
        tabPanel("Intrinseque",
          actionButton("btn_qi", "Evaluer (Whipple, Myers, Bachi, masculinite)",
                       icon = icon("magnifying-glass-chart"), class = "btn-bs"),
          br(), br(), valueBoxOutput("vb_score", width = 4),
          valueBoxOutput("vb_whipple", width = 4), valueBoxOutput("vb_myers", width = 4),
          box(width = 12, title = "Interpretation dynamique", status = "success",
              verbatimTextOutput("interp_qi"))),
        tabPanel("Backcheck",
          p("Chargez les donnees de re-interview puis comparez."),
          fileInput("f_bc", "Donnees backcheck"),
          textInput("id_bc", "Variable identifiant", "id"),
          actionButton("btn_bc", "Evaluer le terrain", class = "btn-bs"),
          verbatimTextOutput("interp_bc")),
        tabPanel("PES / DSE & redressement",
          fluidRow(
            column(3, numericInput("n_pes", "n PES", 5000)),
            column(3, numericInput("n_rec", "n Recensement", 48000)),
            column(3, numericInput("n_app", "n Apparies", 4600)),
            column(3, numericInput("n_err", "n Errones", 200))),
          actionButton("btn_dse", "Estimer la couverture", icon = icon("calculator"), class = "btn-bs"),
          br(), br(),
          fluidRow(valueBoxOutput("vb_omission", 4), valueBoxOutput("vb_couv", 4),
                   valueBoxOutput("vb_coef", 4)),
          box(width = 12, title = "Interpretation", status = "success",
              verbatimTextOutput("interp_dse")))
      )
    ),

    tabItem("analyse",
      box(width = 4, title = "Indicateurs", status = "primary", solidHeader = TRUE,
        selectInput("famille", "Famille",
          c("Pyramide" = "pyramide", "Rapport de masculinite" = "rapport_masc",
            "Dependance" = "dependance", "Age median" = "age_median",
            "Fecondite" = "fecondite", "Mortalite" = "mortalite",
            "Education" = "education", "Emploi" = "emploi",
            "Handicap" = "handicap", "Genre" = "genre")),
        actionButton("btn_indic", "Calculer", class = "btn-bs")),
      box(width = 8, title = "Resultat", status = "info",
        verbatimTextOutput("res_indic"))
    ),

    tabItem("projection",
      box(width = 4, title = "Parametres", status = "primary", solidHeader = TRUE,
        selectInput("meth_proj", "Methode", c("Composantes par cohorte" = "cohort",
                                              "Microsimulation" = "microsimulation")),
        numericInput("annees", "Horizon (annees)", 25, min = 5, max = 50),
        actionButton("btn_proj", "Projeter", class = "btn-bs")),
      box(width = 8, title = "Resultat", status = "info", verbatimTextOutput("res_proj"))
    ),

    tabItem("viz",
      box(width = 12, title = "Pyramide des ages", status = "primary", solidHeader = TRUE,
        plotOutput("plot_pyr", height = 420),
        downloadButton("dl_pyr", "Telecharger (PNG)", class = "btn-bs"))
    ),

    tabItem("diffusion",
      box(width = 6, title = "Rapport de synthese", status = "primary", solidHeader = TRUE,
        selectInput("fmt_rap", "Format", c("HTML" = "html", "Word" = "word", "PDF" = "pdf")),
        downloadButton("dl_rapport", "Generer le rapport", class = "btn-bs")),
      box(width = 6, title = "Tableaux", status = "info",
        downloadButton("dl_tab", "Exporter les tableaux (xlsx)", class = "btn-bs"))
    ),

    tabItem("interpretation",
      box(width = 6, title = "Interpretation dynamique", status = "success", solidHeader = TRUE,
        p("Synthese automatique du dernier resultat calcule."),
        actionButton("btn_interp", "Interpreter", icon = icon("comment-dots"), class = "btn-bs"),
        verbatimTextOutput("interp_glob")),
      box(width = 6, title = "Prompt pour l'IA", status = "warning", solidHeader = TRUE,
        selectInput("public", "Public", c("Technique" = "technique",
                                         "Decideur" = "decideur", "Grand public" = "grand_public")),
        actionButton("btn_prompt", "Generer le prompt", icon = icon("robot"), class = "btn-bs"),
        verbatimTextOutput("prompt_out"))
    ),

    tabItem("config",
      box(width = 12, title = "Pilotage par fichier Excel", status = "primary", solidHeader = TRUE,
        p("Telechargez un modele, renseignez-le, puis re-importez-le pour executer le pipeline complet."),
        downloadButton("dl_modele", "Telecharger le modele de configuration", class = "btn-bs"),
        hr(),
        fileInput("f_cfg", "Importer une configuration renseignee (.xlsx)"),
        actionButton("btn_pipe", "Executer le pipeline", icon = icon("gears"), class = "btn-bs"),
        verbatimTextOutput("pipe_out"))
    )
  )
)

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "baobabStats"),
  sidebar, body
)

# ----------------------------------------------------------------------------
# SERVER
# ----------------------------------------------------------------------------
server <- function(input, output, session) {
  rv <- reactiveValues(data = NULL, dse = NULL, qi = NULL, bc = NULL,
                       indic = NULL, dernier = NULL)

  # --- Collecte ---
  observeEvent(input$btn_import, {
    req(input$fichier)
    rv$data <- tryCatch(bs_collecter(input$fichier$datapath),
                        error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    if (!is.null(rv$data)) showNotification("Donnees importees.", type = "message")
  })
  output$apercu <- DT::renderDT({ req(rv$data); DT::datatable(utils::head(rv$data, 50),
                                  options = list(scrollX = TRUE, pageLength = 5)) })
  output$na_tab <- DT::renderDT({ req(rv$data); DT::datatable(bs_controler_na(rv$data)) })
  output$apercu_simple <- renderTable({ req(rv$data); utils::head(rv$data, 10) })
  output$na_simple <- renderTable({ req(rv$data); bs_controler_na(rv$data) })

  # --- Traitement ---
  observeEvent(input$btn_trait, {
    req(rv$data)
    d <- rv$data
    if (input$harm && "region" %in% names(d))
      d <- tryCatch(bs_harmoniser_regions(d, "region", code_pays = input$pays),
                    error = function(e) d)
    rv$data <- d
    showNotification("Traitement applique.", type = "message")
  })
  output$journal <- renderPrint({
    req(rv$data); tryCatch(bs_journal_transformations(), error = function(e) "Aucun journal.")
  })

  # --- Qualite intrinseque ---
  observeEvent(input$btn_qi, {
    req(rv$data)
    rv$qi <- tryCatch(bs_qualite_intrinseque(rv$data), error = function(e) {
      showNotification(conditionMessage(e), type = "error"); NULL })
    rv$dernier <- rv$qi
  })
  output$vb_score <- renderValueBox({ req(rv$qi)
    valueBox(round(rv$qi$global_score %||% NA, 1), "Score global /100", color = "green") })
  output$vb_whipple <- renderValueBox({ req(rv$qi)
    valueBox(round(rv$qi$age_quality$whipple_combined %||% NA), "Indice de Whipple", color = "teal") })
  output$vb_myers <- renderValueBox({ req(rv$qi)
    valueBox(round(rv$qi$age_quality$myers %||% NA, 1), "Indice de Myers", color = "olive") })
  output$interp_qi <- renderPrint({ req(rv$qi); cat(rv$qi$interpretation %||% "-", sep = "\n") })

  # --- Backcheck ---
  observeEvent(input$btn_bc, {
    req(rv$data, input$f_bc)
    bc <- tryCatch(bs_collecter(input$f_bc$datapath), error = function(e) NULL)
    req(bc)
    rv$bc <- tryCatch(bs_qualite_backcheck(rv$data, bc, id_var = input$id_bc),
                      error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    rv$dernier <- rv$bc
  })
  output$interp_bc <- renderPrint({ req(rv$bc); cat(rv$bc$interpretation %||% "-", sep = "\n") })

  # --- DSE ---
  observeEvent(input$btn_dse, {
    rv$dse <- bs_estimer_dse(n_pes = input$n_pes, n_recensement = input$n_rec,
                             n_apparies = input$n_app, n_errones = input$n_err)
    rv$dernier <- rv$dse
  })
  output$vb_omission <- renderValueBox({ req(rv$dse)
    valueBox(paste0(round(rv$dse$omission_rate, 1), "%"), "Taux d'omission", color = "red") })
  output$vb_couv <- renderValueBox({ req(rv$dse)
    valueBox(paste0(round(rv$dse$coverage_rate, 1), "%"), "Couverture", color = "green") })
  output$vb_coef <- renderValueBox({ req(rv$dse)
    coef <- round(rv$dse$true_population / rv$dse$n_census, 3)
    valueBox(coef, "Coef. redressement", color = "orange") })
  output$interp_dse <- renderPrint({ req(rv$dse); cat(rv$dse$interpretation %||% "-", sep = "\n") })

  # --- Indicateurs ---
  observeEvent(input$btn_indic, {
    req(rv$data)
    rv$indic <- tryCatch(bs_indicateur(rv$data, input$famille),
                         error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    rv$dernier <- rv$indic
  })
  output$res_indic <- renderPrint({ req(rv$indic); print(rv$indic) })

  # --- Projection ---
  observeEvent(input$btn_proj, {
    req(rv$data)
    res <- tryCatch(bs_projeter_population(rv$data, methode = input$meth_proj, annees = input$annees),
                    error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    rv$dernier <- res
    output$res_proj <- renderPrint({ req(res); print(res) })
  })

  # --- Visualisation ---
  pyr <- reactive({ req(rv$data); bs_graph_pyramide(rv$data) })
  output$plot_pyr <- renderPlot({ pyr() })
  output$dl_pyr <- downloadHandler(
    filename = "pyramide_ages.png",
    content = function(file) ggplot2::ggsave(file, pyr(), width = 8, height = 6, dpi = 300))

  # --- Diffusion ---
  output$dl_rapport <- downloadHandler(
    filename = function() paste0("rapport_baobabstats.",
      switch(input$fmt_rap, word = "docx", pdf = "pdf", "html")),
    content = function(file) {
      out <- bs_rapport(rv$data, tempfile(), format = input$fmt_rap)
      file.copy(out, file) })
  output$dl_tab <- downloadHandler(
    filename = "tableaux_baobabstats.xlsx",
    content = function(file) {
      tb <- bs_tableaux(rv$data, interpreter = FALSE)
      d <- tempdir(); bs_exporter_tableaux(tb, d, "xlsx")
      f <- list.files(d, pattern = "\\.xlsx$", full.names = TRUE)[1]
      file.copy(f, file) })

  # --- Interpretation & prompt ---
  observeEvent(input$btn_interp, {
    req(rv$dernier)
    output$interp_glob <- renderPrint({ cat(bs_interpreter(rv$dernier), sep = "\n") })
  })
  observeEvent(input$btn_prompt, {
    req(rv$dernier)
    output$prompt_out <- renderPrint({ cat(bs_prompt(rv$dernier, public = input$public)) })
  })

  # --- Configuration Excel ---
  output$dl_modele <- downloadHandler(
    filename = "baobabstats_config.xlsx",
    content = function(file) bs_config_modele(file))
  observeEvent(input$btn_pipe, {
    req(input$f_cfg)
    output$pipe_out <- renderPrint({
      tryCatch({ res <- bs_pipeline(input$f_cfg$datapath); print(res) },
               error = function(e) cat("Erreur :", conditionMessage(e)))
    })
  })

  `%||%` <- function(a, b) if (is.null(a)) b else a
}

shinyApp(ui, server)
