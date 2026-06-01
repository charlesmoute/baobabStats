#' @title Application Shiny et utilitaires
#' @name bs_app
#' @description Lancement de l'application Shiny unifiee et fonctions utilitaires.
NULL

#' Lancer l'application Shiny baobabStats
#' @param launch.browser Logique : ouvrir dans le navigateur (defaut TRUE).
#' @param port Port (optionnel). @param host Hote (defaut 127.0.0.1).
#' @param ... Arguments transmis a \code{shiny::runApp()}.
#' @export
bs_app <- function(launch.browser = TRUE, port = NULL, host = "127.0.0.1", ...) {
  if (!requireNamespace("shiny", quietly = TRUE))
    cli::cli_abort("Installez {.pkg shiny} : install.packages('shiny')")
  app_dir <- system.file("shiny", package = "baobabStats")
  if (app_dir == "") app_dir <- file.path("inst", "shiny")  # mode developpement
  if (!dir.exists(app_dir)) cli::cli_abort("Application Shiny introuvable.")
  cli::cli_alert_info("Lancement de l'application baobabStats...")
  shiny::runApp(app_dir, launch.browser = launch.browser, port = port,
                host = host, ...)
}

#' Catalogue des fonctions de la suite
#' @return Un \code{tibble} : etape, fonction, description.
#' @export
bs_catalogue <- function() {
  tibble::tribble(
    ~etape,          ~fonction,                       ~description,
    "Collecte",      "bs_collecter",                  "Importer (csv/xlsx/sav/dta/json)",
    "Collecte",      "bs_collecter_cspro/kobo/odk",   "Importer depuis plateformes de collecte",
    "Collecte",      "bs_controler_na",               "Diagnostic des valeurs manquantes",
    "Traitement",    "bs_harmoniser_regions",         "Harmoniser les libelles de region",
    "Traitement",    "bs_nettoyer / bs_imputer",      "Nettoyage et imputation",
    "Traitement",    "bs_detecter_doublons",          "Detection de doublons (ML)",
    "Qualite",       "bs_qualite_intrinseque",        "Whipple, Myers, Bachi, masculinite",
    "Qualite",       "bs_qualite_backcheck",          "Controle de terrain (bcstats)",
    "Qualite",       "bs_apparier_pes / bs_estimer_dse", "PES + systeme dual (couverture/omission)",
    "Qualite",       "bs_coefficients_redressement",  "Coefficients de redressement par strate",
    "Analyse",       "bs_indicateur(s)",              "Indicateurs demographiques",
    "Analyse",       "bs_tableau(x)",                 "Tabulations thematiques",
    "Projection",    "bs_projeter_population",        "Composantes par cohorte / microsimulation",
    "Visualisation", "bs_graph_pyramide / bs_graph_barres", "Graphiques publication (PNG/HTML/PDF)",
    "Diffusion",     "bs_rapport / bs_exporter_sdmx", "Rapports Word/HTML/PDF, export SDMX/DDI",
    "Innovation",    "bs_interpreter",                "Interpretation dynamique des resultats",
    "Innovation",    "bs_prompt",                     "Generation de prompts pour l'IA",
    "Automation",    "bs_config_modele / bs_pipeline","Pilotage par fichier Excel"
  )
}

# operateur null-coalescent (defini aussi ailleurs, garde local pour robustesse)
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a


#' @title Modules complementaires RStudio (addins)
#' @name bs_addins
#' @description
#' baobabStats installe des \emph{addins} RStudio accessibles depuis le menu
#' \emph{Addins} : lancer l'application, creer un modele de configuration, executer
#' un pipeline, interpreter le dernier resultat et generer un prompt. Ils sont
#' declares dans \code{inst/rstudio/addins.dcf} et donc disponibles des
#' l'installation du package.
NULL

#' Addin : lancer l'application Shiny
#' @export
bsin_lancer_app <- function() bs_app()

#' Addin : creer un modele de configuration Excel
#' @export
bsin_creer_config <- function() {
  chemin <- file.path(getwd(), "baobabstats_config.xlsx")
  bs_config_modele(chemin)
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())
    rstudioapi::navigateToFile(chemin)
}

#' Addin : executer un pipeline a partir d'un classeur de configuration
#' @export
bsin_executer_pipeline <- function() {
  if (!(requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())) {
    cli::cli_alert_info("Utiliser : bs_pipeline('baobabstats_config.xlsx')"); return(invisible())
  }
  chemin <- rstudioapi::selectFile(caption = "Choisir le classeur de configuration",
                                   filter = "Excel (*.xlsx)")
  if (!is.null(chemin)) bs_pipeline(chemin)
}

#' Addin : interpreter le dernier resultat calcule
#' @export
bsin_interpreter_dernier <- function() {
  lr <- .baobabstats$last_results
  if (length(lr) == 0) { cli::cli_alert_warning("Aucun resultat recent."); return(invisible()) }
  dernier <- lr[[length(lr)]]
  interp <- bs_interpreter(dernier)
  print(interp)
  invisible(interp)
}

#' Addin : generer un prompt pour le dernier resultat et l'inserer dans l'editeur
#' @export
bsin_prompt_dernier <- function() {
  lr <- .baobabstats$last_results
  if (length(lr) == 0) { cli::cli_alert_warning("Aucun resultat recent."); return(invisible()) }
  p <- bs_prompt(lr[[length(lr)]])
  bs_prompt_copier(p)
  invisible(p)
}
