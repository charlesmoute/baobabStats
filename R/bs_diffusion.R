#' @title Diffusion des resultats
#' @name bs_diffusion
#' @description
#' Production automatisee de livrables : tableaux (xlsx), rapports de synthese et de
#' qualite (Word/HTML/PDF via le moteur CensusAnalytics et \pkg{rmarkdown}), et
#' export vers des formats d'echange statistique (SDMX, metadonnees DDI - concept
#' herite de statAfrikR).
NULL

#' Generer un rapport de synthese
#' @param data data.frame traite.
#' @param fichier Chemin de sortie (sans extension).
#' @param format "html" (defaut), "word" ou "pdf".
#' @param niveau "national" (defaut) ou variable d'unite administrative.
#' @param ... Arguments transmis au moteur de rapports.
#' @return Chemin du fichier produit (invisible).
#' @export
bs_rapport <- function(data, fichier = "rapport_baobabstats", format = "html",
                       niveau = "national", var_age = "age", var_sexe = "sexe", ...) {
  out <- paste0(fichier, switch(format, word = ".docx", pdf = ".pdf", ".html"))
  logo <- tryCatch(bs_logo("complet"), error = function(e) NULL)
  if (!is.null(logo) && !file.exists(logo)) logo <- NULL
  res <- tryCatch(
    generate_synthetic_report(data, output_file = out, output_format = format,
                              age_var = var_age, sex_var = var_sexe,
                              logo_path = logo, ...),
    error = function(e) {
      cli::cli_warn("Rapport via moteur indisponible ({conditionMessage(e)}). Generation d'un rapport minimal.")
      .bs_rapport_minimal(data, out, format)
    })
  cli::cli_alert_success("Rapport produit : {.path {out}}")
  invisible(out)
}

# Rapport minimal autonome (si le moteur complet echoue), aux couleurs de la charte
.bs_rapport_minimal <- function(data, out, format) {
  n <- nrow(data)
  col <- bs_couleurs()
  logo <- tryCatch(bs_logo("complet"), error = function(e) "")
  entete <- if (nzchar(logo) && file.exists(logo))
    sprintf("<img src='%s' alt='baobabStats' style='height:90px'/>\n\n", logo) else ""
  css <- sprintf(paste0(
    "<style>body{font-family:Georgia,serif;color:%s;max-width:820px;margin:24px auto;}",
    "h1{color:%s;border-bottom:3px solid %s;padding-bottom:6px;}",
    "h2{color:%s;} .bs-tag{color:%s;font-style:italic;} ",
    "table{border-collapse:collapse;} td,th{border:1px solid #E4D9BF;padding:6px 12px;}",
    "th{background:%s;color:%s;}</style>\n"),
    col[["encre"]], col[["ecorce"]], col[["or"]], col[["ecorce_moyen"]],
    col[["ecorce_moyen"]], col[["ecorce"]], col[["creme"]])
  lignes <- c(
    if (format == "html") css else "",
    if (format == "html") entete else "",
    "# Rapport de synthese baobabStats",
    "<span class='bs-tag'>Tools for Data \u2014 Rooted in Africa</span>", "",
    sprintf("- Effectif : %s enregistrements", format(n, big.mark = " ")),
    sprintf("- Variables : %d", ncol(data)),
    sprintf("- Genere le : %s", format(Sys.time(), "%Y-%m-%d %H:%M")), "")
  if ("age" %in% names(data))
    lignes <- c(lignes, sprintf("- Age moyen : %.1f ans", mean(as.numeric(data$age), na.rm = TRUE)))
  md <- sub("\\.[a-z]+$", ".md", out)
  writeLines(lignes, md)
  if (format == "html") {
    writeLines(c(css, entete, gsub("^# (.*)$", "<h1>\\1</h1>",
      gsub("^- (.*)$", "<li>\\1</li>", lignes))), sub("\\.[a-z]+$", ".html", out))
  } else if (requireNamespace("rmarkdown", quietly = TRUE)) {
    tryCatch(rmarkdown::render(md, output_file = basename(out), quiet = TRUE),
             error = function(e) NULL)
  }
  out
}

#' Exporter des donnees agregees au format SDMX-like (CSV + structure)
#' @param tableau data.frame agrege a exporter.
#' @param fichier Chemin de sortie (sans extension).
#' @param dimensions Noms des variables-dimensions.
#' @param mesure Nom de la variable de mesure.
#' @return Chemin du fichier ecrit (invisible).
#' @details Implementation legere (CSV structure + manifeste JSON de dimensions).
#'   Pour un SDMX-ML complet, brancher un package dedie en aval.
#' @export
bs_exporter_sdmx <- function(tableau, fichier = "export_sdmx",
                             dimensions = NULL, mesure = NULL) {
  csv <- paste0(fichier, ".csv")
  utils::write.csv(tableau, csv, row.names = FALSE, fileEncoding = "UTF-8")
  manifeste <- list(format = "SDMX-like", genere_par = "baobabStats 1.0.0",
                    dimensions = dimensions %||% setdiff(names(tableau), mesure),
                    mesure = mesure, date = as.character(Sys.Date()))
  jsonlite::write_json(manifeste, paste0(fichier, "_dsd.json"), auto_unbox = TRUE,
                       pretty = TRUE)
  cli::cli_alert_success("Export SDMX-like : {.path {csv}} (+ DSD JSON)")
  invisible(csv)
}

#' Produire les livrables decrits par une feuille de configuration
#' @param data data.frame traite.
#' @param resultats Liste de resultats du pipeline (pour les rapports de qualite).
#' @param diff data.frame issu de la feuille Diffusion.
#' @param dossier Dossier de sortie.
#' @return Liste nommee des livrables produits.
#' @export
bs_diffuser_config <- function(data, resultats, diff,
                               dossier = getOption("baobabstats.sortie")) {
  if (!dir.exists(dossier)) dir.create(dossier, recursive = TRUE)
  actif <- function(x) tolower(as.character(x)) %in% c("oui", "yes", "true", "1")
  sorties <- list()
  for (i in seq_len(nrow(diff))) {
    if (!actif(diff$Produire[i])) next
    liv <- diff$Livrable[i]; fmt <- diff$Format[i] %||% "xlsx"
    sorties[[liv]] <- tryCatch(switch(liv,
      tableaux = if (!is.null(resultats$sorties$tableaux))
        bs_exporter_tableaux(resultats$sorties$tableaux, dossier, format = fmt),
      rapport_synthese = bs_rapport(data, file.path(dossier, "rapport_synthese"), format = fmt),
      rapport_qualite  = bs_rapport(data, file.path(dossier, "rapport_qualite"), format = fmt),
      export_sdmx = bs_exporter_sdmx(
        if ("region" %in% names(data)) as.data.frame(table(data$region)) else as.data.frame(table(data[[1]])),
        file.path(dossier, "export_sdmx")),
      NULL), error = function(e) { cli::cli_warn("{liv} : {conditionMessage(e)}"); NULL })
  }
  sorties
}
