#' @title Tabulations thematiques standardisees
#' @name bs_tableaux
#' @description
#' Generation des tableaux statistiques standards d'un recensement (structure,
#' nuptialite, fecondite, mortalite, migration, handicap, genre). S'appuie sur le
#' moteur CensusAnalytics, le plus complet (calcul du SMAM, themes multiples).
NULL

#' Generer tous les tableaux standards
#' @param data data.frame des microdonnees.
#' @param config Liste de configuration (noms de variables, ponderation).
#' @param interpreter Logique : interpretation globale (defaut TRUE).
#' @return Liste de tableaux (objets gt/data.frame) avec attribut d'interpretation.
#' @export
bs_tableaux <- function(data, config = list(), interpreter = TRUE) {
  res <- generate_all_tables(data, config = config)
  if (isTRUE(interpreter))
    attr(res, "bs_interpretation") <- bs_interpreter(res, type = "tableaux")
  .baobabstats$last_results$tableaux <- res
  res
}

#' Generer un tableau thematique unique
#' @param data data.frame des microdonnees.
#' @param theme Un de : "structure", "pyramide", "nuptialite", "fecondite",
#'   "mortalite", "migration", "handicap", "genre".
#' @param ... Arguments transmis a la fonction moteur.
#' @return Le tableau demande.
#' @export
bs_tableau <- function(data, theme, ...) {
  theme <- match.arg(theme, c("structure", "pyramide", "nuptialite", "fecondite",
                              "mortalite", "migration", "handicap", "genre"))
  fn <- switch(theme,
    structure  = table_population_structure,
    pyramide   = table_age_pyramid,
    nuptialite = table_nuptiality,
    fecondite  = table_fertility,
    mortalite  = table_mortality,
    migration  = table_migration,
    handicap   = table_disability,
    genre      = table_gender
  )
  fn(data, ...)
}

#' Exporter des tableaux (xlsx, csv, html)
#' @param tableaux Liste de tableaux issue de \code{bs_tableaux()}.
#' @param dossier Dossier de sortie.
#' @param format Format : "xlsx" (defaut), "csv", "html".
#' @param prefixe Prefixe des fichiers.
#' @export
bs_exporter_tableaux <- function(tableaux, dossier = getOption("baobabstats.sortie"),
                                 format = "xlsx", prefixe = "baobabstats") {
  if (!dir.exists(dossier)) dir.create(dossier, recursive = TRUE)
  export_tables(tableaux, output_dir = dossier, format = format, prefix = prefixe)
}
