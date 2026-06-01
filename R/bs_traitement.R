#' @title Traitement : nettoyage, imputation, dedoublonnage
#' @name bs_traitement
#' @description
#' Interface unifiee pour la preparation des donnees (moteur CensusAnalytics) :
#' standardisation des toponymes, imputation (MICE, missForest), application de
#' contraintes demographiques et detection de doublons par apprentissage. Toutes
#' les transformations sont tracees (journal recuperable via
#' \code{bs_journal_transformations()}).
NULL

#' Nettoyer un jeu de donnees de recensement
#' @param data data.frame brut.
#' @param var_toponymes Variables toponymiques a standardiser (optionnel).
#' @param methode_imputation "auto", "mice", "missforest" ou NULL (pas d'imputation).
#' @param ... Arguments transmis a \code{clean_census_data()}.
#' @return Objet \code{census_cleaning_result} (donnees nettoyees + journal).
#' @export
bs_nettoyer <- function(data, var_toponymes = NULL, methode_imputation = "auto", ...) {
  res <- clean_census_data(data, toponym_vars = var_toponymes,
                           imputation_method = methode_imputation, ...)
  .baobabstats$last_results$nettoyage <- res
  res
}

#' Imputer les valeurs manquantes
#' @param data data.frame.
#' @param methode "auto", "mice" ou "missforest".
#' @param variables Variables a imputer (defaut : celles comportant des NA).
#' @param ... Arguments transmis au moteur d'imputation.
#' @return data.frame impute.
#' @export
bs_imputer <- function(data, methode = "auto", variables = NULL, ...) {
  if (is.null(variables)) {
    impute_missing(data, method = methode, ...)
  } else if (methode == "missforest") {
    impute_missforest(data, vars_to_impute = variables, ...)
  } else {
    impute_mice(data, vars_to_impute = variables, ...)
  }
}

#' Detecter les doublons (apprentissage non supervise)
#' @param data data.frame.
#' @param cles Variables-cles d'identification (optionnel).
#' @param var_blocage Variable de blocage pour reduire la combinatoire.
#' @param ... Arguments transmis a \code{detect_duplicates()}.
#' @return Objet \code{census_duplicates}.
#' @export
bs_detecter_doublons <- function(data, cles = NULL, var_blocage = NULL, ...) {
  detect_duplicates(data, key_vars = cles, blocking_var = var_blocage, ...)
}

#' Recuperer le journal des transformations
#' @param sous_forme_df Logique : renvoyer un data.frame (defaut TRUE).
#' @export
bs_journal_transformations <- function(sous_forme_df = TRUE) {
  get_transformation_log(as_dataframe = sous_forme_df)
}


#' @title Projections de population
#' @name bs_projections
#' @description
#' Projections par la methode des composantes par cohorte (classique) et par
#' microsimulation pour les niveaux administratifs fins (moteur CensusAnalytics).
#' Le rapport de Leslie et le taux intrinseque (moteur DemoStats) sont accessibles
#' via \code{bs_matrice_leslie()}.
NULL

#' Projeter une population
#' @param data Population de base (microdonnees ou effectifs par age/sexe).
#' @param methode "cohort" (composantes par cohorte) ou "microsimulation".
#' @param annees Horizon de projection en annees (defaut 25).
#' @param ... Arguments transmis a \code{project_population()} (moteur CensusAnalytics).
#' @param interpreter Logique : interpretation (defaut TRUE).
#' @return Objet \code{census_projection} ou \code{census_microsimulation}.
#' @export
bs_projeter_population <- function(data, methode = c("cohort", "microsimulation"),
                                   annees = 25, ..., interpreter = TRUE) {
  methode <- match.arg(methode)
  res <- project_population(data, method = methode, years = annees, ...)
  if (isTRUE(interpreter))
    attr(res, "bs_interpretation") <- bs_interpreter(res, type = "projection")
  .baobabstats$last_results$projection <- res
  res
}

#' Construire des scenarios de projection (haute, moyenne, basse fecondite)
#' @param ... Arguments transmis a \code{create_projection_scenarios()}.
#' @export
bs_scenarios_projection <- function(...) create_projection_scenarios(...)

#' Matrice de Leslie et croissance intrinseque (moteur DemoStats)
#' @param survie Ratios de survie par groupe d'age.
#' @param fecondite Taux de fecondite par groupe d'age.
#' @param ... Arguments transmis a \code{leslie_matrix()}.
#' @return Matrice de Leslie.
#' @export
bs_matrice_leslie <- function(survie, fecondite, ...) {
  leslie_matrix(survival = survie, fertility = fecondite, ...)
}
