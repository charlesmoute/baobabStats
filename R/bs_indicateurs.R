#' @title Indicateurs demographiques
#' @name bs_indicateurs
#' @description
#' Interface unifiee pour le calcul des indicateurs demographiques (structure,
#' fecondite, mortalite, migration, education, emploi, handicap, genre). S'appuie
#' sur le moteur DemoStats. La fonction \code{bs_indicateur()} agit comme un
#' aiguilleur unique ; des alias francais directs sont egalement fournis.
NULL

# Table de routage : famille -> fonction moteur
.bs_indic_routes <- list(
  pyramide      = function(data, ...) age_pyramid(data, ...),
  rapport_masc  = function(data, ...) sex_ratio(data, ...),
  dependance    = function(data, ...) dependency_ratio(data, ...),
  age_median    = function(data, ...) median_age(data, ...),
  fecondite     = function(data, ...) fertility_rates(data, ...),
  mortalite     = function(data, ...) mortality_rates(data, ...),
  migration     = function(data, ...) migration_rates(data, ...),
  education     = function(data, ...) education_indicators(data, ...),
  emploi        = function(data, ...) employment_indicators(data, ...),
  handicap      = function(data, ...) disability_prevalence(data, ...),
  genre         = function(data, ...) gender_indicators(data, ...)
)

#' Calculer un indicateur demographique (aiguilleur)
#'
#' @param data data.frame des microdonnees.
#' @param famille Famille d'indicateur : "pyramide", "rapport_masc", "dependance",
#'   "age_median", "fecondite", "mortalite", "migration", "education", "emploi",
#'   "handicap", "genre".
#' @param ... Arguments transmis a la fonction moteur (noms de variables, ponderation).
#' @param interpreter Logique : ajoute une interpretation textuelle (defaut TRUE).
#' @return Le resultat de l'indicateur (structure list/data.frame selon la famille),
#'   avec attribut \code{bs_interpretation} si demande.
#' @export
bs_indicateur <- function(data, famille, ..., interpreter = TRUE) {
  famille <- match.arg(famille, names(.bs_indic_routes))
  res <- .bs_indic_routes[[famille]](data, ...)
  if (isTRUE(interpreter))
    attr(res, "bs_interpretation") <- bs_interpreter(res, type = paste0("indic_", famille))
  .baobabstats$last_results[[paste0("indic_", famille)]] <- res
  res
}

#' Calculer plusieurs familles d'indicateurs en une passe
#' @param data data.frame des microdonnees.
#' @param familles Vecteur de familles (defaut : toutes).
#' @param ... Arguments communs transmis aux moteurs.
#' @return Liste nommee de resultats.
#' @export
bs_indicateurs_lot <- function(data, familles = names(.bs_indic_routes), ...) {
  familles <- intersect(familles, names(.bs_indic_routes))
  stats::setNames(lapply(familles, function(f) {
    tryCatch(bs_indicateur(data, f, ..., interpreter = FALSE),
             error = function(e) structure(list(erreur = conditionMessage(e)),
                                           class = "bs_erreur"))
  }), familles)
}

# --- Alias francais directs (raccourcis pratiques) ---------------------------
#' @rdname bs_indicateurs
#' @export
bs_pyramide_ages   <- function(data, ...) age_pyramid(data, ...)
#' @rdname bs_indicateurs
#' @export
bs_rapport_masculinite <- function(data, ...) sex_ratio(data, ...)
#' @rdname bs_indicateurs
#' @export
bs_isf <- function(asfr, age_interval = 5) tfr(asfr, age_interval)  # indice synthetique de fecondite
#' @rdname bs_indicateurs
#' @export
bs_table_mortalite <- function(nMx, ...) life_table(nMx, ...)
#' @rdname bs_indicateurs
#' @export
bs_esperance_vie <- function(nMx, ...) {
  lt <- life_table(nMx, ...)
  lt
}
