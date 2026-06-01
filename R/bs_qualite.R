#' @title Qualite des donnees : controle intrinseque et a posteriori
#' @name bs_qualite
#' @description
#' Interface unifiee pour l'evaluation de la qualite. Deux familles :
#' \enumerate{
#'   \item \strong{Intrinseque} : indices d'attraction d'age (Whipple, Myers, Bachi),
#'         rapport de masculinite, completude et coherence (moteur CensusAnalytics).
#'   \item \strong{A posteriori} : controle de terrain par backcheck/bcstats
#'         (moteur DemoStats), appariement post-censitaire (PES), estimation par
#'         systeme dual (DSE) et derivation des coefficients de redressement.
#' }
NULL

#' Controle qualite intrinseque d'un jeu de donnees
#'
#' @param data data.frame des microdonnees individuelles.
#' @param var_age Nom de la variable age (defaut "age").
#' @param var_sexe Nom de la variable sexe (defaut "sexe").
#' @param var_admin Nom de la variable d'unite administrative (optionnel).
#' @param interpreter Logique : ajouter une interpretation textuelle (defaut TRUE).
#' @return Objet de classe \code{census_quality} enrichi (champ \code{interpretation}).
#' @export
bs_qualite_intrinseque <- function(data, var_age = "age", var_sexe = "sexe",
                                    var_admin = NULL, interpreter = TRUE) {
  # Le moteur CensusAnalytics attend "sex" par defaut : on tolere les deux.
  res <- assess_data_quality(data, age_var = var_age, sex_var = var_sexe,
                             admin_var = var_admin)
  if (isTRUE(interpreter))
    res$interpretation <- bs_interpreter(res, type = "qualite_intrinseque")
  .baobabstats$last_results$qualite_intrinseque <- res
  res
}

#' Indices d'attraction d'age (raccourcis)
#' @param age Vecteur numerique d'ages.
#' @return Valeur numerique (Whipple, Bachi) ou liste (Myers).
#' @rdname bs_indices_age
#' @export
bs_whipple <- function(age) whipple_index(age)
#' @rdname bs_indices_age
#' @export
bs_myers <- function(age) myers_blended_index(age)
#' @rdname bs_indices_age
#' @export
bs_bachi <- function(age) bachi_index(age)

#' Controle qualite a posteriori par backcheck (style bcstats)
#'
#' @param donnees_terrain Donnees de l'enquete principale.
#' @param donnees_backcheck Donnees du controle (re-interview).
#' @param id_var Identifiant unique commun.
#' @param enum_var Variable identifiant l'enqueteur (optionnel).
#' @param variables Variables a comparer. Raccourci : toutes traitees comme
#'   "type 2" (erreurs de mesure). Pour un controle fin, utiliser \code{type1_vars}
#'   (identification), \code{type2_vars} (mesure) et \code{type3_vars} (sensibles).
#' @param type1_vars,type2_vars,type3_vars Variables par categorie d'erreur.
#' @param okrange Liste nommee de plages tolerees par variable numerique.
#' @param interpreter Logique : ajouter une interpretation (defaut TRUE).
#' @return Objet \code{bcstats_result} enrichi.
#' @export
bs_qualite_backcheck <- function(donnees_terrain, donnees_backcheck,
                                 id_var, enum_var = NULL, variables = NULL,
                                 type1_vars = NULL, type2_vars = NULL,
                                 type3_vars = NULL, okrange = NULL,
                                 interpreter = TRUE) {
  if (!is.null(variables) && is.null(c(type1_vars, type2_vars, type3_vars)))
    type2_vars <- variables
  res <- bcstats(survey_data = donnees_terrain, backcheck_data = donnees_backcheck,
                 id_var = id_var, enum_var = enum_var,
                 type1_vars = type1_vars, type2_vars = type2_vars,
                 type3_vars = type3_vars, okrange = okrange)
  if (isTRUE(interpreter))
    res$interpretation <- bs_interpreter(res, type = "backcheck")
  .baobabstats$last_results$backcheck <- res
  res
}

#' Appariement post-censitaire (PES)
#'
#' @param donnees_epc Donnees de l'enquete post-censitaire.
#' @param donnees_recensement Donnees du recensement.
#' @param config Liste de configuration (variables d'appariement, blocage, methode).
#' @return Objet \code{pes_match_result}.
#' @details Methodes : "deterministic", "probabilistic", "hybrid" (defaut hybride
#'   via \code{config$method}). S'appuie sur le moteur DemoStats.
#' @export
bs_apparier_pes <- function(donnees_epc, donnees_recensement, config = list()) {
  res <- pes_match(pes_data = donnees_epc, census_data = donnees_recensement,
                   config = config)
  .baobabstats$last_results$pes <- res
  res
}

#' Estimation par systeme dual (DSE)
#'
#' @param match_result Resultat d'appariement issu de \code{bs_apparier_pes()}.
#' @param n_pes,n_recensement,n_apparies,n_errones Alternative : comptages directs
#'   si aucun objet d'appariement n'est disponible.
#' @param interpreter Logique : ajouter une interpretation (defaut TRUE).
#' @return Objet \code{dse_result} (population estimee, taux de couverture/omission...).
#' @export
bs_estimer_dse <- function(match_result = NULL,
                           n_pes = NULL, n_recensement = NULL,
                           n_apparies = NULL, n_errones = 0,
                           interpreter = TRUE) {
  if (!is.null(match_result)) {
    res <- dse_estimate(match_result = match_result)
  } else {
    stopifnot(!is.null(n_pes), !is.null(n_recensement), !is.null(n_apparies))
    res <- dse_calculate(n_pes = n_pes, n_census = n_recensement,
                         n_matched = n_apparies, n_erroneous = n_errones)
    class(res) <- c("dse_result", "list")
  }
  if (isTRUE(interpreter))
    res$interpretation <- bs_interpreter(res, type = "dse")
  .baobabstats$last_results$dse <- res
  res
}

#' Derivation des coefficients de redressement / ajustement
#'
#' @description
#' A partir d'une estimation DSE (ou de comptages), calcule les taux cles et les
#' \strong{coefficients de redressement} a appliquer aux effectifs du recensement
#' pour corriger le sous-denombrement. Peut etre calcule par strate.
#'
#' @param dse Objet \code{dse_result} (issu de \code{bs_estimer_dse()}), optionnel.
#' @param strates data.frame optionnel a colonnes \code{strate}, \code{n_pes},
#'   \code{n_recensement}, \code{n_apparies}, \code{n_errones} pour un calcul stratifie.
#' @param plafond Plafond de securite sur le coefficient (defaut 3) pour eviter des
#'   ajustements extremes dus a de petits effectifs.
#' @return Un \code{tibble} : strate (ou "ensemble"), taux d'omission, taux de
#'   couverture, taux d'inclusion erronee, population corrigee et
#'   \code{coef_redressement} = population_corrigee / effectif_recensement.
#' @export
bs_coefficients_redressement <- function(dse = NULL, strates = NULL, plafond = 3) {
  calc_un <- function(n_pes, n_rec, n_app, n_err, etiquette = "ensemble") {
    d <- dse_calculate(n_pes, n_rec, n_app, n_err)
    coef <- if (is.na(d$true_population) || n_rec == 0) NA_real_ else d$true_population / n_rec
    coef <- pmin(coef, plafond)
    tibble::tibble(
      strate              = etiquette,
      n_recensement       = n_rec,
      population_corrigee = round(d$true_population),
      taux_omission       = round(d$omission_rate, 2),
      taux_couverture     = round(d$coverage_rate, 2),
      taux_inclusion_err  = round(d$erroneous_inclusion_rate, 2),
      coef_redressement   = round(coef, 4)
    )
  }

  if (!is.null(strates)) {
    req <- c("strate", "n_pes", "n_recensement", "n_apparies")
    if (!all(req %in% names(strates)))
      cli::cli_abort("Colonnes requises : {.val {req}}.")
    if (is.null(strates$n_errones)) strates$n_errones <- 0
    res <- purrr::pmap_dfr(strates, function(strate, n_pes, n_recensement,
                                            n_apparies, n_errones = 0, ...) {
      calc_un(n_pes, n_recensement, n_apparies, n_errones, strate)
    })
  } else if (!is.null(dse)) {
    res <- calc_un(dse$n_pes, dse$n_census, dse$n_matched, dse$n_erroneous)
  } else {
    cli::cli_abort("Fournir soit {.arg dse}, soit {.arg strates}.")
  }
  attr(res, "bs_interpretation") <- bs_interpreter(res, type = "redressement")
  class(res) <- c("bs_redressement", class(res))
  .baobabstats$last_results$redressement <- res
  res
}

#' Appliquer les coefficients de redressement a une table d'effectifs
#'
#' @param effectifs data.frame avec une colonne strate et une colonne d'effectifs.
#' @param coefficients Resultat de \code{bs_coefficients_redressement()}.
#' @param var_strate Nom de la colonne strate dans \code{effectifs}.
#' @param var_effectif Nom de la colonne d'effectifs a redresser.
#' @return \code{effectifs} avec une colonne \code{<var>_redresse}.
#' @export
bs_appliquer_redressement <- function(effectifs, coefficients,
                                      var_strate, var_effectif) {
  cle <- coefficients[, c("strate", "coef_redressement")]
  m <- merge(effectifs, cle, by.x = var_strate, by.y = "strate", all.x = TRUE)
  m[[paste0(var_effectif, "_redresse")]] <- round(m[[var_effectif]] * m$coef_redressement)
  tibble::as_tibble(m)
}
