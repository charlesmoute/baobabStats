#' @title Interpretation dynamique des resultats
#' @name bs_interpretation
#' @description
#' Moteur d'interpretation qui transforme un objet de resultats baobabStats en un
#' commentaire narratif en francais, calibre sur les seuils de reference de la
#' demographie (Nations Unies, manuels de recensement). L'interpretation est
#' \emph{dynamique} : elle compare les valeurs observees aux seuils et adapte le
#' verdict (excellent / acceptable / a corriger) ainsi que les recommandations.
#'
#' La fonction \code{bs_interpreter()} est appelee automatiquement par la plupart
#' des fonctions d'analyse ; elle peut aussi etre invoquee directement.
NULL

# Petite fabrique de verdict a partir de seuils ordonnes -----------------------
.bs_verdict <- function(x, seuils, etiquettes) {
  if (is.na(x)) return(NA_character_)
  etiquettes[findInterval(x, seuils) + 1L]
}

#' Interpreter un objet de resultats
#'
#' @param objet Resultat produit par une fonction baobabStats.
#' @param type Type d'interpretation (defaut : detection automatique via la classe).
#' @param format "texte" (vecteur de phrases, defaut) ou "liste" (structure).
#' @return Un objet \code{bs_interpretation} (vecteur de phrases) ou une liste.
#' @export
bs_interpreter <- function(objet, type = NULL, format = c("texte", "liste")) {
  format <- match.arg(format)
  if (is.null(type)) type <- .bs_detecter_type(objet)
  fn <- switch(type,
    qualite_intrinseque = .bs_interp_qualite,
    backcheck           = .bs_interp_backcheck,
    dse                 = .bs_interp_dse,
    redressement        = .bs_interp_redressement,
    projection          = .bs_interp_projection,
    .bs_interp_generique
  )
  res <- tryCatch(fn(objet), error = function(e)
    sprintf("Interpretation indisponible pour ce resultat (%s).", conditionMessage(e)))
  if (grepl("^indic_", type)) res <- .bs_interp_indicateur(objet, type)
  if (identical(format, "liste")) return(as.list(res))
  structure(res, class = "bs_interpretation")
}

.bs_detecter_type <- function(objet) {
  cl <- class(objet)
  if ("dse_result" %in% cl)         return("dse")
  if ("bcstats_result" %in% cl)     return("backcheck")
  if ("census_quality" %in% cl)     return("qualite_intrinseque")
  if ("bs_redressement" %in% cl)    return("redressement")
  if (any(grepl("projection|microsimulation", cl))) return("projection")
  "generique"
}

# --- Interpreteurs specialises -----------------------------------------------

.bs_interp_qualite <- function(q) {
  phrases <- character(0)
  if (!is.null(q$global_score)) {
    v <- .bs_verdict(q$global_score, c(50, 70, 85),
                     c("preoccupante", "moyenne", "bonne", "excellente"))
    phrases <- c(phrases, sprintf(
      "Score global de qualite : %.1f/100, soit une qualite %s.", q$global_score, v))
  }
  ag <- q$age_quality
  if (!is.null(ag)) {
    if (!is.null(ag$whipple_combined)) {
      vw <- .bs_verdict(ag$whipple_combined, c(105, 110, 125, 175),
                        c("tres precise (attraction negligeable)",
                          "precise", "approximative",
                          "grossiere (forte attraction des chiffres ronds)",
                          "tres grossiere"))
      phrases <- c(phrases, sprintf(
        "Indice de Whipple = %.0f : declaration des ages %s.", ag$whipple_combined, vw))
    }
    if (!is.null(ag$myers)) {
      vm <- .bs_verdict(ag$myers, c(5, 10, 20, 30),
                        c("excellente", "bonne", "acceptable", "mediocre", "mauvaise"))
      phrases <- c(phrases, sprintf(
        "Indice de Myers = %.1f : qualite de la repartition par chiffre terminal %s.",
        ag$myers, vm))
    }
  }
  if (!is.null(q$completeness$overall_completeness)) {
    phrases <- c(phrases, sprintf(
      "Completude globale : %.1f%% des cellules sont renseignees.",
      q$completeness$overall_completeness))
  }
  rec <- if (!is.null(ag$whipple_combined) && ag$whipple_combined > 125)
    "Recommandation : envisager un lissage des ages (Carrier-Farrag, Arriaga ou Sprague) avant le calcul des indicateurs sensibles a la structure." else
    "Aucune correction lourde de structure d'age n'est imposee ; verifier neanmoins la coherence par sexe et par region."
  c(phrases, rec)
}

.bs_interp_backcheck <- function(b) {
  phrases <- character(0)
  ter <- if (!is.null(b$overall_error_rate)) b$overall_error_rate * 100 else NA
  if (!is.na(ter)) {
    v <- .bs_verdict(ter, c(2, 5, 10),
                     c("excellente (terrain fiable)", "satisfaisante",
                       "a surveiller", "problematique"))
    phrases <- c(phrases, sprintf(
      "Taux d'erreur global du controle de terrain : %.1f%%, fiabilite %s.", ter, v))
  }
  if (!is.null(b$enumerator_stats) && nrow(b$enumerator_stats) > 0) {
    es <- b$enumerator_stats
    col_taux <- intersect(c("error_rate", "taux_erreur"), names(es))[1]
    if (!is.na(col_taux)) {
      pires <- es[order(-es[[col_taux]]), ][1, ]
      phrases <- c(phrases, sprintf(
        "%d enqueteur(s) evalue(s) ; le taux d'erreur le plus eleve atteint %.1f%%.",
        nrow(es), pires[[col_taux]] * ifelse(max(es[[col_taux]], na.rm = TRUE) <= 1, 100, 1)))
    }
  }
  c(phrases, paste(
    "Recommandation : cibler la formation et la supervision des enqueteurs au-dela de",
    "5% d'erreur, et reprendre les variables les plus discordantes."))
}

.bs_interp_dse <- function(d) {
  phrases <- character(0)
  if (!is.null(d$omission_rate)) {
    v <- .bs_verdict(d$omission_rate, c(2, 5, 10),
                     c("tres bonne couverture", "couverture satisfaisante",
                       "sous-denombrement notable", "sous-denombrement severe"))
    phrases <- c(phrases, sprintf(
      "Taux d'omission estime : %.1f%% (%s).", d$omission_rate, v))
  }
  if (!is.null(d$coverage_rate))
    phrases <- c(phrases, sprintf("Taux de couverture (appariement) : %.1f%%.", d$coverage_rate))
  if (!is.null(d$true_population) && !is.null(d$n_census))
    phrases <- c(phrases, sprintf(
      "Population vraie estimee : %s habitants, contre %s denombres (ecart net : %s).",
      format(round(d$true_population), big.mark = " "),
      format(d$n_census, big.mark = " "),
      format(round(d$true_population - d$n_census), big.mark = " ")))
  if (!is.null(d$erroneous_inclusion_rate))
    phrases <- c(phrases, sprintf(
      "Taux d'inclusions erronees : %.1f%%.", d$erroneous_inclusion_rate))
  c(phrases, paste(
    "Recommandation : appliquer un coefficient de redressement (cf.",
    "bs_coefficients_redressement) par strate plutot qu'un facteur global."))
}

.bs_interp_redressement <- function(r) {
  if (!"coef_redressement" %in% names(r)) return("Coefficients non disponibles.")
  cmin <- min(r$coef_redressement, na.rm = TRUE)
  cmax <- max(r$coef_redressement, na.rm = TRUE)
  om   <- mean(r$taux_omission, na.rm = TRUE)
  phrases <- sprintf(
    "Coefficients de redressement : de %.3f a %.3f selon les strates (omission moyenne %.1f%%).",
    cmin, cmax, om)
  hetero <- if ((cmax - cmin) > 0.15)
    "L'heterogeneite entre strates justifie un redressement differencie (et non uniforme)." else
    "Les coefficients sont homogenes ; un redressement quasi uniforme est acceptable."
  c(phrases, hetero,
    "Appliquer via bs_appliquer_redressement() aux effectifs publies.")
}

.bs_interp_projection <- function(p) {
  phrases <- "Projection realisee."
  res <- tryCatch({
    s <- if (!is.null(p$summary)) p$summary else NULL
    if (!is.null(s) && all(c("year", "total") %in% tolower(names(s)))) {
      nm <- tolower(names(s)); names(s) <- nm
      deb <- s$total[1]; fin <- s$total[nrow(s)]
      tx <- (fin / deb)^(1 / (max(s$year) - min(s$year))) - 1
      c(sprintf("Population projetee de %s a %s sur la periode (%d-%d).",
                format(round(deb), big.mark = " "), format(round(fin), big.mark = " "),
                min(s$year), max(s$year)),
        sprintf("Taux de croissance annuel moyen implicite : %.2f%%.", tx * 100))
    } else phrases
  }, error = function(e) phrases)
  c(res, "Verifier la sensibilite aux hypotheses (fecondite, mortalite, migration) via les scenarios.")
}

.bs_interp_indicateur <- function(x, type) {
  fam <- sub("^indic_", "", type)
  # Cas particuliers a fort interet d'interpretation
  if (fam == "fecondite") {
    isf <- tryCatch(x$tfr %||% attr(x, "tfr"), error = function(e) NULL)
    if (!is.null(isf) && is.numeric(isf)) {
      v <- .bs_verdict(isf, c(2.1, 4, 6),
                       c("inferieure au seuil de remplacement",
                         "moderee (transition avancee)",
                         "elevee (transition en cours)", "tres elevee"))
      return(structure(sprintf(
        "Indice synthetique de fecondite = %.2f enfants/femme : fecondite %s.", isf, v),
        class = "bs_interpretation"))
    }
  }
  if (fam == "rapport_masc") {
    rm <- tryCatch(x$sex_ratio %||% x$ratio %||% NA, error = function(e) NA)
    if (is.numeric(rm) && length(rm) == 1 && !is.na(rm)) {
      v <- if (rm < 95 || rm > 105) "atypique (a verifier : migration, omission differentielle)" else "plausible"
      return(structure(sprintf("Rapport de masculinite = %.1f : valeur %s.", rm, v),
                       class = "bs_interpretation"))
    }
  }
  structure(sprintf("Indicateur '%s' calcule. Examiner les valeurs au regard des references nationales.", fam),
            class = "bs_interpretation")
}

.bs_interp_generique <- function(x) {
  "Resultat calcule. Utilisez bs_prompt() pour generer un prompt d'interpretation detaillee."
}

# operateur null-coalescent interne
`%||%` <- function(a, b) if (is.null(a)) b else a

#' @export
print.bs_interpretation <- function(x, ...) {
  cli::cli_h3("Interpretation baobabStats")
  for (ph in x) cli::cli_li(ph)
  invisible(x)
}
