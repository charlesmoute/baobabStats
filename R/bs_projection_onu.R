#' @title Projection par la methode des cohortes-composantes (ONU)
#' @name bs_projection_onu
#' @description
#' Implementation de la \strong{methode des composantes par cohorte}
#' (cohort-component method), standard des Nations Unies (World Population
#' Prospects) pour la projection demographique. La population par age et sexe est
#' projetee pas a pas en appliquant separement les trois composantes du
#' changement demographique : \strong{fecondite}, \strong{mortalite} et
#' \strong{migration}.
#'
#' A chaque pas (generalement 1 ou 5 ans) :
#' \enumerate{
#'   \item les survivants sont calcules via les quotients de survie par age et sexe ;
#'   \item les naissances sont generees par application des taux de fecondite par
#'     age aux femmes, puis reparties par sexe (sex-ratio a la naissance) et
#'     soumises a la survie ;
#'   \item la migration nette par age et sexe est ajoutee.
#' }
#' Cette methode complete (sans la remplacer) la projection existante
#' \code{bs_projeter_population()}.
NULL

#' Projeter la population par la methode des cohortes-composantes (ONU)
#'
#' @param population data.frame de la population de base par age et sexe :
#'   colonnes \code{age} (ou groupe), \code{sexe}, \code{effectif}. Peut aussi
#'   etre des microdonnees (auto-agregees si \code{effectif} absent).
#' @param survie Quotients/ratios de survie par age et sexe : data.frame
#'   (age, sexe, ratio_survie) ou NULL pour deriver d'une table de mortalite.
#' @param fecondite Taux de fecondite par age (ASFR, pour 1 femme) : data.frame
#'   (age, taux) ou un ISF scalaire reparti sur un schema type.
#' @param migration Migration nette par age et sexe : data.frame
#'   (age, sexe, migration_nette) ou NULL (migration nulle).
#' @param horizon Nombre d'annees a projeter (defaut 10).
#' @param pas Intervalle de projection en annees : 1 ou 5 (defaut 5).
#' @param sex_ratio_naissance Rapport de masculinite a la naissance (defaut 1.03).
#' @param var_age,var_sexe,var_effectif Noms de colonnes.
#' @return Objet \code{bs_projection_onu} : trajectoire par annee, age, sexe ;
#'   et resume par annee. Sortie en \code{tibble}.
#' @examples
#' \dontrun{
#' base <- bs_pyramide_ages(individus)
#' proj <- bs_projeter_onu(base, fecondite = asfr, survie = sx, horizon = 25, pas = 5)
#' proj$resume       # population totale par annee
#' proj$par_age_sexe # detail par age et sexe
#' }
#' @export
bs_projeter_onu <- function(population, survie = NULL, fecondite = NULL,
                            migration = NULL, horizon = 10, pas = 5,
                            sex_ratio_naissance = 1.03,
                            var_age = "age", var_sexe = "sexe",
                            var_effectif = "effectif") {
  pop <- as.data.frame(population, stringsAsFactors = FALSE)

  # Auto-agregation si microdonnees (effectif absent)
  if (!var_effectif %in% names(pop)) {
    if (!all(c(var_age, var_sexe) %in% names(pop)))
      cli::cli_abort("La population doit contenir {.val {var_age}} et {.val {var_sexe}}.")
    grp <- bs_agreger(pop, by = c(var_age, var_sexe))
    names(grp)[names(grp) == "valeur"] <- var_effectif
    pop <- as.data.frame(grp)
  }

  # Groupes d'age quinquennaux si pas = 5
  pop$.ageg <- if (pas == 5) {
    bornes <- seq(0, 100, by = 5)
    cut(as.numeric(pop[[var_age]]), breaks = c(bornes, Inf), right = FALSE,
        labels = paste0(bornes, "-", c(bornes[-1] - 1, "+")))
  } else as.character(pop[[var_age]])

  ages <- sort(unique(as.character(pop$.ageg)))
  sexes <- unique(as.character(pop[[var_sexe]]))
  sexe_f <- .bs_detect_feminin(sexes)

  # Matrice population[age, sexe] de depart
  etat <- list()
  for (s in sexes) {
    v <- stats::setNames(numeric(length(ages)), ages)
    sub <- pop[as.character(pop[[var_sexe]]) == s, ]
    agg <- tapply(sub[[var_effectif]], as.character(sub$.ageg), sum, na.rm = TRUE)
    v[names(agg)] <- as.numeric(agg)
    etat[[s]] <- v
  }

  # Ratios de survie par age/sexe (defaut : schema type si non fourni)
  surv <- .bs_survie_lookup(survie, ages, sexes, pas)
  # ASFR par age (defaut : schema type si non fourni)
  asfr <- .bs_asfr_lookup(fecondite, ages, pas)
  # Migration nette par age/sexe
  mig <- .bs_migration_lookup(migration, ages, sexes)

  annee0 <- 0
  traj <- list()
  enregistrer <- function(an, etat) {
    for (s in sexes) for (a in ages)
      traj[[length(traj) + 1]] <<- data.frame(
        annee = an, age = a, sexe = s, effectif = round(etat[[s]][a]),
        stringsAsFactors = FALSE)
  }
  enregistrer(annee0, etat)

  n_pas <- ceiling(horizon / pas)
  for (k in seq_len(n_pas)) {
    nouvel <- list()
    # 1) Survie + vieillissement (decalage d'un groupe d'age)
    for (s in sexes) {
      v <- etat[[s]]; nv <- stats::setNames(numeric(length(ages)), ages)
      for (i in seq_along(ages)) {
        r <- surv[[s]][ages[i]]
        survivants <- v[ages[i]] * (if (is.na(r)) 0.95 else r)
        cible <- if (i < length(ages)) i + 1 else length(ages)  # dernier groupe ouvert
        nv[ages[cible]] <- nv[ages[cible]] + survivants
      }
      nouvel[[s]] <- nv
    }
    # 2) Naissances (ASFR x femmes), reparties par sexe et soumises a survie
    femmes <- etat[[sexe_f]]
    naiss_tot <- sum(vapply(ages, function(a) {
      f <- asfr[a]; (if (is.na(f)) 0 else f) * femmes[a] * pas
    }, numeric(1)), na.rm = TRUE)
    p_masc <- sex_ratio_naissance / (1 + sex_ratio_naissance)
    naiss <- stats::setNames(numeric(length(sexes)), sexes)
    for (s in sexes) {
      part <- if (s == sexe_f) (1 - p_masc) else p_masc / max(1, (length(sexes) - 1))
      surv_b <- surv[[s]][ages[1]]; surv_b <- if (is.na(surv_b)) 0.97 else surv_b
      nouvel[[s]][ages[1]] <- nouvel[[s]][ages[1]] + naiss_tot * part * surv_b
    }
    # 3) Migration nette
    for (s in sexes) for (a in ages) {
      m <- mig[[s]][a]; if (!is.na(m)) nouvel[[s]][a] <- max(0, nouvel[[s]][a] + m * pas)
    }
    etat <- nouvel
    enregistrer(annee0 + k * pas, etat)
  }

  par_age_sexe <- do.call(rbind, traj)
  resume <- stats::aggregate(effectif ~ annee, data = par_age_sexe, FUN = sum)
  resume$sexe <- "Ensemble"
  resume_sexe <- stats::aggregate(effectif ~ annee + sexe, data = par_age_sexe, FUN = sum)

  res <- list(
    methode = "cohortes-composantes (ONU / WPP)",
    pas = pas, horizon = horizon,
    par_age_sexe = bs_as_sortie(par_age_sexe),
    resume = bs_as_sortie(resume[, c("annee", "effectif")]),
    resume_sexe = bs_as_sortie(resume_sexe),
    hypotheses = list(sex_ratio_naissance = sex_ratio_naissance,
                      survie_fournie = !is.null(survie),
                      fecondite_fournie = !is.null(fecondite),
                      migration_fournie = !is.null(migration)))
  class(res) <- c("bs_projection_onu", "list")
  res
}

#' @export
print.bs_projection_onu <- function(x, ...) {
  cli::cli_h2("Projection par cohortes-composantes (ONU)")
  cli::cli_text("Pas : {x$pas} an(s) - Horizon : {x$horizon} ans")
  print(x$resume)
  invisible(x)
}

# Helpers de schemas types (utilises si composantes non fournies) --------------
.bs_detect_feminin <- function(sexes) {
  f <- grep("f|female|femme|2", tolower(sexes))
  if (length(f)) sexes[f[1]] else sexes[length(sexes)]
}
.bs_survie_lookup <- function(survie, ages, sexes, pas) {
  out <- list()
  for (s in sexes) {
    v <- stats::setNames(rep(NA_real_, length(ages)), ages)
    if (!is.null(survie) && all(c("age", "sexe", "ratio_survie") %in% names(survie))) {
      sub <- survie[as.character(survie$sexe) == s, ]
      v[as.character(sub$age)] <- sub$ratio_survie
    } else {
      # Schema type : survie elevee aux ages actifs, plus faible aux extremes
      idx <- seq_along(ages)
      base <- 0.985 - 0.0009 * pmax(0, idx - length(ages) * 0.6) * 10
      v[] <- pmin(0.998, pmax(0.80, base))
    }
    out[[s]] <- v
  }
  out
}
.bs_asfr_lookup <- function(fecondite, ages, pas) {
  v <- stats::setNames(rep(0, length(ages)), ages)
  if (!is.null(fecondite) && is.data.frame(fecondite) &&
      all(c("age", "taux") %in% names(fecondite))) {
    v[as.character(fecondite$age)] <- fecondite$taux
  } else if (is.numeric(fecondite) && length(fecondite) == 1) {
    # Repartir un ISF sur un schema type (pic 20-29 ans)
    schema <- stats::setNames(rep(0, length(ages)), ages)
    cibles <- c("15-19" = 0.12, "20-24" = 0.27, "25-29" = 0.26, "30-34" = 0.19,
                "35-39" = 0.11, "40-44" = 0.04, "45-49" = 0.01)
    for (nm in names(cibles)) if (nm %in% ages) schema[nm] <- cibles[nm]
    isf <- fecondite
    v[] <- schema * (isf / (sum(schema) * 5))  # ASFR annuel approx
  }
  v
}
.bs_migration_lookup <- function(migration, ages, sexes) {
  out <- list()
  for (s in sexes) {
    v <- stats::setNames(rep(NA_real_, length(ages)), ages)
    if (!is.null(migration) && all(c("age", "sexe", "migration_nette") %in% names(migration))) {
      sub <- migration[as.character(migration$sexe) == s, ]
      v[as.character(sub$age)] <- sub$migration_nette
    }
    out[[s]] <- v
  }
  out
}
