#' @title Concordance backcheck / post-censitaire et controle des variables
#' @name bs_qualite_concordance
#' @description
#' Deux apports complementaires a baobabStats 1.0.1 :
#' \enumerate{
#'   \item \strong{Evaluation de concordance unifiee} : qu'il s'agisse d'un
#'     \emph{backcheck} (re-interview d'une enquete) ou d'une \emph{enquete
#'     post-censitaire} (PES d'un recensement), la meme logique de type
#'     \code{bcstats} est appliquee. L'utilisateur declare les variables
#'     \strong{T1} (critiques, comparaison exacte), \strong{T2} (moderees,
#'     tolerance numerique possible) et \strong{T3} (mineures) via le fichier de
#'     configuration. Sont calcules : taux de concordance, taux d'erreur par
#'     type, \strong{coefficient Kappa de Cohen}, et — pour la PES — taux
#'     d'omission et coefficients de redressement.
#'   \item \strong{Controle de disponibilite des variables} : avant de produire
#'     un tableau d'un plan d'analyse, baobabStats verifie que toutes les
#'     variables necessaires sont configurees. Si une variable manque, le tableau
#'     n'est pas produit et un message explicite est emis.
#' }
NULL

# ---------------------------------------------------------------------------
# 1. CONTROLE DE DISPONIBILITE DES VARIABLES (gating des tableaux)
# ---------------------------------------------------------------------------

#' Verifier la disponibilite des variables requises pour un tableau
#'
#' @description Teste si tous les roles de variables necessaires a un tableau
#'   sont presents dans les donnees (apres mappage). Si une variable manque, le
#'   tableau ne doit pas etre produit ; un message d'information est emis et
#'   journalise.
#' @param roles Vecteur des roles requis (ex. \code{c("age","sexe")}).
#' @param mapping Liste nommee role -> colonne (issue de la feuille Variables).
#' @param data data.frame des donnees.
#' @param tableau Libelle du tableau concerne (pour le message).
#' @return \code{TRUE} si toutes les variables sont disponibles, \code{FALSE} sinon.
#' @export
bs_variables_disponibles <- function(roles, mapping, data, tableau = "Tableau") {
  manquants <- character(0)
  for (r in roles) {
    col <- if (!is.null(mapping) && r %in% names(mapping)) mapping[[r]] else r
    if (is.null(col) || !(col %in% names(data))) manquants <- c(manquants, r)
  }
  if (length(manquants) > 0) {
    msg <- sprintf("Tableau non produit : %s. Variable(s) non configuree(s) : %s.",
                   tableau, paste(manquants, collapse = ", "))
    cli::cli_alert_warning(msg)
    .bs_journal_tableau_ignore(tableau, manquants)
    return(FALSE)
  }
  TRUE
}

# Journal des tableaux ignores (consultable apres coup)
.bs_journal_tableau_ignore <- function(tableau, manquants) {
  if (is.null(.baobabstats$tableaux_ignores))
    .baobabstats$tableaux_ignores <- list()
  .baobabstats$tableaux_ignores[[length(.baobabstats$tableaux_ignores) + 1]] <-
    list(tableau = tableau, variables_manquantes = manquants, heure = Sys.time())
  invisible(NULL)
}

#' Journal des tableaux non produits faute de variables
#'
#' @description Renvoie, sous forme de tibble, la liste des tableaux qui n'ont
#'   pas pu etre produits et les variables manquantes correspondantes.
#' @param reset Logique : vider le journal apres lecture (defaut FALSE).
#' @return Un \code{tibble} (tableau, variables_manquantes, heure) ; vide si rien.
#' @export
bs_tableaux_ignores <- function(reset = FALSE) {
  j <- .baobabstats$tableaux_ignores
  if (is.null(j) || length(j) == 0)
    return(tibble::tibble(tableau = character(0),
                          variables_manquantes = character(0),
                          heure = as.POSIXct(character(0))))
  out <- tibble::tibble(
    tableau = vapply(j, function(x) x$tableau, character(1)),
    variables_manquantes = vapply(j, function(x) paste(x$variables_manquantes, collapse = ", "), character(1)),
    heure = as.POSIXct(vapply(j, function(x) as.numeric(x$heure), numeric(1)), origin = "1970-01-01")
  )
  if (isTRUE(reset)) .baobabstats$tableaux_ignores <- list()
  out
}

#' Roles de variables requis par chaque tableau d'un plan d'analyse
#'
#' @description Referentiel reliant chaque tableau-cle des plans d'analyse (les
#'   12 tomes + qualite + projection) aux roles de variables indispensables a sa
#'   production. Sert au controle de disponibilite avant production.
#' @param theme Code de thematique (optionnel) pour filtrer.
#' @return Un \code{tibble} (theme, tableau, roles_requis).
#' @export
bs_variables_requises <- function(theme = NULL) {
  reqs <- list(
    structure   = list("Structure par grands groupes d'age" = c("age"),
                       "Repartition par unite administrative" = c("region"),
                       "Pyramide des ages" = c("age", "sexe")),
    nuptialite  = list("Repartition selon la situation matrimoniale" = c("situation_matrimoniale"),
                       "Age au premier mariage (SMAM)" = c("situation_matrimoniale", "age", "sexe")),
    education   = list("Alphabetisation" = c("alphabetisation"),
                       "Niveau d'instruction" = c("niveau_instruction"),
                       "Scolarisation" = c("frequentation", "age")),
    emploi      = list("Activite economique" = c("activite"),
                       "Activite par sexe et age" = c("activite", "sexe", "age")),
    fecondite   = list("Fecondite recente" = c("naissances_recentes"),
                       "ISF par age" = c("naissances_recentes", "age", "sexe")),
    mortalite   = list("Mortalite recente" = c("deces"),
                       "Mortalite par age et sexe" = c("deces", "age", "sexe")),
    migration   = list("Migration duree de vie" = c("lieu_naissance", "region"),
                       "Migration recente" = c("residence_anterieure", "region")),
    handicap    = list("Prevalence du handicap" = c("handicap"),
                       "Handicap par age et sexe" = c("handicap", "age", "sexe")),
    habitat     = list("Logement : eau" = c("eau"),
                       "Logement : assainissement" = c("assainissement"),
                       "Logement : energie" = c("energie"),
                       "Logement : murs" = c("murs")),
    equipements = list("Equipements des menages" = c("biens_durables"),
                       "Transferts de la diaspora" = c("transferts")),
    autochtones = list("Effectifs des peuples autochtones" = c("peuple_autochtone"),
                       "Conditions de vie comparees" = c("peuple_autochtone", "region")),
    agriculture = list("Menages agropastoraux" = c("activite_agricole"),
                       "Elevage" = c("elevage")),
    qualite     = list("Attraction des ages" = c("age"),
                       "Rapport de masculinite" = c("age", "sexe")),
    projection  = list("Projection par sexe et unite fine" = c("age", "sexe", "region"))
  )
  rows <- list()
  for (th in names(reqs)) {
    for (tab in names(reqs[[th]])) {
      rows[[length(rows) + 1]] <- list(theme = th, tableau = tab,
                                       roles_requis = paste(reqs[[th]][[tab]], collapse = ", "))
    }
  }
  out <- tibble::tibble(
    theme = vapply(rows, function(x) x$theme, character(1)),
    tableau = vapply(rows, function(x) x$tableau, character(1)),
    roles_requis = vapply(rows, function(x) x$roles_requis, character(1))
  )
  if (!is.null(theme)) out <- out[out$theme == theme, , drop = FALSE]
  out
}

# ---------------------------------------------------------------------------
# 2. EVALUATION DE CONCORDANCE UNIFIEE (backcheck OU post-censitaire)
# ---------------------------------------------------------------------------

#' Evaluer la concordance entre collecte et controle (backcheck ou PES)
#'
#' @description
#' Point d'entree unique, pilotable par configuration, pour evaluer la qualite
#' des donnees par comparaison avec une source de controle :
#' \itemize{
#'   \item \strong{enquete} : la source de controle est un \emph{backcheck}
#'     (re-interview d'un sous-echantillon) ;
#'   \item \strong{recensement} : la source de controle est l'\emph{enquete
#'     post-censitaire} (PES).
#' }
#' Dans les deux cas, la logique \code{bcstats} compare les variables declarees
#' T1/T2/T3 et calcule les taux de concordance, taux d'erreur par type et le
#' \strong{coefficient Kappa de Cohen} par variable. Pour le mode recensement,
#' le taux d'omission et les coefficients de redressement sont aussi derives.
#'
#' @param donnees data.frame de la collecte principale.
#' @param controle data.frame du backcheck (enquete) ou de la PES (recensement).
#' @param mode "enquete" (backcheck) ou "recensement" (post-censitaire).
#' @param id_var Variable identifiant commune aux deux fichiers.
#' @param enum_var Variable agent enqueteur (optionnel ; stats par agent).
#' @param t1,t2,t3 Vecteurs de variables critiques / moderees / mineures.
#' @param okrange Liste nommee variable -> tolerance numerique (T2).
#' @param var_strate Variable de strate (mode recensement : redressement).
#' @param interpreter Logique : interpretation dynamique (defaut TRUE).
#' @return Objet \code{bs_concordance} : resultat bcstats enrichi
#'   (\code{resume} = tibble concordance+kappa par variable ; pour le mode
#'   recensement : \code{omission}, \code{coefficients}).
#' @seealso \code{\link{bs_qualite_backcheck}}, \code{\link{bs_post_censitaire}}
#' @export
bs_evaluer_concordance <- function(donnees, controle,
                                   mode = c("enquete", "recensement"),
                                   id_var = "id", enum_var = NULL,
                                   t1 = NULL, t2 = NULL, t3 = NULL,
                                   okrange = NULL, var_strate = "region",
                                   interpreter = TRUE) {
  mode <- match.arg(mode)
  if (is.null(c(t1, t2, t3)))
    cli::cli_abort("Aucune variable T1/T2/T3 declaree : rien a comparer.")

  res <- bcstats(survey_data = donnees, backcheck_data = controle,
                 id_var = id_var, enum_var = enum_var,
                 type1_vars = t1, type2_vars = t2, type3_vars = t3,
                 okrange = okrange)

  # Tableau de synthese : concordance + kappa par variable
  res$resume <- .bs_resume_concordance(res, t1, t2, t3)
  res$mode <- mode

  # Mode recensement : omission + redressement par strate
  if (mode == "recensement") {
    om <- .bs_taux_omission(donnees, controle, id_var, var_strate)
    res$omission <- om$table
    res$coefficients <- om$coefficients
  }

  if (isTRUE(interpreter)) {
    res$interpretation <- .bs_interp_concordance(res, mode)
  }
  class(res) <- c("bs_concordance", class(res))
  .baobabstats$last_results$concordance <- res
  cli::cli_alert_success(
    "Concordance evaluee ({mode}) : {nrow(res$resume)} variable(s), Kappa moyen = {round(mean(res$resume$kappa, na.rm = TRUE), 3)}.")
  res
}

# Construit le tableau resume concordance + kappa par variable
.bs_resume_concordance <- function(res, t1, t2, t3) {
  vs <- res$variable_stats
  if (is.null(vs) || length(vs) == 0)
    return(tibble::tibble(variable = character(0), type_controle = character(0),
                          n = integer(0), concordance_pct = numeric(0),
                          kappa = numeric(0), interpretation_kappa = character(0)))
  typ_of <- function(v) if (v %in% t1) "T1 (critique)" else
    if (v %in% t2) "T2 (moderee)" else if (v %in% t3) "T3 (mineure)" else "-"
  lignes <- lapply(names(vs), function(v) {
    s <- vs[[v]]
    conc <- if (!is.null(s$concordance_rate)) s$concordance_rate else
      if (!is.null(s$within_range_rate)) s$within_range_rate else s$exact_match_rate
    tibble::tibble(
      variable = v,
      type_controle = typ_of(v),
      n = s$n %||% NA_integer_,
      concordance_pct = round(conc %||% NA_real_, 1),
      kappa = if (!is.null(s$kappa)) s$kappa else NA_real_,
      interpretation_kappa = if (!is.null(s$kappa_interpretation)) s$kappa_interpretation else "n/a (numerique)"
    )
  })
  do.call(rbind, lignes)
}

# Taux d'omission et coefficients de redressement par strate (mode recensement)
.bs_taux_omission <- function(recensement, pes, id_var, var_strate) {
  has_strate <- !is.null(var_strate) && var_strate %in% names(recensement) &&
    var_strate %in% names(pes)
  strates <- if (has_strate) sort(unique(as.character(stats::na.omit(recensement[[var_strate]]))))
             else "ENSEMBLE"
  rows <- lapply(strates, function(s) {
    if (has_strate) {
      rec <- recensement[as.character(recensement[[var_strate]]) == s, , drop = FALSE]
      pe  <- pes[as.character(pes[[var_strate]]) == s, , drop = FALSE]
    } else { rec <- recensement; pe <- pes }
    n_rec <- nrow(rec); n_pes <- nrow(pe)
    # Apparies = identifiants presents dans les deux sources
    n_app <- length(intersect(as.character(rec[[id_var]]), as.character(pe[[id_var]])))
    # Estimateur de Petersen-Lincoln (systeme dual) : N = (n1*n2)/m
    n_dse <- if (n_app > 0) (n_rec * n_pes) / n_app else NA_real_
    taux_omission <- if (!is.na(n_dse) && n_dse > 0) (1 - n_rec / n_dse) * 100 else NA_real_
    coef <- if (n_rec > 0 && !is.na(n_dse)) n_dse / n_rec else NA_real_
    tibble::tibble(strate = s, n_recensement = n_rec, n_pes = n_pes,
                   n_apparies = n_app, n_estime_dse = round(n_dse),
                   taux_omission_pct = round(taux_omission, 2),
                   coef_redressement = round(coef, 4))
  })
  tab <- do.call(rbind, rows)
  coefficients <- tibble::tibble(strate = tab$strate,
                                 coef_redressement = tab$coef_redressement)
  list(table = tab, coefficients = coefficients)
}

.bs_interp_concordance <- function(res, mode) {
  rs <- res$resume
  if (is.null(rs) || nrow(rs) == 0) return("Aucune variable comparee.")
  kmoy <- mean(rs$kappa, na.rm = TRUE)
  cmoy <- mean(rs$concordance_pct, na.rm = TRUE)
  src <- if (mode == "recensement") "post-censitaire" else "de re-interview (backcheck)"
  msg <- sprintf(paste0(
    "L'evaluation %s porte sur %d variable(s). Le taux de concordance moyen est de %.1f%% ",
    "et le Kappa de Cohen moyen de %.3f"), src, nrow(rs), cmoy, kmoy)
  msg <- paste0(msg, if (!is.na(kmoy)) {
    if (kmoy >= 0.80) ", soit un accord presque parfait entre les deux sources." else
    if (kmoy >= 0.60) ", soit un accord substantiel." else
    if (kmoy >= 0.40) ", soit un accord modere appelant une vigilance sur les variables les plus discordantes." else
    ", soit un accord faible : la qualite des donnees de ces variables doit etre examinee."
  } else ".")
  faibles <- rs$variable[!is.na(rs$kappa) & rs$kappa < 0.40]
  if (length(faibles) > 0)
    msg <- paste0(msg, sprintf(" Variables a faible concordance : %s.",
                               paste(faibles, collapse = ", ")))
  if (mode == "recensement" && !is.null(res$omission)) {
    om <- mean(res$omission$taux_omission_pct, na.rm = TRUE)
    if (!is.na(om))
      msg <- paste0(msg, sprintf(" Le taux d'omission moyen estime par systeme dual est de %.2f%% ; ",
        "les coefficients de redressement correspondants sont appliques aux effectifs publies.", om))
  }
  msg
}

#' @export
print.bs_concordance <- function(x, ...) {
  cli::cli_h2("Evaluation de concordance baobabStats ({x$mode})")
  if (!is.null(x$resume)) { cli::cli_alert_info("Concordance et Kappa par variable :"); print(x$resume) }
  if (!is.null(x$omission)) { cli::cli_alert_info("Taux d'omission et redressement :"); print(x$omission) }
  if (!is.null(x$interpretation)) { cli::cli_h3("Interpretation"); cli::cli_text(x$interpretation) }
  invisible(x)
}

