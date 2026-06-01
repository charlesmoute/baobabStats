#' @title Estimation sur petits domaines (Small Area Estimation)
#' @name bs_sae
#' @description
#' Module d'estimation sur petits domaines (\emph{small area estimation}, SAE)
#' implementant les deux approches classiques :
#' \itemize{
#'   \item \strong{Top-down} : un total fiable de niveau superieur (ex. region)
#'     est reparti vers les petits domaines (ex. arrondissements) au prorata de
#'     poids auxiliaires (population, donnees administratives, predicteurs).
#'   \item \strong{Bottom-up} : les estimations directes des petits domaines sont
#'     calculees puis agregees ; un calage (raking) optionnel assure la
#'     coherence avec le total de niveau superieur.
#' }
#' Les resultats peuvent etre \strong{desagreges/agreges par age, sexe et
#' handicap}. Un seul de ces axes peut etre choisi, ou plusieurs ; par defaut les
#' \strong{trois} sont actifs. Le module se configure aussi via le classeur Excel
#' (feuille \strong{SAE}).
NULL

#' Estimation sur petits domaines (top-down ou bottom-up)
#'
#' @param data data.frame des microdonnees (ou estimations directes).
#' @param domaine Variable du petit domaine cible (ex. "arrondissement").
#' @param approche "top_down" ou "bottom_up".
#' @param indicateur Variable a estimer (numerique). NULL = effectifs (comptage).
#' @param total_superieur Pour top_down : data.frame (niveau, total) du niveau
#'   superieur a repartir, OU un total scalaire global.
#' @param niveau_superieur Variable du niveau superieur (ex. "region") reliant
#'   chaque domaine a son total (pour top_down et le calage bottom_up).
#' @param poids_aux Variable de poids auxiliaire pour la repartition top-down
#'   (defaut : effectif observe du domaine).
#' @param par Axes de desagregation : sous-ensemble de c("age","sexe","handicap").
#'   Par defaut les trois sont actifs.
#' @param var_age,var_sexe,var_handicap Noms de colonnes des axes.
#' @param caler Logique (bottom_up) : caler les estimations directes sur le total
#'   superieur (raking). Defaut TRUE si \code{total_superieur} fourni.
#' @param mapping Liste role -> colonne (resolution des axes).
#' @return Objet \code{bs_sae} : estimations par domaine et croisements d'axes
#'   (\code{tibble}), methode, et diagnostic de coherence.
#' @examples
#' \dontrun{
#' # Top-down : repartir les totaux regionaux vers les arrondissements
#' bs_sae(individus, domaine = "arrondissement", approche = "top_down",
#'        total_superieur = totaux_region, niveau_superieur = "region",
#'        par = c("age","sexe","handicap"))
#' }
#' @export
bs_sae <- function(data, domaine, approche = c("top_down", "bottom_up"),
                   indicateur = NULL, total_superieur = NULL,
                   niveau_superieur = NULL, poids_aux = NULL,
                   par = c("age", "sexe", "handicap"),
                   var_age = "age", var_sexe = "sexe", var_handicap = "handicap",
                   caler = NULL, mapping = NULL) {
  approche <- match.arg(approche)
  d <- as.data.frame(data, stringsAsFactors = FALSE)

  # Resolution des axes via mapping eventuel
  axe_col <- list(age = var_age, sexe = var_sexe, handicap = var_handicap)
  if (!is.null(mapping)) {
    for (a in names(axe_col))
      if (a %in% names(mapping)) axe_col[[a]] <- mapping[[a]]
  }
  dom_col <- if (!is.null(mapping) && domaine %in% names(mapping)) mapping[[domaine]] else domaine
  if (!dom_col %in% names(d)) cli::cli_abort("Variable de domaine {.val {domaine}} absente des donnees.")

  # Axes effectivement disponibles
  par <- intersect(par, c("age", "sexe", "handicap"))
  axes_actifs <- par[vapply(par, function(a) axe_col[[a]] %in% names(d), logical(1))]
  manquants <- setdiff(par, axes_actifs)
  if (length(manquants))
    cli::cli_alert_warning("Axe(s) de desagregation ignore(s) (variable absente) : {paste(manquants, collapse=', ')}.")

  # Discretiser l'age en groupes quinquennaux pour la desagregation
  if ("age" %in% axes_actifs) {
    ag <- axe_col[["age"]]
    if (is.numeric(d[[ag]])) {
      b <- seq(0, 100, by = 5)
      d[[".age_grp"]] <- cut(d[[ag]], breaks = c(b, Inf), right = FALSE,
                             labels = paste0(b, "-", c(b[-1] - 1, "+")))
      axe_col[["age"]] <- ".age_grp"
    }
  }

  groupes <- c(dom_col, vapply(axes_actifs, function(a) axe_col[[a]], character(1)))

  # Estimation directe (comptage pondere ou somme) par domaine x axes
  poids <- if (!is.null(mapping) && "ponderation" %in% names(mapping)) mapping[["ponderation"]] else "poids"
  poids <- if (poids %in% names(d)) poids else NULL
  directe <- bs_agreger(d, by = groupes, mesure = indicateur,
                        fun = if (is.null(indicateur)) "n" else "somme",
                        poids = poids)
  directe <- as.data.frame(directe, stringsAsFactors = FALSE)
  names(directe)[names(directe) == "valeur"] <- "estimation_directe"

  # Rattacher le niveau superieur si fourni
  niv_col <- NULL
  if (!is.null(niveau_superieur)) {
    niv_col <- if (!is.null(mapping) && niveau_superieur %in% names(mapping)) mapping[[niveau_superieur]] else niveau_superieur
    if (niv_col %in% names(d)) {
      corr <- unique(d[, c(dom_col, niv_col)])
      directe <- merge(directe, corr, by = dom_col, all.x = TRUE)
    } else niv_col <- NULL
  }

  coherence <- NA_real_
  if (approche == "top_down") {
    res <- .bs_sae_topdown(d, directe, dom_col, niv_col, total_superieur,
                           poids_aux, groupes)
  } else {
    if (is.null(caler)) caler <- !is.null(total_superieur)
    res <- .bs_sae_bottomup(directe, dom_col, niv_col, total_superieur, caler)
    if (isTRUE(caler) && !is.null(total_superieur)) {
      tot_est <- sum(res$estimation, na.rm = TRUE)
      tot_cible <- if (is.data.frame(total_superieur)) sum(total_superieur[[2]], na.rm = TRUE) else sum(total_superieur)
      coherence <- if (tot_cible > 0) tot_est / tot_cible else NA_real_
    }
  }

  out <- list(
    approche = approche,
    axes = axes_actifs,
    domaine = domaine,
    estimations = bs_as_sortie(res),
    coherence = coherence,
    n_domaines = length(unique(res[[dom_col]])))
  class(out) <- c("bs_sae", "list")
  cli::cli_alert_success(
    "SAE {approche} : {out$n_domaines} domaine(s), axes = {paste(axes_actifs, collapse='/')}.")
  out
}

# Top-down : repartir le total superieur au prorata des poids auxiliaires --------
.bs_sae_topdown <- function(d, directe, dom_col, niv_col, total_superieur,
                            poids_aux, groupes) {
  directe$poids_repartition <- if (!is.null(poids_aux) && poids_aux %in% names(d)) {
    pw <- tapply(d[[poids_aux]], d[[dom_col]], sum, na.rm = TRUE)
    as.numeric(pw[as.character(directe[[dom_col]])])
  } else directe$estimation_directe

  # Determiner le total a repartir par niveau superieur
  if (is.data.frame(total_superieur) && !is.null(niv_col) && niv_col %in% names(directe)) {
    tot <- stats::setNames(total_superieur[[2]], as.character(total_superieur[[1]]))
    directe$.tot_niv <- as.numeric(tot[as.character(directe[[niv_col]])])
    grp_key <- directe[[niv_col]]
  } else {
    tot_global <- if (is.numeric(total_superieur)) sum(total_superieur) else sum(directe$estimation_directe, na.rm = TRUE)
    directe$.tot_niv <- tot_global
    grp_key <- rep("GLOBAL", nrow(directe))
  }

  # Part de chaque cellule dans son niveau superieur
  somme_poids <- tapply(directe$poids_repartition, grp_key, sum, na.rm = TRUE)
  directe$.somme <- as.numeric(somme_poids[as.character(grp_key)])
  directe$estimation <- ifelse(directe$.somme > 0,
                               directe$.tot_niv * directe$poids_repartition / directe$.somme,
                               0)
  directe$estimation <- round(directe$estimation)
  directe$.tot_niv <- NULL; directe$.somme <- NULL
  directe
}

# Bottom-up : estimations directes (+ calage optionnel sur total superieur) ------
.bs_sae_bottomup <- function(directe, dom_col, niv_col, total_superieur, caler) {
  directe$estimation <- directe$estimation_directe
  if (isTRUE(caler) && !is.null(total_superieur)) {
    if (is.data.frame(total_superieur) && !is.null(niv_col) && niv_col %in% names(directe)) {
      tot <- stats::setNames(total_superieur[[2]], as.character(total_superieur[[1]]))
      somme_niv <- tapply(directe$estimation_directe, directe[[niv_col]], sum, na.rm = TRUE)
      facteur <- as.numeric(tot[as.character(directe[[niv_col]])]) /
                 as.numeric(somme_niv[as.character(directe[[niv_col]])])
      directe$estimation <- round(directe$estimation_directe * ifelse(is.finite(facteur), facteur, 1))
    } else {
      tot_cible <- if (is.numeric(total_superieur)) sum(total_superieur) else NA
      if (!is.na(tot_cible)) {
        f <- tot_cible / sum(directe$estimation_directe, na.rm = TRUE)
        directe$estimation <- round(directe$estimation_directe * f)
      }
    }
  }
  directe
}

#' @export
print.bs_sae <- function(x, ...) {
  cli::cli_h2("Small Area Estimation - approche {x$approche}")
  cli::cli_text("Domaine : {x$domaine} ({x$n_domaines} unites) - Axes : {paste(x$axes, collapse=', ')}")
  if (!is.na(x$coherence))
    cli::cli_text("Coherence avec le total superieur : {round(100*x$coherence,1)} %")
  print(utils::head(x$estimations, 10))
  invisible(x)
}

#' Agreger des estimations SAE le long d'un ou plusieurs axes
#'
#' @description Recombine une sortie \code{bs_sae} en agregeant sur les axes non
#'   souhaites. Permet de passer d'un detail age x sexe x handicap a une vue par
#'   sexe seul, par exemple.
#' @param sae Objet \code{bs_sae}.
#' @param par Axes a conserver (sous-ensemble de \code{sae$axes}). NULL = total
#'   par domaine.
#' @return Un \code{tibble} agrege.
#' @export
bs_sae_agreger <- function(sae, par = NULL) {
  est <- as.data.frame(sae$estimations, stringsAsFactors = FALSE)
  dom <- sae$domaine
  dom_col <- intersect(c(dom, names(est)[1]), names(est))[1]
  axes_col <- intersect(c(".age_grp", "age", "sexe", "handicap"), names(est))
  garder <- dom_col
  if (!is.null(par)) {
    keep_axes <- intersect(c(par, paste0(".", par, "_grp")), names(est))
    garder <- c(dom_col, keep_axes)
  }
  garder <- intersect(garder, names(est))
  agg <- stats::aggregate(stats::reformulate(garder, response = "estimation"),
                          data = est, FUN = sum, na.rm = TRUE)
  bs_as_sortie(agg)
}
