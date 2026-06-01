#' @title Architecture hybride data.table / tibble
#' @name bs_hybride
#' @description
#' baobabStats adopte une \strong{architecture hybride} pensee pour les volumes
#' d'un recensement (plusieurs millions de lignes) :
#' \itemize{
#'   \item En \strong{interne}, les calculs lourds (tabulations, appariement PES,
#'     agregations par strate, small area estimation) s'appuient sur
#'     \code{data.table} lorsqu'il est disponible : agregations et jointures
#'     5 a 50 fois plus rapides, modification par reference (\code{:=}),
#'     lecture rapide via \code{fread()}.
#'   \item En \strong{sortie}, les fonctions \code{bs_*} renvoient des
#'     \code{tibble} pour rester compatibles avec l'ecosysteme tidyverse des
#'     utilisateurs.
#' }
#' Si \code{data.table} n'est pas installe, baobabStats bascule automatiquement
#' sur une implementation \code{base}/\code{dplyr} equivalente : aucune
#' fonctionnalite n'est perdue, seules les performances changent.
NULL

# Indicateur de disponibilite de data.table (mis en cache) ----------------------
.bs_has_dt <- function() {
  if (is.null(.baobabstats$has_data_table))
    .baobabstats$has_data_table <- requireNamespace("data.table", quietly = TRUE)
  .baobabstats$has_data_table
}

#' Convertir vers data.table pour les calculs internes
#'
#' @description Convertit un data.frame en \code{data.table} (par reference si
#'   possible) pour les operations lourdes. Repli transparent sur data.frame si
#'   \code{data.table} est absent.
#' @param x data.frame ou data.table.
#' @return Un \code{data.table} si le package est disponible, sinon \code{x}.
#' @keywords internal
#' @export
bs_as_dt <- function(x) {
  if (.bs_has_dt()) {
    if (data.table::is.data.table(x)) return(x)
    return(data.table::as.data.table(x))
  }
  x
}

#' Convertir une sortie interne en tibble (contrat de sortie bs_*)
#'
#' @description Normalise la sortie d'une fonction interne en \code{tibble} pour
#'   l'utilisateur final, quel que soit le moteur utilise en interne.
#' @param x data.table, data.frame, matrice ou liste convertible.
#' @return Un \code{tibble}.
#' @keywords internal
#' @export
bs_as_sortie <- function(x) {
  if (is.null(x)) return(NULL)
  if (.bs_has_dt() && data.table::is.data.table(x))
    x <- as.data.frame(x)
  tibble::as_tibble(x)
}

#' Lecture rapide d'un fichier tabulaire (fread si disponible)
#'
#' @description Utilise \code{data.table::fread()} pour les gros fichiers CSV/TSV
#'   (lecture parallele, detection de types), avec repli sur \code{utils::read.csv}.
#' @param chemin Chemin du fichier.
#' @param ... Arguments transmis a \code{fread}/\code{read.csv}.
#' @return Un data.frame (data.table en interne si disponible).
#' @keywords internal
#' @export
bs_fread <- function(chemin, ...) {
  if (.bs_has_dt())
    return(data.table::fread(chemin, showProgress = FALSE, ...))
  utils::read.csv(chemin, stringsAsFactors = FALSE, ...)
}

#' Agregation hybride performante (comptage / somme par groupes)
#'
#' @description Agrege \code{data} par les variables \code{by}. En interne,
#'   utilise la syntaxe \code{data.table} \code{DT[, .(...), by=]} (tres rapide
#'   sur de gros volumes) ; repli sur \code{dplyr} sinon. Renvoie un \code{tibble}.
#' @param data data.frame.
#' @param by Vecteur de noms de variables de regroupement.
#' @param mesure Nom de la variable a agreger (NULL = comptage des lignes).
#' @param fun Fonction d'agregation : "somme", "moyenne", "n" (defaut selon mesure).
#' @param poids Variable de ponderation (optionnel ; pondere somme/moyenne/comptage).
#' @return Un \code{tibble} agrege.
#' @export
bs_agreger <- function(data, by, mesure = NULL, fun = NULL, poids = NULL) {
  if (is.null(fun)) fun <- if (is.null(mesure)) "n" else "somme"
  by <- by[by %in% names(data)]
  if (length(by) == 0) cli::cli_abort("Aucune variable de regroupement valide.")

  if (.bs_has_dt()) {
    DT <- bs_as_dt(data)
    w <- if (!is.null(poids) && poids %in% names(DT)) DT[[poids]] else 1
    if (fun == "n") {
      res <- DT[, .(valeur = sum(if (length(w) == 1) rep(w, .N) else w)),
                by = by]
    } else if (fun == "somme") {
      res <- DT[, .(valeur = sum(get(mesure) * (if (length(w) == 1) 1 else w), na.rm = TRUE)),
                by = by]
    } else { # moyenne (ponderee si poids)
      if (!is.null(poids) && poids %in% names(DT)) {
        res <- DT[, .(valeur = stats::weighted.mean(get(mesure), get(poids), na.rm = TRUE)),
                  by = by]
      } else {
        res <- DT[, .(valeur = mean(get(mesure), na.rm = TRUE)), by = by]
      }
    }
    return(bs_as_sortie(res))
  }

  # Repli dplyr
  data2 <- data
  if (!is.null(poids) && poids %in% names(data2)) {
    w <- data2[[poids]]
  } else w <- rep(1, nrow(data2))
  grp <- interaction(data2[by], drop = TRUE)
  if (fun == "n") {
    agg <- tapply(w, grp, sum)
  } else if (fun == "somme") {
    agg <- tapply(data2[[mesure]] * w, grp, function(v) sum(v, na.rm = TRUE))
  } else {
    agg <- tapply(seq_len(nrow(data2)), grp, function(idx)
      stats::weighted.mean(data2[[mesure]][idx], w[idx], na.rm = TRUE))
  }
  cles <- do.call(rbind, strsplit(names(agg), ".", fixed = TRUE))
  out <- as.data.frame(cles, stringsAsFactors = FALSE)
  names(out) <- by
  out$valeur <- as.numeric(agg)
  bs_as_sortie(out)
}

#' Diagnostic de l'architecture de calcul active
#'
#' @description Indique si \code{data.table} est utilise (mode performant) ou si
#'   baobabStats fonctionne en mode \code{base}/\code{dplyr} (repli).
#' @return Invisible : une liste (moteur, message). Affiche un message.
#' @export
bs_moteur_calcul <- function() {
  dt <- .bs_has_dt()
  moteur <- if (dt) "data.table (mode haute performance)" else "base/dplyr (repli)"
  if (dt) {
    cli::cli_alert_success("Moteur de calcul : {moteur}. Adapte aux volumes de recensement (millions de lignes).")
  } else {
    cli::cli_alert_warning("Moteur de calcul : {moteur}. Installez 'data.table' pour acceler les gros volumes : install.packages('data.table').")
  }
  invisible(list(moteur = moteur, data_table = dt))
}
