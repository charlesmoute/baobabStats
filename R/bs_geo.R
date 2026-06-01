#' @title Referentiels geographiques africains
#' @name bs_geo
#' @description
#' Referentiel interne des subdivisions administratives de premier niveau (regions)
#' pour plusieurs pays africains, et fonction d'harmonisation tolerante aux fautes
#' de saisie (appariement approximatif Jaro-Winkler). Le Cameroun (CM), absent du
#' moteur statAfrikR 0.1.0, est ici integre.
NULL

# Referentiel interne : libelles standardises par pays (niveau 1) ----------------
.bs_referentiel <- list(
  CM = c("Adamaoua", "Centre", "Est", "Extreme-Nord", "Littoral", "Nord",
         "Nord-Ouest", "Ouest", "Sud", "Sud-Ouest"),
  BJ = c("Alibori", "Atacora", "Atlantique", "Borgou", "Collines", "Couffo",
         "Donga", "Littoral", "Mono", "Oueme", "Plateau", "Zou"),
  BF = c("Boucle du Mouhoun", "Cascades", "Centre", "Centre-Est", "Centre-Nord",
         "Centre-Ouest", "Centre-Sud", "Est", "Hauts-Bassins", "Nord",
         "Plateau-Central", "Sahel", "Sud-Ouest"),
  CI = c("Abidjan", "Bas-Sassandra", "Comoe", "Denguele", "Goh-Djiboua",
         "Lacs", "Lagunes", "Montagnes", "Sassandra-Marahoue", "Savanes",
         "Vallee du Bandama", "Woroba", "Yamoussoukro", "Zanzan"),
  SN = c("Dakar", "Diourbel", "Fatick", "Kaffrine", "Kaolack", "Kedougou",
         "Kolda", "Louga", "Matam", "Saint-Louis", "Sedhiou", "Tambacounda",
         "Thies", "Ziguinchor")
)

#' Lister les pays couverts par le referentiel interne
#' @return Un \code{tibble} : code pays, nombre de regions.
#' @export
bs_geo_pays <- function() {
  tibble::tibble(
    code    = names(.bs_referentiel),
    n_regions = vapply(.bs_referentiel, length, integer(1))
  )
}

#' Obtenir le referentiel d'un pays
#' @param code_pays Code ISO-2 (ex. "CM").
#' @return Vecteur des libelles standardises, ou NULL si non couvert.
#' @export
bs_geo_referentiel <- function(code_pays) {
  if (toupper(code_pays) == "CM") {
    reg <- tryCatch(bs_geo_cameroun("region"), error = function(e) NULL)
    if (!is.null(reg)) return(sort(unique(reg$region)))
  }
  .bs_referentiel[[toupper(code_pays)]]
}

# Cache du referentiel hierarchique du Cameroun --------------------------------
.bs_geo_cm_cache <- new.env(parent = emptyenv())

#' Referentiel geographique hierarchique du Cameroun
#'
#' @description Charge le repertoire officiel des localites du Cameroun
#'   (region > departement > arrondissement > ville/canton), issu du repertoire
#'   des localites et villages 2016 : 10 regions, 58 departements,
#'   360 arrondissements, plus de 1 600 villes/cantons. Sert de base a
#'   l'harmonisation geographique et a la jointure des cartes thematiques.
#'
#' @param niveau Niveau de detail : "region", "departement", "arrondissement"
#'   ou "complet" (defaut, toutes les colonnes).
#' @return Un \code{tibble} des unites administratives au niveau demande.
#' @examples
#' \dontrun{
#' bs_geo_cameroun("region")          # 10 regions
#' bs_geo_cameroun("departement")     # 58 departements
#' bs_geo_cameroun("arrondissement")  # 360 arrondissements
#' }
#' @export
bs_geo_cameroun <- function(niveau = c("complet", "region", "departement", "arrondissement")) {
  niveau <- match.arg(niveau)
  if (is.null(.bs_geo_cm_cache$data)) {
    chemin <- system.file("extdata", "referentiel_cm.csv", package = "baobabStats")
    if (!nzchar(chemin) || !file.exists(chemin))
      cli::cli_abort("Referentiel du Cameroun introuvable dans le package.")
    .bs_geo_cm_cache$data <- bs_fread(chemin)
  }
  d <- as.data.frame(.bs_geo_cm_cache$data, stringsAsFactors = FALSE)
  res <- switch(niveau,
    region = unique(d[, c("region", "region_code")]),
    departement = unique(d[, c("region", "region_code", "departement")]),
    arrondissement = unique(d[, c("region", "region_code", "departement", "arrondissement")]),
    complet = d)
  bs_as_sortie(res)
}

#' Harmoniser les libelles de regions (appariement approximatif)
#'
#' @param data data.frame contenant la variable de region.
#' @param var_region Nom de la variable de region a harmoniser.
#' @param code_pays Code ISO-2 du pays pour utiliser le referentiel interne.
#' @param table_correspondance data.frame a deux colonnes \code{original} et
#'   \code{standardise} pour une correspondance manuelle (prioritaire).
#' @param dist_max Distance Jaro-Winkler maximale acceptee (0-1, defaut 0.25).
#' @return La table d'entree avec la variable harmonisee + une variable
#'   \code{<var>_std}, et un attribut \code{bs_correspondances} (journal).
#' @details Reprend la specification statAfrikR : appariement via
#'   \code{stringdist::amatch} (methode Jaro-Winkler). Si le pays n'est pas couvert
#'   et qu'aucune table n'est fournie, un avertissement invite a fournir une table.
#' @export
bs_harmoniser_regions <- function(data, var_region, code_pays = NULL,
                                   table_correspondance = NULL, dist_max = 0.25) {
  if (!var_region %in% names(data))
    cli::cli_abort("Variable {.val {var_region}} absente des donnees.")
  orig <- as.character(data[[var_region]])

  # Source de reference
  if (!is.null(table_correspondance)) {
    ref   <- table_correspondance$standardise
    lookup <- stats::setNames(table_correspondance$standardise,
                              tolower(trimws(table_correspondance$original)))
  } else {
    ref <- bs_geo_referentiel(code_pays)
    if (is.null(ref)) {
      cli::cli_warn(c(
        "Pays {.val {code_pays}} non couvert par le referentiel interne.",
        "i" = "Fournissez une {.arg table_correspondance} (colonnes original/standardise)."
      ))
      return(data)
    }
    lookup <- stats::setNames(ref, tolower(.bs_normaliser(ref)))
  }

  cible_norm <- tolower(.bs_normaliser(names(lookup)))
  src_norm   <- tolower(.bs_normaliser(orig))

  # 1) correspondance exacte ; 2) approximative Jaro-Winkler
  idx <- match(src_norm, cible_norm)
  amanq <- which(is.na(idx))
  if (length(amanq)) {
    ai <- stringdist::amatch(src_norm[amanq], cible_norm,
                             method = "jw", maxDist = dist_max)
    idx[amanq] <- ai
  }
  std <- ifelse(is.na(idx), NA_character_, unname(lookup[idx]))

  journal <- tibble::tibble(original = orig, standardise = std,
                            apparie = !is.na(std))
  journal <- unique(journal)

  newvar <- paste0(var_region, "_std")
  data[[newvar]] <- std
  attr(data, "bs_correspondances") <- journal
  n_non <- sum(is.na(std))
  if (n_non > 0)
    cli::cli_warn("{n_non} valeur(s) non appariee(s) : a verifier (attribut bs_correspondances).")
  data
}

# Normalisation : retire accents, ponctuation, espaces multiples
.bs_normaliser <- function(x) {
  x <- as.character(x)
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9 ]", " ", x)
  x <- gsub("\\s+", " ", trimws(x))
  x
}
