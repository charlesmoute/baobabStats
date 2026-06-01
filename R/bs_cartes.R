#' @title Cartes thematiques (choropleths)
#' @name bs_cartes
#' @description
#' Production de cartes thematiques (choropleths) a partir d'un fond de carte
#' (shapefile / GeoPackage) et d'un indicateur agrege par unite administrative.
#' Le dossier du shapefile et la variable de jointure (fusion) se configurent
#' dans le classeur Excel (feuille \strong{Cartographie}).
#'
#' Le module s'appuie sur \code{sf} (lecture des geometries) et \code{ggplot2}
#' (rendu), aux couleurs de la charte baobabStats. Si \code{sf} n'est pas
#' installe, un message explique comment l'installer sans interrompre le pipeline.
NULL

#' Lire un fond de carte (shapefile, GeoPackage, GeoJSON)
#'
#' @param chemin Dossier contenant le shapefile (.shp) OU chemin direct d'un
#'   fichier .shp/.gpkg/.geojson.
#' @param couche Nom de la couche (optionnel, pour GeoPackage multi-couches).
#' @return Un objet \code{sf}, ou \code{NULL} avec message si \code{sf} absent.
#' @export
bs_lire_shapefile <- function(chemin, couche = NULL) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    cli::cli_warn(c("Le package 'sf' est requis pour les cartes thematiques.",
                    "i" = "Installez-le : install.packages('sf')."))
    return(NULL)
  }
  # Si un dossier est fourni, chercher le premier .shp
  src <- chemin
  if (dir.exists(chemin)) {
    shps <- list.files(chemin, pattern = "\\.shp$", full.names = TRUE, ignore.case = TRUE)
    if (length(shps) == 0) {
      gpkg <- list.files(chemin, pattern = "\\.(gpkg|geojson)$", full.names = TRUE, ignore.case = TRUE)
      if (length(gpkg) == 0) cli::cli_abort("Aucun shapefile/GeoPackage trouve dans {.path {chemin}}.")
      src <- gpkg[1]
    } else src <- shps[1]
  }
  geo <- tryCatch(
    if (is.null(couche)) sf::st_read(src, quiet = TRUE) else sf::st_read(src, layer = couche, quiet = TRUE),
    error = function(e) { cli::cli_warn("Lecture du fond de carte : {conditionMessage(e)}"); NULL })
  geo
}

#' Generer une carte thematique (choropleth)
#'
#' @description Joint un indicateur agrege a un fond de carte et produit une
#'   carte choroplethe aux couleurs baobabStats.
#'
#' @param donnees data.frame contenant la variable de jointure et l'indicateur.
#' @param shapefile Dossier ou fichier du fond de carte, OU un objet \code{sf}
#'   deja charge.
#' @param var_fusion Nom de la variable commune (jointure) cote donnees.
#' @param var_fusion_carte Nom de la variable de jointure cote shapefile
#'   (defaut : identique a \code{var_fusion}).
#' @param indicateur Nom de la variable a representer (numerique).
#' @param titre Titre de la carte.
#' @param palette "sequentiel" (defaut, degrade or) ou "divergent".
#' @param classes Nombre de classes (defaut 5) pour la discretisation.
#' @return Un objet \code{ggplot}, ou \code{NULL} si \code{sf}/\code{ggplot2} absent.
#' @examples
#' \dontrun{
#' agg <- bs_agreger(individus, by = "region", mesure = "alphabetise", fun = "moyenne")
#' bs_carte_thematique(agg, "shapes/regions_cm", var_fusion = "region",
#'                     indicateur = "valeur", titre = "Taux d'alphabetisation")
#' }
#' @export
bs_carte_thematique <- function(donnees, shapefile, var_fusion,
                                var_fusion_carte = NULL, indicateur,
                                titre = NULL, palette = "sequentiel", classes = 5) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_warn("Le package 'ggplot2' est requis pour le rendu des cartes."); return(NULL)
  }
  geo <- if (inherits(shapefile, "sf")) shapefile else bs_lire_shapefile(shapefile)
  if (is.null(geo)) return(NULL)
  if (is.null(var_fusion_carte)) var_fusion_carte <- var_fusion

  if (!var_fusion_carte %in% names(geo)) {
    cli::cli_warn(c("Variable de fusion {.val {var_fusion_carte}} absente du fond de carte.",
                    "i" = "Colonnes disponibles : {paste(setdiff(names(geo), attr(geo, 'sf_column')), collapse=', ')}."))
    return(NULL)
  }
  if (!indicateur %in% names(donnees)) cli::cli_abort("Indicateur {.val {indicateur}} absent des donnees.")

  # Normaliser les cles de jointure (casse/espaces)
  geo[[".cle"]] <- .bs_normaliser(as.character(geo[[var_fusion_carte]]))
  don <- as.data.frame(donnees, stringsAsFactors = FALSE)
  don[[".cle"]] <- .bs_normaliser(as.character(don[[var_fusion]]))

  carte <- merge(geo, don[, c(".cle", indicateur)], by = ".cle", all.x = TRUE)

  col <- tryCatch(bs_couleurs(), error = function(e)
    list(creme = "#FBF6EC", or = "#C8932A", ecorce = "#6B4226", encre = "#2E2620"))

  ggplot2::ggplot(carte) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data[[indicateur]]), color = "white", linewidth = 0.2) +
    ggplot2::scale_fill_gradient(low = col[["creme"]], high = col[["ecorce"]],
                                 na.value = "grey85", name = indicateur) +
    ggplot2::labs(title = titre %||% paste("Carte thematique :", indicateur),
                  caption = "baobabStats - Tools for Data, Rooted in Africa") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = col[["ecorce"]]),
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      legend.position = "right")
}

#' Produire en lot les cartes decrites dans la configuration Excel
#'
#' @description Lit la feuille \strong{Cartographie} du classeur de configuration
#'   (dossier du shapefile, variable de fusion, indicateurs a cartographier) et
#'   genere les cartes correspondantes. Appelee automatiquement par
#'   \code{bs_pipeline()} a l'etape de visualisation.
#'
#' @param data data.frame des microdonnees harmonisees.
#' @param carto data.frame issu de la feuille Cartographie.
#' @param dossier Dossier de sortie des cartes (PNG).
#' @param mapping Liste role -> colonne (pour resoudre les indicateurs).
#' @return Vecteur des chemins de cartes produites (invisible).
#' @export
bs_cartes_config <- function(data, carto, dossier = "cartes", mapping = NULL) {
  if (!is.data.frame(carto) || nrow(carto) == 0) return(invisible(character(0)))
  if (!dir.exists(dossier)) dir.create(dossier, recursive = TRUE)
  sorties <- character(0)

  # Parametres globaux (1re ligne ou colonnes dediees)
  get_param <- function(nom, defaut = NULL) {
    if (nom %in% names(carto)) {
      v <- carto[[nom]][1]
      if (!is.na(v) && nzchar(as.character(v))) return(as.character(v))
    }
    defaut
  }
  shp_dossier <- get_param("Shapefile", get_param("Dossier_shapefile"))
  var_fusion  <- get_param("Variable_fusion", "region")
  var_fusion_carte <- get_param("Variable_fusion_carte", var_fusion)

  if (is.null(shp_dossier) || !nzchar(shp_dossier)) {
    cli::cli_warn("Feuille Cartographie : dossier du shapefile non renseigne. Cartes ignorees.")
    return(invisible(character(0)))
  }
  geo <- bs_lire_shapefile(shp_dossier)
  if (is.null(geo)) return(invisible(character(0)))

  # Chaque ligne active = un indicateur a cartographier
  prod_col <- intersect(c("Produire", "produire"), names(carto))
  for (i in seq_len(nrow(carto))) {
    if (length(prod_col) && !.bs_actif(carto[[prod_col[1]]][i])) next
    ind_role <- if ("Indicateur" %in% names(carto)) as.character(carto$Indicateur[i]) else NA
    if (is.na(ind_role) || !nzchar(ind_role)) next
    col <- if (!is.null(mapping) && ind_role %in% names(mapping)) mapping[[ind_role]] else ind_role
    if (!col %in% names(data)) {
      cli::cli_alert_warning("Carte ignoree : indicateur '{ind_role}' non disponible dans les donnees.")
      next
    }
    # Agreger l'indicateur par l'unite de fusion
    fus_col <- if (!is.null(mapping) && var_fusion %in% names(mapping)) mapping[[var_fusion]] else var_fusion
    if (!fus_col %in% names(data)) {
      cli::cli_alert_warning("Carte ignoree : variable de fusion '{var_fusion}' absente des donnees.")
      next
    }
    agg <- tryCatch(
      if (is.numeric(data[[col]]))
        bs_agreger(data, by = fus_col, mesure = col, fun = "moyenne")
      else {
        # variable categorielle -> proportion de la 1re modalite positive
        d2 <- data; d2$.ind <- as.integer(.bs_normaliser(as.character(d2[[col]])) %in%
          c("oui","yes","1","alphabetise","occupe"))
        bs_agreger(d2, by = fus_col, mesure = ".ind", fun = "moyenne")
      },
      error = function(e) NULL)
    if (is.null(agg)) next
    names(agg)[names(agg) == fus_col] <- var_fusion
    titre <- get_param2(carto, i, "Titre", paste("Carte :", ind_role))
    g <- tryCatch(bs_carte_thematique(agg, geo, var_fusion = var_fusion,
            var_fusion_carte = var_fusion_carte, indicateur = "valeur", titre = titre),
          error = function(e) { cli::cli_warn("Carte '{ind_role}': {conditionMessage(e)}"); NULL })
    if (is.null(g)) next
    f <- file.path(dossier, paste0("carte_", gsub("[^A-Za-z0-9]+", "_", ind_role), ".png"))
    tryCatch({ ggplot2::ggsave(f, g, width = 8, height = 7, dpi = 200); sorties <- c(sorties, f) },
             error = function(e) cli::cli_warn("Enregistrement carte : {conditionMessage(e)}"))
  }
  if (length(sorties)) cli::cli_alert_success("{length(sorties)} carte(s) thematique(s) produite(s) dans {.path {dossier}}.")
  invisible(sorties)
}

get_param2 <- function(carto, i, nom, defaut) {
  if (nom %in% names(carto)) {
    v <- carto[[nom]][i]
    if (!is.na(v) && nzchar(as.character(v))) return(as.character(v))
  }
  defaut
}

.bs_actif <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return(FALSE)
  tolower(trimws(as.character(x[1]))) %in% c("oui", "yes", "true", "1", "o", "y")
}
