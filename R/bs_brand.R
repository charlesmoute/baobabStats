#' @title Identite visuelle baobabStats
#' @name bs_brand
#' @description
#' Centralise l'identite visuelle du package : palette de couleurs derivee du logo
#' (baobab enracine dans l'Afrique), acces aux differentes versions du logo et
#' palettes pretes a l'emploi pour les graphiques. Cette identite est appliquee de
#' maniere coherente a toutes les sorties : graphiques (ggplot2), application Shiny,
#' classeurs Excel et rapports.
#'
#' Esprit de la charte : tons chauds et terriens evoquant la savane et le baobab —
#' brun ecorce (dominante), or savane (accent chaud), vert feuille (accent vegetal),
#' sur fonds creme et sable.
NULL

#' Couleurs de la charte baobabStats
#'
#' @param x Nom(s) de couleur(s) a extraire. Si \code{NULL} (defaut), renvoie tout
#'   le vecteur nomme.
#' @return Un vecteur nomme de codes hexadecimaux.
#' @details Noms disponibles :
#'   \code{ecorce}, \code{ecorce_fonce}, \code{ecorce_moyen}, \code{feuille},
#'   \code{feuille_fonce}, \code{or}, \code{or_clair}, \code{sable}, \code{creme},
#'   \code{argile}, \code{encre}, \code{gris}.
#' @examples
#' bs_couleurs()
#' bs_couleurs(c("ecorce", "or"))
#' @export
bs_couleurs <- function(x = NULL) {
  pal <- c(
    ecorce        = "#4A3826",  # brun ecorce - DOMINANTE
    ecorce_fonce  = "#2E2114",  # brun profond - fonds sombres
    ecorce_moyen  = "#73592F",  # brun moyen (tronc clair)
    feuille       = "#6E7E2F",  # vert feuille / olive - accent vegetal
    feuille_fonce = "#4F5A22",  # vert profond
    or            = "#DDA94A",  # or savane - accent chaud
    or_clair      = "#ECC97C",  # or clair
    sable         = "#F3E6C8",  # sable
    creme         = "#FBF6EC",  # creme - fond clair
    argile        = "#A86A38",  # terre / argile - accent tertiaire
    encre         = "#33271A",  # texte
    gris          = "#8A8378"   # gris chaud
  )
  if (is.null(x)) return(pal)
  manquant <- setdiff(x, names(pal))
  if (length(manquant))
    cli::cli_warn("Couleur(s) inconnue(s) : {paste(manquant, collapse = ', ')}")
  pal[intersect(x, names(pal))]
}

#' Palette discrete baobabStats pour graphiques
#'
#' @param n Nombre de couleurs souhaitees.
#' @param type "qualitatif" (defaut, couleurs distinctes) ou "sequentiel"
#'   (degrade ecorce -> or).
#' @return Un vecteur de codes hexadecimaux de longueur \code{n}.
#' @export
bs_palette <- function(n = 6, type = c("qualitatif", "sequentiel")) {
  type <- match.arg(type)
  if (type == "sequentiel") {
    rampe <- grDevices::colorRampPalette(bs_couleurs(c("ecorce_fonce", "ecorce",
                                                       "argile", "or", "or_clair")))
    return(rampe(n))
  }
  base <- unname(bs_couleurs(c("ecorce", "or", "feuille", "argile",
                               "feuille_fonce", "ecorce_moyen", "or_clair", "gris")))
  if (n <= length(base)) return(base[seq_len(n)])
  grDevices::colorRampPalette(base)(n)
}

# Couleurs homme/femme pour les pyramides (cohérentes avec la charte)
.bs_sexe_couleurs <- function() {
  c(Homme = unname(bs_couleurs("feuille_fonce")),
    Femme = unname(bs_couleurs("or")))
}

#' Chemin vers un fichier logo baobabStats
#'
#' @param version Une de : \code{"full"} (logo + texte, fond clair),
#'   \code{"emblem"} (arbre + Afrique sans texte, fond clair),
#'   \code{"full_cream"} (logo + texte crème, pour fonds sombres),
#'   \code{"emblem_cream"} (emblème crème, pour fonds sombres),
#'   \code{"full_transparent"} (logo + texte couleur sur fond noir),
#'   \code{"emblem_transparent"} (emblème couleur sur fond noir).
#' @return Le chemin du fichier PNG (chaine de caracteres).
#' @examples
#' \dontrun{
#' bs_logo("emblem")        # emblème couleur (fonds clairs)
#' bs_logo("full_cream")    # logo+texte crème (fonds sombres)
#' }
#' @export
bs_logo <- function(version = c("full", "emblem", "full_cream", "emblem_cream",
                                "full_transparent", "emblem_transparent")) {
  version <- match.arg(version)
  fichier <- switch(version,
    full                = "logo_full.png",
    emblem              = "logo_emblem.png",
    full_cream          = "logo_full_cream.png",
    emblem_cream        = "logo_emblem_cream.png",
    full_transparent    = "logo_full_transparent.png",
    emblem_transparent  = "logo_emblem_transparent.png")
  chemin <- system.file("branding", fichier, package = "baobabStats")
  if (!nzchar(chemin)) chemin <- file.path("inst", "branding", fichier)
  chemin
}

#' Ajouter le logo baobabStats a un graphique ggplot (filigrane discret)
#'
#' @param plot Un objet ggplot.
#' @param version Version du logo (voir \code{bs_logo()}). Defaut "embleme_transparent".
#' @param position "bas_droite" (defaut), "bas_gauche", "haut_droite", "haut_gauche".
#' @param taille Fraction de la largeur du graphique occupee par le logo (defaut 0.12).
#' @param alpha Transparence (0-1, defaut 0.5).
#' @return Un objet ggplot (avec le logo) si \pkg{cowplot} est disponible, sinon le
#'   graphique inchange avec un avertissement.
#' @export
bs_ajouter_logo <- function(plot, version = "embleme_transparent",
                            position = c("bas_droite", "bas_gauche",
                                         "haut_droite", "haut_gauche"),
                            taille = 0.12, alpha = 0.5) {
  position <- match.arg(position)
  if (!requireNamespace("cowplot", quietly = TRUE) ||
      !requireNamespace("magick", quietly = TRUE)) {
    cli::cli_warn("Installez {.pkg cowplot} et {.pkg magick} pour incruster le logo.")
    return(plot)
  }
  img <- magick::image_read(bs_logo(version))
  coords <- switch(position,
    bas_droite  = list(x = 1 - taille, y = 0.0),
    bas_gauche  = list(x = 0.0,        y = 0.0),
    haut_droite = list(x = 1 - taille, y = 1 - taille),
    haut_gauche = list(x = 0.0,        y = 1 - taille))
  cowplot::ggdraw(plot) +
    cowplot::draw_image(img, x = coords$x, y = coords$y,
                        width = taille, height = taille)
}
