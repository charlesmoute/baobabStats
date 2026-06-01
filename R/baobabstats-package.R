#' baobabStats : Suite integree pour les recensements et enquetes en Afrique
#'
#' @description
#' baobabStats reunit sous une API unique et coherente, entierement en francais,
#' trois moteurs experimentaux complementaires :
#' \itemize{
#'   \item \strong{DemoStats} : appariement post-censitaire (PES), estimation par
#'         systeme dual (DSE), controle qualite par backcheck (bcstats), indicateurs
#'         demographiques, projections.
#'   \item \strong{CensusAnalytics} : controle qualite intrinseque (Whipple, Myers,
#'         Bachi), nettoyage et imputation, dedoublonnage par apprentissage,
#'         tabulations thematiques, microsimulation, rapports.
#'   \item \strong{statAfrikR} : collecte (CSPro/Kobo/ODK), referentiels geographiques
#'         africains, plans de sondage, diffusion (SDMX/DDI).
#' }
#'
#' La suite est organisee en sept etapes du cycle statistique, accessibles via le
#' prefixe \code{bs_} : \code{bs_collecter*}, \code{bs_traiter*}, \code{bs_qualite*},
#' \code{bs_indicateur*}/\code{bs_tableau*}, \code{bs_projeter*}, \code{bs_visualiser*}
#' et \code{bs_diffuser*}. Trois innovations transverses la completent :
#' configuration par Excel (\code{bs_config_*}), interpretation dynamique
#' (\code{bs_interpreter}) et generation de prompts (\code{bs_prompt}).
#'
#' @section Demarrage rapide:
#' \preformatted{
#' library(baobabStats)
#' bs_app()                       # lance l'application Shiny
#' cfg <- bs_config_modele("ma_config.xlsx")  # cree un modele de configuration
#' res <- bs_pipeline(cfg)        # execute le pipeline pilote par Excel
#' }
#'
#' @keywords internal
#' @aliases baobabStats-package
"_PACKAGE"

# Environnement interne du package (registre, cache, options runtime).
.baobabstats <- new.env(parent = emptyenv())

#' @keywords internal
.onLoad <- function(libname, pkgname) {
  .baobabstats$config <- NULL
  .baobabstats$last_results <- list()
  op <- options()
  defaults <- list(
    baobabstats.langue = "fr",
    baobabstats.pays = "CM",
    baobabstats.theme = "baobabstats",
    baobabstats.sortie = file.path(getwd(), "sorties_baobabstats")
  )
  toset <- !(names(defaults) %in% names(op))
  if (any(toset)) options(defaults[toset])
  invisible()
}

#' @keywords internal
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "baobabStats 1.0.0  -  Tools for Data, Rooted in Africa\n",
    "  Suite integree recensements & enquetes : DemoStats + CensusAnalytics + statAfrikR\n",
    "  Demarrer : bs_app()  |  Aide : ?baobabStats  |  Catalogue : bs_catalogue()"
  )
}
