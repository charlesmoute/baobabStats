#' @title Alias anglais (internationalisation de l'API)
#' @name bs_aliases_en
#' @description
#' Pour faciliter l'adoption internationale du package sans rompre la
#' compatibilite francophone, baobabStats expose des \strong{alias anglais} pour
#' les fonctions principales. Les noms francais \code{bs_*} restent l'API
#' canonique ; les alias anglais pointent vers exactement les memes fonctions
#' (aucune duplication de logique). Toutes les fonctions existantes sont
#' conservees a l'identique.
#'
#' La veritable internationalisation des \emph{sorties} (rapports, messages,
#' interpretations) passe par le parametre \code{langue = "fr"/"en"} la ou il est
#' disponible (par ex. \code{bs_prompt()}), independamment du nom de la fonction
#' appelee.
#'
#' @section Correspondance des alias :
#' \tabular{ll}{
#'   \strong{Alias anglais} \tab \strong{Fonction canonique} \cr
#'   \code{bs_collect} \tab \code{bs_collecter} \cr
#'   \code{bs_clean} \tab \code{bs_nettoyer} \cr
#'   \code{bs_impute} \tab \code{bs_imputer} \cr
#'   \code{bs_harmonize_regions} \tab \code{bs_harmoniser_regions} \cr
#'   \code{bs_intrinsic_quality} \tab \code{bs_qualite_intrinseque} \cr
#'   \code{bs_evaluate_concordance} \tab \code{bs_evaluer_concordance} \cr
#'   \code{bs_indicator} \tab \code{bs_indicateur} \cr
#'   \code{bs_indicators_batch} \tab \code{bs_indicateurs_lot} \cr
#'   \code{bs_age_pyramid} \tab \code{bs_pyramide_ages} \cr
#'   \code{bs_sex_ratio} \tab \code{bs_rapport_masculinite} \cr
#'   \code{bs_life_table} \tab \code{bs_table_mortalite} \cr
#'   \code{bs_life_expectancy} \tab \code{bs_esperance_vie} \cr
#'   \code{bs_project_population} \tab \code{bs_projeter_population} \cr
#'   \code{bs_project_un} \tab \code{bs_projeter_onu} \cr
#'   \code{bs_projection_scenarios} \tab \code{bs_scenarios_projection} \cr
#'   \code{bs_leslie_matrix} \tab \code{bs_matrice_leslie} \cr
#'   \code{bs_tables} \tab \code{bs_tableaux} \cr
#'   \code{bs_table} \tab \code{bs_tableau} \cr
#'   \code{bs_export_tables} \tab \code{bs_exporter_tableaux} \cr
#'   \code{bs_thematic_map} \tab \code{bs_carte_thematique} \cr
#'   \code{bs_read_shapefile} \tab \code{bs_lire_shapefile} \cr
#'   \code{bs_aggregate} \tab \code{bs_agreger} \cr
#'   \code{bs_compute_engine} \tab \code{bs_moteur_calcul} \cr
#'   \code{bs_geo_cameroon} \tab \code{bs_geo_cameroun} \cr
#'   \code{bs_geo_countries} \tab \code{bs_geo_pays} \cr
#'   \code{bs_interpret} \tab \code{bs_interpreter} \cr
#'   \code{bs_report} \tab \code{bs_rapport} \cr
#'   \code{bs_thematic_reports} \tab \code{bs_rapports_thematiques} \cr
#'   \code{bs_colors} \tab \code{bs_couleurs} \cr
#'   \code{bs_add_logo} \tab \code{bs_ajouter_logo} \cr
#'   \code{bs_available_variables} \tab \code{bs_variables_disponibles} \cr
#'   \code{bs_required_variables} \tab \code{bs_variables_requises}
#' }
NULL

# Collecte & traitement
#' @export
bs_collect <- function(...) bs_collecter(...)
#' @export
bs_clean <- function(...) bs_nettoyer(...)
#' @export
bs_impute <- function(...) bs_imputer(...)
#' @export
bs_harmonize_regions <- function(...) bs_harmoniser_regions(...)

# Qualite
#' @export
bs_intrinsic_quality <- function(...) bs_qualite_intrinseque(...)
#' @export
bs_evaluate_concordance <- function(...) bs_evaluer_concordance(...)
#' @export
bs_available_variables <- function(...) bs_variables_disponibles(...)
#' @export
bs_required_variables <- function(...) bs_variables_requises(...)

# Indicateurs
#' @export
bs_indicator <- function(...) bs_indicateur(...)
#' @export
bs_indicators_batch <- function(...) bs_indicateurs_lot(...)
#' @export
bs_age_pyramid <- function(...) bs_pyramide_ages(...)
#' @export
bs_sex_ratio <- function(...) bs_rapport_masculinite(...)
#' @export
bs_life_table <- function(...) bs_table_mortalite(...)
#' @export
bs_life_expectancy <- function(...) bs_esperance_vie(...)

# Projections
#' @export
bs_project_population <- function(...) bs_projeter_population(...)
#' @export
bs_project_un <- function(...) bs_projeter_onu(...)
#' @export
bs_projection_scenarios <- function(...) bs_scenarios_projection(...)
#' @export
bs_leslie_matrix <- function(...) bs_matrice_leslie(...)

# Tabulations
#' @export
bs_tables <- function(...) bs_tableaux(...)
#' @export
bs_table <- function(...) bs_tableau(...)
#' @export
bs_export_tables <- function(...) bs_exporter_tableaux(...)

# Cartes & hybride
#' @export
bs_thematic_map <- function(...) bs_carte_thematique(...)
#' @export
bs_read_shapefile <- function(...) bs_lire_shapefile(...)
#' @export
bs_aggregate <- function(...) bs_agreger(...)
#' @export
bs_compute_engine <- function(...) bs_moteur_calcul(...)

# Geographie
#' @export
bs_geo_cameroon <- function(...) bs_geo_cameroun(...)
#' @export
bs_geo_countries <- function(...) bs_geo_pays(...)

# Interpretation, rapports, charte
#' @export
bs_interpret <- function(...) bs_interpreter(...)
#' @export
bs_report <- function(...) bs_rapport(...)
#' @export
bs_thematic_reports <- function(...) bs_rapports_thematiques(...)
#' @export
bs_colors <- function(...) bs_couleurs(...)
#' @export
bs_add_logo <- function(...) bs_ajouter_logo(...)
