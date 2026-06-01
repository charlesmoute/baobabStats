#' @title CensusAnalytics: Analyse Complète des Données de Recensement
#' @description Package R complet pour l'analyse des données de recensement
#' @docType package
#' @name CensusAnalytics-package
#' @aliases CensusAnalytics
#' @keywords internal
"_PACKAGE"

# Global variables to avoid R CMD check notes
utils::globalVariables(c(
  ".", "age", "sex", "admin_unit", "weight", "n", "pct", "value",
  "year", "cohort", "status", "disability", "migration_status",
  "marital_status", "children_born", "children_alive", "cluster_id",
  "duplicate_score", "transformation_id", "original_value", "new_value"
))

#' Charger des données de recensement
#'
#' @description Charge des données de recensement à partir de différents formats
#' @param file_path Chemin vers le fichier de données
#' @param format Format du fichier ("csv", "xlsx", "sav", "dta", "rds")
#' @param encoding Encodage du fichier (défaut: "UTF-8")
#' @param ... Arguments supplémentaires passés aux fonctions de lecture
#' @return Un data.frame contenant les données de recensement
#' @export
#' @examples
#' \dontrun{
#' data <- load_census_data("recensement.csv")
#' }
load_census_data <- function(file_path, format = NULL, encoding = "UTF-8", ...) {
  if (is.null(format)) {
    format <- tolower(tools::file_ext(file_path))
  }
  
  data <- switch(format,
    "csv" = data.table::fread(file_path, encoding = encoding, ...),
    "xlsx" = readxl::read_excel(file_path, ...),
    "xls" = readxl::read_excel(file_path, ...),
    "sav" = haven::read_sav(file_path, ...),
    "dta" = haven::read_dta(file_path, ...),
    "rds" = readRDS(file_path),
    stop("Format non supporté: ", format)
  )
  
  # Convertir en data.frame standard
  data <- as.data.frame(data)
  
  # Ajouter des métadonnées
  attr(data, "census_source") <- file_path
  attr(data, "census_loaded_at") <- Sys.time()
  attr(data, "census_nrow") <- nrow(data)
  attr(data, "census_ncol") <- ncol(data)
  
  message(sprintf("Données chargées: %d observations, %d variables", nrow(data), ncol(data)))
  
  return(data)
}

#' Valider la structure des données de recensement
#'
#' @description Vérifie que les données contiennent les variables requises
#' @param data Data.frame contenant les données de recensement
#' @param required_vars Vecteur des noms de variables requises
#' @param warn_missing Afficher un avertissement pour les variables manquantes
#' @return Liste avec le statut de validation et les détails
#' @export
validate_census_data <- function(data, required_vars = NULL, warn_missing = TRUE) {
  
  # Variables standard pour un recensement
  standard_vars <- list(
    identification = c("id", "household_id", "individual_id"),
    geography = c("region", "province", "commune", "admin_unit"),
    demographics = c("age", "sex", "marital_status"),
    fertility = c("children_born", "children_alive", "children_last_year"),
    mortality = c("deaths_household", "maternal_deaths"),
    migration = c("birthplace", "residence_5years", "migration_status"),
    disability = c("disability", "disability_type", "disability_severity"),
    education = c("education_level", "literacy"),
    employment = c("employment_status", "occupation", "industry")
  )
  
  if (is.null(required_vars)) {
    required_vars <- c("age", "sex")
  }
  
  # Vérifier les variables présentes
  present_vars <- names(data)
  missing_required <- setdiff(required_vars, present_vars)
  
  # Identifier les catégories de variables présentes
  categories_present <- sapply(standard_vars, function(vars) {
    any(vars %in% present_vars)
  })
  
  # Résultat de validation
  validation <- list(
    is_valid = length(missing_required) == 0,
    total_vars = length(present_vars),
    total_obs = nrow(data),
    missing_required = missing_required,
    categories_present = names(categories_present)[categories_present],
    categories_missing = names(categories_present)[!categories_present],
    data_types = sapply(data, class)
  )
  
  if (warn_missing && length(missing_required) > 0) {
    warning("Variables requises manquantes: ", paste(missing_required, collapse = ", "))
  }
  
  # Afficher un résumé
  message("\n=== Validation des données de recensement ===")
  message(sprintf("Observations: %d", validation$total_obs))
  message(sprintf("Variables: %d", validation$total_vars))
  message(sprintf("Catégories présentes: %s", paste(validation$categories_present, collapse = ", ")))
  
  return(validation)
}

#' Résumer les données de recensement
#'
#' @description Produit un résumé statistique des données de recensement
#' @param data Data.frame contenant les données de recensement
#' @param by_admin Grouper par unité administrative
#' @return Liste contenant les résumés statistiques
#' @export
summarize_census <- function(data, by_admin = NULL) {
  
  summary_list <- list()
  
  # Résumé global
  summary_list$global <- list(
    n_total = nrow(data),
    n_complete = sum(complete.cases(data)),
    pct_complete = round(sum(complete.cases(data)) / nrow(data) * 100, 2),
    missing_by_var = sapply(data, function(x) sum(is.na(x)))
  )
  
  # Résumé par sexe si disponible
  if ("sex" %in% names(data)) {
    summary_list$by_sex <- table(data$sex, useNA = "ifany")
  }
  
  # Résumé par âge si disponible
  if ("age" %in% names(data)) {
    summary_list$age_stats <- list(
      min = min(data$age, na.rm = TRUE),
      max = max(data$age, na.rm = TRUE),
      mean = round(mean(data$age, na.rm = TRUE), 2),
      median = median(data$age, na.rm = TRUE)
    )
  }
  
  # Résumé par unité administrative
  if (!is.null(by_admin) && by_admin %in% names(data)) {
    summary_list$by_admin <- data %>%
      dplyr::group_by(!!rlang::sym(by_admin)) %>%
      dplyr::summarise(
        n = dplyr::n(),
        pct = round(dplyr::n() / nrow(data) * 100, 2),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(n))
  }
  
  return(summary_list)
}

#' Créer des groupes d'âge
#'
#' @description Crée des groupes d'âge quinquennaux ou personnalisés
#' @param age Vecteur d'âges
#' @param breaks Points de coupure pour les groupes (défaut: quinquennaux)
#' @param labels Étiquettes pour les groupes
#' @param last_open Dernier groupe ouvert (ex: "85+")
#' @return Facteur avec les groupes d'âge
#' @export
create_age_groups <- function(age, breaks = NULL, labels = NULL, last_open = TRUE) {
  
  if (is.null(breaks)) {
    # Groupes quinquennaux standard
    breaks <- c(0, seq(5, 85, by = 5), Inf)
  }
  
  if (is.null(labels)) {
    n_groups <- length(breaks) - 1
    labels <- character(n_groups)
    for (i in 1:(n_groups - 1)) {
      labels[i] <- sprintf("%d-%d", breaks[i], breaks[i + 1] - 1)
    }
    if (last_open) {
      labels[n_groups] <- sprintf("%d+", breaks[n_groups])
    } else {
      labels[n_groups] <- sprintf("%d-%d", breaks[n_groups], breaks[n_groups + 1] - 1)
    }
  }
  
  age_group <- cut(age, breaks = breaks, labels = labels, right = FALSE, include.lowest = TRUE)
  
  return(age_group)
}

#' Recoder des variables
#'
#' @description Recode des variables selon un dictionnaire de correspondance
#' @param x Vecteur à recoder
#' @param mapping Liste ou vecteur nommé de correspondances
#' @param default Valeur par défaut pour les valeurs non trouvées
#' @return Vecteur recodé
#' @export
recode_variables <- function(x, mapping, default = NA) {
  
  if (is.list(mapping)) {
    mapping <- unlist(mapping)
  }
  
  result <- mapping[as.character(x)]
  result[is.na(result)] <- default
  
  return(unname(result))
}

#' Générer un identifiant unique de transformation
#'
#' @description Crée un identifiant unique pour tracer les transformations
#' @return Chaîne de caractères avec l'identifiant
#' @keywords internal
generate_transformation_id <- function() {
  paste0("TR_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", 
         substr(uuid::UUIDgenerate(), 1, 8))
}

#' Calculer un hash pour la traçabilité
#'
#' @description Calcule un hash MD5 pour un objet
#' @param x Objet à hasher
#' @return Chaîne de caractères avec le hash
#' @keywords internal
calculate_hash <- function(x) {
  digest::digest(x, algo = "md5")
}

#' Formater les nombres pour l'affichage
#'
#' @description Formate les nombres avec séparateurs de milliers
#' @param x Nombre à formater
#' @param digits Nombre de décimales
#' @return Chaîne de caractères formatée
#' @keywords internal
format_number <- function(x, digits = 0) {
  format(round(x, digits), big.mark = " ", scientific = FALSE)
}

#' Formater les pourcentages
#'
#' @description Formate les pourcentages avec le symbole %
#' @param x Nombre à formater (proportion ou pourcentage)
#' @param digits Nombre de décimales
#' @param as_proportion Si TRUE, multiplie par 100
#' @return Chaîne de caractères formatée
#' @keywords internal
format_percent <- function(x, digits = 1, as_proportion = FALSE) {
  if (as_proportion) x <- x * 100
  paste0(format(round(x, digits), nsmall = digits), "%")
}
