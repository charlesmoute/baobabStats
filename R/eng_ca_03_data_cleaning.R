#' @title Module de Nettoyage Automatisé des Données
#' @description Fonctions pour le nettoyage des données avec traçabilité complète
#' @name data_cleaning
NULL

# Variable globale pour stocker le journal des transformations
.transformation_log <- new.env(parent = emptyenv())
.transformation_log$entries <- list()

#' Initialiser le journal des transformations
#'
#' @description Initialise ou réinitialise le journal des transformations
#' @return Invisible NULL
#' @keywords internal
init_transformation_log <- function() {
  .transformation_log$entries <- list()
  .transformation_log$session_id <- generate_transformation_id()
  .transformation_log$start_time <- Sys.time()
  invisible(NULL)
}

#' Ajouter une entrée au journal
#'
#' @description Ajoute une transformation au journal
#' @param operation Type d'opération
#' @param variable Variable concernée
#' @param n_affected Nombre d'enregistrements affectés
#' @param details Détails supplémentaires
#' @keywords internal
log_transformation <- function(operation, variable, n_affected, details = NULL) {
  entry <- list(
    id = length(.transformation_log$entries) + 1,
    timestamp = Sys.time(),
    operation = operation,
    variable = variable,
    n_affected = n_affected,
    details = details
  )
  .transformation_log$entries[[length(.transformation_log$entries) + 1]] <- entry
}

#' Obtenir le journal des transformations
#'
#' @description Récupère le journal complet des transformations
#' @param as_dataframe Retourner sous forme de data.frame
#' @return Liste ou data.frame des transformations
#' @export
get_transformation_log <- function(as_dataframe = TRUE) {
  
  if (length(.transformation_log$entries) == 0) {
    message("Aucune transformation enregistrée")
    return(NULL)
  }
  
  if (as_dataframe) {
    log_df <- do.call(rbind, lapply(.transformation_log$entries, function(e) {
      data.frame(
        id = e$id,
        timestamp = as.character(e$timestamp),
        operation = e$operation,
        variable = e$variable,
        n_affected = e$n_affected,
        details = if (is.null(e$details)) NA else paste(names(e$details), e$details, sep = "=", collapse = "; "),
        stringsAsFactors = FALSE
      )
    }))
    return(log_df)
  }
  
  return(.transformation_log$entries)
}

#' Standardiser les toponymes
#'
#' @description Standardise les noms de lieux avec correspondance floue
#' @param data Data.frame contenant les données
#' @param toponym_vars Vecteur des noms de variables contenant des toponymes
#' @param reference_list Liste de référence des toponymes standards (optionnel)
#' @param method Méthode de correspondance ("jw", "lv", "cosine", "jaccard")
#' @param threshold Seuil de similarité (0-1)
#' @param log_changes Enregistrer les changements dans le journal
#' @return Data.frame avec les toponymes standardisés
#' @export
#' @examples
#' \dontrun{
#' data_clean <- standardize_toponyms(data, c("region", "commune"))
#' }
standardize_toponyms <- function(data, toponym_vars, reference_list = NULL,
                                  method = "jw", threshold = 0.85,
                                  log_changes = TRUE) {
  
  if (log_changes) init_transformation_log()
  
  data_clean <- data
  
  for (var in toponym_vars) {
    if (!var %in% names(data)) {
      warning("Variable '", var, "' non trouvée dans les données")
      next
    }
    
    original_values <- data_clean[[var]]
    unique_values <- unique(na.omit(original_values))
    
    # Si pas de liste de référence, créer une liste à partir des valeurs les plus fréquentes
    if (is.null(reference_list)) {
      freq_table <- sort(table(original_values), decreasing = TRUE)
      # Prendre les valeurs représentant au moins 1% des données
      threshold_count <- max(1, length(original_values) * 0.01)
      reference <- names(freq_table[freq_table >= threshold_count])
    } else {
      reference <- reference_list[[var]]
      if (is.null(reference)) reference <- reference_list
    }
    
    # Pré-traitement des valeurs
    clean_values <- toupper(trimws(unique_values))
    clean_reference <- toupper(trimws(reference))
    
    # Créer la matrice de distance
    dist_matrix <- stringdist::stringdistmatrix(
      clean_values, 
      clean_reference, 
      method = method
    )
    
    # Convertir en similarité
    max_len <- outer(nchar(clean_values), nchar(clean_reference), pmax)
    similarity_matrix <- 1 - dist_matrix / max_len
    
    # Créer le mapping
    mapping <- character(length(unique_values))
    names(mapping) <- unique_values
    
    for (i in seq_along(unique_values)) {
      best_match_idx <- which.max(similarity_matrix[i, ])
      best_similarity <- similarity_matrix[i, best_match_idx]
      
      if (best_similarity >= threshold) {
        mapping[i] <- reference[best_match_idx]
      } else {
        mapping[i] <- unique_values[i]  # Garder la valeur originale
      }
    }
    
    # Appliquer le mapping
    data_clean[[var]] <- mapping[as.character(original_values)]
    
    # Compter les changements
    n_changed <- sum(original_values != data_clean[[var]], na.rm = TRUE)
    
    if (log_changes && n_changed > 0) {
      log_transformation(
        operation = "standardize_toponym",
        variable = var,
        n_affected = n_changed,
        details = list(
          method = method,
          threshold = threshold,
          n_unique_before = length(unique_values),
          n_unique_after = length(unique(na.omit(data_clean[[var]])))
        )
      )
    }
    
    message(sprintf("Variable '%s': %d valeurs standardisées", var, n_changed))
  }
  
  # Ajouter les métadonnées
  attr(data_clean, "toponyms_standardized") <- TRUE
  attr(data_clean, "standardization_method") <- method
  
  return(data_clean)
}

#' Imputation des données manquantes avec MICE
#'
#' @description Impute les valeurs manquantes en utilisant l'algorithme MICE
#' @param data Data.frame contenant les données
#' @param vars_to_impute Variables à imputer (NULL = toutes)
#' @param m Nombre d'imputations multiples
#' @param maxit Nombre maximum d'itérations
#' @param method Méthode d'imputation par variable
#' @param seed Graine pour la reproductibilité
#' @param log_changes Enregistrer les changements
#' @return Data.frame avec les valeurs imputées
#' @export
impute_mice <- function(data, vars_to_impute = NULL, m = 5, maxit = 10,
                        method = NULL, seed = 123, log_changes = TRUE) {
  
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop("Le package 'mice' est requis. Installez-le avec install.packages('mice')")
  }
  
  set.seed(seed)
  
  # Sélectionner les variables à imputer
  if (is.null(vars_to_impute)) {
    # Imputer toutes les variables avec des valeurs manquantes
    vars_with_na <- names(data)[sapply(data, function(x) any(is.na(x)))]
    vars_to_impute <- vars_with_na
  }
  
  if (length(vars_to_impute) == 0) {
    message("Aucune variable à imputer")
    return(data)
  }
  
  # Compter les valeurs manquantes avant
  na_before <- sapply(data[vars_to_impute], function(x) sum(is.na(x)))
  
  message("Imputation MICE en cours...")
  message(sprintf("Variables à imputer: %s", paste(vars_to_impute, collapse = ", ")))
  message(sprintf("Valeurs manquantes totales: %d", sum(na_before)))
  
  # Exécuter MICE
  mice_result <- mice::mice(
    data, 
    m = m, 
    maxit = maxit, 
    method = method,
    seed = seed,
    printFlag = FALSE
  )
  
  # Compléter avec la première imputation
  data_imputed <- mice::complete(mice_result, 1)
  
  # Compter les valeurs imputées
  na_after <- sapply(data_imputed[vars_to_impute], function(x) sum(is.na(x)))
  n_imputed <- sum(na_before - na_after)
  
  if (log_changes) {
    for (var in vars_to_impute) {
      if (na_before[var] > na_after[var]) {
        log_transformation(
          operation = "impute_mice",
          variable = var,
          n_affected = na_before[var] - na_after[var],
          details = list(m = m, maxit = maxit, seed = seed)
        )
      }
    }
  }
  
  message(sprintf("✓ %d valeurs imputées avec MICE", n_imputed))
  
  # Stocker les informations d'imputation
  attr(data_imputed, "mice_result") <- mice_result
  attr(data_imputed, "imputation_method") <- "mice"
  
  return(data_imputed)
}

#' Imputation des données manquantes avec missForest
#'
#' @description Impute les valeurs manquantes en utilisant Random Forest
#' @param data Data.frame contenant les données
#' @param vars_to_impute Variables à imputer (NULL = toutes)
#' @param maxiter Nombre maximum d'itérations
#' @param ntree Nombre d'arbres dans la forêt
#' @param parallelize Utiliser le calcul parallèle
#' @param seed Graine pour la reproductibilité
#' @param log_changes Enregistrer les changements
#' @return Data.frame avec les valeurs imputées
#' @export
impute_missforest <- function(data, vars_to_impute = NULL, maxiter = 10,
                               ntree = 100, parallelize = "no",
                               seed = 123, log_changes = TRUE) {
  
  if (!requireNamespace("missForest", quietly = TRUE)) {
    stop("Le package 'missForest' est requis. Installez-le avec install.packages('missForest')")
  }
  
  set.seed(seed)
  
  # Compter les valeurs manquantes avant
  na_before <- sapply(data, function(x) sum(is.na(x)))
  total_na_before <- sum(na_before)
  
  if (total_na_before == 0) {
    message("Aucune valeur manquante à imputer")
    return(data)
  }
  
  message("Imputation missForest en cours...")
  message(sprintf("Valeurs manquantes totales: %d", total_na_before))
  
  # Exécuter missForest
  mf_result <- missForest::missForest(
    data,
    maxiter = maxiter,
    ntree = ntree,
    parallelize = parallelize,
    verbose = FALSE
  )
  
  data_imputed <- mf_result$ximp
  
  # Compter les valeurs imputées
  na_after <- sapply(data_imputed, function(x) sum(is.na(x)))
  n_imputed <- total_na_before - sum(na_after)
  
  if (log_changes) {
    for (var in names(data)) {
      if (na_before[var] > na_after[var]) {
        log_transformation(
          operation = "impute_missforest",
          variable = var,
          n_affected = na_before[var] - na_after[var],
          details = list(maxiter = maxiter, ntree = ntree, 
                        oob_error = mf_result$OOBerror)
        )
      }
    }
  }
  
  message(sprintf("✓ %d valeurs imputées avec missForest", n_imputed))
  message(sprintf("Erreur OOB: %.4f", mean(mf_result$OOBerror, na.rm = TRUE)))
  
  # Stocker les informations
  attr(data_imputed, "missforest_result") <- mf_result
  attr(data_imputed, "imputation_method") <- "missforest"
  
  return(data_imputed)
}

#' Imputation intelligente basée sur des modèles démographiques
#'
#' @description Impute les valeurs manquantes en utilisant des contraintes démographiques
#' @param data Data.frame contenant les données
#' @param method Méthode d'imputation ("mice", "missforest", "demographic", "auto")
#' @param demographic_constraints Liste de contraintes démographiques
#' @param seed Graine pour la reproductibilité
#' @param log_changes Enregistrer les changements
#' @return Data.frame avec les valeurs imputées
#' @export
impute_missing <- function(data, method = "auto", 
                           demographic_constraints = NULL,
                           seed = 123, log_changes = TRUE) {
  
  if (log_changes) init_transformation_log()
  
  # Analyser les patterns de données manquantes
  na_pattern <- analyze_missing_pattern(data)
  
  message("\n=== IMPUTATION DES DONNÉES MANQUANTES ===")
  message(sprintf("Taux global de données manquantes: %.2f%%", na_pattern$overall_rate * 100))
  
  # Choisir la méthode automatiquement si nécessaire
  if (method == "auto") {
    if (na_pattern$overall_rate < 0.05) {
      method <- "mice"
    } else if (na_pattern$overall_rate < 0.20) {
      method <- "missforest"
    } else {
      method <- "demographic"
    }
    message(sprintf("Méthode sélectionnée automatiquement: %s", method))
  }
  
  # Appliquer les contraintes démographiques d'abord
  if (!is.null(demographic_constraints) || method == "demographic") {
    data <- apply_demographic_constraints(data, demographic_constraints, log_changes)
  }
  
  # Imputer les valeurs restantes
  if (method == "mice") {
    data <- impute_mice(data, seed = seed, log_changes = log_changes)
  } else if (method == "missforest") {
    data <- impute_missforest(data, seed = seed, log_changes = log_changes)
  }
  
  return(data)
}

#' Analyser les patterns de données manquantes
#'
#' @description Analyse la structure des données manquantes
#' @param data Data.frame à analyser
#' @return Liste avec les statistiques des données manquantes
#' @keywords internal
analyze_missing_pattern <- function(data) {
  
  n_total <- nrow(data) * ncol(data)
  n_missing <- sum(is.na(data))
  
  by_variable <- sapply(data, function(x) {
    c(n_missing = sum(is.na(x)),
      pct_missing = mean(is.na(x)) * 100)
  })
  
  # Pattern de données manquantes
  complete_cases <- sum(complete.cases(data))
  
  list(
    overall_rate = n_missing / n_total,
    by_variable = t(by_variable),
    complete_cases = complete_cases,
    pct_complete_cases = complete_cases / nrow(data) * 100
  )
}

#' Appliquer des contraintes démographiques
#'
#' @description Impute les valeurs en respectant les contraintes démographiques
#' @param data Data.frame contenant les données
#' @param constraints Liste de contraintes
#' @param log_changes Enregistrer les changements
#' @return Data.frame avec les valeurs imputées
#' @keywords internal
apply_demographic_constraints <- function(data, constraints = NULL, log_changes = TRUE) {
  
  # Contraintes par défaut
  if (is.null(constraints)) {
    constraints <- list(
      # Âge doit être positif
      age = list(min = 0, max = 120),
      # Enfants nés ne peut pas être négatif
      children_born = list(min = 0),
      # Enfants survivants <= enfants nés
      children_alive = list(max_var = "children_born")
    )
  }
  
  n_corrected <- 0
  
  # Appliquer les contraintes de plage
  for (var in names(constraints)) {
    if (!var %in% names(data)) next
    
    constraint <- constraints[[var]]
    
    # Contrainte minimum
    if (!is.null(constraint$min)) {
      invalid <- !is.na(data[[var]]) & data[[var]] < constraint$min
      if (any(invalid)) {
        data[[var]][invalid] <- constraint$min
        n_corrected <- n_corrected + sum(invalid)
        
        if (log_changes) {
          log_transformation(
            operation = "demographic_constraint",
            variable = var,
            n_affected = sum(invalid),
            details = list(constraint = paste("min =", constraint$min))
          )
        }
      }
    }
    
    # Contrainte maximum
    if (!is.null(constraint$max)) {
      invalid <- !is.na(data[[var]]) & data[[var]] > constraint$max
      if (any(invalid)) {
        data[[var]][invalid] <- constraint$max
        n_corrected <- n_corrected + sum(invalid)
        
        if (log_changes) {
          log_transformation(
            operation = "demographic_constraint",
            variable = var,
            n_affected = sum(invalid),
            details = list(constraint = paste("max =", constraint$max))
          )
        }
      }
    }
    
    # Contrainte par rapport à une autre variable
    if (!is.null(constraint$max_var) && constraint$max_var %in% names(data)) {
      invalid <- !is.na(data[[var]]) & !is.na(data[[constraint$max_var]]) &
                 data[[var]] > data[[constraint$max_var]]
      if (any(invalid)) {
        data[[var]][invalid] <- data[[constraint$max_var]][invalid]
        n_corrected <- n_corrected + sum(invalid)
        
        if (log_changes) {
          log_transformation(
            operation = "demographic_constraint",
            variable = var,
            n_affected = sum(invalid),
            details = list(constraint = paste("max_var =", constraint$max_var))
          )
        }
      }
    }
  }
  
  if (n_corrected > 0) {
    message(sprintf("✓ %d valeurs corrigées selon les contraintes démographiques", n_corrected))
  }
  
  return(data)
}

#' Nettoyage complet des données de recensement
#'
#' @description Effectue un nettoyage complet des données avec traçabilité
#' @param data Data.frame contenant les données de recensement
#' @param toponym_vars Variables de toponymes à standardiser
#' @param imputation_method Méthode d'imputation
#' @param remove_duplicates Supprimer les doublons évidents
#' @param apply_constraints Appliquer les contraintes démographiques
#' @param seed Graine pour la reproductibilité
#' @return Liste avec les données nettoyées et le journal
#' @export
clean_census_data <- function(data, toponym_vars = NULL,
                               imputation_method = "auto",
                               remove_duplicates = TRUE,
                               apply_constraints = TRUE,
                               seed = 123) {
  
  init_transformation_log()
  
  message("\n========================================")
  message("  NETTOYAGE DES DONNÉES DE RECENSEMENT")
  message("========================================\n")
  
  # Statistiques initiales
  initial_stats <- list(
    n_rows = nrow(data),
    n_cols = ncol(data),
    n_missing = sum(is.na(data)),
    hash = calculate_hash(data)
  )
  
  message(sprintf("Données initiales: %d lignes, %d colonnes", 
                  initial_stats$n_rows, initial_stats$n_cols))
  message(sprintf("Valeurs manquantes: %d", initial_stats$n_missing))
  
  data_clean <- data
  
  # 1. Standardisation des toponymes
  if (!is.null(toponym_vars) && length(toponym_vars) > 0) {
    message("\n--- Étape 1: Standardisation des toponymes ---")
    data_clean <- standardize_toponyms(data_clean, toponym_vars, log_changes = TRUE)
  }
  
  # 2. Application des contraintes démographiques
  if (apply_constraints) {
    message("\n--- Étape 2: Application des contraintes démographiques ---")
    data_clean <- apply_demographic_constraints(data_clean, log_changes = TRUE)
  }
  
  # 3. Suppression des doublons évidents
  if (remove_duplicates) {
    message("\n--- Étape 3: Suppression des doublons évidents ---")
    n_before <- nrow(data_clean)
    data_clean <- unique(data_clean)
    n_removed <- n_before - nrow(data_clean)
    
    if (n_removed > 0) {
      log_transformation(
        operation = "remove_exact_duplicates",
        variable = "all",
        n_affected = n_removed,
        details = NULL
      )
      message(sprintf("✓ %d doublons exacts supprimés", n_removed))
    }
  }
  
  # 4. Imputation des valeurs manquantes
  message("\n--- Étape 4: Imputation des valeurs manquantes ---")
  data_clean <- impute_missing(data_clean, method = imputation_method, 
                               seed = seed, log_changes = TRUE)
  
  # Statistiques finales
  final_stats <- list(
    n_rows = nrow(data_clean),
    n_cols = ncol(data_clean),
    n_missing = sum(is.na(data_clean)),
    hash = calculate_hash(data_clean)
  )
  
  message("\n========================================")
  message("  RÉSUMÉ DU NETTOYAGE")
  message("========================================")
  message(sprintf("Lignes: %d → %d", initial_stats$n_rows, final_stats$n_rows))
  message(sprintf("Valeurs manquantes: %d → %d", initial_stats$n_missing, final_stats$n_missing))
  
  # Récupérer le journal
  transformation_log <- get_transformation_log(as_dataframe = TRUE)
  
  result <- list(
    data = data_clean,
    transformation_log = transformation_log,
    initial_stats = initial_stats,
    final_stats = final_stats,
    cleaning_timestamp = Sys.time()
  )
  
  class(result) <- c("census_cleaning_result", "list")
  
  return(result)
}

#' Valider le nettoyage des données
#'
#' @description Vérifie que le nettoyage a été effectué correctement
#' @param cleaning_result Résultat de clean_census_data
#' @return Liste avec les résultats de validation
#' @export
validate_cleaning <- function(cleaning_result) {
  
  if (!inherits(cleaning_result, "census_cleaning_result")) {
    stop("L'argument doit être un résultat de clean_census_data")
  }
  
  validation <- list(
    is_valid = TRUE,
    checks = list()
  )
  
  data <- cleaning_result$data
  
  # Vérification 1: Pas de valeurs manquantes critiques
  critical_vars <- c("age", "sex")
  for (var in critical_vars) {
    if (var %in% names(data)) {
      n_missing <- sum(is.na(data[[var]]))
      validation$checks[[paste0("missing_", var)]] <- list(
        passed = n_missing == 0,
        n_missing = n_missing
      )
      if (n_missing > 0) validation$is_valid <- FALSE
    }
  }
  
  # Vérification 2: Contraintes démographiques respectées
  if ("age" %in% names(data)) {
    invalid_ages <- sum(data$age < 0 | data$age > 120, na.rm = TRUE)
    validation$checks$valid_ages <- list(
      passed = invalid_ages == 0,
      n_invalid = invalid_ages
    )
    if (invalid_ages > 0) validation$is_valid <- FALSE
  }
  
  # Vérification 3: Cohérence enfants nés/survivants
  if (all(c("children_born", "children_alive") %in% names(data))) {
    inconsistent <- sum(data$children_alive > data$children_born, na.rm = TRUE)
    validation$checks$children_consistency <- list(
      passed = inconsistent == 0,
      n_inconsistent = inconsistent
    )
    if (inconsistent > 0) validation$is_valid <- FALSE
  }
  
  # Résumé
  validation$summary <- sprintf(
    "Validation %s: %d/%d vérifications passées",
    if (validation$is_valid) "RÉUSSIE" else "ÉCHOUÉE",
    sum(sapply(validation$checks, function(x) x$passed)),
    length(validation$checks)
  )
  
  message(validation$summary)
  
  return(validation)
}

#' Méthode print pour census_cleaning_result
#'
#' @param x Objet census_cleaning_result
#' @param ... Arguments supplémentaires
#' @export
print.census_cleaning_result <- function(x, ...) {
  cat("\n=== RÉSULTAT DU NETTOYAGE DES DONNÉES ===\n\n")
  
  cat("Date du nettoyage:", format(x$cleaning_timestamp, "%Y-%m-%d %H:%M:%S"), "\n\n")
  
  cat("--- Statistiques ---\n")
  cat(sprintf("Lignes: %d → %d (-%d)\n", 
              x$initial_stats$n_rows, 
              x$final_stats$n_rows,
              x$initial_stats$n_rows - x$final_stats$n_rows))
  cat(sprintf("Valeurs manquantes: %d → %d (-%d)\n",
              x$initial_stats$n_missing,
              x$final_stats$n_missing,
              x$initial_stats$n_missing - x$final_stats$n_missing))
  
  cat("\n--- Journal des transformations ---\n")
  if (!is.null(x$transformation_log) && nrow(x$transformation_log) > 0) {
    print(x$transformation_log[, c("operation", "variable", "n_affected")], row.names = FALSE)
  } else {
    cat("Aucune transformation enregistrée\n")
  }
  
  cat("\n--- Hashes pour reproductibilité ---\n")
  cat(sprintf("Initial: %s\n", x$initial_stats$hash))
  cat(sprintf("Final: %s\n", x$final_stats$hash))
  
  invisible(x)
}
