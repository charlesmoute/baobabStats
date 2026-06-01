#' @title Module de Détection et Gestion des Doublons
#' @description Fonctions pour détecter et gérer les doublons par apprentissage non supervisé
#' @name duplicate_detection
NULL

#' Détecter les doublons potentiels
#'
#' @description Détecte les enregistrements potentiellement dupliqués
#' @param data Data.frame contenant les données de recensement
#' @param key_vars Variables clés pour la comparaison
#' @param blocking_var Variable de blocage pour réduire les comparaisons
#' @param string_vars Variables textuelles (utiliseront la distance de chaîne)
#' @param numeric_vars Variables numériques (utiliseront la distance euclidienne)
#' @param threshold Seuil de similarité pour considérer comme doublon (0-1)
#' @param method Méthode de clustering ("dbscan", "hdbscan", "hierarchical")
#' @param sample_size Taille de l'échantillon pour les grands jeux de données
#' @return Liste avec les doublons détectés et les statistiques
#' @export
#' @examples
#' \dontrun{
#' duplicates <- detect_duplicates(
#'   data, 
#'   key_vars = c("name", "age", "sex", "admin_unit"),
#'   blocking_var = "admin_unit"
#' )
#' }
detect_duplicates <- function(data, key_vars = NULL, blocking_var = NULL,
                               string_vars = NULL, numeric_vars = NULL,
                               threshold = 0.85, method = "dbscan",
                               sample_size = NULL) {
  
  message("\n========================================")
  message("  DÉTECTION DES DOUBLONS")
  message("========================================\n")
  
  # Identifier automatiquement les types de variables si non spécifiés
  if (is.null(key_vars)) {
    # Utiliser toutes les variables sauf les identifiants
    key_vars <- setdiff(names(data), c("id", "household_id", "individual_id"))
  }
  
  if (is.null(string_vars)) {
    string_vars <- key_vars[sapply(data[key_vars], is.character)]
  }
  
  if (is.null(numeric_vars)) {
    numeric_vars <- key_vars[sapply(data[key_vars], is.numeric)]
  }
  
  message(sprintf("Variables clés: %s", paste(key_vars, collapse = ", ")))
  message(sprintf("Variables textuelles: %s", paste(string_vars, collapse = ", ")))
  message(sprintf("Variables numériques: %s", paste(numeric_vars, collapse = ", ")))
  
  # Échantillonnage si nécessaire
  if (!is.null(sample_size) && nrow(data) > sample_size) {
    message(sprintf("Échantillonnage: %d sur %d enregistrements", sample_size, nrow(data)))
    sample_idx <- sample(nrow(data), sample_size)
    data_sample <- data[sample_idx, ]
  } else {
    data_sample <- data
    sample_idx <- 1:nrow(data)
  }
  
  # Ajouter un index original
  data_sample$.original_idx <- sample_idx
  
  results <- list()
  
  if (!is.null(blocking_var) && blocking_var %in% names(data_sample)) {
    # Détection par blocs
    message(sprintf("\nDétection par blocs (variable: %s)...", blocking_var))
    
    blocks <- unique(data_sample[[blocking_var]])
    all_duplicates <- list()
    
    for (block in blocks) {
      block_data <- data_sample[data_sample[[blocking_var]] == block, ]
      
      if (nrow(block_data) < 2) next
      
      block_duplicates <- detect_duplicates_in_block(
        block_data, key_vars, string_vars, numeric_vars, 
        threshold, method
      )
      
      if (!is.null(block_duplicates) && nrow(block_duplicates) > 0) {
        block_duplicates$block <- block
        all_duplicates[[length(all_duplicates) + 1]] <- block_duplicates
      }
    }
    
    if (length(all_duplicates) > 0) {
      results$duplicates <- do.call(rbind, all_duplicates)
    } else {
      results$duplicates <- data.frame()
    }
    
  } else {
    # Détection globale
    message("\nDétection globale (sans blocage)...")
    results$duplicates <- detect_duplicates_in_block(
      data_sample, key_vars, string_vars, numeric_vars, 
      threshold, method
    )
  }
  
  # Statistiques
  n_duplicates <- if (!is.null(results$duplicates)) nrow(results$duplicates) else 0
  
  results$statistics <- list(
    n_total = nrow(data),
    n_analyzed = nrow(data_sample),
    n_potential_duplicates = n_duplicates,
    pct_duplicates = round(n_duplicates / nrow(data_sample) * 100, 2),
    threshold_used = threshold,
    method_used = method
  )
  
  message(sprintf("\n✓ Détection terminée: %d doublons potentiels trouvés (%.2f%%)",
                  n_duplicates, results$statistics$pct_duplicates))
  
  class(results) <- c("census_duplicates", "list")
  
  return(results)
}

#' Détecter les doublons dans un bloc
#'
#' @description Fonction interne pour détecter les doublons dans un sous-ensemble
#' @param block_data Données du bloc
#' @param key_vars Variables clés
#' @param string_vars Variables textuelles
#' @param numeric_vars Variables numériques
#' @param threshold Seuil de similarité
#' @param method Méthode de clustering
#' @return Data.frame avec les paires de doublons
#' @keywords internal
detect_duplicates_in_block <- function(block_data, key_vars, string_vars, 
                                        numeric_vars, threshold, method) {
  
  if (nrow(block_data) < 2) return(NULL)
  
  # Calculer la matrice de similarité
  similarity_matrix <- compute_similarity_matrix(
    block_data, key_vars, string_vars, numeric_vars
  )
  
  # Convertir en matrice de distance
  distance_matrix <- 1 - similarity_matrix
  
  # Appliquer le clustering
  if (method == "dbscan") {
    clusters <- cluster_dbscan(distance_matrix, threshold)
  } else if (method == "hdbscan") {
    clusters <- cluster_hdbscan(distance_matrix)
  } else {
    clusters <- cluster_hierarchical(distance_matrix, threshold)
  }
  
  # Identifier les paires de doublons
  duplicates <- identify_duplicate_pairs(
    block_data, clusters, similarity_matrix, threshold
  )
  
  return(duplicates)
}

#' Calculer la matrice de similarité
#'
#' @description Calcule la similarité entre tous les enregistrements
#' @param data Données à comparer
#' @param key_vars Variables clés
#' @param string_vars Variables textuelles
#' @param numeric_vars Variables numériques
#' @return Matrice de similarité
#' @keywords internal
compute_similarity_matrix <- function(data, key_vars, string_vars, numeric_vars) {
  
  n <- nrow(data)
  similarity_matrix <- matrix(0, n, n)
  
  n_vars <- length(key_vars)
  
  # Similarité pour les variables textuelles
  if (length(string_vars) > 0) {
    for (var in string_vars) {
      if (!var %in% names(data)) next
      
      values <- as.character(data[[var]])
      values[is.na(values)] <- ""
      
      # Matrice de distance de Jaro-Winkler
      dist_matrix <- stringdist::stringdistmatrix(values, values, method = "jw")
      sim_matrix <- 1 - dist_matrix
      
      similarity_matrix <- similarity_matrix + sim_matrix / n_vars
    }
  }
  
  # Similarité pour les variables numériques
  if (length(numeric_vars) > 0) {
    for (var in numeric_vars) {
      if (!var %in% names(data)) next
      
      values <- as.numeric(data[[var]])
      values[is.na(values)] <- mean(values, na.rm = TRUE)
      
      # Normaliser
      if (sd(values, na.rm = TRUE) > 0) {
        values_norm <- (values - min(values, na.rm = TRUE)) / 
                       (max(values, na.rm = TRUE) - min(values, na.rm = TRUE))
      } else {
        values_norm <- rep(0.5, length(values))
      }
      
      # Distance euclidienne normalisée
      dist_matrix <- as.matrix(dist(values_norm))
      max_dist <- max(dist_matrix, na.rm = TRUE)
      if (max_dist > 0) {
        sim_matrix <- 1 - dist_matrix / max_dist
      } else {
        sim_matrix <- matrix(1, n, n)
      }
      
      similarity_matrix <- similarity_matrix + sim_matrix / n_vars
    }
  }
  
  # Similarité pour les variables catégorielles
  cat_vars <- setdiff(key_vars, c(string_vars, numeric_vars))
  if (length(cat_vars) > 0) {
    for (var in cat_vars) {
      if (!var %in% names(data)) next
      
      values <- as.character(data[[var]])
      
      # Similarité exacte
      sim_matrix <- outer(values, values, "==")
      sim_matrix[is.na(sim_matrix)] <- 0
      
      similarity_matrix <- similarity_matrix + sim_matrix / n_vars
    }
  }
  
  # Diagonale = 1
  diag(similarity_matrix) <- 1
  
  return(similarity_matrix)
}

#' Clustering DBSCAN
#'
#' @description Applique DBSCAN pour regrouper les enregistrements similaires
#' @param distance_matrix Matrice de distance
#' @param threshold Seuil de distance
#' @return Vecteur des clusters
#' @keywords internal
cluster_dbscan <- function(distance_matrix, threshold) {
  
  if (!requireNamespace("dbscan", quietly = TRUE)) {
    stop("Le package 'dbscan' est requis")
  }
  
  # Convertir en objet dist
  dist_obj <- as.dist(distance_matrix)
  
  # Paramètres DBSCAN
  eps <- 1 - threshold  # Distance maximale
  minPts <- 2  # Minimum 2 points pour former un cluster
  
  # Appliquer DBSCAN
  db_result <- dbscan::dbscan(dist_obj, eps = eps, minPts = minPts)
  
  return(db_result$cluster)
}

#' Clustering HDBSCAN
#'
#' @description Applique HDBSCAN pour regrouper les enregistrements similaires
#' @param distance_matrix Matrice de distance
#' @return Vecteur des clusters
#' @keywords internal
cluster_hdbscan <- function(distance_matrix) {
  
  if (!requireNamespace("dbscan", quietly = TRUE)) {
    stop("Le package 'dbscan' est requis")
  }
  
  # Convertir en objet dist
  dist_obj <- as.dist(distance_matrix)
  
  # Appliquer HDBSCAN
  hdb_result <- dbscan::hdbscan(dist_obj, minPts = 2)
  
  return(hdb_result$cluster)
}

#' Clustering hiérarchique
#'
#' @description Applique un clustering hiérarchique
#' @param distance_matrix Matrice de distance
#' @param threshold Seuil pour couper l'arbre
#' @return Vecteur des clusters
#' @keywords internal
cluster_hierarchical <- function(distance_matrix, threshold) {
  
  # Convertir en objet dist
  dist_obj <- as.dist(distance_matrix)
  
  # Clustering hiérarchique
  hc <- hclust(dist_obj, method = "average")
  
  # Couper l'arbre
  clusters <- cutree(hc, h = 1 - threshold)
  
  return(clusters)
}

#' Identifier les paires de doublons
#'
#' @description Identifie les paires d'enregistrements dupliqués
#' @param data Données originales
#' @param clusters Vecteur des clusters
#' @param similarity_matrix Matrice de similarité
#' @param threshold Seuil de similarité
#' @return Data.frame avec les paires de doublons
#' @keywords internal
identify_duplicate_pairs <- function(data, clusters, similarity_matrix, threshold) {
  
  # Identifier les clusters avec plus d'un membre
  cluster_table <- table(clusters)
  duplicate_clusters <- as.numeric(names(cluster_table[cluster_table > 1 & names(cluster_table) != "0"]))
  
  if (length(duplicate_clusters) == 0) {
    return(data.frame())
  }
  
  pairs <- list()
  
  for (cl in duplicate_clusters) {
    members <- which(clusters == cl)
    
    if (length(members) < 2) next
    
    # Créer toutes les paires possibles
    for (i in 1:(length(members) - 1)) {
      for (j in (i + 1):length(members)) {
        idx1 <- members[i]
        idx2 <- members[j]
        
        similarity <- similarity_matrix[idx1, idx2]
        
        if (similarity >= threshold) {
          pair <- data.frame(
            record1_idx = data$.original_idx[idx1],
            record2_idx = data$.original_idx[idx2],
            cluster_id = cl,
            similarity = round(similarity, 4),
            stringsAsFactors = FALSE
          )
          pairs[[length(pairs) + 1]] <- pair
        }
      }
    }
  }
  
  if (length(pairs) == 0) {
    return(data.frame())
  }
  
  result <- do.call(rbind, pairs)
  result <- result[order(-result$similarity), ]
  
  return(result)
}

#' Regrouper les doublons en clusters
#'
#' @description Regroupe les enregistrements dupliqués en clusters
#' @param data Data.frame contenant les données
#' @param duplicates Résultat de detect_duplicates
#' @return Data.frame avec les clusters de doublons
#' @export
cluster_duplicates <- function(data, duplicates) {
  
  if (!inherits(duplicates, "census_duplicates")) {
    stop("L'argument 'duplicates' doit être un résultat de detect_duplicates")
  }
  
  if (is.null(duplicates$duplicates) || nrow(duplicates$duplicates) == 0) {
    message("Aucun doublon à regrouper")
    return(NULL)
  }
  
  dup_data <- duplicates$duplicates
  
  # Créer un graphe des connexions
  all_indices <- unique(c(dup_data$record1_idx, dup_data$record2_idx))
  
  # Utiliser une approche de composantes connexes
  # Initialiser chaque enregistrement dans son propre cluster
  cluster_assignment <- setNames(1:length(all_indices), all_indices)
  
  # Fusionner les clusters pour les paires connectées
  for (i in 1:nrow(dup_data)) {
    idx1 <- as.character(dup_data$record1_idx[i])
    idx2 <- as.character(dup_data$record2_idx[i])
    
    cl1 <- cluster_assignment[idx1]
    cl2 <- cluster_assignment[idx2]
    
    if (cl1 != cl2) {
      # Fusionner les clusters
      cluster_assignment[cluster_assignment == cl2] <- cl1
    }
  }
  
  # Renommer les clusters de manière séquentielle
  unique_clusters <- unique(cluster_assignment)
  cluster_mapping <- setNames(1:length(unique_clusters), unique_clusters)
  cluster_assignment <- cluster_mapping[as.character(cluster_assignment)]
  
  # Créer le résultat
  result <- data.frame(
    original_idx = as.numeric(names(cluster_assignment)),
    duplicate_cluster = as.numeric(cluster_assignment),
    stringsAsFactors = FALSE
  )
  
  # Ajouter les données originales
  result <- merge(result, data, by.x = "original_idx", by.y = 0, all.x = TRUE)
  
  # Trier par cluster
  result <- result[order(result$duplicate_cluster, result$original_idx), ]
  
  # Statistiques par cluster
  cluster_stats <- result %>%
    dplyr::group_by(duplicate_cluster) %>%
    dplyr::summarise(
      n_records = dplyr::n(),
      .groups = "drop"
    )
  
  message(sprintf("✓ %d clusters de doublons identifiés", max(result$duplicate_cluster)))
  message(sprintf("  Taille moyenne des clusters: %.1f enregistrements", 
                  mean(cluster_stats$n_records)))
  
  return(list(
    clustered_data = result,
    cluster_stats = cluster_stats
  ))
}

#' Fusionner les doublons
#'
#' @description Fusionne les enregistrements dupliqués en un seul
#' @param data Data.frame contenant les données
#' @param duplicates Résultat de detect_duplicates ou cluster_duplicates
#' @param merge_strategy Stratégie de fusion ("first", "most_complete", "consensus")
#' @param priority_vars Variables prioritaires pour la fusion
#' @return Data.frame avec les doublons fusionnés
#' @export
merge_duplicates <- function(data, duplicates, merge_strategy = "most_complete",
                              priority_vars = NULL) {
  
  message("\n=== FUSION DES DOUBLONS ===\n")
  
  # Obtenir les clusters
  if (inherits(duplicates, "census_duplicates")) {
    clusters <- cluster_duplicates(data, duplicates)
  } else if (is.list(duplicates) && "clustered_data" %in% names(duplicates)) {
    clusters <- duplicates
  } else {
    stop("Format de doublons non reconnu")
  }
  
  if (is.null(clusters)) {
    message("Aucun doublon à fusionner")
    return(data)
  }
  
  clustered_data <- clusters$clustered_data
  
  # Identifier les indices des doublons
  duplicate_indices <- unique(clustered_data$original_idx)
  
  # Séparer les données
  data_no_duplicates <- data[-duplicate_indices, ]
  
  # Fusionner chaque cluster
  merged_records <- list()
  
  for (cl in unique(clustered_data$duplicate_cluster)) {
    cluster_records <- clustered_data[clustered_data$duplicate_cluster == cl, ]
    
    merged <- merge_cluster_records(
      cluster_records, 
      merge_strategy, 
      priority_vars
    )
    
    merged_records[[length(merged_records) + 1]] <- merged
  }
  
  # Combiner les enregistrements fusionnés
  merged_df <- do.call(rbind, merged_records)
  
  # Combiner avec les non-doublons
  # S'assurer que les colonnes correspondent
  common_cols <- intersect(names(data_no_duplicates), names(merged_df))
  
  result <- rbind(
    data_no_duplicates[, common_cols],
    merged_df[, common_cols]
  )
  
  n_removed <- nrow(data) - nrow(result)
  
  message(sprintf("✓ Fusion terminée: %d enregistrements supprimés", n_removed))
  message(sprintf("  Données originales: %d lignes", nrow(data)))
  message(sprintf("  Données après fusion: %d lignes", nrow(result)))
  
  return(result)
}

#' Fusionner les enregistrements d'un cluster
#'
#' @description Fusionne les enregistrements d'un cluster de doublons
#' @param cluster_records Enregistrements du cluster
#' @param strategy Stratégie de fusion
#' @param priority_vars Variables prioritaires
#' @return Enregistrement fusionné
#' @keywords internal
merge_cluster_records <- function(cluster_records, strategy, priority_vars) {
  
  if (nrow(cluster_records) == 1) {
    return(cluster_records[1, ])
  }
  
  # Exclure les colonnes de métadonnées
  data_cols <- setdiff(names(cluster_records), 
                       c("original_idx", "duplicate_cluster"))
  
  if (strategy == "first") {
    # Prendre le premier enregistrement
    merged <- cluster_records[1, data_cols]
    
  } else if (strategy == "most_complete") {
    # Prendre l'enregistrement le plus complet
    completeness <- apply(cluster_records[, data_cols], 1, function(x) sum(!is.na(x)))
    best_idx <- which.max(completeness)
    merged <- cluster_records[best_idx, data_cols]
    
    # Compléter avec les autres enregistrements
    for (col in data_cols) {
      if (is.na(merged[[col]])) {
        non_na_values <- cluster_records[[col]][!is.na(cluster_records[[col]])]
        if (length(non_na_values) > 0) {
          merged[[col]] <- non_na_values[1]
        }
      }
    }
    
  } else if (strategy == "consensus") {
    # Prendre la valeur la plus fréquente pour chaque variable
    merged <- cluster_records[1, data_cols]
    
    for (col in data_cols) {
      values <- cluster_records[[col]]
      values <- values[!is.na(values)]
      
      if (length(values) > 0) {
        # Mode (valeur la plus fréquente)
        freq_table <- table(values)
        merged[[col]] <- names(freq_table)[which.max(freq_table)]
      }
    }
  }
  
  return(merged)
}

#' Générer un rapport sur les doublons
#'
#' @description Génère un rapport détaillé sur les doublons détectés
#' @param duplicates Résultat de detect_duplicates
#' @param data Data.frame original (optionnel, pour plus de détails)
#' @param output_file Chemin du fichier de sortie
#' @return Liste avec le rapport
#' @export
duplicate_report <- function(duplicates, data = NULL, output_file = NULL) {
  
  if (!inherits(duplicates, "census_duplicates")) {
    stop("L'argument doit être un résultat de detect_duplicates")
  }
  
  report <- list(
    summary = duplicates$statistics,
    timestamp = Sys.time()
  )
  
  # Distribution des scores de similarité
  if (!is.null(duplicates$duplicates) && nrow(duplicates$duplicates) > 0) {
    report$similarity_distribution <- summary(duplicates$duplicates$similarity)
    
    # Top 10 des paires les plus similaires
    report$top_duplicates <- head(duplicates$duplicates, 10)
    
    # Distribution par bloc (si applicable)
    if ("block" %in% names(duplicates$duplicates)) {
      report$by_block <- duplicates$duplicates %>%
        dplyr::group_by(block) %>%
        dplyr::summarise(
          n_pairs = dplyr::n(),
          avg_similarity = mean(similarity),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(n_pairs))
    }
  }
  
  # Afficher le rapport
  cat("\n========================================\n")
  cat("  RAPPORT SUR LES DOUBLONS\n")
  cat("========================================\n\n")
  
  cat("--- Résumé ---\n")
  cat(sprintf("Enregistrements analysés: %d\n", report$summary$n_analyzed))
  cat(sprintf("Doublons potentiels: %d (%.2f%%)\n", 
              report$summary$n_potential_duplicates,
              report$summary$pct_duplicates))
  cat(sprintf("Méthode utilisée: %s\n", report$summary$method_used))
  cat(sprintf("Seuil de similarité: %.2f\n", report$summary$threshold_used))
  
  if (!is.null(report$similarity_distribution)) {
    cat("\n--- Distribution des similarités ---\n")
    print(report$similarity_distribution)
  }
  
  if (!is.null(report$top_duplicates)) {
    cat("\n--- Top 10 des paires les plus similaires ---\n")
    print(report$top_duplicates)
  }
  
  # Sauvegarder si demandé
  if (!is.null(output_file)) {
    saveRDS(report, output_file)
    message(sprintf("\nRapport sauvegardé: %s", output_file))
  }
  
  invisible(report)
}

#' Méthode print pour census_duplicates
#'
#' @param x Objet census_duplicates
#' @param ... Arguments supplémentaires
#' @export
print.census_duplicates <- function(x, ...) {
  cat("\n=== RÉSULTAT DE DÉTECTION DES DOUBLONS ===\n\n")
  
  cat("--- Statistiques ---\n")
  cat(sprintf("Enregistrements totaux: %d\n", x$statistics$n_total))
  cat(sprintf("Enregistrements analysés: %d\n", x$statistics$n_analyzed))
  cat(sprintf("Doublons potentiels: %d (%.2f%%)\n", 
              x$statistics$n_potential_duplicates,
              x$statistics$pct_duplicates))
  cat(sprintf("Méthode: %s\n", x$statistics$method_used))
  cat(sprintf("Seuil: %.2f\n", x$statistics$threshold_used))
  
  if (!is.null(x$duplicates) && nrow(x$duplicates) > 0) {
    cat("\n--- Aperçu des doublons ---\n")
    print(head(x$duplicates, 5))
    if (nrow(x$duplicates) > 5) {
      cat(sprintf("... et %d autres paires\n", nrow(x$duplicates) - 5))
    }
  }
  
  invisible(x)
}
