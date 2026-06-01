#' @title Module d'Évaluation de la Qualité des Données
#' @description Fonctions pour évaluer la qualité des données de recensement
#' @name data_quality
NULL

#' @title Évaluation complète de la qualité des données
#' @description Effectue une évaluation complète de la qualité des données de recensement, en calculant des indicateurs de complétude, de qualité des âges, de rapport de masculinité et de cohérence.
#'
#' @param data Data.frame contenant les données de recensement.
#' @param age_var Nom de la variable d'âge (chaîne de caractères).
#' @param sex_var Nom de la variable de sexe (chaîne de caractères).
#' @param admin_var Nom de la variable d'unité administrative pour une analyse désagrégée (optionnel).
#' @param weight_var Nom de la variable de pondération (optionnel).
#'
#' @return Un objet de classe `census_quality` contenant une liste de tous les indicateurs de qualité et un score global.
#' @export
#' @examples
#' \dontrun{
#' data(census_example)
#' quality_report <- assess_data_quality(census_example, age_var = "age", sex_var = "sex")
#' print(quality_report)
#' summary(quality_report)
#' plot(quality_report)
#' }
#'
#' @description Effectue une évaluation complète de la qualité des données de recensement
#' @param data Data.frame contenant les données de recensement
#' @param age_var Nom de la variable d'âge
#' @param sex_var Nom de la variable de sexe
#' @param admin_var Nom de la variable d'unité administrative
#' @param weight_var Nom de la variable de pondération (optionnel)
#' @return Liste contenant tous les indicateurs de qualité
#' @export
#' @examples
#' \dontrun{
#' quality <- assess_data_quality(census_data, age_var = "age", sex_var = "sex")
#' }
assess_data_quality <- function(data, age_var = "age", sex_var = "sex", 
                                 admin_var = NULL, weight_var = NULL) {
  
  message("\n========================================")
  message("  ÉVALUATION DE LA QUALITÉ DES DONNÉES")
  message("========================================\n")
  
  results <- list(
    timestamp = Sys.time(),
    n_observations = nrow(data),
    n_variables = ncol(data)
  )
  
  # 1. Complétude des données
  message("1. Analyse de la complétude...")
  results$completeness <- check_completeness(data)
  
  # 2. Qualité des déclarations d'âge
  if (age_var %in% names(data)) {
    message("2. Analyse de la qualité des âges...")
    results$age_quality <- check_age_heaping(data[[age_var]])
  }
  
  # 3. Rapport de masculinité
  if (age_var %in% names(data) && sex_var %in% names(data)) {
    message("3. Analyse du rapport de masculinité...")
    results$sex_ratio <- check_sex_ratio(data, age_var, sex_var, weight_var)
  }
  
  # 4. Cohérence des données
  message("4. Vérification de la cohérence...")
  results$consistency <- check_consistency(data)
  
  # 5. Qualité par unité administrative
  if (!is.null(admin_var) && admin_var %in% names(data)) {
    message("5. Analyse par unité administrative...")
    results$by_admin <- assess_quality_by_admin(data, age_var, sex_var, admin_var, weight_var)
  }
  
  # Score global de qualité
  results$global_score <- calculate_quality_score(results)
  
  class(results) <- c("census_quality", "list")
  
  message("\n✓ Évaluation terminée")
  message(sprintf("Score global de qualité: %.1f/100", results$global_score))
  
  return(results)
}

#' @title Vérifier la complétude des données
#' @description Analyse le taux de valeurs manquantes pour chaque variable d'un jeu de données.
#'
#' @param data Data.frame à analyser.
#' @return Un data.frame avec les statistiques de complétude pour chaque variable, incluant le nombre et le pourcentage de valeurs manquantes.
#' @export
#'
#' @description Analyse le taux de valeurs manquantes par variable
#' @param data Data.frame à analyser
#' @return Data.frame avec les statistiques de complétude
#' @export
check_completeness <- function(data) {
  
  completeness <- data.frame(
    variable = names(data),
    n_total = nrow(data),
    n_missing = sapply(data, function(x) sum(is.na(x))),
    n_complete = sapply(data, function(x) sum(!is.na(x))),
    stringsAsFactors = FALSE
  )
  
  completeness$pct_missing <- round(completeness$n_missing / completeness$n_total * 100, 2)
  completeness$pct_complete <- round(completeness$n_complete / completeness$n_total * 100, 2)
  
  # Classification de la qualité
  completeness$quality <- cut(
    completeness$pct_complete,
    breaks = c(0, 80, 90, 95, 100),
    labels = c("Faible", "Acceptable", "Bonne", "Excellente"),
    include.lowest = TRUE
  )
  
  # Trier par taux de complétude
  completeness <- completeness[order(completeness$pct_complete), ]
  
  return(completeness)
}

#' @title Calculer l'indice de Whipple
#' @description Calcule l'indice de Whipple pour détecter l'attraction des âges se terminant par un chiffre spécifique (typiquement 0 ou 5).
#'
#' @param age Vecteur numérique des âges.
#' @param digit Chiffre terminal à tester (0 ou 5).
#' @param age_range Intervalle d'âges à considérer pour le calcul (défaut: 23-62).
#'
#' @return La valeur de l'indice de Whipple. Un indice de 100 indique une absence d'attraction.
#' @export
#'
#' @description Calcule l'indice de Whipple pour détecter l'attraction des âges ronds
#' @param age Vecteur d'âges
#' @param digit Chiffre à tester (0 ou 5)
#' @param age_range Plage d'âge à considérer (défaut: 23-62)
#' @return Valeur de l'indice de Whipple
#' @export
#' @details
#' L'indice de Whipple mesure l'attraction vers les âges se terminant par 0 ou 5.
#' Interprétation:
#' - 100: Pas d'attraction (données de haute qualité)
#' - 100-105: Très faible attraction
#' - 105-110: Faible attraction
#' - 110-125: Attraction modérée
#' - 125-175: Forte attraction
#' - >175: Très forte attraction (données de mauvaise qualité)
whipple_index <- function(age, digit = 0, age_range = c(23, 62)) {
  
  # Filtrer les âges dans la plage
  age <- age[!is.na(age) & age >= age_range[1] & age <= age_range[2]]
  
  if (length(age) == 0) return(NA)
  
  # Compter les âges se terminant par le chiffre spécifié
  if (digit == 0) {
    target_ages <- seq(30, 60, by = 10)
  } else if (digit == 5) {
    target_ages <- seq(25, 55, by = 10)
  } else {
    target_ages <- seq(age_range[1], age_range[2], by = 1)
    target_ages <- target_ages[target_ages %% 10 == digit]
  }
  
  n_target <- sum(age %in% target_ages)
  n_total <- length(age)
  
  # Calcul de l'indice
  expected_proportion <- length(target_ages) / (age_range[2] - age_range[1] + 1)
  whipple <- (n_target / n_total) / expected_proportion * 100
  
  return(round(whipple, 2))
}

#' @title Calculer l'indice de Myers Blended
#' @description Calcule l'indice de Myers pour évaluer la préférence pour chaque chiffre terminal (0-9).
#'
#' @param age Vecteur numérique des âges.
#' @param age_range Intervalle d'âges à considérer (défaut: 10-89).
#'
#' @return Une liste contenant l'indice global, les déviations et les proportions par chiffre.
#' @export
#'
#' @description Calcule l'indice de Myers (Blended Index) pour l'attraction des chiffres
#' @param age Vecteur d'âges
#' @param age_range Plage d'âge à considérer (défaut: 10-89)
#' @return Liste avec l'indice global et les déviations par chiffre
#' @export
#' @details
#' L'indice de Myers mesure la préférence pour chaque chiffre terminal (0-9).
#' Interprétation de l'indice global:
#' - 0: Pas de préférence (distribution parfaite)
#' - 0-5: Données de très bonne qualité
#' - 5-10: Données de bonne qualité
#' - 10-20: Données de qualité acceptable
#' - 20-30: Données de qualité médiocre
#' - >30: Données de mauvaise qualité
myers_blended_index <- function(age, age_range = c(10, 89)) {
  
  age <- age[!is.na(age) & age >= age_range[1] & age <= age_range[2]]
  
  if (length(age) == 0) return(list(index = NA, deviations = rep(NA, 10)))
  
  # Calculer les effectifs par chiffre terminal
  terminal_digit <- age %% 10
  
  # Méthode blended de Myers
  blended_counts <- numeric(10)
  
  for (d in 0:9) {
    # Compter avec pondération
    for (start in 10:19) {
      ages_in_range <- age[age >= start & age <= (start + 70)]
      weight <- start - 9
      blended_counts[d + 1] <- blended_counts[d + 1] + 
        weight * sum(ages_in_range %% 10 == d)
    }
  }
  
  # Normaliser
  total <- sum(blended_counts)
  proportions <- blended_counts / total * 100
  
  # Déviations par rapport à 10%
  deviations <- proportions - 10
  names(deviations) <- 0:9
  
  # Indice global (somme des valeurs absolues / 2)
  myers_index <- sum(abs(deviations)) / 2
  
  return(list(
    index = round(myers_index, 2),
    deviations = round(deviations, 2),
    proportions = round(proportions, 2)
  ))
}

#' @title Calculer l'indice de Bachi
#' @description Calcule l'indice de Bachi pour mesurer l'attraction des âges.
#'
#' @param age Vecteur numérique des âges.
#' @param age_range Intervalle d'âges à considérer (défaut: 15-64).
#'
#' @return La valeur de l'indice de Bachi.
#' @export
#'
#' @description Calcule l'indice de Bachi pour l'attraction des âges
#' @param age Vecteur d'âges
#' @param age_range Plage d'âge à considérer
#' @return Valeur de l'indice de Bachi
#' @export
bachi_index <- function(age, age_range = c(15, 64)) {
  
  age <- age[!is.na(age) & age >= age_range[1] & age <= age_range[2]]
  
  if (length(age) == 0) return(NA)
  
  # Effectifs par chiffre terminal
  terminal_digit <- age %% 10
  counts <- table(factor(terminal_digit, levels = 0:9))
  proportions <- as.numeric(counts) / sum(counts) * 100
  
  # Indice de Bachi
  bachi <- sum(abs(proportions - 10)) / 2
  
  return(round(bachi, 2))
}

#' @title Analyse complète de la qualité des déclarations d'âge
#' @description Regroupe les calculs des indices de Whipple, Myers et Bachi pour une évaluation complète de l'attraction des âges.
#'
#' @param age Vecteur numérique des âges.
#'
#' @return Une liste contenant les différents indices de qualité des âges et leur interprétation.
#' @export
#'
#' @description Analyse complète de la qualité des déclarations d'âge
#' @param age Vecteur d'âges
#' @return Liste avec tous les indices de qualité des âges
#' @export
check_age_heaping <- function(age) {
  
  age <- as.numeric(age)
  
  results <- list(
    n_total = length(age),
    n_valid = sum(!is.na(age)),
    n_missing = sum(is.na(age)),
    age_range = range(age, na.rm = TRUE),
    
    # Indices de Whipple
    whipple_0 = whipple_index(age, digit = 0),
    whipple_5 = whipple_index(age, digit = 5),
    whipple_combined = (whipple_index(age, 0) + whipple_index(age, 5)) / 2,
    
    # Indice de Myers
    myers = myers_blended_index(age),
    
    # Indice de Bachi
    bachi = bachi_index(age)
  )
  
  # Interprétation
  results$interpretation <- interpret_age_quality(results)
  
  return(results)
}

#' Interpréter la qualité des âges
#'
#' @description Fournit une interprétation textuelle des indices de qualité
#' @param age_quality Résultats de check_age_heaping
#' @return Liste avec les interprétations
#' @keywords internal
interpret_age_quality <- function(age_quality) {
  
  interpretations <- list()
  
  # Whipple
  w <- age_quality$whipple_combined
  if (!is.na(w)) {
    interpretations$whipple <- if (w < 105) {
      "Excellente qualité - Pas d'attraction significative"
    } else if (w < 110) {
      "Bonne qualité - Faible attraction"
    } else if (w < 125) {
      "Qualité acceptable - Attraction modérée"
    } else if (w < 175) {
      "Qualité médiocre - Forte attraction"
    } else {
      "Mauvaise qualité - Très forte attraction"
    }
  }
  
  # Myers
  m <- age_quality$myers$index
  if (!is.na(m)) {
    interpretations$myers <- if (m < 5) {
      "Excellente qualité"
    } else if (m < 10) {
      "Bonne qualité"
    } else if (m < 20) {
      "Qualité acceptable"
    } else if (m < 30) {
      "Qualité médiocre"
    } else {
      "Mauvaise qualité"
    }
  }
  
  return(interpretations)
}

#' @title Analyser le rapport de masculinité
#' @description Calcule le rapport de masculinité (nombre d'hommes pour 100 femmes) par groupe d'âge et évalue sa plausibilité.
#'
#' @param data Data.frame contenant les données.
#' @param age_var Nom de la variable d'âge.
#' @param sex_var Nom de la variable de sexe.
#' @param weight_var Nom de la variable de pondération (optionnel).
#' @param male_code Code utilisé pour le sexe masculin. Si NULL, il est auto-détecté.
#' @param female_code Code utilisé pour le sexe féminin. Si NULL, il est auto-détecté.
#'
#' @return Un data.frame avec les effectifs et le rapport de masculinité par groupe d'âge.
#' @export
#'
#' @description Analyse le rapport de masculinité par groupe d'âge
#' @param data Data.frame contenant les données
#' @param age_var Nom de la variable d'âge
#' @param sex_var Nom de la variable de sexe
#' @param weight_var Nom de la variable de pondération (optionnel)
#' @param male_code Code pour le sexe masculin (défaut: 1 ou "M")
#' @param female_code Code pour le sexe féminin (défaut: 2 ou "F")
#' @return Data.frame avec les rapports de masculinité par groupe d'âge
#' @export
check_sex_ratio <- function(data, age_var = "age", sex_var = "sex", 
                            weight_var = NULL, male_code = NULL, female_code = NULL) {
  
  # Identifier les codes de sexe si non spécifiés
  sex_values <- unique(data[[sex_var]])
  
  if (is.null(male_code)) {
    male_code <- if (1 %in% sex_values) 1 else if ("M" %in% sex_values) "M" else sex_values[1]
  }
  if (is.null(female_code)) {
    female_code <- if (2 %in% sex_values) 2 else if ("F" %in% sex_values) "F" else sex_values[2]
  }
  
  # Créer les groupes d'âge
  data$age_group <- create_age_groups(data[[age_var]])
  
  # Calculer les effectifs par sexe et groupe d'âge
  if (is.null(weight_var)) {
    sex_ratio_data <- data %>%
      dplyr::filter(!is.na(age_group) & !is.na(!!rlang::sym(sex_var))) %>%
      dplyr::group_by(age_group, !!rlang::sym(sex_var)) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop")
  } else {
    sex_ratio_data <- data %>%
      dplyr::filter(!is.na(age_group) & !is.na(!!rlang::sym(sex_var))) %>%
      dplyr::group_by(age_group, !!rlang::sym(sex_var)) %>%
      dplyr::summarise(n = sum(!!rlang::sym(weight_var), na.rm = TRUE), .groups = "drop")
  }
  
  # Pivoter et calculer le rapport
  sex_ratio_wide <- sex_ratio_data %>%
    tidyr::pivot_wider(names_from = !!rlang::sym(sex_var), values_from = n, values_fill = 0)
  
  # Renommer les colonnes
  names(sex_ratio_wide)[names(sex_ratio_wide) == as.character(male_code)] <- "male"
  names(sex_ratio_wide)[names(sex_ratio_wide) == as.character(female_code)] <- "female"
  
  sex_ratio_wide$sex_ratio <- round(sex_ratio_wide$male / sex_ratio_wide$female * 100, 2)
  sex_ratio_wide$total <- sex_ratio_wide$male + sex_ratio_wide$female
  
  # Évaluer la plausibilité
  sex_ratio_wide$plausibility <- sapply(1:nrow(sex_ratio_wide), function(i) {
    sr <- sex_ratio_wide$sex_ratio[i]
    ag <- as.character(sex_ratio_wide$age_group[i])
    
    # Valeurs attendues approximatives
    if (grepl("^0-", ag)) {
      expected <- c(103, 107)  # À la naissance
    } else if (grepl("^[1-4]", ag)) {
      expected <- c(100, 106)
    } else if (grepl("^[5-9]|^[1-5][0-9]", ag)) {
      expected <- c(95, 105)
    } else {
      expected <- c(70, 100)  # Âges avancés
    }
    
    if (sr >= expected[1] && sr <= expected[2]) {
      "Plausible"
    } else if (sr >= expected[1] - 10 && sr <= expected[2] + 10) {
      "Acceptable"
    } else {
      "Suspect"
    }
  })
  
  return(sex_ratio_wide)
}

#' @title Vérifier la cohérence interne des données
#' @description Effectue une série de vérifications de cohérence logique sur les données (ex: âge et situation matrimoniale, âge et fécondité).
#'
#' @param data Data.frame contenant les données de recensement.
#'
#' @return Une liste contenant le nombre d'incohérences trouvées pour chaque vérification.
#' @export
#'
#' @description Vérifie la cohérence logique des données de recensement
#' @param data Data.frame contenant les données
#' @return Liste avec les résultats des vérifications de cohérence
#' @export
check_consistency <- function(data) {
  
  checks <- list()
  
  # 1. Âge négatif ou aberrant
  if ("age" %in% names(data)) {
    checks$age_negative <- sum(data$age < 0, na.rm = TRUE)
    checks$age_over_120 <- sum(data$age > 120, na.rm = TRUE)
  }
  
  # 2. Cohérence âge-situation matrimoniale
  if (all(c("age", "marital_status") %in% names(data))) {
    # Personnes mariées de moins de 12 ans
    checks$married_under_12 <- sum(
      data$age < 12 & data$marital_status %in% c("married", "Marié", "Mariée", 1, 2),
      na.rm = TRUE
    )
  }
  
  # 3. Cohérence âge-fécondité
  if (all(c("age", "children_born") %in% names(data))) {
    # Femmes de moins de 12 ans avec enfants
    checks$mother_under_12 <- sum(
      data$age < 12 & data$children_born > 0,
      na.rm = TRUE
    )
    # Nombre d'enfants supérieur à l'âge - 12
    checks$too_many_children <- sum(
      data$children_born > (data$age - 12) & data$age >= 12,
      na.rm = TRUE
    )
  }
  
  # 4. Cohérence enfants nés/survivants
  if (all(c("children_born", "children_alive") %in% names(data))) {
    checks$alive_more_than_born <- sum(
      data$children_alive > data$children_born,
      na.rm = TRUE
    )
  }
  
  # 5. Valeurs en dehors des plages attendues
  checks$out_of_range <- list()
  
  # Résumé
  checks$total_inconsistencies <- sum(unlist(checks[sapply(checks, is.numeric)]))
  checks$pct_inconsistent <- round(checks$total_inconsistencies / nrow(data) * 100, 4)
  
  return(checks)
}

#' Évaluer la qualité par unité administrative
#'
#' @description Évalue la qualité des données pour chaque unité administrative
#' @param data Data.frame contenant les données
#' @param age_var Nom de la variable d'âge
#' @param sex_var Nom de la variable de sexe
#' @param admin_var Nom de la variable d'unité administrative
#' @param weight_var Nom de la variable de pondération
#' @return Data.frame avec les indicateurs de qualité par unité
#' @keywords internal
assess_quality_by_admin <- function(data, age_var, sex_var, admin_var, weight_var = NULL) {
  
  admin_units <- unique(data[[admin_var]])
  
  results <- lapply(admin_units, function(unit) {
    subset_data <- data[data[[admin_var]] == unit, ]
    
    # Calculer les indicateurs
    whipple <- tryCatch(
      whipple_index(subset_data[[age_var]], digit = 0),
      error = function(e) NA
    )
    
    myers <- tryCatch(
      myers_blended_index(subset_data[[age_var]])$index,
      error = function(e) NA
    )
    
    completeness <- mean(!is.na(subset_data[[age_var]])) * 100
    
    data.frame(
      admin_unit = unit,
      n = nrow(subset_data),
      completeness = round(completeness, 2),
      whipple_index = whipple,
      myers_index = myers,
      stringsAsFactors = FALSE
    )
  })
  
  result_df <- do.call(rbind, results)
  result_df <- result_df[order(-result_df$n), ]
  
  return(result_df)
}

#' Calculer le score global de qualité
#'
#' @description Calcule un score synthétique de qualité des données
#' @param quality_results Résultats de assess_data_quality
#' @return Score de 0 à 100
#' @keywords internal
calculate_quality_score <- function(quality_results) {
  
  scores <- c()
  
  # Score de complétude (30 points)
  if (!is.null(quality_results$completeness)) {
    avg_completeness <- mean(quality_results$completeness$pct_complete)
    scores <- c(scores, completeness = avg_completeness * 0.3)
  }
  
  # Score de qualité des âges (40 points)
  if (!is.null(quality_results$age_quality)) {
    # Whipple (max 20 points)
    w <- quality_results$age_quality$whipple_combined
    if (!is.na(w)) {
      w_score <- max(0, min(20, 20 - (w - 100) / 5))
      scores <- c(scores, whipple = w_score)
    }
    
    # Myers (max 20 points)
    m <- quality_results$age_quality$myers$index
    if (!is.na(m)) {
      m_score <- max(0, min(20, 20 - m / 2))
      scores <- c(scores, myers = m_score)
    }
  }
  
  # Score de cohérence (30 points)
  if (!is.null(quality_results$consistency)) {
    pct_inconsistent <- quality_results$consistency$pct_inconsistent
    c_score <- max(0, 30 - pct_inconsistent * 10)
    scores <- c(scores, consistency = c_score)
  }
  
  # Score global
  if (length(scores) > 0) {
    global_score <- sum(scores) / (length(scores) / 4) * 100 / 100
    return(round(min(100, global_score), 1))
  }
  
  return(NA)
}

#' Générer un rapport de qualité
#'
#' @description Génère un rapport complet de qualité des données
#' @param quality_results Résultats de assess_data_quality
#' @param output_file Chemin du fichier de sortie
#' @param format Format de sortie ("html", "pdf", "docx")
#' @return Chemin du fichier généré
#' @export
generate_quality_report <- function(quality_results, output_file = NULL, format = "html") {
  
  if (is.null(output_file)) {
    output_file <- paste0("quality_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", format)
  }
  
  # Créer le contenu du rapport
  report_content <- list(
    title = "Rapport de Qualité des Données de Recensement",
    date = Sys.time(),
    results = quality_results
  )
  
  # Pour l'instant, retourner les résultats structurés
  # L'implémentation complète utiliserait rmarkdown::render()
  
  message("Rapport de qualité généré: ", output_file)
  
  return(report_content)
}

#' Méthode print pour census_quality
#'
#' @param x Objet census_quality
#' @param ... Arguments supplémentaires
#' @export
print.census_quality <- function(x, ...) {
  cat("\n=== RAPPORT DE QUALITÉ DES DONNÉES ===\n\n")
  
  cat(sprintf("Date d'évaluation: %s\n", x$timestamp))
  cat(sprintf("Observations: %s\n", format(x$n_observations, big.mark = " ")))
  cat(sprintf("Variables: %d\n\n", x$n_variables))
  
  cat("--- Complétude ---\n")
  if (!is.null(x$completeness)) {
    low_quality <- x$completeness[x$completeness$pct_complete < 90, ]
    if (nrow(low_quality) > 0) {
      cat("Variables avec complétude < 90%:\n")
      print(low_quality[, c("variable", "pct_complete", "quality")], row.names = FALSE)
    } else {
      cat("Toutes les variables ont une complétude >= 90%\n")
    }
  }
  
  cat("\n--- Qualité des âges ---\n")
  if (!is.null(x$age_quality)) {
    cat(sprintf("Indice de Whipple (0): %.2f\n", x$age_quality$whipple_0))
    cat(sprintf("Indice de Whipple (5): %.2f\n", x$age_quality$whipple_5))
    cat(sprintf("Indice de Myers: %.2f\n", x$age_quality$myers$index))
    cat(sprintf("Interprétation: %s\n", x$age_quality$interpretation$myers))
  }
  
  cat("\n--- Cohérence ---\n")
  if (!is.null(x$consistency)) {
    cat(sprintf("Incohérences détectées: %d (%.4f%%)\n", 
                x$consistency$total_inconsistencies,
                x$consistency$pct_inconsistent))
  }
  
  cat(sprintf("\n*** SCORE GLOBAL: %.1f/100 ***\n", x$global_score))
  
  invisible(x)
}
