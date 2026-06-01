#' @title Module de Projection de Population
#' @description Fonctions pour les projections par méthode des cohortes et micro-simulation
#' @name population_projection
NULL

#' Projection par la méthode des composantes par cohortes
#'
#' @description Projette la population par la méthode classique des cohortes
#' @param base_population Data.frame avec la population de base par âge et sexe
#' @param fertility_rates Taux de fécondité par âge
#' @param mortality_rates Quotients de mortalité par âge et sexe
#' @param migration_rates Taux de migration nette par âge et sexe (optionnel)
#' @param years Nombre d'années de projection
#' @param sex_ratio_birth Rapport de masculinité à la naissance (défaut: 105)
#' @param admin_var Variable d'unité administrative (optionnel)
#' @param disability_rates Taux de handicap par âge (optionnel)
#' @return Liste avec les projections par année
#' @export
#' @examples
#' \dontrun{
#' projection <- project_cohort_component(
#'   base_population,
#'   fertility_rates,
#'   mortality_rates,
#'   years = 25
#' )
#' }
project_cohort_component <- function(base_population, fertility_rates, 
                                      mortality_rates, migration_rates = NULL,
                                      years = 25, sex_ratio_birth = 105,
                                      admin_var = NULL, disability_rates = NULL) {
  
  message("\n========================================")
  message("  PROJECTION PAR COHORTES")
  message("========================================\n")
  
  # Valider les entrées
  validate_projection_inputs(base_population, fertility_rates, mortality_rates)
  
  # Initialiser les résultats
  projections <- list()
  projections[[1]] <- base_population
  names(projections)[1] <- "year_0"
  
  # Projection année par année
  for (year in 1:years) {
    message(sprintf("Projection année %d/%d...", year, years))
    
    current_pop <- projections[[year]]
    
    # 1. Appliquer la mortalité (survivants)
    survivors <- apply_mortality(current_pop, mortality_rates)
    
    # 2. Vieillir la population d'un an
    aged_pop <- age_population(survivors)
    
    # 3. Calculer les naissances
    births <- calculate_births(current_pop, fertility_rates, sex_ratio_birth)
    
    # 4. Ajouter les naissances (groupe 0-1 an)
    new_pop <- add_births(aged_pop, births)
    
    # 5. Appliquer la migration (si disponible)
    if (!is.null(migration_rates)) {
      new_pop <- apply_migration(new_pop, migration_rates)
    }
    
    # 6. Appliquer les taux de handicap (si disponible)
    if (!is.null(disability_rates)) {
      new_pop <- apply_disability_rates(new_pop, disability_rates)
    }
    
    # Stocker le résultat
    projections[[year + 1]] <- new_pop
    names(projections)[year + 1] <- paste0("year_", year)
  }
  
  # Créer l'objet résultat
  result <- list(
    projections = projections,
    parameters = list(
      years = years,
      sex_ratio_birth = sex_ratio_birth,
      base_year = 0,
      projection_years = 0:years
    ),
    summary = summarize_projections(projections)
  )
  
  class(result) <- c("census_projection", "list")
  
  message(sprintf("\n✓ Projection terminée: %d années projetées", years))
  
  return(result)
}

#' Valider les entrées de projection
#'
#' @description Vérifie la validité des données d'entrée
#' @param base_population Population de base
#' @param fertility_rates Taux de fécondité
#' @param mortality_rates Taux de mortalité
#' @keywords internal
validate_projection_inputs <- function(base_population, fertility_rates, mortality_rates) {
  
  # Vérifier la structure de la population de base
  required_cols <- c("age_group", "sex", "population")
  missing_cols <- setdiff(required_cols, names(base_population))
  
  if (length(missing_cols) > 0) {
    stop("Colonnes manquantes dans base_population: ", paste(missing_cols, collapse = ", "))
  }
  
  # Vérifier les taux de fécondité
  if (!is.numeric(fertility_rates) && !is.data.frame(fertility_rates)) {
    stop("fertility_rates doit être un vecteur numérique ou un data.frame")
  }
  
  # Vérifier les taux de mortalité
  if (!is.data.frame(mortality_rates)) {
    stop("mortality_rates doit être un data.frame avec age_group, sex, et qx")
  }
  
  message("✓ Validation des entrées réussie")
}

#' Appliquer la mortalité
#'
#' @description Calcule les survivants après application de la mortalité
#' @param population Population actuelle
#' @param mortality_rates Quotients de mortalité
#' @return Population survivante
#' @keywords internal
apply_mortality <- function(population, mortality_rates) {
  
  # Fusionner avec les taux de mortalité
  pop_with_qx <- merge(population, mortality_rates, 
                       by = c("age_group", "sex"), all.x = TRUE)
  
  # Remplacer les NA par 0
  pop_with_qx$qx[is.na(pop_with_qx$qx)] <- 0
  
  # Calculer les survivants
  pop_with_qx$survivors <- pop_with_qx$population * (1 - pop_with_qx$qx)
  
  # Retourner la population mise à jour
  result <- pop_with_qx[, c("age_group", "sex", "survivors")]
  names(result)[3] <- "population"
  
  return(result)
}

#' Vieillir la population
#'
#' @description Fait vieillir la population d'un an
#' @param population Population actuelle
#' @return Population vieillie
#' @keywords internal
age_population <- function(population) {
  
  # Définir l'ordre des groupes d'âge
  age_order <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", 
                 "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                 "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
  
  # Pour simplifier, on garde les mêmes groupes quinquennaux
  # En réalité, il faudrait un modèle plus sophistiqué
  
  # Décaler les populations vers le groupe d'âge supérieur
  # (approximation pour les groupes quinquennaux)
  
  # Taux de passage au groupe supérieur (1/5 par an pour groupes quinquennaux)
  transition_rate <- 0.2
  
  result <- population
  
  for (sex_val in unique(population$sex)) {
    sex_data <- population[population$sex == sex_val, ]
    sex_data <- sex_data[order(match(sex_data$age_group, age_order)), ]
    
    new_pop <- numeric(nrow(sex_data))
    
    for (i in 1:nrow(sex_data)) {
      # Population restant dans le groupe
      staying <- sex_data$population[i] * (1 - transition_rate)
      
      # Population venant du groupe précédent
      if (i > 1) {
        incoming <- sex_data$population[i - 1] * transition_rate
      } else {
        incoming <- 0
      }
      
      new_pop[i] <- staying + incoming
    }
    
    # Mettre à jour le dernier groupe (ouvert)
    new_pop[length(new_pop)] <- new_pop[length(new_pop)] + 
                                 sex_data$population[length(new_pop)] * transition_rate
    
    result$population[result$sex == sex_val] <- new_pop
  }
  
  return(result)
}

#' Calculer les naissances
#'
#' @description Calcule le nombre de naissances
#' @param population Population actuelle
#' @param fertility_rates Taux de fécondité par âge
#' @param sex_ratio_birth Rapport de masculinité à la naissance
#' @return Data.frame avec les naissances par sexe
#' @keywords internal
calculate_births <- function(population, fertility_rates, sex_ratio_birth) {
  
  # Groupes d'âge fertiles
  fertile_ages <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")
  
  # Filtrer les femmes en âge de procréer
  women <- population[population$sex %in% c(2, "F", "Female", "Femme") & 
                      population$age_group %in% fertile_ages, ]
  
  # Appliquer les taux de fécondité
  if (is.data.frame(fertility_rates)) {
    women <- merge(women, fertility_rates, by = "age_group", all.x = TRUE)
    women$births <- women$population * women$asfr / 1000
  } else {
    # Vecteur simple de taux
    women$births <- women$population * fertility_rates[match(women$age_group, fertile_ages)] / 1000
  }
  
  total_births <- sum(women$births, na.rm = TRUE)
  
  # Répartir par sexe selon le rapport de masculinité
  male_births <- total_births * sex_ratio_birth / (100 + sex_ratio_birth)
  female_births <- total_births * 100 / (100 + sex_ratio_birth)
  
  births <- data.frame(
    sex = c(1, 2),  # ou c("M", "F")
    births = c(male_births, female_births)
  )
  
  return(births)
}

#' Ajouter les naissances
#'
#' @description Ajoute les naissances au groupe 0-4 ans
#' @param population Population actuelle
#' @param births Naissances par sexe
#' @return Population mise à jour
#' @keywords internal
add_births <- function(population, births) {
  
  # Identifier le premier groupe d'âge
  first_age_group <- "0-4"
  
  for (i in 1:nrow(births)) {
    sex_val <- births$sex[i]
    birth_count <- births$births[i]
    
    # Ajouter au groupe 0-4
    idx <- which(population$age_group == first_age_group & population$sex == sex_val)
    
    if (length(idx) > 0) {
      population$population[idx] <- population$population[idx] + birth_count
    }
  }
  
  return(population)
}

#' Appliquer la migration
#'
#' @description Applique les taux de migration nette
#' @param population Population actuelle
#' @param migration_rates Taux de migration par âge et sexe
#' @return Population mise à jour
#' @keywords internal
apply_migration <- function(population, migration_rates) {
  
  # Fusionner avec les taux de migration
  pop_with_mig <- merge(population, migration_rates, 
                        by = c("age_group", "sex"), all.x = TRUE)
  
  # Remplacer les NA par 0
  pop_with_mig$migration_rate[is.na(pop_with_mig$migration_rate)] <- 0
  
  # Appliquer la migration
  pop_with_mig$population <- pop_with_mig$population * (1 + pop_with_mig$migration_rate / 1000)
  
  # Retourner la population mise à jour
  result <- pop_with_mig[, c("age_group", "sex", "population")]
  
  return(result)
}

#' Appliquer les taux de handicap
#'
#' @description Calcule la population avec handicap
#' @param population Population actuelle
#' @param disability_rates Taux de handicap par âge
#' @return Population avec colonne handicap
#' @keywords internal
apply_disability_rates <- function(population, disability_rates) {
  
  # Fusionner avec les taux de handicap
  pop_with_dis <- merge(population, disability_rates, 
                        by = "age_group", all.x = TRUE)
  
  # Remplacer les NA par 0
  pop_with_dis$disability_rate[is.na(pop_with_dis$disability_rate)] <- 0
  
  # Calculer la population avec handicap
  pop_with_dis$population_disabled <- pop_with_dis$population * pop_with_dis$disability_rate / 100
  pop_with_dis$population_not_disabled <- pop_with_dis$population - pop_with_dis$population_disabled
  
  return(pop_with_dis)
}

#' Projection par micro-simulation
#'
#' @description Projette la population par micro-simulation
#' @param individual_data Data.frame avec les données individuelles
#' @param fertility_model Modèle de fécondité
#' @param mortality_model Modèle de mortalité
#' @param migration_model Modèle de migration (optionnel)
#' @param disability_model Modèle de handicap (optionnel)
#' @param years Nombre d'années de projection
#' @param n_simulations Nombre de simulations
#' @param seed Graine pour la reproductibilité
#' @param admin_var Variable d'unité administrative
#' @return Liste avec les projections par simulation
#' @export
project_microsimulation <- function(individual_data, fertility_model = NULL,
                                     mortality_model = NULL, migration_model = NULL,
                                     disability_model = NULL, years = 25,
                                     n_simulations = 100, seed = 123,
                                     admin_var = NULL) {
  
  message("\n========================================")
  message("  PROJECTION PAR MICRO-SIMULATION")
  message("========================================\n")
  
  set.seed(seed)
  
  # Créer les modèles par défaut si non fournis
  if (is.null(fertility_model)) {
    fertility_model <- create_default_fertility_model(individual_data)
  }
  
  if (is.null(mortality_model)) {
    mortality_model <- create_default_mortality_model(individual_data)
  }
  
  # Initialiser les résultats
  all_simulations <- list()
  
  message(sprintf("Exécution de %d simulations sur %d années...", n_simulations, years))
  
  # Barre de progression
  pb <- txtProgressBar(min = 0, max = n_simulations, style = 3)
  
  for (sim in 1:n_simulations) {
    # Copier les données individuelles
    sim_data <- individual_data
    sim_data$sim_id <- sim
    
    # Projection année par année
    yearly_results <- list()
    yearly_results[[1]] <- summarize_simulation(sim_data, admin_var)
    
    for (year in 1:years) {
      sim_data <- simulate_year(
        sim_data, 
        fertility_model, 
        mortality_model,
        migration_model,
        disability_model,
        admin_var
      )
      
      yearly_results[[year + 1]] <- summarize_simulation(sim_data, admin_var)
    }
    
    all_simulations[[sim]] <- yearly_results
    setTxtProgressBar(pb, sim)
  }
  
  close(pb)
  
  # Agréger les résultats
  aggregated <- aggregate_simulations(all_simulations, years)
  
  result <- list(
    simulations = all_simulations,
    aggregated = aggregated,
    parameters = list(
      years = years,
      n_simulations = n_simulations,
      seed = seed
    )
  )
  
  class(result) <- c("census_microsimulation", "list")
  
  message(sprintf("\n✓ Micro-simulation terminée: %d simulations", n_simulations))
  
  return(result)
}

#' Créer un modèle de fécondité par défaut
#'
#' @description Crée un modèle de fécondité basé sur les données
#' @param data Données individuelles
#' @return Fonction de modèle de fécondité
#' @keywords internal
create_default_fertility_model <- function(data) {
  
  # Taux de fécondité par âge (approximation)
  default_asfr <- data.frame(
    age_min = c(15, 20, 25, 30, 35, 40, 45),
    age_max = c(19, 24, 29, 34, 39, 44, 49),
    asfr = c(0.05, 0.15, 0.18, 0.15, 0.08, 0.03, 0.01)  # par an
  )
  
  # Fonction de probabilité de naissance
  fertility_model <- function(age, sex) {
    if (sex %in% c(2, "F", "Female", "Femme") && age >= 15 && age < 50) {
      idx <- which(age >= default_asfr$age_min & age <= default_asfr$age_max)
      if (length(idx) > 0) {
        return(default_asfr$asfr[idx])
      }
    }
    return(0)
  }
  
  return(fertility_model)
}

#' Créer un modèle de mortalité par défaut
#'
#' @description Crée un modèle de mortalité basé sur les données
#' @param data Données individuelles
#' @return Fonction de modèle de mortalité
#' @keywords internal
create_default_mortality_model <- function(data) {
  
  # Table de mortalité simplifiée (Coale-Demeny West, niveau 20)
  default_qx <- data.frame(
    age = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85),
    qx_male = c(0.03, 0.004, 0.002, 0.002, 0.003, 0.004, 0.005, 0.006, 0.008, 
                0.012, 0.018, 0.028, 0.042, 0.065, 0.100, 0.155, 0.235, 0.350, 0.500),
    qx_female = c(0.025, 0.003, 0.002, 0.001, 0.002, 0.003, 0.003, 0.004, 0.005,
                  0.008, 0.012, 0.018, 0.028, 0.045, 0.075, 0.120, 0.195, 0.310, 0.480)
  )
  
  # Fonction de probabilité de décès
  mortality_model <- function(age, sex) {
    # Trouver le groupe d'âge
    idx <- max(which(default_qx$age <= age))
    
    if (sex %in% c(1, "M", "Male", "Homme")) {
      return(default_qx$qx_male[idx] / 5)  # Convertir en taux annuel
    } else {
      return(default_qx$qx_female[idx] / 5)
    }
  }
  
  return(mortality_model)
}

#' Simuler une année
#'
#' @description Simule les événements démographiques pour une année
#' @param data Données individuelles
#' @param fertility_model Modèle de fécondité
#' @param mortality_model Modèle de mortalité
#' @param migration_model Modèle de migration
#' @param disability_model Modèle de handicap
#' @param admin_var Variable d'unité administrative
#' @return Données mises à jour
#' @keywords internal
simulate_year <- function(data, fertility_model, mortality_model,
                          migration_model, disability_model, admin_var) {
  
  n <- nrow(data)
  
  # 1. Vieillir tout le monde d'un an
  data$age <- data$age + 1
  
  # 2. Simuler les décès
  death_probs <- sapply(1:n, function(i) {
    mortality_model(data$age[i], data$sex[i])
  })
  deaths <- runif(n) < death_probs
  data <- data[!deaths, ]
  
  # 3. Simuler les naissances
  n_current <- nrow(data)
  birth_probs <- sapply(1:n_current, function(i) {
    fertility_model(data$age[i], data$sex[i])
  })
  births <- runif(n_current) < birth_probs
  n_births <- sum(births)
  
  if (n_births > 0) {
    # Créer les nouveaux-nés
    new_births <- data.frame(
      age = rep(0, n_births),
      sex = sample(c(1, 2), n_births, replace = TRUE, prob = c(0.512, 0.488)),
      stringsAsFactors = FALSE
    )
    
    # Hériter de l'unité administrative de la mère
    if (!is.null(admin_var) && admin_var %in% names(data)) {
      mothers <- data[births, ]
      new_births[[admin_var]] <- mothers[[admin_var]]
    }
    
    # Ajouter d'autres variables si nécessaire
    for (col in setdiff(names(data), c("age", "sex", admin_var))) {
      if (col %in% names(new_births)) next
      new_births[[col]] <- NA
    }
    
    # Ajouter les naissances
    data <- rbind(data, new_births[, names(data)])
  }
  
  # 4. Simuler la migration (si modèle fourni)
  if (!is.null(migration_model)) {
    # Implémenter la logique de migration
  }
  
  # 5. Mettre à jour le statut de handicap (si modèle fourni)
  if (!is.null(disability_model) && "disability" %in% names(data)) {
    # Implémenter la logique de handicap
  }
  
  return(data)
}

#' Résumer une simulation
#'
#' @description Crée un résumé agrégé d'une simulation
#' @param data Données de simulation
#' @param admin_var Variable d'unité administrative
#' @return Data.frame résumé
#' @keywords internal
summarize_simulation <- function(data, admin_var = NULL) {
  
  # Créer les groupes d'âge
  data$age_group <- create_age_groups(data$age)
  
  # Résumé par âge et sexe
  summary_base <- data %>%
    dplyr::group_by(age_group, sex) %>%
    dplyr::summarise(
      population = dplyr::n(),
      .groups = "drop"
    )
  
  # Ajouter le résumé par unité administrative si disponible
  if (!is.null(admin_var) && admin_var %in% names(data)) {
    summary_admin <- data %>%
      dplyr::group_by(!!rlang::sym(admin_var), age_group, sex) %>%
      dplyr::summarise(
        population = dplyr::n(),
        .groups = "drop"
      )
    
    return(list(total = summary_base, by_admin = summary_admin))
  }
  
  return(list(total = summary_base))
}

#' Agréger les simulations
#'
#' @description Agrège les résultats de toutes les simulations
#' @param simulations Liste des simulations
#' @param years Nombre d'années
#' @return Liste avec les statistiques agrégées
#' @keywords internal
aggregate_simulations <- function(simulations, years) {
  
  n_sims <- length(simulations)
  
  # Agréger par année
  yearly_stats <- list()
  
  for (year in 0:years) {
    year_data <- lapply(simulations, function(sim) {
      sim[[year + 1]]$total
    })
    
    # Combiner toutes les simulations
    combined <- do.call(rbind, year_data)
    
    # Calculer les statistiques
    stats <- combined %>%
      dplyr::group_by(age_group, sex) %>%
      dplyr::summarise(
        mean_pop = mean(population),
        median_pop = median(population),
        sd_pop = sd(population),
        q05 = quantile(population, 0.05),
        q95 = quantile(population, 0.95),
        .groups = "drop"
      )
    
    yearly_stats[[paste0("year_", year)]] <- stats
  }
  
  return(yearly_stats)
}

#' Projection de population (interface unifiée)
#'
#' @description Interface unifiée pour les projections de population
#' @param data Données de recensement ou population de base
#' @param method Méthode de projection ("cohort", "microsim", "both")
#' @param years Nombre d'années de projection
#' @param fertility_rates Taux de fécondité
#' @param mortality_rates Taux de mortalité
#' @param migration_rates Taux de migration (optionnel)
#' @param disability_rates Taux de handicap (optionnel)
#' @param admin_var Variable d'unité administrative
#' @param n_simulations Nombre de simulations (pour microsim)
#' @param seed Graine pour la reproductibilité
#' @return Résultat de projection
#' @export
project_population <- function(data, method = "cohort", years = 25,
                                fertility_rates = NULL, mortality_rates = NULL,
                                migration_rates = NULL, disability_rates = NULL,
                                admin_var = NULL, n_simulations = 100,
                                seed = 123) {
  
  message("\n========================================")
  message("  PROJECTION DE POPULATION")
  message(sprintf("  Méthode: %s", method))
  message("========================================\n")
  
  # Préparer les données si nécessaire
  if (method %in% c("cohort", "both")) {
    # Agréger les données individuelles en population de base
    if ("age" %in% names(data) && nrow(data) > 1000) {
      base_population <- prepare_base_population(data, admin_var)
    } else {
      base_population <- data
    }
    
    # Créer les taux par défaut si non fournis
    if (is.null(fertility_rates)) {
      fertility_rates <- create_default_fertility_rates()
    }
    if (is.null(mortality_rates)) {
      mortality_rates <- create_default_mortality_rates()
    }
  }
  
  results <- list()
  
  # Projection par cohortes
  if (method %in% c("cohort", "both")) {
    results$cohort <- project_cohort_component(
      base_population, fertility_rates, mortality_rates,
      migration_rates, years, admin_var = admin_var,
      disability_rates = disability_rates
    )
  }
  
  # Projection par micro-simulation
  if (method %in% c("microsim", "both")) {
    results$microsim <- project_microsimulation(
      data, years = years, n_simulations = n_simulations,
      seed = seed, admin_var = admin_var
    )
  }
  
  # Retourner le résultat approprié
  if (method == "both") {
    class(results) <- c("census_projection_combined", "list")
    return(results)
  } else if (method == "cohort") {
    return(results$cohort)
  } else {
    return(results$microsim)
  }
}

#' Préparer la population de base
#'
#' @description Agrège les données individuelles en population de base
#' @param data Données individuelles
#' @param admin_var Variable d'unité administrative
#' @return Data.frame avec la population de base
#' @keywords internal
prepare_base_population <- function(data, admin_var = NULL) {
  
  # Créer les groupes d'âge
  data$age_group <- create_age_groups(data$age)
  
  if (is.null(admin_var)) {
    base_pop <- data %>%
      dplyr::filter(!is.na(age_group) & !is.na(sex)) %>%
      dplyr::group_by(age_group, sex) %>%
      dplyr::summarise(
        population = dplyr::n(),
        .groups = "drop"
      )
  } else {
    base_pop <- data %>%
      dplyr::filter(!is.na(age_group) & !is.na(sex)) %>%
      dplyr::group_by(!!rlang::sym(admin_var), age_group, sex) %>%
      dplyr::summarise(
        population = dplyr::n(),
        .groups = "drop"
      )
  }
  
  return(base_pop)
}

#' Créer des taux de fécondité par défaut
#'
#' @description Crée une table de taux de fécondité par défaut
#' @return Data.frame avec les taux de fécondité
#' @keywords internal
create_default_fertility_rates <- function() {
  data.frame(
    age_group = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
    asfr = c(50, 150, 180, 150, 80, 30, 10)  # pour 1000 femmes
  )
}

#' Créer des taux de mortalité par défaut
#'
#' @description Crée une table de taux de mortalité par défaut
#' @return Data.frame avec les taux de mortalité
#' @keywords internal
create_default_mortality_rates <- function() {
  
  age_groups <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                  "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                  "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
  
  # Quotients quinquennaux approximatifs
  qx_male <- c(0.03, 0.005, 0.003, 0.005, 0.008, 0.010, 0.012, 0.018,
               0.028, 0.045, 0.070, 0.105, 0.160, 0.240, 0.350, 0.480, 0.620, 1.0)
  qx_female <- c(0.025, 0.004, 0.002, 0.003, 0.005, 0.006, 0.008, 0.012,
                 0.018, 0.028, 0.045, 0.070, 0.110, 0.170, 0.270, 0.400, 0.550, 1.0)
  
  mortality_rates <- rbind(
    data.frame(age_group = age_groups, sex = 1, qx = qx_male),
    data.frame(age_group = age_groups, sex = 2, qx = qx_female)
  )
  
  return(mortality_rates)
}

#' Créer des scénarios de projection
#'
#' @description Crée différents scénarios de projection
#' @param base_fertility Taux de fécondité de base
#' @param base_mortality Taux de mortalité de base
#' @param scenarios Types de scénarios ("low", "medium", "high")
#' @return Liste de scénarios
#' @export
create_projection_scenarios <- function(base_fertility = NULL, 
                                         base_mortality = NULL,
                                         scenarios = c("low", "medium", "high")) {
  
  if (is.null(base_fertility)) {
    base_fertility <- create_default_fertility_rates()
  }
  if (is.null(base_mortality)) {
    base_mortality <- create_default_mortality_rates()
  }
  
  scenario_list <- list()
  
  for (scenario in scenarios) {
    fertility_adj <- switch(scenario,
      "low" = 0.8,
      "medium" = 1.0,
      "high" = 1.2
    )
    
    mortality_adj <- switch(scenario,
      "low" = 1.1,
      "medium" = 1.0,
      "high" = 0.9
    )
    
    scenario_list[[scenario]] <- list(
      fertility_rates = transform(base_fertility, asfr = asfr * fertility_adj),
      mortality_rates = transform(base_mortality, qx = pmin(1, qx * mortality_adj))
    )
  }
  
  return(scenario_list)
}

#' Agréger les projections
#'
#' @description Agrège les projections par unité administrative ou autre variable
#' @param projection Résultat de projection
#' @param by Variable de regroupement
#' @return Data.frame agrégé
#' @export
aggregate_projections <- function(projection, by = NULL) {
  
  if (!inherits(projection, c("census_projection", "census_microsimulation"))) {
    stop("L'argument doit être un résultat de projection")
  }
  
  # Extraire les projections
  if (inherits(projection, "census_projection")) {
    projections <- projection$projections
  } else {
    projections <- projection$aggregated
  }
  
  # Agréger
  aggregated <- lapply(names(projections), function(year_name) {
    year_data <- projections[[year_name]]
    
    if (is.null(by)) {
      # Agrégation totale
      total <- sum(year_data$population, na.rm = TRUE)
      data.frame(year = year_name, total_population = total)
    } else {
      # Agrégation par variable
      year_data %>%
        dplyr::group_by(!!rlang::sym(by)) %>%
        dplyr::summarise(
          population = sum(population, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(year = year_name)
    }
  })
  
  result <- do.call(rbind, aggregated)
  
  return(result)
}

#' Exporter les projections
#'
#' @description Exporte les projections dans différents formats
#' @param projection Résultat de projection
#' @param output_dir Répertoire de sortie
#' @param format Format de sortie ("xlsx", "csv")
#' @param prefix Préfixe pour les noms de fichiers
#' @return Vecteur des chemins des fichiers créés
#' @export
export_projections <- function(projection, output_dir = ".", 
                                format = "xlsx", prefix = "projection") {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  files_created <- character()
  
  # Extraire les données
  if (inherits(projection, "census_projection")) {
    projections <- projection$projections
  } else if (inherits(projection, "census_microsimulation")) {
    projections <- projection$aggregated
  } else {
    stop("Format de projection non reconnu")
  }
  
  if (format == "xlsx") {
    wb <- openxlsx::createWorkbook()
    
    for (year_name in names(projections)) {
      openxlsx::addWorksheet(wb, year_name)
      openxlsx::writeData(wb, year_name, projections[[year_name]])
    }
    
    xlsx_file <- file.path(output_dir, paste0(prefix, "_projections.xlsx"))
    openxlsx::saveWorkbook(wb, xlsx_file, overwrite = TRUE)
    files_created <- c(files_created, xlsx_file)
    
  } else if (format == "csv") {
    csv_dir <- file.path(output_dir, paste0(prefix, "_csv"))
    if (!dir.exists(csv_dir)) dir.create(csv_dir)
    
    for (year_name in names(projections)) {
      csv_file <- file.path(csv_dir, paste0(year_name, ".csv"))
      write.csv(projections[[year_name]], csv_file, row.names = FALSE)
      files_created <- c(files_created, csv_file)
    }
  }
  
  message(sprintf("✓ Projections exportées: %d fichiers", length(files_created)))
  
  return(files_created)
}

#' Résumer les projections
#'
#' @description Crée un résumé des projections
#' @param projections Liste des projections par année
#' @return Data.frame résumé
#' @keywords internal
summarize_projections <- function(projections) {
  
  summary_data <- lapply(names(projections), function(year_name) {
    year_data <- projections[[year_name]]
    
    data.frame(
      year = year_name,
      total_population = sum(year_data$population, na.rm = TRUE),
      male_population = sum(year_data$population[year_data$sex %in% c(1, "M")], na.rm = TRUE),
      female_population = sum(year_data$population[year_data$sex %in% c(2, "F")], na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, summary_data)
}

#' Méthode print pour census_projection
#'
#' @param x Objet census_projection
#' @param ... Arguments supplémentaires
#' @export
print.census_projection <- function(x, ...) {
  cat("\n=== RÉSULTAT DE PROJECTION PAR COHORTES ===\n\n")
  
  cat("--- Paramètres ---\n")
  cat(sprintf("Années projetées: %d\n", x$parameters$years))
  cat(sprintf("Rapport de masculinité à la naissance: %.1f\n", x$parameters$sex_ratio_birth))
  
  cat("\n--- Résumé ---\n")
  print(x$summary)
  
  invisible(x)
}

#' Méthode print pour census_microsimulation
#'
#' @param x Objet census_microsimulation
#' @param ... Arguments supplémentaires
#' @export
print.census_microsimulation <- function(x, ...) {
  cat("\n=== RÉSULTAT DE MICRO-SIMULATION ===\n\n")
  
  cat("--- Paramètres ---\n")
  cat(sprintf("Années projetées: %d\n", x$parameters$years))
  cat(sprintf("Nombre de simulations: %d\n", x$parameters$n_simulations))
  cat(sprintf("Graine aléatoire: %d\n", x$parameters$seed))
  
  cat("\n--- Résumé (année finale) ---\n")
  final_year <- paste0("year_", x$parameters$years)
  if (final_year %in% names(x$aggregated)) {
    print(head(x$aggregated[[final_year]], 10))
  }
  
  invisible(x)
}
