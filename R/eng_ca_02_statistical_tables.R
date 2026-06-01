#' @title Module de Production des Tableaux Statistiques
#' @description Fonctions pour produire des tableaux statistiques standardisés
#' @name statistical_tables
NULL

#' Tableau de structure de la population
#'
#' @description Produit un tableau de la structure par âge et sexe
#' @param data Data.frame contenant les données de recensement
#' @param age_var Nom de la variable d'âge
#' @param sex_var Nom de la variable de sexe
#' @param admin_var Nom de la variable d'unité administrative (optionnel)
#' @param weight_var Nom de la variable de pondération (optionnel)
#' @param age_groups Type de groupes d'âge ("quinquennal", "decennal", "custom")
#' @param custom_breaks Points de coupure personnalisés
#' @return Data.frame avec la structure de la population
#' @export
#' @examples
#' \dontrun{
#' structure <- table_population_structure(census_data, "age", "sex")
#' }
table_population_structure <- function(data, age_var = "age", sex_var = "sex",
                                        admin_var = NULL, weight_var = NULL,
                                        age_groups = "quinquennal", custom_breaks = NULL) {
  
  # Définir les groupes d'âge
  if (age_groups == "quinquennal") {
    breaks <- c(0, seq(5, 85, by = 5), Inf)
  } else if (age_groups == "decennal") {
    breaks <- c(0, seq(10, 80, by = 10), Inf)
  } else if (!is.null(custom_breaks)) {
    breaks <- custom_breaks
  } else {
    breaks <- c(0, seq(5, 85, by = 5), Inf)
  }
  
  # Créer les groupes d'âge
  data$age_group <- create_age_groups(data[[age_var]], breaks = breaks)
  
  # Calculer les effectifs
  if (is.null(weight_var)) {
    if (is.null(admin_var)) {
      result <- data %>%
        dplyr::filter(!is.na(age_group) & !is.na(!!rlang::sym(sex_var))) %>%
        dplyr::group_by(age_group, !!rlang::sym(sex_var)) %>%
        dplyr::summarise(effectif = dplyr::n(), .groups = "drop")
    } else {
      result <- data %>%
        dplyr::filter(!is.na(age_group) & !is.na(!!rlang::sym(sex_var))) %>%
        dplyr::group_by(!!rlang::sym(admin_var), age_group, !!rlang::sym(sex_var)) %>%
        dplyr::summarise(effectif = dplyr::n(), .groups = "drop")
    }
  } else {
    if (is.null(admin_var)) {
      result <- data %>%
        dplyr::filter(!is.na(age_group) & !is.na(!!rlang::sym(sex_var))) %>%
        dplyr::group_by(age_group, !!rlang::sym(sex_var)) %>%
        dplyr::summarise(effectif = sum(!!rlang::sym(weight_var), na.rm = TRUE), .groups = "drop")
    } else {
      result <- data %>%
        dplyr::filter(!is.na(age_group) & !is.na(!!rlang::sym(sex_var))) %>%
        dplyr::group_by(!!rlang::sym(admin_var), age_group, !!rlang::sym(sex_var)) %>%
        dplyr::summarise(effectif = sum(!!rlang::sym(weight_var), na.rm = TRUE), .groups = "drop")
    }
  }
  
  # Pivoter pour avoir hommes et femmes en colonnes
  result_wide <- result %>%
    tidyr::pivot_wider(
      names_from = !!rlang::sym(sex_var),
      values_from = effectif,
      values_fill = 0
    )
  
  # Calculer les totaux et pourcentages
  sex_cols <- setdiff(names(result_wide), c("age_group", admin_var))
  
  result_wide$total <- rowSums(result_wide[, sex_cols], na.rm = TRUE)
  
  # Pourcentages
  total_pop <- sum(result_wide$total)
  result_wide$pct_total <- round(result_wide$total / total_pop * 100, 2)
  
  # Rapport de masculinité
  if (length(sex_cols) >= 2) {
    result_wide$sex_ratio <- round(result_wide[[sex_cols[1]]] / result_wide[[sex_cols[2]]] * 100, 2)
  }
  
  # Ajouter les métadonnées
  attr(result_wide, "table_type") <- "population_structure"
  attr(result_wide, "total_population") <- total_pop
  
  class(result_wide) <- c("census_table", class(result_wide))
  
  return(result_wide)
}

#' Tableau de la pyramide des âges
#'
#' @description Produit les données pour une pyramide des âges
#' @param data Data.frame contenant les données de recensement
#' @param age_var Nom de la variable d'âge
#' @param sex_var Nom de la variable de sexe
#' @param weight_var Nom de la variable de pondération (optionnel)
#' @param male_code Code pour le sexe masculin
#' @param female_code Code pour le sexe féminin
#' @return Data.frame formaté pour la pyramide des âges
#' @export
table_age_pyramid <- function(data, age_var = "age", sex_var = "sex",
                               weight_var = NULL, male_code = 1, female_code = 2) {
  
  # Créer les groupes d'âge quinquennaux
  data$age_group <- create_age_groups(data[[age_var]])
  
  # Calculer les effectifs par sexe
  if (is.null(weight_var)) {
    pyramid_data <- data %>%
      dplyr::filter(!is.na(age_group) & !is.na(!!rlang::sym(sex_var))) %>%
      dplyr::group_by(age_group, !!rlang::sym(sex_var)) %>%
      dplyr::summarise(effectif = dplyr::n(), .groups = "drop")
  } else {
    pyramid_data <- data %>%
      dplyr::filter(!is.na(age_group) & !is.na(!!rlang::sym(sex_var))) %>%
      dplyr::group_by(age_group, !!rlang::sym(sex_var)) %>%
      dplyr::summarise(effectif = sum(!!rlang::sym(weight_var), na.rm = TRUE), .groups = "drop")
  }
  
  # Pivoter
  pyramid_wide <- pyramid_data %>%
    tidyr::pivot_wider(
      names_from = !!rlang::sym(sex_var),
      values_from = effectif,
      values_fill = 0
    )
  
  # Renommer et formater pour la pyramide
  names(pyramid_wide)[names(pyramid_wide) == as.character(male_code)] <- "male"
  names(pyramid_wide)[names(pyramid_wide) == as.character(female_code)] <- "female"
  
  # Calculer les pourcentages
  total_pop <- sum(pyramid_wide$male, na.rm = TRUE) + sum(pyramid_wide$female, na.rm = TRUE)
  pyramid_wide$male_pct <- round(pyramid_wide$male / total_pop * 100, 2)
  pyramid_wide$female_pct <- round(pyramid_wide$female / total_pop * 100, 2)
  
  # Pour la visualisation, les hommes sont négatifs
  pyramid_wide$male_pct_viz <- -pyramid_wide$male_pct
  
  attr(pyramid_wide, "table_type") <- "age_pyramid"
  class(pyramid_wide) <- c("census_table", class(pyramid_wide))
  
  return(pyramid_wide)
}

#' Tableau de nuptialité
#'
#' @description Produit un tableau sur la situation matrimoniale
#' @param data Data.frame contenant les données de recensement
#' @param marital_var Nom de la variable de situation matrimoniale
#' @param age_var Nom de la variable d'âge
#' @param sex_var Nom de la variable de sexe
#' @param admin_var Nom de la variable d'unité administrative (optionnel)
#' @param weight_var Nom de la variable de pondération (optionnel)
#' @param min_age Âge minimum pour l'analyse (défaut: 12)
#' @return Data.frame avec les indicateurs de nuptialité
#' @export
table_nuptiality <- function(data, marital_var = "marital_status", 
                              age_var = "age", sex_var = "sex",
                              admin_var = NULL, weight_var = NULL,
                              min_age = 12) {
  
  # Filtrer les personnes en âge de se marier
  data_filtered <- data[data[[age_var]] >= min_age & !is.na(data[[marital_var]]), ]
  
  # Créer les groupes d'âge
  data_filtered$age_group <- create_age_groups(data_filtered[[age_var]])
  
  # Distribution par situation matrimoniale et sexe
  if (is.null(weight_var)) {
    nuptiality <- data_filtered %>%
      dplyr::group_by(!!rlang::sym(sex_var), !!rlang::sym(marital_var)) %>%
      dplyr::summarise(effectif = dplyr::n(), .groups = "drop")
  } else {
    nuptiality <- data_filtered %>%
      dplyr::group_by(!!rlang::sym(sex_var), !!rlang::sym(marital_var)) %>%
      dplyr::summarise(effectif = sum(!!rlang::sym(weight_var), na.rm = TRUE), .groups = "drop")
  }
  
  # Calculer les pourcentages par sexe
  nuptiality <- nuptiality %>%
    dplyr::group_by(!!rlang::sym(sex_var)) %>%
    dplyr::mutate(
      total_sex = sum(effectif),
      pct = round(effectif / total_sex * 100, 2)
    ) %>%
    dplyr::ungroup()
  
  # Tableau par âge et situation matrimoniale
  nuptiality_by_age <- data_filtered %>%
    dplyr::group_by(age_group, !!rlang::sym(sex_var), !!rlang::sym(marital_var)) %>%
    dplyr::summarise(effectif = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(age_group, !!rlang::sym(sex_var)) %>%
    dplyr::mutate(pct = round(effectif / sum(effectif) * 100, 2)) %>%
    dplyr::ungroup()
  
  # Indicateurs synthétiques
  indicators <- list(
    # Proportion de célibataires définitifs (50 ans et plus jamais mariés)
    celibat_definitif = data_filtered %>%
      dplyr::filter(!!rlang::sym(age_var) >= 50) %>%
      dplyr::group_by(!!rlang::sym(sex_var)) %>%
      dplyr::summarise(
        n_total = dplyr::n(),
        n_celibataires = sum(!!rlang::sym(marital_var) %in% c("single", "célibataire", "Célibataire", 1)),
        pct_celibat_definitif = round(n_celibataires / n_total * 100, 2),
        .groups = "drop"
      ),
    
    # Âge moyen au premier mariage (approximation)
    singulate_mean_age = calculate_smam(data_filtered, age_var, sex_var, marital_var)
  )
  
  result <- list(
    distribution = nuptiality,
    by_age = nuptiality_by_age,
    indicators = indicators
  )
  
  attr(result, "table_type") <- "nuptiality"
  class(result) <- c("census_table", "list")
  
  return(result)
}

#' Calculer l'âge moyen au premier mariage (SMAM)
#'
#' @description Calcule le Singulate Mean Age at Marriage
#' @param data Data.frame filtré
#' @param age_var Variable d'âge
#' @param sex_var Variable de sexe
#' @param marital_var Variable de situation matrimoniale
#' @return Data.frame avec le SMAM par sexe
#' @keywords internal
calculate_smam <- function(data, age_var, sex_var, marital_var) {
  
  # Calculer la proportion de célibataires par groupe d'âge et sexe
  data$age_group_5 <- cut(data[[age_var]], 
                          breaks = c(seq(15, 50, 5), Inf),
                          labels = c("15-19", "20-24", "25-29", "30-34", 
                                    "35-39", "40-44", "45-49", "50+"),
                          right = FALSE)
  
  celibataires <- data %>%
    dplyr::filter(!is.na(age_group_5)) %>%
    dplyr::group_by(!!rlang::sym(sex_var), age_group_5) %>%
    dplyr::summarise(
      n_total = dplyr::n(),
      n_celibataires = sum(!!rlang::sym(marital_var) %in% c("single", "célibataire", "Célibataire", 1)),
      pct_celibataires = n_celibataires / n_total,
      .groups = "drop"
    )
  
  # Calculer le SMAM par sexe
  smam_by_sex <- celibataires %>%
    dplyr::filter(age_group_5 != "50+") %>%
    dplyr::group_by(!!rlang::sym(sex_var)) %>%
    dplyr::summarise(
      # Formule simplifiée du SMAM
      sum_celibataires = sum(pct_celibataires * 5),
      pct_50_plus = dplyr::first(celibataires$pct_celibataires[celibataires$age_group_5 == "50+"]),
      smam = 15 + (sum_celibataires - 35 * pct_50_plus) / (1 - pct_50_plus),
      .groups = "drop"
    )
  
  return(smam_by_sex)
}

#' Tableau de fécondité
#'
#' @description Produit un tableau des indicateurs de fécondité
#' @param data Data.frame contenant les données de recensement
#' @param age_var Nom de la variable d'âge
#' @param children_born_var Variable du nombre d'enfants nés vivants
#' @param children_alive_var Variable du nombre d'enfants survivants
#' @param children_last_year_var Variable des naissances des 12 derniers mois
#' @param sex_var Variable de sexe (pour filtrer les femmes)
#' @param female_code Code pour le sexe féminin
#' @param admin_var Unité administrative (optionnel)
#' @param weight_var Variable de pondération (optionnel)
#' @return Liste avec les tableaux et indicateurs de fécondité
#' @export
table_fertility <- function(data, age_var = "age",
                            children_born_var = "children_born",
                            children_alive_var = "children_alive",
                            children_last_year_var = "children_last_year",
                            sex_var = "sex", female_code = 2,
                            admin_var = NULL, weight_var = NULL) {
  
  # Filtrer les femmes en âge de procréer (15-49 ans)
  women <- data %>%
    dplyr::filter(
      !!rlang::sym(sex_var) == female_code,
      !!rlang::sym(age_var) >= 15,
      !!rlang::sym(age_var) <= 49
    )
  
  women$age_group <- create_age_groups(
    women[[age_var]], 
    breaks = c(15, 20, 25, 30, 35, 40, 45, 50)
  )
  
  # Parité moyenne par groupe d'âge
  if (children_born_var %in% names(women)) {
    parity <- women %>%
      dplyr::filter(!is.na(age_group)) %>%
      dplyr::group_by(age_group) %>%
      dplyr::summarise(
        n_women = dplyr::n(),
        mean_children_born = round(mean(!!rlang::sym(children_born_var), na.rm = TRUE), 2),
        median_children_born = median(!!rlang::sym(children_born_var), na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    parity <- NULL
  }
  
  # Taux de fécondité par âge (si naissances des 12 derniers mois disponibles)
  if (children_last_year_var %in% names(women)) {
    asfr <- women %>%
      dplyr::filter(!is.na(age_group)) %>%
      dplyr::group_by(age_group) %>%
      dplyr::summarise(
        n_women = dplyr::n(),
        births = sum(!!rlang::sym(children_last_year_var), na.rm = TRUE),
        asfr = round(births / n_women * 1000, 2),
        .groups = "drop"
      )
    
    # Indice synthétique de fécondité (ISF)
    isf <- sum(asfr$asfr) * 5 / 1000
  } else {
    asfr <- NULL
    isf <- NA
  }
  
  # Ratio de survie des enfants
  if (all(c(children_born_var, children_alive_var) %in% names(women))) {
    child_survival <- women %>%
      dplyr::filter(
        !!rlang::sym(children_born_var) > 0,
        !is.na(age_group)
      ) %>%
      dplyr::group_by(age_group) %>%
      dplyr::summarise(
        n_women = dplyr::n(),
        total_born = sum(!!rlang::sym(children_born_var), na.rm = TRUE),
        total_alive = sum(!!rlang::sym(children_alive_var), na.rm = TRUE),
        survival_ratio = round(total_alive / total_born * 100, 2),
        .groups = "drop"
      )
  } else {
    child_survival <- NULL
  }
  
  # Indicateurs synthétiques
  indicators <- list(
    isf = round(isf, 2),
    parite_finale = if (!is.null(parity)) {
      parity$mean_children_born[parity$age_group == "45-49"]
    } else NA,
    taux_survie_enfants = if (!is.null(child_survival)) {
      round(sum(child_survival$total_alive) / sum(child_survival$total_born) * 100, 2)
    } else NA
  )
  
  result <- list(
    parity = parity,
    asfr = asfr,
    child_survival = child_survival,
    indicators = indicators
  )
  
  attr(result, "table_type") <- "fertility"
  class(result) <- c("census_table", "list")
  
  return(result)
}

#' Tableau de mortalité
#'
#' @description Produit un tableau des indicateurs de mortalité
#' @param data Data.frame contenant les données de recensement
#' @param age_var Variable d'âge
#' @param sex_var Variable de sexe
#' @param deaths_var Variable des décès dans le ménage (12 derniers mois)
#' @param children_born_var Variable du nombre d'enfants nés
#' @param children_alive_var Variable du nombre d'enfants survivants
#' @param admin_var Unité administrative (optionnel)
#' @param weight_var Variable de pondération (optionnel)
#' @return Liste avec les tableaux et indicateurs de mortalité
#' @export
table_mortality <- function(data, age_var = "age", sex_var = "sex",
                            deaths_var = "deaths_household",
                            children_born_var = "children_born",
                            children_alive_var = "children_alive",
                            admin_var = NULL, weight_var = NULL) {
  
  results <- list()
  
  # Mortalité des enfants (méthode de Brass)
  if (all(c(children_born_var, children_alive_var, sex_var) %in% names(data))) {
    
    # Filtrer les femmes
    women <- data %>%
      dplyr::filter(!!rlang::sym(sex_var) %in% c(2, "F", "Female", "Femme"))
    
    women$age_group <- create_age_groups(
      women[[age_var]], 
      breaks = c(15, 20, 25, 30, 35, 40, 45, 50)
    )
    
    # Proportion d'enfants décédés par groupe d'âge de la mère
    child_mortality <- women %>%
      dplyr::filter(
        !is.na(age_group),
        !!rlang::sym(children_born_var) > 0
      ) %>%
      dplyr::group_by(age_group) %>%
      dplyr::summarise(
        n_women = dplyr::n(),
        children_born = sum(!!rlang::sym(children_born_var), na.rm = TRUE),
        children_alive = sum(!!rlang::sym(children_alive_var), na.rm = TRUE),
        children_dead = children_born - children_alive,
        pct_dead = round(children_dead / children_born * 100, 2),
        .groups = "drop"
      )
    
    results$child_mortality <- child_mortality
    
    # Estimation du quotient de mortalité infantile (q1) - méthode simplifiée
    if (nrow(child_mortality) > 0) {
      # Utiliser le groupe 20-24 comme approximation de q1
      q1_approx <- child_mortality$pct_dead[child_mortality$age_group == "20-24"]
      results$q1_estimate <- if (length(q1_approx) > 0) q1_approx else NA
    }
  }
  
  # Décès dans les ménages (si disponible)
  if (deaths_var %in% names(data)) {
    deaths_summary <- data %>%
      dplyr::summarise(
        n_households = dplyr::n_distinct(household_id),
        total_deaths = sum(!!rlang::sym(deaths_var), na.rm = TRUE),
        households_with_deaths = sum(!!rlang::sym(deaths_var) > 0, na.rm = TRUE),
        .groups = "drop"
      )
    
    results$deaths_summary <- deaths_summary
  }
  
  # Indicateurs synthétiques
  results$indicators <- list(
    taux_mortalite_infantile_estime = results$q1_estimate,
    taux_mortalite_juvenile = if (!is.null(results$child_mortality)) {
      # Approximation à partir du groupe 25-29
      results$child_mortality$pct_dead[results$child_mortality$age_group == "25-29"]
    } else NA
  )
  
  attr(results, "table_type") <- "mortality"
  class(results) <- c("census_table", "list")
  
  return(results)
}

#' Tableau de migration
#'
#' @description Produit un tableau des indicateurs de migration
#' @param data Data.frame contenant les données de recensement
#' @param birthplace_var Variable du lieu de naissance
#' @param residence_var Variable du lieu de résidence actuel
#' @param residence_5y_var Variable du lieu de résidence il y a 5 ans
#' @param age_var Variable d'âge
#' @param sex_var Variable de sexe
#' @param admin_var Unité administrative (optionnel)
#' @param weight_var Variable de pondération (optionnel)
#' @return Liste avec les tableaux et indicateurs de migration
#' @export
table_migration <- function(data, birthplace_var = "birthplace",
                            residence_var = "residence",
                            residence_5y_var = "residence_5years",
                            age_var = "age", sex_var = "sex",
                            admin_var = NULL, weight_var = NULL) {
  
  results <- list()
  
  # Migration durée de vie (lieu de naissance vs résidence actuelle)
  if (all(c(birthplace_var, residence_var) %in% names(data))) {
    
    lifetime_migration <- data %>%
      dplyr::mutate(
        migrant_lifetime = !!rlang::sym(birthplace_var) != !!rlang::sym(residence_var)
      ) %>%
      dplyr::group_by(!!rlang::sym(sex_var)) %>%
      dplyr::summarise(
        n_total = dplyr::n(),
        n_migrants = sum(migrant_lifetime, na.rm = TRUE),
        pct_migrants = round(n_migrants / n_total * 100, 2),
        .groups = "drop"
      )
    
    results$lifetime_migration <- lifetime_migration
    
    # Matrice origine-destination
    od_matrix <- data %>%
      dplyr::filter(!is.na(!!rlang::sym(birthplace_var)) & !is.na(!!rlang::sym(residence_var))) %>%
      dplyr::group_by(
        origin = !!rlang::sym(birthplace_var),
        destination = !!rlang::sym(residence_var)
      ) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::filter(origin != destination)
    
    results$od_matrix_lifetime <- od_matrix
  }
  
  # Migration récente (5 dernières années)
  if (all(c(residence_5y_var, residence_var) %in% names(data))) {
    
    # Filtrer les personnes de 5 ans et plus
    data_5plus <- data %>%
      dplyr::filter(!!rlang::sym(age_var) >= 5)
    
    recent_migration <- data_5plus %>%
      dplyr::mutate(
        migrant_recent = !!rlang::sym(residence_5y_var) != !!rlang::sym(residence_var)
      ) %>%
      dplyr::group_by(!!rlang::sym(sex_var)) %>%
      dplyr::summarise(
        n_total = dplyr::n(),
        n_migrants = sum(migrant_recent, na.rm = TRUE),
        pct_migrants = round(n_migrants / n_total * 100, 2),
        .groups = "drop"
      )
    
    results$recent_migration <- recent_migration
    
    # Par groupe d'âge
    data_5plus$age_group <- create_age_groups(data_5plus[[age_var]])
    
    migration_by_age <- data_5plus %>%
      dplyr::mutate(
        migrant_recent = !!rlang::sym(residence_5y_var) != !!rlang::sym(residence_var)
      ) %>%
      dplyr::group_by(age_group, !!rlang::sym(sex_var)) %>%
      dplyr::summarise(
        n_total = dplyr::n(),
        n_migrants = sum(migrant_recent, na.rm = TRUE),
        pct_migrants = round(n_migrants / n_total * 100, 2),
        .groups = "drop"
      )
    
    results$migration_by_age <- migration_by_age
  }
  
  # Solde migratoire par unité administrative
  if (!is.null(admin_var) && admin_var %in% names(data)) {
    if (all(c(birthplace_var, residence_var) %in% names(data))) {
      
      # Immigrants par unité
      immigrants <- data %>%
        dplyr::filter(!!rlang::sym(birthplace_var) != !!rlang::sym(residence_var)) %>%
        dplyr::group_by(!!rlang::sym(admin_var)) %>%
        dplyr::summarise(immigrants = dplyr::n(), .groups = "drop")
      
      # Émigrants par unité (approximation)
      emigrants <- data %>%
        dplyr::filter(!!rlang::sym(birthplace_var) != !!rlang::sym(residence_var)) %>%
        dplyr::group_by(origin = !!rlang::sym(birthplace_var)) %>%
        dplyr::summarise(emigrants = dplyr::n(), .groups = "drop")
      
      # Population par unité
      pop_by_admin <- data %>%
        dplyr::group_by(!!rlang::sym(admin_var)) %>%
        dplyr::summarise(population = dplyr::n(), .groups = "drop")
      
      # Fusionner
      migration_balance <- pop_by_admin %>%
        dplyr::left_join(immigrants, by = admin_var) %>%
        dplyr::left_join(emigrants, by = setNames("origin", admin_var)) %>%
        dplyr::mutate(
          immigrants = tidyr::replace_na(immigrants, 0),
          emigrants = tidyr::replace_na(emigrants, 0),
          net_migration = immigrants - emigrants,
          migration_rate = round(net_migration / population * 1000, 2)
        )
      
      results$migration_balance <- migration_balance
    }
  }
  
  attr(results, "table_type") <- "migration"
  class(results) <- c("census_table", "list")
  
  return(results)
}

#' Tableau sur le handicap
#'
#' @description Produit un tableau des indicateurs sur le handicap
#' @param data Data.frame contenant les données de recensement
#' @param disability_var Variable de handicap (oui/non ou type)
#' @param disability_type_var Variable du type de handicap (optionnel)
#' @param disability_severity_var Variable de la sévérité (optionnel)
#' @param age_var Variable d'âge
#' @param sex_var Variable de sexe
#' @param admin_var Unité administrative (optionnel)
#' @param weight_var Variable de pondération (optionnel)
#' @return Liste avec les tableaux et indicateurs sur le handicap
#' @export
table_disability <- function(data, disability_var = "disability",
                              disability_type_var = "disability_type",
                              disability_severity_var = "disability_severity",
                              age_var = "age", sex_var = "sex",
                              admin_var = NULL, weight_var = NULL) {
  
  results <- list()
  
  # Prévalence globale du handicap
  if (disability_var %in% names(data)) {
    
    # Identifier les personnes avec handicap
    data$has_disability <- data[[disability_var]] %in% c(1, "Yes", "Oui", "yes", "oui", TRUE)
    
    prevalence_global <- data %>%
      dplyr::summarise(
        n_total = dplyr::n(),
        n_disabled = sum(has_disability, na.rm = TRUE),
        prevalence = round(n_disabled / n_total * 100, 2)
      )
    
    results$prevalence_global <- prevalence_global
    
    # Par sexe
    prevalence_by_sex <- data %>%
      dplyr::group_by(!!rlang::sym(sex_var)) %>%
      dplyr::summarise(
        n_total = dplyr::n(),
        n_disabled = sum(has_disability, na.rm = TRUE),
        prevalence = round(n_disabled / n_total * 100, 2),
        .groups = "drop"
      )
    
    results$prevalence_by_sex <- prevalence_by_sex
    
    # Par groupe d'âge
    data$age_group <- create_age_groups(data[[age_var]])
    
    prevalence_by_age <- data %>%
      dplyr::filter(!is.na(age_group)) %>%
      dplyr::group_by(age_group) %>%
      dplyr::summarise(
        n_total = dplyr::n(),
        n_disabled = sum(has_disability, na.rm = TRUE),
        prevalence = round(n_disabled / n_total * 100, 2),
        .groups = "drop"
      )
    
    results$prevalence_by_age <- prevalence_by_age
    
    # Par âge et sexe
    prevalence_by_age_sex <- data %>%
      dplyr::filter(!is.na(age_group)) %>%
      dplyr::group_by(age_group, !!rlang::sym(sex_var)) %>%
      dplyr::summarise(
        n_total = dplyr::n(),
        n_disabled = sum(has_disability, na.rm = TRUE),
        prevalence = round(n_disabled / n_total * 100, 2),
        .groups = "drop"
      )
    
    results$prevalence_by_age_sex <- prevalence_by_age_sex
  }
  
  # Par type de handicap
  if (disability_type_var %in% names(data)) {
    
    disability_types <- data %>%
      dplyr::filter(!is.na(!!rlang::sym(disability_type_var))) %>%
      dplyr::group_by(!!rlang::sym(disability_type_var)) %>%
      dplyr::summarise(
        n = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::mutate(pct = round(n / sum(n) * 100, 2)) %>%
      dplyr::arrange(dplyr::desc(n))
    
    results$by_type <- disability_types
  }
  
  # Par sévérité
  if (disability_severity_var %in% names(data)) {
    
    disability_severity <- data %>%
      dplyr::filter(!is.na(!!rlang::sym(disability_severity_var))) %>%
      dplyr::group_by(!!rlang::sym(disability_severity_var)) %>%
      dplyr::summarise(
        n = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::mutate(pct = round(n / sum(n) * 100, 2))
    
    results$by_severity <- disability_severity
  }
  
  # Par unité administrative
  if (!is.null(admin_var) && admin_var %in% names(data) && disability_var %in% names(data)) {
    
    prevalence_by_admin <- data %>%
      dplyr::group_by(!!rlang::sym(admin_var)) %>%
      dplyr::summarise(
        n_total = dplyr::n(),
        n_disabled = sum(has_disability, na.rm = TRUE),
        prevalence = round(n_disabled / n_total * 100, 2),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(prevalence))
    
    results$prevalence_by_admin <- prevalence_by_admin
  }
  
  attr(results, "table_type") <- "disability"
  class(results) <- c("census_table", "list")
  
  return(results)
}

#' Tableau sur le genre
#'
#' @description Produit un tableau des indicateurs de genre
#' @param data Data.frame contenant les données de recensement
#' @param sex_var Variable de sexe
#' @param age_var Variable d'âge
#' @param education_var Variable de niveau d'éducation
#' @param employment_var Variable de statut d'emploi
#' @param head_household_var Variable de chef de ménage
#' @param admin_var Unité administrative (optionnel)
#' @param weight_var Variable de pondération (optionnel)
#' @return Liste avec les tableaux et indicateurs de genre
#' @export
table_gender <- function(data, sex_var = "sex", age_var = "age",
                          education_var = "education_level",
                          employment_var = "employment_status",
                          head_household_var = "head_household",
                          admin_var = NULL, weight_var = NULL) {
  
  results <- list()
  
  # Rapport de masculinité global
  sex_distribution <- data %>%
    dplyr::filter(!is.na(!!rlang::sym(sex_var))) %>%
    dplyr::group_by(!!rlang::sym(sex_var)) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    dplyr::mutate(pct = round(n / sum(n) * 100, 2))
  
  results$sex_distribution <- sex_distribution
  
  # Rapport de masculinité par groupe d'âge
  data$age_group <- create_age_groups(data[[age_var]])
  
  sex_ratio_by_age <- data %>%
    dplyr::filter(!is.na(age_group) & !is.na(!!rlang::sym(sex_var))) %>%
    dplyr::group_by(age_group, !!rlang::sym(sex_var)) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = !!rlang::sym(sex_var), values_from = n, values_fill = 0)
  
  results$sex_ratio_by_age <- sex_ratio_by_age
  
  # Écart de genre dans l'éducation
  if (education_var %in% names(data)) {
    
    education_by_sex <- data %>%
      dplyr::filter(!is.na(!!rlang::sym(education_var)) & !is.na(!!rlang::sym(sex_var))) %>%
      dplyr::group_by(!!rlang::sym(sex_var), !!rlang::sym(education_var)) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::group_by(!!rlang::sym(sex_var)) %>%
      dplyr::mutate(pct = round(n / sum(n) * 100, 2)) %>%
      dplyr::ungroup()
    
    results$education_by_sex <- education_by_sex
  }
  
  # Écart de genre dans l'emploi
  if (employment_var %in% names(data)) {
    
    # Filtrer la population en âge de travailler (15-64 ans)
    working_age <- data %>%
      dplyr::filter(!!rlang::sym(age_var) >= 15 & !!rlang::sym(age_var) <= 64)
    
    employment_by_sex <- working_age %>%
      dplyr::filter(!is.na(!!rlang::sym(employment_var)) & !is.na(!!rlang::sym(sex_var))) %>%
      dplyr::group_by(!!rlang::sym(sex_var), !!rlang::sym(employment_var)) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::group_by(!!rlang::sym(sex_var)) %>%
      dplyr::mutate(pct = round(n / sum(n) * 100, 2)) %>%
      dplyr::ungroup()
    
    results$employment_by_sex <- employment_by_sex
  }
  
  # Chefs de ménage par sexe
  if (head_household_var %in% names(data)) {
    
    head_by_sex <- data %>%
      dplyr::filter(!!rlang::sym(head_household_var) %in% c(1, "Yes", "Oui", TRUE)) %>%
      dplyr::group_by(!!rlang::sym(sex_var)) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(pct = round(n / sum(n) * 100, 2))
    
    results$household_heads_by_sex <- head_by_sex
  }
  
  # Indice de parité (si données d'éducation disponibles)
  if (education_var %in% names(data)) {
    # Calculer l'indice de parité pour l'éducation secondaire et supérieure
    # (ratio femmes/hommes)
    results$gender_parity_index <- "À calculer selon les niveaux d'éducation disponibles"
  }
  
  attr(results, "table_type") <- "gender"
  class(results) <- c("census_table", "list")
  
  return(results)
}

#' Générer tous les tableaux statistiques
#'
#' @description Génère l'ensemble des tableaux statistiques standard
#' @param data Data.frame contenant les données de recensement
#' @param config Liste de configuration des variables
#' @return Liste contenant tous les tableaux
#' @export
generate_all_tables <- function(data, config = list()) {
  
  # Configuration par défaut
  default_config <- list(
    age_var = "age",
    sex_var = "sex",
    admin_var = NULL,
    weight_var = NULL,
    marital_var = "marital_status",
    children_born_var = "children_born",
    children_alive_var = "children_alive",
    children_last_year_var = "children_last_year",
    disability_var = "disability",
    birthplace_var = "birthplace",
    residence_var = "residence",
    residence_5y_var = "residence_5years",
    education_var = "education_level",
    employment_var = "employment_status"
  )
  
  # Fusionner avec la configuration fournie
  config <- modifyList(default_config, config)
  
  message("\n=== GÉNÉRATION DES TABLEAUX STATISTIQUES ===\n")
  
  all_tables <- list()
  
  # 1. Structure de la population
  message("1. Tableau de structure de la population...")
  all_tables$structure <- tryCatch(
    table_population_structure(data, config$age_var, config$sex_var, 
                               config$admin_var, config$weight_var),
    error = function(e) { message("  Erreur: ", e$message); NULL }
  )
  
  # 2. Pyramide des âges
  message("2. Données de la pyramide des âges...")
  all_tables$pyramid <- tryCatch(
    table_age_pyramid(data, config$age_var, config$sex_var, config$weight_var),
    error = function(e) { message("  Erreur: ", e$message); NULL }
  )
  
  # 3. Nuptialité
  message("3. Tableau de nuptialité...")
  all_tables$nuptiality <- tryCatch(
    table_nuptiality(data, config$marital_var, config$age_var, config$sex_var,
                     config$admin_var, config$weight_var),
    error = function(e) { message("  Erreur: ", e$message); NULL }
  )
  
  # 4. Fécondité
  message("4. Tableau de fécondité...")
  all_tables$fertility <- tryCatch(
    table_fertility(data, config$age_var, config$children_born_var,
                    config$children_alive_var, config$children_last_year_var,
                    config$sex_var),
    error = function(e) { message("  Erreur: ", e$message); NULL }
  )
  
  # 5. Mortalité
  message("5. Tableau de mortalité...")
  all_tables$mortality <- tryCatch(
    table_mortality(data, config$age_var, config$sex_var,
                    children_born_var = config$children_born_var,
                    children_alive_var = config$children_alive_var),
    error = function(e) { message("  Erreur: ", e$message); NULL }
  )
  
  # 6. Migration
  message("6. Tableau de migration...")
  all_tables$migration <- tryCatch(
    table_migration(data, config$birthplace_var, config$residence_var,
                    config$residence_5y_var, config$age_var, config$sex_var,
                    config$admin_var),
    error = function(e) { message("  Erreur: ", e$message); NULL }
  )
  
  # 7. Handicap
  message("7. Tableau sur le handicap...")
  all_tables$disability <- tryCatch(
    table_disability(data, config$disability_var, age_var = config$age_var,
                     sex_var = config$sex_var, admin_var = config$admin_var),
    error = function(e) { message("  Erreur: ", e$message); NULL }
  )
  
  # 8. Genre
  message("8. Tableau sur le genre...")
  all_tables$gender <- tryCatch(
    table_gender(data, config$sex_var, config$age_var, config$education_var,
                 config$employment_var, admin_var = config$admin_var),
    error = function(e) { message("  Erreur: ", e$message); NULL }
  )
  
  message("\n✓ Génération des tableaux terminée")
  
  class(all_tables) <- c("census_tables_collection", "list")
  
  return(all_tables)
}

#' Exporter les tableaux statistiques
#'
#' @description Exporte les tableaux dans différents formats
#' @param tables Liste de tableaux (résultat de generate_all_tables)
#' @param output_dir Répertoire de sortie
#' @param format Format de sortie ("xlsx", "csv", "both")
#' @param prefix Préfixe pour les noms de fichiers
#' @return Vecteur des chemins des fichiers créés
#' @export
export_tables <- function(tables, output_dir = ".", format = "xlsx", prefix = "census") {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  files_created <- character()
  
  if (format %in% c("xlsx", "both")) {
    # Créer un classeur Excel avec plusieurs feuilles
    wb <- openxlsx::createWorkbook()
    
    for (table_name in names(tables)) {
      table_data <- tables[[table_name]]
      
      if (is.data.frame(table_data)) {
        openxlsx::addWorksheet(wb, table_name)
        openxlsx::writeData(wb, table_name, table_data)
      } else if (is.list(table_data)) {
        # Pour les listes, exporter chaque élément data.frame
        for (sub_name in names(table_data)) {
          if (is.data.frame(table_data[[sub_name]])) {
            sheet_name <- paste0(substr(table_name, 1, 15), "_", substr(sub_name, 1, 10))
            openxlsx::addWorksheet(wb, sheet_name)
            openxlsx::writeData(wb, sheet_name, table_data[[sub_name]])
          }
        }
      }
    }
    
    xlsx_file <- file.path(output_dir, paste0(prefix, "_tables.xlsx"))
    openxlsx::saveWorkbook(wb, xlsx_file, overwrite = TRUE)
    files_created <- c(files_created, xlsx_file)
    message("Fichier Excel créé: ", xlsx_file)
  }
  
  if (format %in% c("csv", "both")) {
    csv_dir <- file.path(output_dir, paste0(prefix, "_csv"))
    if (!dir.exists(csv_dir)) dir.create(csv_dir)
    
    for (table_name in names(tables)) {
      table_data <- tables[[table_name]]
      
      if (is.data.frame(table_data)) {
        csv_file <- file.path(csv_dir, paste0(table_name, ".csv"))
        write.csv(table_data, csv_file, row.names = FALSE)
        files_created <- c(files_created, csv_file)
      }
    }
    message("Fichiers CSV créés dans: ", csv_dir)
  }
  
  return(files_created)
}
