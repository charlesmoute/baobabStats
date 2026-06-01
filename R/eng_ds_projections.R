#' @title Population Projections
#' @description Functions for population projections using cohort-component method
#' @name projections
NULL

#' Population Projection
#'
#' Project population using the cohort-component method
#'
#' @param base_pop Data frame or matrix with base population by age and sex
#' @param fertility Age-specific fertility rates (ASFR per woman)
#' @param mortality Survival ratios or life table
#' @param migration Net migration by age and sex (optional)
#' @param years Number of years to project
#' @param interval Projection interval in years (default 5)
#' @param sex_ratio_birth Sex ratio at birth (males per 100 females)
#' @param scenarios List of alternative scenarios (optional)
#'
#' @return Population projection results
#' @export
#'
#' @examples
#' \dontrun{
#' proj <- ds_project_population(
#'   base_pop = pop_2020,
#'   fertility = asfr_2020,
#'   mortality = survival_2020,
#'   years = 30
#' )
#' }
ds_project_population <- function(base_pop, fertility, mortality,
                                migration = NULL, years = 25,
                                interval = 5, sex_ratio_birth = 105,
                                scenarios = NULL) {
  
  # Validate and prepare inputs
  inputs <- prepare_projection_inputs(base_pop, fertility, mortality, 
                                       migration, interval)
  
  # Number of projection periods
  n_periods <- years / interval
  
  # Initialize results storage
  n_ages <- length(inputs$age_groups)
  results <- list(
    male = matrix(0, nrow = n_ages, ncol = n_periods + 1),
    female = matrix(0, nrow = n_ages, ncol = n_periods + 1)
  )
  
  rownames(results$male) <- inputs$age_groups
  rownames(results$female) <- inputs$age_groups
  colnames(results$male) <- seq(0, years, by = interval)
  colnames(results$female) <- seq(0, years, by = interval)
  
  # Set base population
  results$male[, 1] <- inputs$pop_male
  results$female[, 1] <- inputs$pop_female
  
  # Project each period
  for (t in 1:n_periods) {
    projection <- cohort_component_step(
      pop_male = results$male[, t],
      pop_female = results$female[, t],
      survival_male = inputs$survival_male,
      survival_female = inputs$survival_female,
      fertility = inputs$fertility,
      migration_male = inputs$migration_male,
      migration_female = inputs$migration_female,
      sex_ratio_birth = sex_ratio_birth,
      interval = interval
    )
    
    results$male[, t + 1] <- projection$male
    results$female[, t + 1] <- projection$female
  }
  
  # Calculate summary statistics
  results$total <- results$male + results$female
  results$summary <- compute_projection_summary(results, inputs$age_groups, interval)
  
  # Add scenarios if provided
  if (!is.null(scenarios)) {
    results$scenarios <- project_scenarios(inputs, scenarios, years, interval, 
                                            sex_ratio_birth)
  }
  
  # Store inputs for reference
  results$inputs <- inputs
  results$parameters <- list(
    years = years,
    interval = interval,
    sex_ratio_birth = sex_ratio_birth
  )
  
  class(results) <- c("population_projection", class(results))
  return(results)
}

#' Prepare Projection Inputs
#' @keywords internal
prepare_projection_inputs <- function(base_pop, fertility, mortality, 
                                       migration, interval) {
  
  # Standard 5-year age groups
  age_groups <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
                  "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
                  "65-69", "70-74", "75-79", "80+")
  n_ages <- length(age_groups)
  
  # Process base population
  if (is.data.frame(base_pop)) {
    # Expect columns: age_group, male, female
    pop_male <- base_pop$male
    pop_female <- base_pop$female
  } else if (is.matrix(base_pop)) {
    pop_male <- base_pop[, 1]
    pop_female <- base_pop[, 2]
  } else {
    stop("base_pop must be a data frame or matrix")
  }
  
  # Ensure correct length
  if (length(pop_male) != n_ages) {
    warning("Population vector length does not match standard age groups")
  }
  
  # Process fertility (ASFR for ages 15-49)
  fertility_full <- rep(0, n_ages)
  fertility_ages <- 4:10  # Indices for 15-19 to 45-49
  if (length(fertility) == 7) {
    fertility_full[fertility_ages] <- fertility
  } else {
    fertility_full[fertility_ages] <- fertility[1:7]
  }
  
  # Process mortality (survival ratios)
  if (is.data.frame(mortality) && "nLx" %in% names(mortality)) {
    # Life table input - compute survival ratios
    survival <- compute_survival_ratios(mortality, interval)
    survival_male <- survival
    survival_female <- survival
  } else if (is.list(mortality) && "male" %in% names(mortality)) {
    survival_male <- mortality$male
    survival_female <- mortality$female
  } else if (is.numeric(mortality)) {
    survival_male <- mortality
    survival_female <- mortality
  } else {
    # Default survival ratios (approximate)
    survival_male <- c(0.98, rep(0.995, 10), 0.99, 0.98, 0.97, 0.95, 0.90, 0.80)
    survival_female <- c(0.985, rep(0.997, 10), 0.995, 0.99, 0.98, 0.96, 0.92, 0.85)
  }
  
  # Ensure survival ratios have correct length
  if (length(survival_male) < n_ages) {
    survival_male <- c(survival_male, rep(tail(survival_male, 1), n_ages - length(survival_male)))
  }
  if (length(survival_female) < n_ages) {
    survival_female <- c(survival_female, rep(tail(survival_female, 1), n_ages - length(survival_female)))
  }
  
  # Process migration
  if (is.null(migration)) {
    migration_male <- rep(0, n_ages)
    migration_female <- rep(0, n_ages)
  } else if (is.data.frame(migration)) {
    migration_male <- migration$male
    migration_female <- migration$female
  } else if (is.list(migration)) {
    migration_male <- migration$male
    migration_female <- migration$female
  } else {
    migration_male <- rep(migration / 2, n_ages)
    migration_female <- rep(migration / 2, n_ages)
  }
  
  list(
    age_groups = age_groups,
    pop_male = pop_male,
    pop_female = pop_female,
    fertility = fertility_full,
    survival_male = survival_male,
    survival_female = survival_female,
    migration_male = migration_male,
    migration_female = migration_female
  )
}

#' Compute Survival Ratios from Life Table
#' @keywords internal
compute_survival_ratios <- function(life_table, interval = 5) {
  
  nLx <- life_table$nLx
  n <- length(nLx)
  
  # Survival ratio = nLx(x+n) / nLx(x)
  survival <- numeric(n)
  for (i in 1:(n-1)) {
    survival[i] <- nLx[i+1] / nLx[i]
  }
  # Last age group survives within itself
  survival[n] <- nLx[n] / (nLx[n-1] + nLx[n])
  
  return(survival)
}

#' Cohort-Component Projection Step
#'
#' Perform one step of cohort-component projection
#'
#' @param pop_male Male population by age
#' @param pop_female Female population by age
#' @param survival_male Male survival ratios
#' @param survival_female Female survival ratios
#' @param fertility ASFR by age group
#' @param migration_male Male net migration
#' @param migration_female Female net migration
#' @param sex_ratio_birth Sex ratio at birth
#' @param interval Projection interval
#'
#' @return List with projected male and female populations
#' @export
cohort_component_step <- function(pop_male, pop_female, survival_male, survival_female,
                                   fertility, migration_male, migration_female,
                                   sex_ratio_birth = 105, interval = 5) {
  
  n_ages <- length(pop_male)
  
  # Initialize new population vectors
  new_male <- numeric(n_ages)
  new_female <- numeric(n_ages)
  
  # Survive existing population
  for (i in 2:n_ages) {
    new_male[i] <- pop_male[i-1] * survival_male[i-1]
    new_female[i] <- pop_female[i-1] * survival_female[i-1]
  }
  
  # Last age group: survivors from previous + survivors within
  new_male[n_ages] <- new_male[n_ages] + pop_male[n_ages] * survival_male[n_ages]
  new_female[n_ages] <- new_female[n_ages] + pop_female[n_ages] * survival_female[n_ages]
  
  # Calculate births
  # Average female population in reproductive ages during interval
  avg_female <- (pop_female + new_female) / 2
  
  # Births = sum(ASFR * women) * interval
  births <- sum(fertility * avg_female) * interval
  
  # Survive births to end of interval
  # Approximate: use average of infant and child survival
  infant_survival <- (survival_female[1] + survival_male[1]) / 2
  
  # Split by sex
  prop_male <- sex_ratio_birth / (100 + sex_ratio_birth)
  prop_female <- 100 / (100 + sex_ratio_birth)
  
  new_male[1] <- births * prop_male * infant_survival
  new_female[1] <- births * prop_female * infant_survival
  
  # Add migration
  new_male <- new_male + migration_male
  new_female <- new_female + migration_female
  
  # Ensure non-negative
  new_male <- pmax(new_male, 0)
  new_female <- pmax(new_female, 0)
  
  list(male = new_male, female = new_female)
}

#' Compute Projection Summary
#' @keywords internal
compute_projection_summary <- function(results, age_groups, interval) {
  
  n_periods <- ncol(results$total)
  years <- as.numeric(colnames(results$total))
  
  summary <- data.frame(
    year = years,
    total = colSums(results$total),
    male = colSums(results$male),
    female = colSums(results$female),
    stringsAsFactors = FALSE
  )
  
  summary$sex_ratio <- summary$male / summary$female * 100
  
  # Growth rates
  summary$growth_rate <- c(NA, diff(log(summary$total)) / interval * 100)
  
  # Age structure indicators
  young_idx <- which(age_groups %in% c("0-4", "5-9", "10-14"))
  old_idx <- which(age_groups %in% c("65-69", "70-74", "75-79", "80+"))
  working_idx <- setdiff(1:length(age_groups), c(young_idx, old_idx))
  
  summary$pct_young <- colSums(results$total[young_idx, ]) / summary$total * 100
  summary$pct_working <- colSums(results$total[working_idx, ]) / summary$total * 100
  summary$pct_old <- colSums(results$total[old_idx, ]) / summary$total * 100
  summary$dependency_ratio <- (colSums(results$total[young_idx, ]) + 
                                 colSums(results$total[old_idx, ])) /
    colSums(results$total[working_idx, ]) * 100
  
  return(summary)
}

#' Project Alternative Scenarios
#' @keywords internal
project_scenarios <- function(inputs, scenarios, years, interval, sex_ratio_birth) {
  
  scenario_results <- list()
  
  for (scenario_name in names(scenarios)) {
    scenario <- scenarios[[scenario_name]]
    
    # Modify inputs based on scenario
    modified_inputs <- inputs
    
    if (!is.null(scenario$fertility_factor)) {
      modified_inputs$fertility <- inputs$fertility * scenario$fertility_factor
    }
    if (!is.null(scenario$mortality_factor)) {
      # Improve survival
      modified_inputs$survival_male <- 1 - (1 - inputs$survival_male) * scenario$mortality_factor
      modified_inputs$survival_female <- 1 - (1 - inputs$survival_female) * scenario$mortality_factor
    }
    if (!is.null(scenario$migration)) {
      modified_inputs$migration_male <- scenario$migration$male
      modified_inputs$migration_female <- scenario$migration$female
    }
    
    # Run projection
    n_periods <- years / interval
    n_ages <- length(inputs$age_groups)
    
    male <- matrix(0, nrow = n_ages, ncol = n_periods + 1)
    female <- matrix(0, nrow = n_ages, ncol = n_periods + 1)
    
    male[, 1] <- inputs$pop_male
    female[, 1] <- inputs$pop_female
    
    for (t in 1:n_periods) {
      proj <- cohort_component_step(
        male[, t], female[, t],
        modified_inputs$survival_male, modified_inputs$survival_female,
        modified_inputs$fertility,
        modified_inputs$migration_male, modified_inputs$migration_female,
        sex_ratio_birth, interval
      )
      male[, t + 1] <- proj$male
      female[, t + 1] <- proj$female
    }
    
    scenario_results[[scenario_name]] <- list(
      male = male,
      female = female,
      total = male + female,
      summary = data.frame(
        year = seq(0, years, by = interval),
        total = colSums(male + female)
      )
    )
  }
  
  return(scenario_results)
}

#' Leslie Matrix Construction
#'
#' Construct Leslie matrix for population projection
#'
#' @param survival Survival probabilities by age
#' @param fertility Fertility rates by age
#' @param sex Sex for which to construct matrix
#' @param sex_ratio_birth Sex ratio at birth (for female matrix)
#'
#' @return Leslie matrix
#' @export
leslie_matrix <- function(survival, fertility, sex = "female", sex_ratio_birth = 105) {
  
  n <- length(survival)
  L <- matrix(0, nrow = n, ncol = n)
  
  # Sub-diagonal: survival probabilities
  for (i in 2:n) {
    L[i, i-1] <- survival[i-1]
  }
  
  # Last diagonal element (open age group)
  L[n, n] <- survival[n]
  
  # First row: fertility (for females)
  if (sex == "female") {
    prop_female <- 100 / (100 + sex_ratio_birth)
    L[1, ] <- fertility * prop_female * survival[1]
  } else {
    prop_male <- sex_ratio_birth / (100 + sex_ratio_birth)
    L[1, ] <- fertility * prop_male * survival[1]
  }
  
  return(L)
}

#' Intrinsic Growth Rate
#'
#' Calculate intrinsic rate of natural increase from Leslie matrix
#'
#' @param leslie Leslie matrix
#'
#' @return Intrinsic growth rate (r)
#' @export
intrinsic_growth_rate <- function(leslie) {
  # Dominant eigenvalue
  eigenvalues <- eigen(leslie)$values
  lambda <- max(Re(eigenvalues))
  
  # r = ln(lambda) / interval
  # Assuming 5-year interval
  r <- log(lambda) / 5
  
  return(r)
}

#' Stable Age Distribution
#'
#' Calculate stable age distribution from Leslie matrix
#'
#' @param leslie Leslie matrix
#'
#' @return Stable age distribution (proportions)
#' @export
stable_age_distribution <- function(leslie) {
  # Right eigenvector of dominant eigenvalue
  eigen_result <- eigen(leslie)
  dominant_idx <- which.max(Re(eigen_result$values))
  
  stable <- Re(eigen_result$vectors[, dominant_idx])
  stable <- stable / sum(stable)
  
  return(stable)
}

#' Print method for population_projection
#' @export
print.population_projection <- function(x, ...) {
  cat("\n========================================\n")
  cat("     POPULATION PROJECTION RESULTS\n")
  cat("========================================\n\n")
  
  cat("Projection parameters:\n")
  cat("  Projection period:", x$parameters$years, "years\n")
  cat("  Interval:", x$parameters$interval, "years\n")
  cat("  Sex ratio at birth:", x$parameters$sex_ratio_birth, "\n\n")
  
  cat("Population Summary:\n")
  print(x$summary[, c("year", "total", "growth_rate", "dependency_ratio")], 
        row.names = FALSE, digits = 1)
  
  if (!is.null(x$scenarios)) {
    cat("\nScenario Comparisons (final year):\n")
    for (name in names(x$scenarios)) {
      final_pop <- tail(x$scenarios[[name]]$summary$total, 1)
      cat("  ", name, ":", format(round(final_pop), big.mark = ","), "\n")
    }
  }
}

#' Plot method for population_projection
#' @export
plot.population_projection <- function(x, type = "total", ...) {
  
  if (type == "total") {
    plot(x$summary$year, x$summary$total / 1e6,
         type = "l", lwd = 2, col = "blue",
         xlab = "Year", ylab = "Population (millions)",
         main = "Population Projection")
    
    if (!is.null(x$scenarios)) {
      colors <- c("red", "green", "orange", "purple")
      i <- 1
      for (name in names(x$scenarios)) {
        lines(x$scenarios[[name]]$summary$year,
              x$scenarios[[name]]$summary$total / 1e6,
              col = colors[i], lwd = 2, lty = 2)
        i <- i + 1
      }
      legend("topleft", c("Base", names(x$scenarios)),
             col = c("blue", colors[1:length(x$scenarios)]),
             lty = c(1, rep(2, length(x$scenarios))), lwd = 2)
    }
  } else if (type == "pyramid") {
    # Plot final year pyramid
    # Implementation would go here
  }
}
