#' @title Dual System Estimation Functions
#' @description Functions for computing coverage estimates using DSE methodology
#' @name dse
NULL

#' Dual System Estimation
#'
#' Estimate true population using the Chandrasekaran-Deming estimator
#'
#' @param match_result Result from pes_match or manual data
#' @param data Data frame with matching results (alternative to match_result)
#' @param strata_vars Variables for stratified estimation
#' @param weights Survey weights (optional)
#'
#' @return DSE estimation results
#' @export
#'
#' @examples
#' \dontrun{
#' dse_result <- dse_estimate(match_result, strata_vars = c("sexe", "age_group"))
#' }
dse_estimate <- function(match_result = NULL, data = NULL, 
                          strata_vars = NULL, weights = NULL) {
  
  # Get data from match_result or direct input
  if (!is.null(match_result)) {
    # Extract components from match result
    n_pes <- match_result$statistics$n_pes_total
    n_census <- match_result$statistics$n_census_total
    n_matched <- match_result$statistics$n_matched
    n_erroneous <- match_result$statistics$n_unmatched_census
    
    if (!is.null(strata_vars) && nrow(match_result$matched) > 0) {
      # Stratified estimation
      return(dse_stratified(match_result, strata_vars))
    }
    
  } else if (!is.null(data)) {
    # Direct data input - expect columns: in_pes, in_census, matched
    n_pes <- sum(data$in_pes, na.rm = TRUE)
    n_census <- sum(data$in_census, na.rm = TRUE)
    n_matched <- sum(data$matched, na.rm = TRUE)
    n_erroneous <- sum(data$in_census & !data$matched, na.rm = TRUE)
  } else {
    stop("Either match_result or data must be provided")
  }
  
  # Basic DSE calculation
  result <- dse_calculate(n_pes, n_census, n_matched, n_erroneous)
  
  class(result) <- c("dse_result", class(result))
  return(result)
}

#' Core DSE Calculation
#'
#' @param n_pes Number of persons in PES
#' @param n_census Number of persons in census
#' @param n_matched Number of matched persons
#' @param n_erroneous Number of erroneous inclusions
#'
#' @return List of DSE estimates
#' @keywords internal
dse_calculate <- function(n_pes, n_census, n_matched, n_erroneous) {
  
  # Census population (corrected for erroneous inclusions)
  census_corrected <- n_census - n_erroneous
  
  # True population estimate (Chandrasekaran-Deming)
  # N_hat = N_pes * (N_census - E) / N_matched
  if (n_matched > 0) {
    true_pop <- n_pes * census_corrected / n_matched
  } else {
    true_pop <- NA
    warning("No matched records - cannot estimate true population")
  }
  
  # Coverage rate (match rate)
  coverage_rate <- n_matched / n_pes * 100
  
  # Net coverage error
  net_error <- true_pop - n_census
  net_error_rate <- net_error / true_pop * 100
  
 # Omissions
  omissions <- true_pop - n_census + n_erroneous
  omission_rate <- omissions / true_pop * 100
  
  # Erroneous inclusion rate
  erroneous_rate <- n_erroneous / n_census * 100
  
  # Gross coverage error
  gross_error <- omissions + n_erroneous
  gross_error_rate <- gross_error / n_census * 100
  
  list(
    # Input values
    n_pes = n_pes,
    n_census = n_census,
    n_matched = n_matched,
    n_erroneous = n_erroneous,
    
    # Estimates
    true_population = true_pop,
    census_corrected = census_corrected,
    
    # Coverage measures
    coverage_rate = coverage_rate,
    
    # Error measures
    net_coverage_error = net_error,
    net_coverage_error_rate = net_error_rate,
    omissions = omissions,
    omission_rate = omission_rate,
    erroneous_inclusions = n_erroneous,
    erroneous_inclusion_rate = erroneous_rate,
    gross_coverage_error = gross_error,
    gross_coverage_error_rate = gross_error_rate
  )
}

#' Stratified DSE Estimation
#' @keywords internal
dse_stratified <- function(match_result, strata_vars) {
  
  matched_data <- match_result$matched
  unmatched_pes <- match_result$unmatched_pes
  unmatched_census <- match_result$unmatched_census
  
  # Create strata key
  create_strata_key <- function(df, vars) {
    if (nrow(df) == 0) return(character(0))
    apply(df[, vars, drop = FALSE], 1, paste, collapse = "_")
  }
  
  # Get all strata
  all_strata <- unique(c(
    create_strata_key(matched_data, strata_vars),
    create_strata_key(unmatched_pes, strata_vars),
    create_strata_key(unmatched_census, strata_vars)
  ))
  
  # Calculate DSE for each stratum
  strata_results <- list()
  
  for (stratum in all_strata) {
    # Count in each source
    n_matched <- sum(create_strata_key(matched_data, strata_vars) == stratum)
    n_unmatched_pes <- sum(create_strata_key(unmatched_pes, strata_vars) == stratum)
    n_unmatched_census <- sum(create_strata_key(unmatched_census, strata_vars) == stratum)
    
    n_pes <- n_matched + n_unmatched_pes
    n_census <- n_matched + n_unmatched_census
    n_erroneous <- n_unmatched_census
    
    strata_results[[stratum]] <- dse_calculate(n_pes, n_census, n_matched, n_erroneous)
    strata_results[[stratum]]$stratum <- stratum
  }
  
  # Aggregate results
  total_true_pop <- sum(sapply(strata_results, function(x) x$true_population), na.rm = TRUE)
  total_census <- sum(sapply(strata_results, function(x) x$n_census))
  total_omissions <- sum(sapply(strata_results, function(x) x$omissions), na.rm = TRUE)
  total_erroneous <- sum(sapply(strata_results, function(x) x$n_erroneous))
  
  # Create summary table
  summary_table <- do.call(rbind, lapply(strata_results, function(x) {
    data.frame(
      stratum = x$stratum,
      n_pes = x$n_pes,
      n_census = x$n_census,
      n_matched = x$n_matched,
      true_population = round(x$true_population),
      coverage_rate = round(x$coverage_rate, 1),
      omission_rate = round(x$omission_rate, 1),
      erroneous_rate = round(x$erroneous_inclusion_rate, 1),
      stringsAsFactors = FALSE
    )
  }))
  
  list(
    by_strata = strata_results,
    summary_table = summary_table,
    total = list(
      true_population = total_true_pop,
      census_population = total_census,
      net_coverage_error = total_true_pop - total_census,
      net_coverage_error_rate = (total_true_pop - total_census) / total_true_pop * 100,
      omissions = total_omissions,
      omission_rate = total_omissions / total_true_pop * 100,
      erroneous_inclusions = total_erroneous,
      erroneous_inclusion_rate = total_erroneous / total_census * 100
    ),
    strata_vars = strata_vars
  )
}

#' Coverage Rate Calculation
#'
#' @param n_matched Number of matched records
#' @param n_pes Total PES records
#'
#' @return Coverage rate as percentage
#' @export
coverage_rate <- function(n_matched, n_pes) {
  n_matched / n_pes * 100
}

#' Omission Rate Calculation
#'
#' @param omissions Number of omissions
#' @param true_pop True population estimate
#'
#' @return Omission rate as percentage
#' @export
omission_rate <- function(omissions, true_pop) {
  omissions / true_pop * 100
}

#' Content Error Analysis
#'
#' Analyze content errors between matched PES and Census records
#'
#' @param matched_data Data frame of matched records
#' @param variables Variables to analyze
#' @param pes_suffix Suffix for PES variables
#' @param census_suffix Suffix for Census variables
#'
#' @return Content error analysis results
#' @export
content_error <- function(matched_data, variables, 
                           pes_suffix = "_pes", census_suffix = "_census") {
  
  results <- list()
  
  for (var in variables) {
    var_pes <- paste0(var, pes_suffix)
    var_census <- paste0(var, census_suffix)
    
    if (!var_pes %in% names(matched_data) || !var_census %in% names(matched_data)) {
      next
    }
    
    pes_vals <- matched_data[[var_pes]]
    census_vals <- matched_data[[var_census]]
    n <- length(pes_vals)
    
    # Create confusion matrix
    if (is.numeric(pes_vals)) {
      # For numeric variables, create age groups or bins
      results[[var]] <- content_error_numeric(pes_vals, census_vals, var)
    } else {
      # For categorical variables
      results[[var]] <- content_error_categorical(pes_vals, census_vals, var)
    }
  }
  
  # Aggregate statistics
  ndr_values <- sapply(results, function(x) x$net_difference_rate)
  agreement_values <- sapply(results, function(x) x$agreement_rate)
  
  list(
    by_variable = results,
    summary = data.frame(
      variable = names(results),
      agreement_rate = round(agreement_values, 1),
      net_difference_rate = round(ndr_values, 2),
      stringsAsFactors = FALSE
    ),
    mean_agreement = mean(agreement_values, na.rm = TRUE),
    variables_below_90 = names(agreement_values)[agreement_values < 90]
  )
}

#' Content Error for Numeric Variables
#' @keywords internal
content_error_numeric <- function(pes_vals, census_vals, var_name) {
  
  n <- length(pes_vals)
  valid <- !is.na(pes_vals) & !is.na(census_vals)
  
  pes_valid <- pes_vals[valid]
  census_valid <- census_vals[valid]
  n_valid <- sum(valid)
  
  # Exact agreement
  exact_match <- sum(pes_valid == census_valid)
  
  # Within tolerance (e.g., ±1 for age)
  within_1 <- sum(abs(pes_valid - census_valid) <= 1)
  within_2 <- sum(abs(pes_valid - census_valid) <= 2)
  
  # Mean difference
  mean_diff <- mean(census_valid - pes_valid)
  
  # Net difference rate
  ndr <- mean_diff / mean(c(mean(pes_valid), mean(census_valid))) * 100
  
  list(
    variable = var_name,
    type = "numeric",
    n = n_valid,
    exact_match = exact_match,
    agreement_rate = exact_match / n_valid * 100,
    within_1 = within_1,
    within_1_rate = within_1 / n_valid * 100,
    within_2 = within_2,
    within_2_rate = within_2 / n_valid * 100,
    mean_difference = mean_diff,
    net_difference_rate = ndr,
    mean_pes = mean(pes_valid),
    mean_census = mean(census_valid)
  )
}

#' Content Error for Categorical Variables
#' @keywords internal
content_error_categorical <- function(pes_vals, census_vals, var_name) {
  
  pes_vals <- as.character(pes_vals)
  census_vals <- as.character(census_vals)
  
  valid <- !is.na(pes_vals) & !is.na(census_vals) & 
    pes_vals != "" & census_vals != ""
  
  pes_valid <- pes_vals[valid]
  census_valid <- census_vals[valid]
  n_valid <- sum(valid)
  
  # Confusion matrix
  confusion <- table(Census = census_valid, PES = pes_valid)
  
  # Agreement (diagonal)
  categories <- union(unique(pes_valid), unique(census_valid))
  agreement <- sum(sapply(categories, function(cat) {
    sum(pes_valid == cat & census_valid == cat)
  }))
  
  # Net difference rate for each category
  ndr_by_cat <- sapply(categories, function(cat) {
    n_census <- sum(census_valid == cat)
    n_pes <- sum(pes_valid == cat)
    (n_census - n_pes) / n_valid * 100
  })
  
  # Index of inconsistency
  inconsistency <- compute_inconsistency_index(pes_valid, census_valid, categories)
  
  list(
    variable = var_name,
    type = "categorical",
    n = n_valid,
    agreement = agreement,
    agreement_rate = agreement / n_valid * 100,
    confusion_matrix = confusion,
    net_difference_rate = mean(abs(ndr_by_cat)),
    ndr_by_category = ndr_by_cat,
    index_of_inconsistency = inconsistency,
    gross_difference_rate = (n_valid - agreement) / n_valid * 100
  )
}

#' Compute Index of Inconsistency
#' @keywords internal
compute_inconsistency_index <- function(pes_vals, census_vals, categories) {
  
  n <- length(pes_vals)
  
  indices <- sapply(categories, function(cat) {
    x_census <- sum(census_vals == cat)
    x_pes <- sum(pes_vals == cat)
    x_both <- sum(census_vals == cat & pes_vals == cat)
    
    numerator <- x_census + x_pes - 2 * x_both
    denominator <- (x_census * (n - x_pes) + x_pes * (n - x_census)) / n
    
    if (denominator > 0) {
      numerator / denominator * 100
    } else {
      0
    }
  })
  
  mean(indices, na.rm = TRUE)
}

#' Print method for dse_result
#' @export
print.dse_result <- function(x, ...) {
  cat("\n========================================\n")
  cat("    DUAL SYSTEM ESTIMATION RESULTS\n")
  cat("========================================\n\n")
  
  cat("Input Data:\n")
  cat("  PES population:", format(x$n_pes, big.mark = ","), "\n")
  cat("  Census population:", format(x$n_census, big.mark = ","), "\n")
  cat("  Matched population:", format(x$n_matched, big.mark = ","), "\n")
  cat("  Erroneous inclusions:", format(x$n_erroneous, big.mark = ","), "\n\n")
  
  cat("Estimates:\n")
  cat("  True population:", format(round(x$true_population), big.mark = ","), "\n")
  cat("  Coverage rate:", sprintf("%.1f%%", x$coverage_rate), "\n\n")
  
  cat("Coverage Errors:\n")
  cat("  Net coverage error:", format(round(x$net_coverage_error), big.mark = ","),
      sprintf("(%.1f%%)", x$net_coverage_error_rate), "\n")
  cat("  Omissions:", format(round(x$omissions), big.mark = ","),
      sprintf("(%.1f%%)", x$omission_rate), "\n")
  cat("  Erroneous inclusions:", format(x$n_erroneous, big.mark = ","),
      sprintf("(%.1f%%)", x$erroneous_inclusion_rate), "\n")
  cat("  Gross coverage error:", format(round(x$gross_coverage_error), big.mark = ","),
      sprintf("(%.1f%%)", x$gross_coverage_error_rate), "\n")
}
