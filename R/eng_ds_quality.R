#' @title Data Quality Evaluation Functions (bcstats style)
#' @description Functions for evaluating data quality inspired by Stata's bcstats
#' @name quality
NULL

#' Back Check Statistics (bcstats)
#'
#' Performs back-check analysis comparing survey data with verification data,
#' inspired by Stata's bcstats command
#'
#' @param survey_data Data frame with original survey data
#' @param backcheck_data Data frame with back-check/verification data
#' @param id_var Variable name for unique identifier
#' @param enum_var Variable name for enumerator ID (optional)
#' @param type1_vars Character vector of Type 1 variables (should not change)
#' @param type2_vars Character vector of Type 2 variables (may change slightly)
#' @param type3_vars Character vector of Type 3 variables (can change)
#' @param okrange Named list of acceptable ranges for numeric variables
#' @param ttest Logical, perform t-tests for numeric variables
#' @param reliability Logical, compute reliability statistics
#'
#' @return A bcstats_result object containing comparison statistics
#' @export
#'
#' @examples
#' \dontrun{
#' result <- bcstats(
#'   survey_data = survey,
#'   backcheck_data = backcheck,
#'   id_var = "hhid",
#'   enum_var = "enumerator",
#'   type1_vars = c("sexe", "lien_cm"),
#'   type2_vars = c("age", "niveau_instruction"),
#'   type3_vars = c("occupation", "revenu")
#' )
#' }
bcstats <- function(survey_data, backcheck_data,
                    id_var,
                    enum_var = NULL,
                    type1_vars = NULL,
                    type2_vars = NULL,
                    type3_vars = NULL,
                    okrange = NULL,
                    ttest = TRUE,
                    reliability = TRUE) {
  
  # Validate inputs
  stopifnot(is.data.frame(survey_data), is.data.frame(backcheck_data))
  stopifnot(id_var %in% names(survey_data), id_var %in% names(backcheck_data))
  
  # Merge datasets
  all_vars <- c(type1_vars, type2_vars, type3_vars)
  
  # Check variables exist
  missing_survey <- setdiff(all_vars, names(survey_data))
  missing_bc <- setdiff(all_vars, names(backcheck_data))
  
  if (length(missing_survey) > 0) {
    warning("Variables missing in survey data: ", paste(missing_survey, collapse = ", "))
    all_vars <- setdiff(all_vars, missing_survey)
  }
  if (length(missing_bc) > 0) {
    warning("Variables missing in backcheck data: ", paste(missing_bc, collapse = ", "))
    all_vars <- setdiff(all_vars, missing_bc)
  }
  
  # Select relevant columns
  survey_subset <- survey_data[, c(id_var, enum_var, all_vars), drop = FALSE]
  bc_subset <- backcheck_data[, c(id_var, all_vars), drop = FALSE]
  
  # Rename backcheck columns
  names(bc_subset)[-1] <- paste0(names(bc_subset)[-1], "_bc")
  
  # Merge
  merged <- merge(survey_subset, bc_subset, by = id_var, all = FALSE)
  
  if (nrow(merged) == 0) {
    stop("No matching records found between survey and backcheck data")
  }
  
  # Initialize results
  results <- list(
    n_compared = nrow(merged),
    n_survey = nrow(survey_data),
    n_backcheck = nrow(backcheck_data),
    variable_stats = list(),
    type1_errors = data.frame(),
    type2_errors = data.frame(),
    type3_errors = data.frame(),
    enumerator_stats = NULL,
    overall_error_rate = 0
  )
  
  # Analyze each variable
  all_errors <- data.frame()
  
  for (var in all_vars) {
    var_bc <- paste0(var, "_bc")
    
    if (!var_bc %in% names(merged)) next
    
    survey_vals <- merged[[var]]
    bc_vals <- merged[[var_bc]]
    
    # Compute comparison statistics
    var_stats <- compute_variable_stats(survey_vals, bc_vals, var, okrange[[var]])
    results$variable_stats[[var]] <- var_stats
    
    # Identify discrepancies
    discrepancies <- identify_discrepancies(merged, var, var_bc, id_var, enum_var)
    
    if (nrow(discrepancies) > 0) {
      discrepancies$variable <- var
      
      # Classify by type
      if (var %in% type1_vars) {
        discrepancies$error_type <- "Type 1 (Critical)"
        results$type1_errors <- rbind(results$type1_errors, discrepancies)
      } else if (var %in% type2_vars) {
        discrepancies$error_type <- "Type 2 (Moderate)"
        results$type2_errors <- rbind(results$type2_errors, discrepancies)
      } else {
        discrepancies$error_type <- "Type 3 (Minor)"
        results$type3_errors <- rbind(results$type3_errors, discrepancies)
      }
      
      all_errors <- rbind(all_errors, discrepancies)
    }
  }
  
  # Compute overall error rate
  total_comparisons <- nrow(merged) * length(all_vars)
  total_errors <- nrow(all_errors)
  results$overall_error_rate <- total_errors / total_comparisons * 100
  
  # Compute error rates by type
  results$type1_error_rate <- nrow(results$type1_errors) / 
    (nrow(merged) * length(type1_vars)) * 100
  results$type2_error_rate <- nrow(results$type2_errors) / 
    (nrow(merged) * length(type2_vars)) * 100
  results$type3_error_rate <- nrow(results$type3_errors) / 
    (nrow(merged) * length(type3_vars)) * 100
  
  # Enumerator-level statistics
  if (!is.null(enum_var) && enum_var %in% names(merged)) {
    results$enumerator_stats <- compute_enumerator_stats(merged, all_errors, enum_var)
  }
  
  # Reliability statistics
  if (reliability) {
    results$reliability <- compute_reliability(results$variable_stats)
  }
  
  # T-tests for numeric variables
  if (ttest) {
    results$ttests <- list()
    for (var in all_vars) {
      var_bc <- paste0(var, "_bc")
      if (is.numeric(merged[[var]]) && is.numeric(merged[[var_bc]])) {
        results$ttests[[var]] <- t.test(merged[[var]], merged[[var_bc]], paired = TRUE)
      }
    }
  }
  
  class(results) <- c("bcstats_result", class(results))
  return(results)
}

#' Compute Variable Statistics
#' @keywords internal
compute_variable_stats <- function(survey_vals, bc_vals, var_name, okrange = NULL) {
  
  n <- length(survey_vals)
  
  # Handle different variable types
  if (is.numeric(survey_vals) && is.numeric(bc_vals)) {
    # Numeric variable
    exact_match <- sum(survey_vals == bc_vals, na.rm = TRUE)
    
    # Check within acceptable range
    if (!is.null(okrange)) {
      diff <- abs(survey_vals - bc_vals)
      within_range <- sum(diff <= okrange, na.rm = TRUE)
    } else {
      within_range <- exact_match
    }
    
    # Compute differences
    differences <- survey_vals - bc_vals
    
    stats <- list(
      variable = var_name,
      type = "numeric",
      n = n,
      n_missing_survey = sum(is.na(survey_vals)),
      n_missing_bc = sum(is.na(bc_vals)),
      exact_match = exact_match,
      exact_match_rate = exact_match / n * 100,
      within_range = within_range,
      within_range_rate = within_range / n * 100,
      mean_diff = mean(differences, na.rm = TRUE),
      sd_diff = sd(differences, na.rm = TRUE),
      median_diff = median(differences, na.rm = TRUE),
      mean_survey = mean(survey_vals, na.rm = TRUE),
      mean_bc = mean(bc_vals, na.rm = TRUE)
    )
    
  } else {
    # Categorical variable
    survey_vals <- as.character(survey_vals)
    bc_vals <- as.character(bc_vals)
    
    exact_match <- sum(survey_vals == bc_vals, na.rm = TRUE)

    # Cohen's Kappa (accord au-dela du hasard) sur les paires completes
    kp <- cohen_kappa(survey_vals, bc_vals)

    stats <- list(
      variable = var_name,
      type = "categorical",
      n = n,
      n_missing_survey = sum(is.na(survey_vals) | survey_vals == ""),
      n_missing_bc = sum(is.na(bc_vals) | bc_vals == ""),
      exact_match = exact_match,
      exact_match_rate = exact_match / n * 100,
      concordance_rate = exact_match / n * 100,
      kappa = kp$kappa,
      kappa_interpretation = kp$interpretation,
      n_categories_survey = length(unique(survey_vals[!is.na(survey_vals)])),
      n_categories_bc = length(unique(bc_vals[!is.na(bc_vals)]))
    )
  }
  
  return(stats)
}

#' Coefficient Kappa de Cohen entre deux series categorielles appariees
#'
#' @param a,b Vecteurs de meme longueur (saisie initiale et controle).
#' @return Liste : kappa, po (accord observe), pe (accord attendu),
#'   interpretation (echelle de Landis & Koch).
#' @keywords internal
cohen_kappa <- function(a, b) {
  a <- as.character(a); b <- as.character(b)
  ok <- !(is.na(a) | is.na(b) | a == "" | b == "")
  a <- a[ok]; b <- b[ok]
  n <- length(a)
  if (n == 0) return(list(kappa = NA_real_, po = NA_real_, pe = NA_real_,
                          interpretation = "indeterminee (aucune paire complete)"))
  lv <- sort(unique(c(a, b)))
  if (length(lv) < 2) {
    # Une seule modalite : accord parfait par construction, kappa indefini
    po <- mean(a == b)
    return(list(kappa = NA_real_, po = po, pe = NA_real_,
                interpretation = "indeterminee (une seule modalite)"))
  }
  af <- factor(a, levels = lv); bf <- factor(b, levels = lv)
  ct <- table(af, bf)
  po <- sum(diag(ct)) / n
  rs <- rowSums(ct) / n; cs <- colSums(ct) / n
  pe <- sum(rs * cs)
  kappa <- if (abs(1 - pe) < 1e-12) NA_real_ else (po - pe) / (1 - pe)
  interp <- if (is.na(kappa)) "indeterminee" else
    if (kappa < 0)    "desaccord (kappa negatif)" else
    if (kappa < 0.20) "accord tres faible" else
    if (kappa < 0.40) "accord faible" else
    if (kappa < 0.60) "accord modere" else
    if (kappa < 0.80) "accord substantiel" else
                      "accord presque parfait"
  list(kappa = round(kappa, 3), po = round(po, 4), pe = round(pe, 4),
       interpretation = interp)
}

#' Identify Discrepancies
#' @keywords internal
identify_discrepancies <- function(data, var, var_bc, id_var, enum_var) {
  
  survey_vals <- data[[var]]
  bc_vals <- data[[var_bc]]
  
  # Find non-matching records
  if (is.numeric(survey_vals)) {
    mismatch <- which(survey_vals != bc_vals & !is.na(survey_vals) & !is.na(bc_vals))
  } else {
    survey_vals <- as.character(survey_vals)
    bc_vals <- as.character(bc_vals)
    mismatch <- which(survey_vals != bc_vals & 
                        !is.na(survey_vals) & !is.na(bc_vals) &
                        survey_vals != "" & bc_vals != "")
  }
  
  if (length(mismatch) == 0) {
    return(data.frame())
  }
  
  result <- data.frame(
    id = data[[id_var]][mismatch],
    survey_value = survey_vals[mismatch],
    backcheck_value = bc_vals[mismatch],
    stringsAsFactors = FALSE
  )
  
  if (!is.null(enum_var) && enum_var %in% names(data)) {
    result$enumerator <- data[[enum_var]][mismatch]
  }
  
  return(result)
}

#' Compute Enumerator Statistics
#' @keywords internal
compute_enumerator_stats <- function(merged, all_errors, enum_var) {
  
  # Count interviews per enumerator
  enum_counts <- table(merged[[enum_var]])
  
  # Count errors per enumerator
  if (nrow(all_errors) > 0 && "enumerator" %in% names(all_errors)) {
    error_counts <- table(all_errors$enumerator)
  } else {
    error_counts <- setNames(rep(0, length(enum_counts)), names(enum_counts))
  }
  
  # Create summary
  enums <- names(enum_counts)
  
  data.frame(
    enumerator = enums,
    n_interviews = as.numeric(enum_counts[enums]),
    n_errors = as.numeric(error_counts[enums]),
    error_rate = as.numeric(error_counts[enums]) / as.numeric(enum_counts[enums]) * 100,
    stringsAsFactors = FALSE
  )
}

#' Compute Reliability Statistics
#' @keywords internal
compute_reliability <- function(variable_stats) {
  
  # Extract match rates
  match_rates <- sapply(variable_stats, function(x) x$exact_match_rate)
  
  list(
    mean_reliability = mean(match_rates, na.rm = TRUE),
    min_reliability = min(match_rates, na.rm = TRUE),
    max_reliability = max(match_rates, na.rm = TRUE),
    variables_below_90 = names(match_rates)[match_rates < 90],
    variables_below_80 = names(match_rates)[match_rates < 80]
  )
}

#' Print method for bcstats_result
#' @export
print.bcstats_result <- function(x, ...) {
  cat("\n========================================\n")
  cat("       BACK CHECK STATISTICS (bcstats)\n")
  cat("========================================\n\n")
  
  cat("Sample Information:\n")
  cat("  Survey records:", x$n_survey, "\n")
  cat("  Backcheck records:", x$n_backcheck, "\n")
  cat("  Matched for comparison:", x$n_compared, "\n\n")
  
  cat("Error Rates:\n")
  cat("  Overall error rate:", sprintf("%.2f%%", x$overall_error_rate), "\n")
  cat("  Type 1 (Critical):", sprintf("%.2f%%", x$type1_error_rate), 
      sprintf("(%d errors)", nrow(x$type1_errors)), "\n")
  cat("  Type 2 (Moderate):", sprintf("%.2f%%", x$type2_error_rate),
      sprintf("(%d errors)", nrow(x$type2_errors)), "\n")
  cat("  Type 3 (Minor):", sprintf("%.2f%%", x$type3_error_rate),
      sprintf("(%d errors)", nrow(x$type3_errors)), "\n\n")
  
  if (!is.null(x$reliability)) {
    cat("Reliability:\n")
    cat("  Mean reliability:", sprintf("%.1f%%", x$reliability$mean_reliability), "\n")
    if (length(x$reliability$variables_below_80) > 0) {
      cat("  Variables below 80%:", paste(x$reliability$variables_below_80, collapse = ", "), "\n")
    }
  }
  
  cat("\n")
}

#' Quality Check Function
#'
#' Comprehensive data quality assessment
#'
#' @param data Data frame to check
#' @param config Quality check configuration
#'
#' @return Quality check results
#' @export
quality_check <- function(data, config = list()) {
  
  results <- list(
    n_records = nrow(data),
    n_variables = ncol(data),
    completeness = completeness_check(data),
    consistency = consistency_check(data, config$rules),
    outliers = outlier_detection(data, config$numeric_vars),
    duplicates = find_duplicates(data, config$id_vars)
  )
  
  # Overall quality score
  results$quality_score <- compute_quality_score(results)
  
  class(results) <- c("quality_check_result", class(results))
  return(results)
}

#' Completeness Check
#'
#' Check for missing values and completeness
#'
#' @param data Data frame
#'
#' @return Completeness statistics
#' @export
completeness_check <- function(data) {
  
  # Per variable
  var_completeness <- sapply(data, function(x) {
    n_missing <- sum(is.na(x) | x == "" | x == ".")
    100 - (n_missing / length(x) * 100)
  })
  
  # Per record
  record_completeness <- apply(data, 1, function(x) {
    n_missing <- sum(is.na(x) | x == "" | x == ".")
    100 - (n_missing / length(x) * 100)
  })
  
  list(
    overall = mean(var_completeness),
    by_variable = sort(var_completeness),
    variables_below_90 = names(var_completeness)[var_completeness < 90],
    variables_below_80 = names(var_completeness)[var_completeness < 80],
    records_complete = sum(record_completeness == 100),
    records_incomplete = sum(record_completeness < 100),
    mean_record_completeness = mean(record_completeness)
  )
}

#' Consistency Check
#'
#' Check logical consistency rules
#'
#' @param data Data frame
#' @param rules List of consistency rules
#'
#' @return Consistency check results
#' @export
consistency_check <- function(data, rules = NULL) {
  
  # Default demographic consistency rules
  default_rules <- list(
    # Age consistency
    age_positive = list(
      condition = "age >= 0",
      description = "Age must be non-negative"
    ),
    age_reasonable = list(
      condition = "age <= 120",
      description = "Age must be <= 120"
    ),
    # Mother's age at birth
    mother_age = list(
      condition = "is.na(age_mere) | (age_mere >= 12 & age_mere <= 55)",
      description = "Mother's age at birth must be 12-55"
    ),
    # Sex coding
    sex_valid = list(
      condition = "sexe %in% c(1, 2, 'M', 'F', 'H', 'Masculin', 'Feminin')",
      description = "Sex must be valid code"
    )
  )
  
  # Merge with user rules
  if (!is.null(rules)) {
    rules <- c(default_rules, rules)
  } else {
    rules <- default_rules
  }
  
  # Check each rule
  results <- list()
  
  for (rule_name in names(rules)) {
    rule <- rules[[rule_name]]
    
    # Check if required variables exist
    tryCatch({
      violations <- with(data, !eval(parse(text = rule$condition)))
      violations[is.na(violations)] <- FALSE
      
      results[[rule_name]] <- list(
        description = rule$description,
        n_violations = sum(violations),
        violation_rate = sum(violations) / nrow(data) * 100,
        violation_indices = which(violations)
      )
    }, error = function(e) {
      results[[rule_name]] <<- list(
        description = rule$description,
        error = "Could not evaluate rule - variables may be missing"
      )
    })
  }
  
  # Summary
  total_violations <- sum(sapply(results, function(x) {
    if (!is.null(x$n_violations)) x$n_violations else 0
  }))
  
  list(
    rules_checked = length(results),
    total_violations = total_violations,
    results = results
  )
}

#' Outlier Detection
#'
#' Detect outliers in numeric variables
#'
#' @param data Data frame
#' @param numeric_vars Variables to check (NULL = all numeric)
#' @param method Method: "iqr", "zscore", or "both"
#' @param threshold Threshold for outlier detection
#'
#' @return Outlier detection results
#' @export
outlier_detection <- function(data, numeric_vars = NULL, method = "both", threshold = 3) {
  
  # Get numeric variables
  if (is.null(numeric_vars)) {
    numeric_vars <- names(data)[sapply(data, is.numeric)]
  }
  
  results <- list()
  
  for (var in numeric_vars) {
    if (!var %in% names(data)) next
    
    x <- data[[var]]
    x <- x[!is.na(x)]
    
    if (length(x) < 10) next
    
    outliers_iqr <- c()
    outliers_zscore <- c()
    
    # IQR method
    if (method %in% c("iqr", "both")) {
      q1 <- quantile(x, 0.25)
      q3 <- quantile(x, 0.75)
      iqr <- q3 - q1
      lower <- q1 - 1.5 * iqr
      upper <- q3 + 1.5 * iqr
      outliers_iqr <- which(data[[var]] < lower | data[[var]] > upper)
    }
    
    # Z-score method
    if (method %in% c("zscore", "both")) {
      z <- (data[[var]] - mean(x)) / sd(x)
      outliers_zscore <- which(abs(z) > threshold)
    }
    
    # Combine
    all_outliers <- unique(c(outliers_iqr, outliers_zscore))
    
    results[[var]] <- list(
      n_outliers = length(all_outliers),
      outlier_rate = length(all_outliers) / nrow(data) * 100,
      outlier_indices = all_outliers,
      min = min(x),
      max = max(x),
      mean = mean(x),
      sd = sd(x)
    )
  }
  
  list(
    variables_checked = length(results),
    total_outliers = sum(sapply(results, function(x) x$n_outliers)),
    by_variable = results
  )
}

#' Find Duplicates
#' @keywords internal
find_duplicates <- function(data, id_vars = NULL) {
  
  if (is.null(id_vars)) {
    # Use all columns to find exact duplicates
    dups <- duplicated(data) | duplicated(data, fromLast = TRUE)
  } else {
    # Find duplicates based on ID variables
    id_data <- data[, id_vars, drop = FALSE]
    dups <- duplicated(id_data) | duplicated(id_data, fromLast = TRUE)
  }
  
  list(
    n_duplicates = sum(dups),
    duplicate_rate = sum(dups) / nrow(data) * 100,
    duplicate_indices = which(dups)
  )
}

#' Compute Quality Score
#' @keywords internal
compute_quality_score <- function(results) {
  
  # Weighted score based on different aspects
  completeness_score <- results$completeness$overall
  
  consistency_score <- 100 - min(100, results$consistency$total_violations / 
                                   results$n_records * 100)
  
  outlier_score <- 100 - min(100, results$outliers$total_outliers / 
                               results$n_records * 10)
  
  duplicate_score <- 100 - results$duplicates$duplicate_rate
  
  # Weighted average
  score <- 0.4 * completeness_score + 
    0.3 * consistency_score + 
    0.2 * outlier_score + 
    0.1 * duplicate_score
  
  list(
    overall = score,
    completeness = completeness_score,
    consistency = consistency_score,
    outliers = outlier_score,
    duplicates = duplicate_score,
    grade = ifelse(score >= 90, "A",
                   ifelse(score >= 80, "B",
                          ifelse(score >= 70, "C",
                                 ifelse(score >= 60, "D", "F"))))
  )
}

#' Print method for quality_check_result
#' @export
print.quality_check_result <- function(x, ...) {
  cat("\n========================================\n")
  cat("       DATA QUALITY ASSESSMENT\n")
  cat("========================================\n\n")
  
  cat("Dataset: ", x$n_records, " records, ", x$n_variables, " variables\n\n")
  
  cat("Quality Score:", sprintf("%.1f", x$quality_score$overall), 
      paste0("(Grade: ", x$quality_score$grade, ")"), "\n\n")
  
  cat("Component Scores:\n")
  cat("  Completeness:", sprintf("%.1f%%", x$quality_score$completeness), "\n")
  cat("  Consistency:", sprintf("%.1f%%", x$quality_score$consistency), "\n")
  cat("  Outliers:", sprintf("%.1f%%", x$quality_score$outliers), "\n")
  cat("  Duplicates:", sprintf("%.1f%%", x$quality_score$duplicates), "\n\n")
  
  if (length(x$completeness$variables_below_80) > 0) {
    cat("Variables with <80% completeness:\n")
    cat("  ", paste(x$completeness$variables_below_80, collapse = ", "), "\n\n")
  }
  
  cat("Consistency violations:", x$consistency$total_violations, "\n")
  cat("Outliers detected:", x$outliers$total_outliers, "\n")
  cat("Duplicate records:", x$duplicates$n_duplicates, "\n")
}
