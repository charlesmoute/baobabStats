#' @title PES Matching Functions
#' @description Functions for matching Post Enumeration Survey records with Census records
#' @name matching
NULL

#' Automatic PES Matching
#'
#' Performs automatic matching between PES and Census records using multiple criteria
#'
#' @param pes_data Data frame containing PES records
#' @param census_data Data frame containing Census records
#' @param match_vars Character vector of variable names to use for matching
#' @param blocking_vars Character vector of variables to use for blocking (exact match required)
#' @param threshold Numeric threshold for match score (0-1), default 0.8
#' @param method Matching method: "deterministic", "probabilistic", or "hybrid"
#' @param weights Named numeric vector of weights for each match variable
#'
#' @return A list containing matched pairs, unmatched PES, unmatched Census, and match statistics
#' @export
#'
#' @examples
#' \dontrun{
#' result <- pes_match_auto(pes_data, census_data,
#'                          match_vars = c("nom", "prenom", "age", "sexe"),
#'                          blocking_vars = c("ea_code", "sexe"))
#' }
pes_match_auto <- function(pes_data, census_data,
                           match_vars = c("nom", "prenom", "age", "sexe"),
                           blocking_vars = NULL,
                           threshold = 0.8,
                           method = "hybrid",
                           weights = NULL) {
  
  # Validate inputs
  stopifnot(is.data.frame(pes_data), is.data.frame(census_data))
  
  # Check required variables exist
  all_vars <- unique(c(match_vars, blocking_vars))
  missing_pes <- setdiff(all_vars, names(pes_data))
  missing_census <- setdiff(all_vars, names(census_data))
  
  if (length(missing_pes) > 0) {
    stop("Variables missing in PES data: ", paste(missing_pes, collapse = ", "))
  }
  if (length(missing_census) > 0) {
    stop("Variables missing in Census data: ", paste(missing_census, collapse = ", "))
  }
  
  # Add unique IDs if not present
  if (!"pes_id" %in% names(pes_data)) {
    pes_data$pes_id <- seq_len(nrow(pes_data))
  }
  if (!"census_id" %in% names(census_data)) {
    census_data$census_id <- seq_len(nrow(census_data))
  }
  
  # Set default weights
  if (is.null(weights)) {
    weights <- setNames(rep(1, length(match_vars)), match_vars)
    # Higher weight for name variables
    if ("nom" %in% match_vars) weights["nom"] <- 2
    if ("prenom" %in% match_vars) weights["prenom"] <- 1.5
  }
  
  # Normalize weights
  weights <- weights / sum(weights)
  
  # Perform matching based on method
  if (method == "deterministic") {
    result <- deterministic_match(pes_data, census_data, match_vars, blocking_vars)
  } else if (method == "probabilistic") {
    result <- probabilistic_match(pes_data, census_data, match_vars, blocking_vars, 
                                   threshold, weights)
  } else {
    # Hybrid: deterministic first, then probabilistic for remaining
    result <- hybrid_match(pes_data, census_data, match_vars, blocking_vars, 
                           threshold, weights)
  }
  
  # Add match statistics
  result$statistics <- compute_match_statistics(result, nrow(pes_data), nrow(census_data))
  
  return(result)
}

#' Deterministic Matching
#' @keywords internal
deterministic_match <- function(pes_data, census_data, match_vars, blocking_vars) {
  
  # Create match keys
  pes_data$match_key <- apply(pes_data[, match_vars, drop = FALSE], 1, 
                               function(x) paste(tolower(trimws(as.character(x))), collapse = "|"))
  census_data$match_key <- apply(census_data[, match_vars, drop = FALSE], 1,
                                  function(x) paste(tolower(trimws(as.character(x))), collapse = "|"))
  
  # If blocking vars specified, add to key
  if (!is.null(blocking_vars)) {
    pes_data$block_key <- apply(pes_data[, blocking_vars, drop = FALSE], 1,
                                 function(x) paste(as.character(x), collapse = "|"))
    census_data$block_key <- apply(census_data[, blocking_vars, drop = FALSE], 1,
                                    function(x) paste(as.character(x), collapse = "|"))
    pes_data$full_key <- paste(pes_data$block_key, pes_data$match_key, sep = "||")
    census_data$full_key <- paste(census_data$block_key, census_data$match_key, sep = "||")
  } else {
    pes_data$full_key <- pes_data$match_key
    census_data$full_key <- census_data$match_key
  }
  
  # Find exact matches
  matched <- merge(pes_data, census_data, by = "full_key", suffixes = c("_pes", "_census"))
  
  # Get unmatched records
  matched_pes_ids <- unique(matched$pes_id)
  matched_census_ids <- unique(matched$census_id)
  
  unmatched_pes <- pes_data[!pes_data$pes_id %in% matched_pes_ids, ]
  unmatched_census <- census_data[!census_data$census_id %in% matched_census_ids, ]
  
  # Clean up temporary columns
  matched$match_key_pes <- NULL
  matched$match_key_census <- NULL
  matched$block_key_pes <- NULL
  matched$block_key_census <- NULL
  matched$full_key <- NULL
  
  matched$match_score <- 1.0
  matched$match_type <- "deterministic"
  
  return(list(
    matched = matched,
    unmatched_pes = unmatched_pes,
    unmatched_census = unmatched_census,
    method = "deterministic"
  ))
}

#' Probabilistic Matching
#' @keywords internal
probabilistic_match <- function(pes_data, census_data, match_vars, blocking_vars,
                                 threshold, weights) {
  
  matched_pairs <- data.frame()
  
  # Apply blocking if specified
  if (!is.null(blocking_vars)) {
    pes_data$block_key <- apply(pes_data[, blocking_vars, drop = FALSE], 1,
                                 function(x) paste(as.character(x), collapse = "|"))
    census_data$block_key <- apply(census_data[, blocking_vars, drop = FALSE], 1,
                                    function(x) paste(as.character(x), collapse = "|"))
    blocks <- unique(pes_data$block_key)
  } else {
    pes_data$block_key <- "all"
    census_data$block_key <- "all"
    blocks <- "all"
  }
  
  used_census_ids <- c()
  
  for (block in blocks) {
    pes_block <- pes_data[pes_data$block_key == block, ]
    census_block <- census_data[census_data$block_key == block & 
                                  !census_data$census_id %in% used_census_ids, ]
    
    if (nrow(pes_block) == 0 || nrow(census_block) == 0) next
    
    # Compute similarity scores for each pair
    for (i in seq_len(nrow(pes_block))) {
      scores <- numeric(nrow(census_block))
      
      for (j in seq_len(nrow(census_block))) {
        var_scores <- numeric(length(match_vars))
        
        for (k in seq_along(match_vars)) {
          var <- match_vars[k]
          val_pes <- as.character(pes_block[i, var])
          val_census <- as.character(census_block[j, var])
          
          # Compute similarity based on variable type
          if (var %in% c("age", "annee_naissance")) {
            # Numeric: use absolute difference
            diff <- abs(as.numeric(val_pes) - as.numeric(val_census))
            var_scores[k] <- max(0, 1 - diff / 5)  # Tolerance of 5 years
          } else if (var %in% c("sexe", "sex")) {
            # Exact match for sex
            var_scores[k] <- ifelse(tolower(val_pes) == tolower(val_census), 1, 0)
          } else {
            # String similarity for names
            if (is.na(val_pes) || is.na(val_census) || val_pes == "" || val_census == "") {
              var_scores[k] <- 0
            } else {
              dist <- stringdist::stringdist(tolower(val_pes), tolower(val_census), 
                                              method = "jw")
              var_scores[k] <- 1 - dist
            }
          }
        }
        
        # Weighted average score
        scores[j] <- sum(var_scores * weights[match_vars])
      }
      
      # Find best match above threshold
      best_idx <- which.max(scores)
      if (scores[best_idx] >= threshold) {
        pair <- cbind(
          pes_block[i, ],
          census_block[best_idx, ],
          match_score = scores[best_idx],
          match_type = "probabilistic"
        )
        matched_pairs <- rbind(matched_pairs, pair)
        used_census_ids <- c(used_census_ids, census_block$census_id[best_idx])
      }
    }
  }
  
  # Get unmatched records
  matched_pes_ids <- if (nrow(matched_pairs) > 0) unique(matched_pairs$pes_id) else c()
  matched_census_ids <- used_census_ids
  
  unmatched_pes <- pes_data[!pes_data$pes_id %in% matched_pes_ids, ]
  unmatched_census <- census_data[!census_data$census_id %in% matched_census_ids, ]
  
  return(list(
    matched = matched_pairs,
    unmatched_pes = unmatched_pes,
    unmatched_census = unmatched_census,
    method = "probabilistic"
  ))
}

#' Hybrid Matching (Deterministic + Probabilistic)
#' @keywords internal
hybrid_match <- function(pes_data, census_data, match_vars, blocking_vars,
                          threshold, weights) {
  
  # First pass: deterministic matching
  det_result <- deterministic_match(pes_data, census_data, match_vars, blocking_vars)
  
  # Second pass: probabilistic matching on remaining records
  if (nrow(det_result$unmatched_pes) > 0 && nrow(det_result$unmatched_census) > 0) {
    prob_result <- probabilistic_match(
      det_result$unmatched_pes,
      det_result$unmatched_census,
      match_vars, blocking_vars, threshold, weights
    )
    
    # Combine results
    all_matched <- rbind(det_result$matched, prob_result$matched)
    
    return(list(
      matched = all_matched,
      unmatched_pes = prob_result$unmatched_pes,
      unmatched_census = prob_result$unmatched_census,
      method = "hybrid"
    ))
  } else {
    return(det_result)
  }
}

#' Compute Match Statistics
#' @keywords internal
compute_match_statistics <- function(result, n_pes, n_census) {
  n_matched <- nrow(result$matched)
  n_unmatched_pes <- nrow(result$unmatched_pes)
  n_unmatched_census <- nrow(result$unmatched_census)
  
  list(
    n_pes_total = n_pes,
    n_census_total = n_census,
    n_matched = n_matched,
    n_unmatched_pes = n_unmatched_pes,
    n_unmatched_census = n_unmatched_census,
    match_rate_pes = n_matched / n_pes * 100,
    match_rate_census = n_matched / n_census * 100,
    omission_estimate = n_unmatched_pes,
    erroneous_inclusion_estimate = n_unmatched_census
  )
}

#' Main PES Matching Function
#'
#' Wrapper function for PES matching with additional options
#'
#' @param pes_data PES data frame
#' @param census_data Census data frame
#' @param config List of configuration options
#'
#' @return Match result object
#' @export
pes_match <- function(pes_data, census_data, config = list()) {
  
  # Default configuration
  default_config <- list(
    match_vars = c("nom", "prenom", "age", "sexe"),
    blocking_vars = c("ea_code"),
    threshold = 0.8,
    method = "hybrid",
    weights = NULL,
    age_tolerance = 2,
    name_method = "jw"
  )
  
  # Merge with user config
  config <- modifyList(default_config, config)
  
  # Run matching
  result <- pes_match_auto(
    pes_data = pes_data,
    census_data = census_data,
    match_vars = config$match_vars,
    blocking_vars = config$blocking_vars,
    threshold = config$threshold,
    method = config$method,
    weights = config$weights
  )
  
  # Add configuration to result
  result$config <- config
  
  class(result) <- c("pes_match_result", class(result))
  
  return(result)
}

#' Manual Matching Interface
#'
#' Prepare data for manual matching review
#'
#' @param match_result Result from pes_match_auto
#' @param n_review Number of uncertain matches to review
#'
#' @return Data frame of cases for manual review
#' @export
pes_match_manual <- function(match_result, n_review = 100) {
  
  # Get matches with scores between 0.6 and 0.9 (uncertain)
  if (nrow(match_result$matched) == 0) {
    return(data.frame())
  }
  
  uncertain <- match_result$matched[
    match_result$matched$match_score >= 0.6 & 
      match_result$matched$match_score < 0.9,
  ]
  
  # Sort by score (most uncertain first)
  uncertain <- uncertain[order(uncertain$match_score), ]
  
  # Return top n for review
  head(uncertain, n_review)
}

#' Reconciliation Visit Preparation
#'
#' Prepare cases requiring field reconciliation
#'
#' @param match_result Result from pes_match
#'
#' @return List of cases for reconciliation visits
#' @export
pes_reconciliation <- function(match_result) {
  
  # Cases in census but not in PES (potential erroneous enumerations)
  census_only <- match_result$unmatched_census
  census_only$reconciliation_type <- "census_only"
  census_only$reconciliation_question <- "Verify if person was resident at census date"
  

  # Cases in PES but not in census (potential omissions)
  pes_only <- match_result$unmatched_pes
  pes_only$reconciliation_type <- "pes_only"
  pes_only$reconciliation_question <- "Verify residence status and if enumerated elsewhere"
  
  list(
    census_only = census_only,
    pes_only = pes_only,
    total_reconciliation = nrow(census_only) + nrow(pes_only)
  )
}

#' Print method for pes_match_result
#' @export
print.pes_match_result <- function(x, ...) {
  cat("PES Matching Result\n")
  cat("===================\n")
  cat("Method:", x$method, "\n\n")
  cat("Statistics:\n")
  cat("  PES records:", x$statistics$n_pes_total, "\n")
  cat("  Census records:", x$statistics$n_census_total, "\n")
  cat("  Matched:", x$statistics$n_matched, 
      sprintf("(%.1f%% of PES)", x$statistics$match_rate_pes), "\n")
  cat("  Unmatched PES:", x$statistics$n_unmatched_pes, "(potential omissions)\n")
  cat("  Unmatched Census:", x$statistics$n_unmatched_census, "(potential erroneous inclusions)\n")
}
