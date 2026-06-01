#' @title Tabulations and Export Functions
#' @description Functions for creating standard demographic tables and exports
#' @name tabulations
NULL

#' Create Standard Demographic Tables
#'
#' Generate standard tabulations for census/survey data
#'
#' @param data Data frame with demographic data
#' @param tables Character vector of table types to create
#' @param config Configuration list with variable names
#' @param weight_var Weight variable (optional)
#'
#' @return List of tables
#' @export
#'
#' @examples
#' \dontrun{
#' tables <- create_tables(
#'   data = census_data,
#'   tables = c("population", "age_sex", "fertility", "education"),
#'   config = list(age_var = "age", sex_var = "sexe")
#' )
#' }
create_tables <- function(data, tables = "all", config = list(), weight_var = NULL) {
  
  # Default configuration
  default_config <- list(
    age_var = "age",
    sex_var = "sexe",
    region_var = "region",
    urban_var = "milieu",
    education_var = "niveau_instruction",
    activity_var = "situation_activite",
    marital_var = "etat_matrimonial",
    disability_var = "handicap",
    migration_var = "migrant",
    births_var = "naissances_12m",
    deaths_var = "deces_12m"
  )
  
  config <- modifyList(default_config, config)
  
  # Available table types
  all_tables <- c("population", "age_sex", "age_sex_region", "fertility",
                  "mortality", "education", "employment", "marital",
                  "disability", "migration", "households")
  
  if ("all" %in% tables) {
    tables <- all_tables
  }
  
  results <- list()
  
  # Generate each requested table
  for (table_type in tables) {
    tryCatch({
      results[[table_type]] <- switch(table_type,
                                       "population" = table_population(data, config, weight_var),
                                       "age_sex" = table_age_sex(data, config, weight_var),
                                       "age_sex_region" = table_age_sex_region(data, config, weight_var),
                                       "fertility" = ds_table_fertility(data, config, weight_var),
                                       "mortality" = ds_table_mortality(data, config, weight_var),
                                       "education" = table_education(data, config, weight_var),
                                       "employment" = table_employment(data, config, weight_var),
                                       "marital" = table_marital(data, config, weight_var),
                                       "disability" = ds_table_disability(data, config, weight_var),
                                       "migration" = ds_table_migration(data, config, weight_var),
                                       "households" = table_households(data, config, weight_var),
                                       NULL
      )
    }, error = function(e) {
      warning(paste("Could not create table", table_type, ":", e$message))
      results[[table_type]] <<- NULL
    })
  }
  
  class(results) <- c("demostats_tables", class(results))
  return(results)
}

#' Population Summary Table
#' @keywords internal
table_population <- function(data, config, weight_var) {
  
  if (!is.null(weight_var) && weight_var %in% names(data)) {
    w <- data[[weight_var]]
  } else {
    w <- rep(1, nrow(data))
  }
  
  data$sex_std <- standardize_sex(data[[config$sex_var]])
  
  total <- sum(w, na.rm = TRUE)
  male <- sum(w[data$sex_std == "Male"], na.rm = TRUE)
  female <- sum(w[data$sex_std == "Female"], na.rm = TRUE)
  
  summary_table <- data.frame(
    Indicator = c("Total Population", "Male", "Female", "Sex Ratio",
                  "Median Age", "Mean Age"),
    Value = c(
      format(round(total), big.mark = ","),
      format(round(male), big.mark = ","),
      format(round(female), big.mark = ","),
      sprintf("%.1f", male / female * 100),
      sprintf("%.1f", median(data[[config$age_var]], na.rm = TRUE)),
      sprintf("%.1f", mean(data[[config$age_var]], na.rm = TRUE))
    ),
    stringsAsFactors = FALSE
  )
  
  # By region if available
  by_region <- NULL
  if (config$region_var %in% names(data)) {
    by_region <- aggregate(w, by = list(Region = data[[config$region_var]]), 
                            FUN = sum, na.rm = TRUE)
    names(by_region)[2] <- "Population"
    by_region$Percent <- by_region$Population / total * 100
    by_region <- by_region[order(-by_region$Population), ]
  }
  
  # By urban/rural if available
  by_urban <- NULL
  if (config$urban_var %in% names(data)) {
    by_urban <- aggregate(w, by = list(Milieu = data[[config$urban_var]]),
                           FUN = sum, na.rm = TRUE)
    names(by_urban)[2] <- "Population"
    by_urban$Percent <- by_urban$Population / total * 100
  }
  
  list(
    summary = summary_table,
    by_region = by_region,
    by_urban = by_urban
  )
}

#' Age-Sex Distribution Table
#' @keywords internal
table_age_sex <- function(data, config, weight_var) {
  
  if (!is.null(weight_var) && weight_var %in% names(data)) {
    w <- data[[weight_var]]
  } else {
    w <- rep(1, nrow(data))
  }
  
  data$sex_std <- standardize_sex(data[[config$sex_var]])
  data$age_group <- ds_create_age_groups(data[[config$age_var]])
  
  # Cross-tabulation
  tab <- tapply(w, list(data$age_group, data$sex_std), sum, na.rm = TRUE)
  tab[is.na(tab)] <- 0
  
  # Create data frame
  result <- data.frame(
    Age_Group = rownames(tab),
    Male = tab[, "Male"],
    Female = tab[, "Female"],
    Total = tab[, "Male"] + tab[, "Female"],
    stringsAsFactors = FALSE
  )
  
  result$Pct_Male <- result$Male / sum(result$Male) * 100
  result$Pct_Female <- result$Female / sum(result$Female) * 100
  result$Pct_Total <- result$Total / sum(result$Total) * 100
  result$Sex_Ratio <- result$Male / result$Female * 100
  
  # Add totals row
  totals <- data.frame(
    Age_Group = "TOTAL",
    Male = sum(result$Male),
    Female = sum(result$Female),
    Total = sum(result$Total),
    Pct_Male = 100,
    Pct_Female = 100,
    Pct_Total = 100,
    Sex_Ratio = sum(result$Male) / sum(result$Female) * 100
  )
  
  result <- rbind(result, totals)
  
  return(result)
}

#' Age-Sex by Region Table
#' @keywords internal
table_age_sex_region <- function(data, config, weight_var) {
  
  if (!config$region_var %in% names(data)) {
    return(NULL)
  }
  
  if (!is.null(weight_var) && weight_var %in% names(data)) {
    w <- data[[weight_var]]
  } else {
    w <- rep(1, nrow(data))
  }
  
  data$sex_std <- standardize_sex(data[[config$sex_var]])
  data$age_group <- ds_create_age_groups(data[[config$age_var]])
  
  regions <- unique(data[[config$region_var]])
  
  results <- lapply(regions, function(r) {
    subset_data <- data[data[[config$region_var]] == r, ]
    subset_w <- w[data[[config$region_var]] == r]
    
    tab <- tapply(subset_w, list(subset_data$age_group, subset_data$sex_std), 
                  sum, na.rm = TRUE)
    tab[is.na(tab)] <- 0
    
    data.frame(
      Region = r,
      Age_Group = rownames(tab),
      Male = tab[, "Male"],
      Female = tab[, "Female"],
      Total = tab[, "Male"] + tab[, "Female"],
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, results)
}

#' Fertility Table
#' @keywords internal
ds_table_fertility <- function(data, config, weight_var) {
  
  if (!config$births_var %in% names(data)) {
    return(NULL)
  }
  
  # Filter women 15-49
  data$sex_std <- standardize_sex(data[[config$sex_var]])
  women <- data[data$sex_std == "Female" & 
                  data[[config$age_var]] >= 15 & 
                  data[[config$age_var]] < 50, ]
  
  if (!is.null(weight_var) && weight_var %in% names(women)) {
    w <- women[[weight_var]]
  } else {
    w <- rep(1, nrow(women))
  }
  
  # Age groups for fertility
  women$age_group <- cut(women[[config$age_var]],
                          breaks = c(15, 20, 25, 30, 35, 40, 45, 50),
                          labels = c("15-19", "20-24", "25-29", "30-34",
                                     "35-39", "40-44", "45-49"),
                          right = FALSE)
  
  # ASFR calculation
  births <- tapply(women[[config$births_var]] * w, women$age_group, sum, na.rm = TRUE)
  women_count <- tapply(w, women$age_group, sum, na.rm = TRUE)
  
  result <- data.frame(
    Age_Group = names(births),
    Women = women_count,
    Births = births,
    ASFR = births / women_count * 1000,
    stringsAsFactors = FALSE
  )
  
  # TFR
  tfr_value <- sum(result$ASFR) * 5 / 1000
  
  # Add summary
  attr(result, "TFR") <- tfr_value
  attr(result, "GFR") <- sum(births) / sum(women_count) * 1000
  
  return(result)
}

#' Mortality Table
#' @keywords internal
ds_table_mortality <- function(data, config, weight_var) {
  
  if (!config$deaths_var %in% names(data)) {
    return(NULL)
  }
  
  if (!is.null(weight_var) && weight_var %in% names(data)) {
    w <- data[[weight_var]]
  } else {
    w <- rep(1, nrow(data))
  }
  
  data$age_group <- create_mortality_age_groups(data[[config$age_var]])
  
  deaths <- tapply(data[[config$deaths_var]] * w, data$age_group, sum, na.rm = TRUE)
  pop <- tapply(w, data$age_group, sum, na.rm = TRUE)
  
  result <- data.frame(
    Age_Group = names(deaths),
    Population = pop,
    Deaths = deaths,
    ASMR = deaths / pop * 1000,
    stringsAsFactors = FALSE
  )
  
  # CDR
  cdr_value <- sum(deaths, na.rm = TRUE) / sum(pop, na.rm = TRUE) * 1000
  attr(result, "CDR") <- cdr_value
  
  return(result)
}

#' Education Table
#' @keywords internal
table_education <- function(data, config, weight_var) {
  
  if (!config$education_var %in% names(data)) {
    return(NULL)
  }
  
  if (!is.null(weight_var) && weight_var %in% names(data)) {
    w <- data[[weight_var]]
  } else {
    w <- rep(1, nrow(data))
  }
  
  data$sex_std <- standardize_sex(data[[config$sex_var]])
  
  # Overall distribution
  overall <- tapply(w, data[[config$education_var]], sum, na.rm = TRUE)
  overall_pct <- overall / sum(overall) * 100
  
  # By sex
  male_dist <- tapply(w[data$sex_std == "Male"], 
                       data[[config$education_var]][data$sex_std == "Male"],
                       sum, na.rm = TRUE)
  female_dist <- tapply(w[data$sex_std == "Female"],
                         data[[config$education_var]][data$sex_std == "Female"],
                         sum, na.rm = TRUE)
  
  levels <- names(overall)
  
  result <- data.frame(
    Education_Level = levels,
    Total = overall[levels],
    Pct_Total = overall_pct[levels],
    Male = male_dist[levels],
    Female = female_dist[levels],
    stringsAsFactors = FALSE
  )
  
  result$Pct_Male <- result$Male / sum(result$Male, na.rm = TRUE) * 100
  result$Pct_Female <- result$Female / sum(result$Female, na.rm = TRUE) * 100
  
  return(result)
}

#' Employment Table
#' @keywords internal
table_employment <- function(data, config, weight_var) {
  
  if (!config$activity_var %in% names(data)) {
    return(NULL)
  }
  
  # Working age population (15-64)
  wap <- data[data[[config$age_var]] >= 15 & data[[config$age_var]] <= 64, ]
  
  if (!is.null(weight_var) && weight_var %in% names(wap)) {
    w <- wap[[weight_var]]
  } else {
    w <- rep(1, nrow(wap))
  }
  
  wap$sex_std <- standardize_sex(wap[[config$sex_var]])
  
  # Activity distribution
  activity_dist <- tapply(w, wap[[config$activity_var]], sum, na.rm = TRUE)
  
  result <- data.frame(
    Activity_Status = names(activity_dist),
    Total = activity_dist,
    Percent = activity_dist / sum(activity_dist) * 100,
    stringsAsFactors = FALSE
  )
  
  return(result)
}

#' Marital Status Table
#' @keywords internal
table_marital <- function(data, config, weight_var) {
  
  if (!config$marital_var %in% names(data)) {
    return(NULL)
  }
  
  # Adults 15+
  adults <- data[data[[config$age_var]] >= 15, ]
  
  if (!is.null(weight_var) && weight_var %in% names(adults)) {
    w <- adults[[weight_var]]
  } else {
    w <- rep(1, nrow(adults))
  }
  
  adults$sex_std <- standardize_sex(adults[[config$sex_var]])
  
  # Distribution by sex
  male_dist <- tapply(w[adults$sex_std == "Male"],
                       adults[[config$marital_var]][adults$sex_std == "Male"],
                       sum, na.rm = TRUE)
  female_dist <- tapply(w[adults$sex_std == "Female"],
                         adults[[config$marital_var]][adults$sex_std == "Female"],
                         sum, na.rm = TRUE)
  
  levels <- union(names(male_dist), names(female_dist))
  
  result <- data.frame(
    Marital_Status = levels,
    Male = male_dist[levels],
    Female = female_dist[levels],
    stringsAsFactors = FALSE
  )
  
  result$Male[is.na(result$Male)] <- 0
  result$Female[is.na(result$Female)] <- 0
  result$Total <- result$Male + result$Female
  
  return(result)
}

#' Disability Table
#' @keywords internal
ds_table_disability <- function(data, config, weight_var) {
  
  if (!config$disability_var %in% names(data)) {
    return(NULL)
  }
  
  if (!is.null(weight_var) && weight_var %in% names(data)) {
    w <- data[[weight_var]]
  } else {
    w <- rep(1, nrow(data))
  }
  
  data$sex_std <- standardize_sex(data[[config$sex_var]])
  data$age_group <- ds_create_age_groups(data[[config$age_var]])
  
  # Identify disabled
  disabled <- data[[config$disability_var]] == 1 | 
    data[[config$disability_var]] == TRUE |
    (is.numeric(data[[config$disability_var]]) & data[[config$disability_var]] >= 3)
  
  # By age and sex
  result <- data.frame(
    Age_Group = levels(data$age_group),
    stringsAsFactors = FALSE
  )
  
  for (sex in c("Male", "Female")) {
    disabled_count <- tapply(w[disabled & data$sex_std == sex],
                              data$age_group[disabled & data$sex_std == sex],
                              sum, na.rm = TRUE)
    total_count <- tapply(w[data$sex_std == sex],
                           data$age_group[data$sex_std == sex],
                           sum, na.rm = TRUE)
    
    result[[paste0("Disabled_", sex)]] <- disabled_count[result$Age_Group]
    result[[paste0("Total_", sex)]] <- total_count[result$Age_Group]
    result[[paste0("Rate_", sex)]] <- disabled_count[result$Age_Group] / 
      total_count[result$Age_Group] * 100
  }
  
  return(result)
}

#' Migration Table
#' @keywords internal
ds_table_migration <- function(data, config, weight_var) {
  
  if (!config$migration_var %in% names(data)) {
    return(NULL)
  }
  
  if (!is.null(weight_var) && weight_var %in% names(data)) {
    w <- data[[weight_var]]
  } else {
    w <- rep(1, nrow(data))
  }
  
  data$sex_std <- standardize_sex(data[[config$sex_var]])
  data$age_group <- ds_create_age_groups(data[[config$age_var]])
  
  # Identify migrants
  migrants <- data[[config$migration_var]] == 1 | 
    data[[config$migration_var]] == TRUE |
    tolower(as.character(data[[config$migration_var]])) %in% c("yes", "oui", "migrant")
  
  # By age and sex
  result <- data.frame(
    Age_Group = levels(data$age_group),
    stringsAsFactors = FALSE
  )
  
  for (sex in c("Male", "Female")) {
    migrant_count <- tapply(w[migrants & data$sex_std == sex],
                             data$age_group[migrants & data$sex_std == sex],
                             sum, na.rm = TRUE)
    total_count <- tapply(w[data$sex_std == sex],
                           data$age_group[data$sex_std == sex],
                           sum, na.rm = TRUE)
    
    result[[paste0("Migrants_", sex)]] <- migrant_count[result$Age_Group]
    result[[paste0("Total_", sex)]] <- total_count[result$Age_Group]
    result[[paste0("Rate_", sex)]] <- migrant_count[result$Age_Group] / 
      total_count[result$Age_Group] * 100
  }
  
  return(result)
}

#' Households Table
#' @keywords internal
table_households <- function(data, config, weight_var) {
  # Placeholder - requires household-level data
  return(NULL)
}

#' Export Tables
#'
#' Export tables to various formats
#'
#' @param tables Tables object from create_tables
#' @param format Output format: "xlsx", "csv", "html"
#' @param file Output file path
#' @param ... Additional arguments
#'
#' @return Invisible file path
#' @export
ds_export_tables <- function(tables, format = "xlsx", file = "tables_output", ...) {
  
  if (format == "xlsx") {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("Package 'openxlsx' required for Excel export")
    }
    
    wb <- openxlsx::createWorkbook()
    
    for (name in names(tables)) {
      if (!is.null(tables[[name]])) {
        if (is.data.frame(tables[[name]])) {
          openxlsx::addWorksheet(wb, name)
          openxlsx::writeData(wb, name, tables[[name]])
        } else if (is.list(tables[[name]])) {
          # Handle nested lists
          for (subname in names(tables[[name]])) {
            if (is.data.frame(tables[[name]][[subname]])) {
              sheet_name <- paste0(name, "_", subname)
              if (nchar(sheet_name) > 31) {
                sheet_name <- substr(sheet_name, 1, 31)
              }
              openxlsx::addWorksheet(wb, sheet_name)
              openxlsx::writeData(wb, sheet_name, tables[[name]][[subname]])
            }
          }
        }
      }
    }
    
    file_path <- paste0(file, ".xlsx")
    openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
    
  } else if (format == "csv") {
    dir.create(file, showWarnings = FALSE)
    
    for (name in names(tables)) {
      if (!is.null(tables[[name]]) && is.data.frame(tables[[name]])) {
        write.csv(tables[[name]], file.path(file, paste0(name, ".csv")), 
                  row.names = FALSE)
      }
    }
    
    file_path <- file
    
  } else if (format == "html") {
    if (!requireNamespace("knitr", quietly = TRUE)) {
      stop("Package 'knitr' required for HTML export")
    }
    
    html_content <- "<html><head><style>
      table { border-collapse: collapse; margin: 20px 0; }
      th, td { border: 1px solid #ddd; padding: 8px; text-align: right; }
      th { background-color: #4CAF50; color: white; }
      h2 { color: #333; }
    </style></head><body>"
    
    for (name in names(tables)) {
      if (!is.null(tables[[name]]) && is.data.frame(tables[[name]])) {
        html_content <- paste0(html_content, "<h2>", name, "</h2>")
        html_content <- paste0(html_content, knitr::kable(tables[[name]], format = "html"))
      }
    }
    
    html_content <- paste0(html_content, "</body></html>")
    
    file_path <- paste0(file, ".html")
    writeLines(html_content, file_path)
  }
  
  message("Tables exported to: ", file_path)
  invisible(file_path)
}

#' Print method for demostats_tables
#' @export
print.demostats_tables <- function(x, ...) {
  cat("\nDemographic Tables Summary\n")
  cat("==========================\n\n")
  
  for (name in names(x)) {
    if (!is.null(x[[name]])) {
      if (is.data.frame(x[[name]])) {
        cat(name, ":", nrow(x[[name]]), "rows\n")
      } else if (is.list(x[[name]])) {
        cat(name, ": (nested list with", length(x[[name]]), "elements)\n")
      }
    }
  }
  
  cat("\nUse ds_export_tables() to save to file.\n")
}
