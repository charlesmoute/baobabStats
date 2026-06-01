#' @title Population Structure Indicators
#' @description Functions for computing population structure and fertility indicators
#' @name indicators_structure
NULL

#' Age Pyramid Data
#'
#' Prepare data for age-sex pyramid visualization
#'
#' @param data Data frame with population data
#' @param age_var Name of age variable
#' @param sex_var Name of sex variable
#' @param weight_var Name of weight variable (optional)
#' @param age_groups Custom age groups (optional)
#'
#' @return Data frame ready for pyramid plotting
#' @export
age_pyramid <- function(data, age_var = "age", sex_var = "sexe", 
                         weight_var = NULL, age_groups = NULL) {
  
  # Default age groups
  if (is.null(age_groups)) {
    age_groups <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf)
    age_labels <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
                    "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", 
                    "65-69", "70-74", "75-79", "80+")
  } else {
    age_labels <- paste0(head(age_groups, -1), "-", tail(age_groups, -1) - 1)
    age_labels[length(age_labels)] <- paste0(age_groups[length(age_groups) - 1], "+")
  }
  
  # Create age groups
  data$age_group <- cut(data[[age_var]], breaks = age_groups, 
                        labels = age_labels, right = FALSE)
  
  # Standardize sex variable
  data$sex_std <- standardize_sex(data[[sex_var]])
  
  # Calculate counts
  if (!is.null(weight_var) && weight_var %in% names(data)) {
    pyramid_data <- aggregate(data[[weight_var]], 
                               by = list(age_group = data$age_group, 
                                         sex = data$sex_std),
                               FUN = sum, na.rm = TRUE)
    names(pyramid_data)[3] <- "count"
  } else {
    pyramid_data <- as.data.frame(table(age_group = data$age_group, 
                                         sex = data$sex_std))
    names(pyramid_data)[3] <- "count"
  }
  
  # Calculate percentages
  total <- sum(pyramid_data$count)
  pyramid_data$percent <- pyramid_data$count / total * 100
  
  # Make male values negative for pyramid
  pyramid_data$pyramid_value <- ifelse(pyramid_data$sex == "Male", 
                                        -pyramid_data$percent, 
                                        pyramid_data$percent)
  
  # Add totals by sex
  attr(pyramid_data, "total_male") <- sum(pyramid_data$count[pyramid_data$sex == "Male"])
  attr(pyramid_data, "total_female") <- sum(pyramid_data$count[pyramid_data$sex == "Female"])
  attr(pyramid_data, "total") <- total
  
  class(pyramid_data) <- c("age_pyramid_data", class(pyramid_data))
  return(pyramid_data)
}

#' Standardize Sex Variable
#' @keywords internal
standardize_sex <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("^m$|^1$|^masculin$|^homme$|^male$|^h$", "Male", x)
  x <- gsub("^f$|^2$|^feminin$|^femme$|^female$", "Female", x)
  return(x)
}

#' Sex Ratio
#'
#' Calculate sex ratio (males per 100 females)
#'
#' @param data Data frame or named vector with male/female counts
#' @param sex_var Sex variable name
#' @param age_var Age variable for age-specific ratios (optional)
#' @param weight_var Weight variable (optional)
#'
#' @return Sex ratio(s)
#' @export
sex_ratio <- function(data, sex_var = "sexe", age_var = NULL, weight_var = NULL) {
  
  if (is.data.frame(data)) {
    data$sex_std <- standardize_sex(data[[sex_var]])
    
    if (!is.null(age_var)) {
      # Age-specific sex ratios
      data$age_group <- ds_create_age_groups(data[[age_var]])
      
      if (!is.null(weight_var)) {
        counts <- aggregate(data[[weight_var]], 
                            by = list(age_group = data$age_group, sex = data$sex_std),
                            FUN = sum, na.rm = TRUE)
      } else {
        counts <- as.data.frame(table(age_group = data$age_group, sex = data$sex_std))
      }
      names(counts)[3] <- "n"
      
      # Pivot to wide format
      wide <- reshape(counts, direction = "wide", 
                      idvar = "age_group", timevar = "sex", v.names = "n")
      
      wide$sex_ratio <- wide$n.Male / wide$n.Female * 100
      
      return(wide[, c("age_group", "n.Male", "n.Female", "sex_ratio")])
      
    } else {
      # Overall sex ratio
      if (!is.null(weight_var)) {
        males <- sum(data[[weight_var]][data$sex_std == "Male"], na.rm = TRUE)
        females <- sum(data[[weight_var]][data$sex_std == "Female"], na.rm = TRUE)
      } else {
        males <- sum(data$sex_std == "Male", na.rm = TRUE)
        females <- sum(data$sex_std == "Female", na.rm = TRUE)
      }
      
      return(males / females * 100)
    }
  } else {
    # Assume named vector with Male and Female
    return(data["Male"] / data["Female"] * 100)
  }
}

#' Dependency Ratio
#'
#' Calculate age dependency ratios
#'
#' @param data Data frame with age data
#' @param age_var Age variable name
#' @param weight_var Weight variable (optional)
#' @param young_cutoff Age cutoff for young dependents (default 15)
#' @param old_cutoff Age cutoff for old dependents (default 65)
#'
#' @return List of dependency ratios
#' @export
dependency_ratio <- function(data, age_var = "age", weight_var = NULL,
                              young_cutoff = 15, old_cutoff = 65) {
  
  ages <- data[[age_var]]
  
  if (!is.null(weight_var) && weight_var %in% names(data)) {
    weights <- data[[weight_var]]
    young <- sum(weights[ages < young_cutoff], na.rm = TRUE)
    working <- sum(weights[ages >= young_cutoff & ages < old_cutoff], na.rm = TRUE)
    old <- sum(weights[ages >= old_cutoff], na.rm = TRUE)
  } else {
    young <- sum(ages < young_cutoff, na.rm = TRUE)
    working <- sum(ages >= young_cutoff & ages < old_cutoff, na.rm = TRUE)
    old <- sum(ages >= old_cutoff, na.rm = TRUE)
  }
  
  total <- young + working + old
  
  list(
    young_population = young,
    working_population = working,
    old_population = old,
    total_population = total,
    young_dependency_ratio = young / working * 100,
    old_dependency_ratio = old / working * 100,
    total_dependency_ratio = (young + old) / working * 100,
    youth_proportion = young / total * 100,
    working_proportion = working / total * 100,
    elderly_proportion = old / total * 100
  )
}

#' Median Age
#'
#' Calculate median age of population
#'
#' @param data Data frame or numeric vector of ages
#' @param age_var Age variable name
#' @param weight_var Weight variable (optional)
#' @param by_sex Calculate by sex (optional)
#' @param sex_var Sex variable name
#'
#' @return Median age(s)
#' @export
median_age <- function(data, age_var = "age", weight_var = NULL, 
                        by_sex = FALSE, sex_var = "sexe") {
  
  if (is.numeric(data)) {
    return(median(data, na.rm = TRUE))
  }
  
  if (!is.null(weight_var) && weight_var %in% names(data)) {
    # Weighted median
    weighted_median <- function(x, w) {
      valid <- !is.na(x) & !is.na(w)
      x <- x[valid]
      w <- w[valid]
      ord <- order(x)
      x <- x[ord]
      w <- w[ord]
      cumw <- cumsum(w)
      x[which(cumw >= sum(w) / 2)[1]]
    }
    
    if (by_sex) {
      data$sex_std <- standardize_sex(data[[sex_var]])
      result <- tapply(seq_len(nrow(data)), data$sex_std, function(idx) {
        weighted_median(data[[age_var]][idx], data[[weight_var]][idx])
      })
      return(as.list(result))
    } else {
      return(weighted_median(data[[age_var]], data[[weight_var]]))
    }
  } else {
    if (by_sex) {
      data$sex_std <- standardize_sex(data[[sex_var]])
      result <- tapply(data[[age_var]], data$sex_std, median, na.rm = TRUE)
      return(as.list(result))
    } else {
      return(median(data[[age_var]], na.rm = TRUE))
    }
  }
}

#' Create Standard Age Groups
#' @keywords internal
ds_create_age_groups <- function(ages, width = 5, max_age = 80) {
  breaks <- c(seq(0, max_age, by = width), Inf)
  labels <- c(paste0(seq(0, max_age - width, by = width), "-", 
                     seq(width - 1, max_age - 1, by = width)),
              paste0(max_age, "+"))
  cut(ages, breaks = breaks, labels = labels, right = FALSE)
}

#' Mean Age
#'
#' Calculate mean age of population
#'
#' @param data Data frame or numeric vector
#' @param age_var Age variable name
#' @param weight_var Weight variable (optional)
#'
#' @return Mean age
#' @export
mean_age <- function(data, age_var = "age", weight_var = NULL) {
  
  if (is.numeric(data)) {
    return(mean(data, na.rm = TRUE))
  }
  
  if (!is.null(weight_var) && weight_var %in% names(data)) {
    weighted.mean(data[[age_var]], data[[weight_var]], na.rm = TRUE)
  } else {
    mean(data[[age_var]], na.rm = TRUE)
  }
}

#' Population Growth Rate
#'
#' Calculate population growth rate between two time points
#'
#' @param pop1 Population at time 1
#' @param pop2 Population at time 2
#' @param years Number of years between measurements
#' @param method "exponential" or "geometric"
#'
#' @return Annual growth rate (percentage)
#' @export
growth_rate <- function(pop1, pop2, years, method = "exponential") {
  
  if (method == "exponential") {
    # r = ln(P2/P1) / t
    rate <- log(pop2 / pop1) / years * 100
  } else {
    # r = (P2/P1)^(1/t) - 1
    rate <- ((pop2 / pop1)^(1 / years) - 1) * 100
  }
  
  return(rate)
}

#' Doubling Time
#'
#' Calculate population doubling time
#'
#' @param growth_rate Annual growth rate (percentage)
#'
#' @return Doubling time in years
#' @export
doubling_time <- function(growth_rate) {
  if (growth_rate <= 0) {
    return(Inf)
  }
  log(2) / (growth_rate / 100)
}

# ============================================================
# FERTILITY INDICATORS
# ============================================================

#' Fertility Rates
#'
#' Calculate various fertility rates
#'
#' @param data Data frame with women and births data
#' @param births_var Variable indicating number of births or birth indicator
#' @param age_var Age variable
#' @param sex_var Sex variable
#' @param weight_var Weight variable (optional)
#' @param reference_period Reference period in years (default 1)
#'
#' @return List of fertility indicators
#' @export
fertility_rates <- function(data, births_var = "births_12m", age_var = "age",
                             sex_var = "sexe", weight_var = NULL,
                             reference_period = 1) {
  
  # Filter women of reproductive age
  data$sex_std <- standardize_sex(data[[sex_var]])
  women <- data[data$sex_std == "Female" & 
                  data[[age_var]] >= 15 & data[[age_var]] < 50, ]
  
  # Create 5-year age groups for ASFR
  women$age_group <- cut(women[[age_var]], 
                          breaks = c(15, 20, 25, 30, 35, 40, 45, 50),
                          labels = c("15-19", "20-24", "25-29", "30-34", 
                                     "35-39", "40-44", "45-49"),
                          right = FALSE)
  
  # Calculate ASFR
  if (!is.null(weight_var) && weight_var %in% names(women)) {
    asfr_data <- aggregate(
      cbind(births = women[[births_var]], women = women[[weight_var]]),
      by = list(age_group = women$age_group),
      FUN = sum, na.rm = TRUE
    )
  } else {
    asfr_data <- aggregate(
      women[[births_var]],
      by = list(age_group = women$age_group),
      FUN = function(x) c(births = sum(x, na.rm = TRUE), women = length(x))
    )
    asfr_data <- cbind(asfr_data[, 1, drop = FALSE], 
                        as.data.frame(asfr_data$x))
  }
  
  # ASFR per 1000 women
  asfr_data$asfr <- asfr_data$births / asfr_data$women * 1000 / reference_period
  
  # TFR (sum of ASFR * 5 / 1000)
  tfr <- sum(asfr_data$asfr) * 5 / 1000
  
  # GFR (General Fertility Rate)
  total_births <- sum(asfr_data$births)
  total_women <- sum(asfr_data$women)
  gfr <- total_births / total_women * 1000 / reference_period
  
  # CBR requires total population
  total_pop <- if (!is.null(weight_var)) sum(data[[weight_var]], na.rm = TRUE) else nrow(data)
  cbr <- total_births / total_pop * 1000 / reference_period
  
  list(
    asfr = asfr_data,
    tfr = tfr,
    gfr = gfr,
    cbr = cbr,
    total_births = total_births,
    women_15_49 = total_women,
    total_population = total_pop,
    reference_period = reference_period
  )
}

#' Total Fertility Rate
#'
#' Calculate TFR from ASFR values
#'
#' @param asfr Vector of age-specific fertility rates (per 1000)
#' @param age_interval Width of age intervals (default 5)
#'
#' @return TFR value
#' @export
tfr <- function(asfr, age_interval = 5) {
  sum(asfr) * age_interval / 1000
}

#' Age-Specific Fertility Rate
#'
#' Calculate ASFR for a specific age group
#'
#' @param births Number of births to women in age group
#' @param women Number of women in age group
#' @param period Reference period in years
#'
#' @return ASFR per 1000 women
#' @export
asfr <- function(births, women, period = 1) {
  births / women / period * 1000
}

#' Crude Birth Rate
#'
#' @param births Total births
#' @param population Total population
#' @param period Reference period
#'
#' @return CBR per 1000 population
#' @export
cbr <- function(births, population, period = 1) {
  births / population / period * 1000
}

#' Child-Woman Ratio
#'
#' Calculate ratio of children 0-4 to women 15-49
#'
#' @param data Data frame
#' @param age_var Age variable
#' @param sex_var Sex variable
#' @param weight_var Weight variable (optional)
#'
#' @return Child-woman ratio
#' @export
child_woman_ratio <- function(data, age_var = "age", sex_var = "sexe", 
                               weight_var = NULL) {
  
  data$sex_std <- standardize_sex(data[[sex_var]])
  
  if (!is.null(weight_var) && weight_var %in% names(data)) {
    children <- sum(data[[weight_var]][data[[age_var]] < 5], na.rm = TRUE)
    women <- sum(data[[weight_var]][data$sex_std == "Female" & 
                                      data[[age_var]] >= 15 & 
                                      data[[age_var]] < 50], na.rm = TRUE)
  } else {
    children <- sum(data[[age_var]] < 5, na.rm = TRUE)
    women <- sum(data$sex_std == "Female" & 
                   data[[age_var]] >= 15 & 
                   data[[age_var]] < 50, na.rm = TRUE)
  }
  
  children / women * 1000
}

#' Gross Reproduction Rate
#'
#' @param asfr Vector of ASFR values
#' @param sex_ratio_birth Sex ratio at birth (males per 100 females)
#' @param age_interval Age interval width
#'
#' @return GRR value
#' @export
grr <- function(asfr, sex_ratio_birth = 105, age_interval = 5) {
  prop_female <- 100 / (100 + sex_ratio_birth)
  sum(asfr) * age_interval / 1000 * prop_female
}

#' Net Reproduction Rate
#'
#' @param asfr Vector of ASFR values
#' @param survival Vector of survival probabilities to each age group
#' @param sex_ratio_birth Sex ratio at birth
#' @param age_interval Age interval width
#'
#' @return NRR value
#' @export
nrr <- function(asfr, survival, sex_ratio_birth = 105, age_interval = 5) {
  prop_female <- 100 / (100 + sex_ratio_birth)
  sum(asfr * survival) * age_interval / 1000 * prop_female
}

#' Mean Age at Childbearing
#'
#' @param asfr Vector of ASFR values
#' @param age_midpoints Midpoints of age groups
#'
#' @return Mean age at childbearing
#' @export
mean_age_childbearing <- function(asfr, age_midpoints = c(17, 22, 27, 32, 37, 42, 47)) {
  sum(asfr * age_midpoints) / sum(asfr)
}
