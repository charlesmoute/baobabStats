#' @title Mortality Indicators
#' @description Functions for computing mortality rates and life tables
#' @name indicators_mortality
NULL

#' Mortality Rates
#'
#' Calculate various mortality rates
#'
#' @param data Data frame with deaths and population data
#' @param deaths_var Variable indicating deaths
#' @param age_var Age variable
#' @param sex_var Sex variable (optional)
#' @param weight_var Weight variable (optional)
#' @param reference_period Reference period in years
#'
#' @return List of mortality indicators
#' @export
mortality_rates <- function(data, deaths_var = "deaths_12m", age_var = "age",
                             sex_var = NULL, weight_var = NULL,
                             reference_period = 1) {
  
  # Total population and deaths
  if (!is.null(weight_var) && weight_var %in% names(data)) {
    total_pop <- sum(data[[weight_var]], na.rm = TRUE)
    total_deaths <- sum(data[[deaths_var]] * data[[weight_var]], na.rm = TRUE)
  } else {
    total_pop <- nrow(data)
    total_deaths <- sum(data[[deaths_var]], na.rm = TRUE)
  }
  
  # CDR
  cdr <- total_deaths / total_pop * 1000 / reference_period
  
  # Age-specific mortality rates
  data$age_group <- create_mortality_age_groups(data[[age_var]])
  
  if (!is.null(weight_var) && weight_var %in% names(data)) {
    asmr_data <- aggregate(
      cbind(deaths = data[[deaths_var]] * data[[weight_var]], 
            pop = data[[weight_var]]),
      by = list(age_group = data$age_group),
      FUN = sum, na.rm = TRUE
    )
  } else {
    asmr_data <- data.frame(
      age_group = levels(data$age_group),
      deaths = tapply(data[[deaths_var]], data$age_group, sum, na.rm = TRUE),
      pop = tapply(rep(1, nrow(data)), data$age_group, sum)
    )
  }
  
  asmr_data$asmr <- asmr_data$deaths / asmr_data$pop * 1000 / reference_period
  
  # IMR (Infant Mortality Rate)
  infants <- data[data[[age_var]] < 1, ]
  if (!is.null(weight_var) && weight_var %in% names(data)) {
    infant_deaths <- sum(infants[[deaths_var]] * infants[[weight_var]], na.rm = TRUE)
    births_approx <- sum(data[[weight_var]][data[[age_var]] < 1], na.rm = TRUE)
  } else {
    infant_deaths <- sum(infants[[deaths_var]], na.rm = TRUE)
    births_approx <- nrow(infants)
  }
  imr <- infant_deaths / births_approx * 1000 / reference_period
  
  # Under-5 mortality
  under5 <- data[data[[age_var]] < 5, ]
  if (!is.null(weight_var) && weight_var %in% names(data)) {
    u5_deaths <- sum(under5[[deaths_var]] * under5[[weight_var]], na.rm = TRUE)
    u5_pop <- sum(data[[weight_var]][data[[age_var]] < 5], na.rm = TRUE)
  } else {
    u5_deaths <- sum(under5[[deaths_var]], na.rm = TRUE)
    u5_pop <- nrow(under5)
  }
  u5mr <- u5_deaths / u5_pop * 1000 / reference_period
  
  list(
    cdr = cdr,
    imr = imr,
    u5mr = u5mr,
    asmr = asmr_data,
    total_deaths = total_deaths,
    total_population = total_pop,
    reference_period = reference_period
  )
}

#' Create Mortality Age Groups
#' @keywords internal
create_mortality_age_groups <- function(ages) {
  breaks <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, Inf)
  labels <- c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
              "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
              "70-74", "75-79", "80-84", "85+")
  cut(ages, breaks = breaks, labels = labels, right = FALSE)
}

#' Life Table Construction
#'
#' Construct abridged life table from age-specific mortality rates
#'
#' @param nMx Vector of age-specific mortality rates (nMx)
#' @param age_groups Vector of age group labels
#' @param n Vector of age interval widths
#' @param ax Vector of average years lived in interval by those dying (optional)
#' @param sex "male", "female", or "both" for ax estimation
#' @param radix Radix of life table (default 100000)
#'
#' @return Life table data frame
#' @export
life_table <- function(nMx, age_groups = NULL, n = NULL, ax = NULL, 
                        sex = "both", radix = 100000) {
  
  k <- length(nMx)
  
  # Default age groups
  if (is.null(age_groups)) {
    age_groups <- c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                    "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                    "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
    age_groups <- age_groups[1:k]
  }
  
  # Default interval widths
  if (is.null(n)) {
    n <- c(1, 4, rep(5, k - 2))
    n[k] <- NA  # Open interval
  }
  
  # Estimate ax if not provided
  if (is.null(ax)) {
    ax <- estimate_ax(nMx, n, sex)
  }
  
  # Calculate nqx (probability of dying)
  nqx <- numeric(k)
  for (i in 1:(k-1)) {
    nqx[i] <- (n[i] * nMx[i]) / (1 + (n[i] - ax[i]) * nMx[i])
  }
  nqx[k] <- 1  # Everyone dies in open interval
  
  # Ensure nqx is between 0 and 1
  nqx <- pmin(pmax(nqx, 0), 1)
  
  # Calculate lx (survivors)
  lx <- numeric(k)
  lx[1] <- radix
  for (i in 2:k) {
    lx[i] <- lx[i-1] * (1 - nqx[i-1])
  }
  
  # Calculate ndx (deaths)
  ndx <- numeric(k)
  for (i in 1:(k-1)) {
    ndx[i] <- lx[i] - lx[i+1]
  }
  ndx[k] <- lx[k]
  
  # Calculate nLx (person-years lived)
  nLx <- numeric(k)
  for (i in 1:(k-1)) {
    nLx[i] <- n[i] * lx[i+1] + ax[i] * ndx[i]
  }
  # Open interval
  if (nMx[k] > 0) {
    nLx[k] <- lx[k] / nMx[k]
  } else {
    nLx[k] <- 0
  }
  
  # Calculate Tx (total person-years)
  Tx <- numeric(k)
  Tx[k] <- nLx[k]
  for (i in (k-1):1) {
    Tx[i] <- Tx[i+1] + nLx[i]
  }
  
  # Calculate ex (life expectancy)
  ex <- Tx / lx
  
  # Create life table
  lt <- data.frame(
    age_group = age_groups,
    n = n,
    nMx = round(nMx, 6),
    nax = round(ax, 3),
    nqx = round(nqx, 6),
    lx = round(lx, 0),
    ndx = round(ndx, 0),
    nLx = round(nLx, 0),
    Tx = round(Tx, 0),
    ex = round(ex, 2)
  )
  
  class(lt) <- c("life_table", class(lt))
  return(lt)
}

#' Estimate ax values
#' @keywords internal
estimate_ax <- function(nMx, n, sex = "both") {
  
  k <- length(nMx)
  ax <- numeric(k)
  
  # Coale-Demeny estimation for infant mortality
  if (sex == "male") {
    if (nMx[1] >= 0.107) {
      ax[1] <- 0.330
    } else {
      ax[1] <- 0.045 + 2.684 * nMx[1]
    }
  } else if (sex == "female") {
    if (nMx[1] >= 0.107) {
      ax[1] <- 0.350
    } else {
      ax[1] <- 0.053 + 2.800 * nMx[1]
    }
  } else {
    # Average
    if (nMx[1] >= 0.107) {
      ax[1] <- 0.340
    } else {
      ax[1] <- 0.049 + 2.742 * nMx[1]
    }
  }
  
  # 1-4 age group
  if (k > 1) {
    if (sex == "male") {
      if (nMx[1] >= 0.107) {
        ax[2] <- 1.352
      } else {
        ax[2] <- 1.651 - 2.816 * nMx[1]
      }
    } else if (sex == "female") {
      if (nMx[1] >= 0.107) {
        ax[2] <- 1.361
      } else {
        ax[2] <- 1.522 - 1.518 * nMx[1]
      }
    } else {
      if (nMx[1] >= 0.107) {
        ax[2] <- 1.356
      } else {
        ax[2] <- 1.587 - 2.167 * nMx[1]
      }
    }
  }
  
  # Other age groups: assume deaths occur at midpoint
  for (i in 3:(k-1)) {
    ax[i] <- n[i] / 2
  }
  
  # Open interval: use reciprocal of mortality rate
  if (nMx[k] > 0) {
    ax[k] <- 1 / nMx[k]
  } else {
    ax[k] <- 5  # Default
  }
  
  return(ax)
}

#' Infant Mortality Rate
#'
#' @param infant_deaths Deaths under age 1
#' @param live_births Live births in same period
#'
#' @return IMR per 1000 live births
#' @export
imr <- function(infant_deaths, live_births) {
  infant_deaths / live_births * 1000
}

#' Child Mortality Rate (1-4)
#'
#' @param child_deaths Deaths ages 1-4
#' @param children_1_4 Population ages 1-4
#' @param period Reference period
#'
#' @return CMR per 1000
#' @export
cmr <- function(child_deaths, children_1_4, period = 1) {
  child_deaths / children_1_4 / period * 1000
}

#' Crude Death Rate
#'
#' @param deaths Total deaths
#' @param population Total population
#' @param period Reference period
#'
#' @return CDR per 1000
#' @export
cdr <- function(deaths, population, period = 1) {
  deaths / population / period * 1000
}

#' Under-5 Mortality Rate
#'
#' @param u5_deaths Deaths under age 5
#' @param live_births Live births
#'
#' @return U5MR per 1000
#' @export
u5mr <- function(u5_deaths, live_births) {
  u5_deaths / live_births * 1000
}

#' Maternal Mortality Ratio
#'
#' @param maternal_deaths Maternal deaths
#' @param live_births Live births
#'
#' @return MMR per 100,000 live births
#' @export
mmr <- function(maternal_deaths, live_births) {
  maternal_deaths / live_births * 100000
}

#' Neonatal Mortality Rate
#'
#' @param neonatal_deaths Deaths in first 28 days
#' @param live_births Live births
#'
#' @return NMR per 1000
#' @export
nmr <- function(neonatal_deaths, live_births) {
  neonatal_deaths / live_births * 1000
}

#' Post-Neonatal Mortality Rate
#'
#' @param pnm_deaths Deaths 28 days to 1 year
#' @param live_births Live births
#'
#' @return PNMR per 1000
#' @export
pnmr <- function(pnm_deaths, live_births) {
  pnm_deaths / live_births * 1000
}

#' Stillbirth Rate
#'
#' @param stillbirths Number of stillbirths
#' @param total_births Total births (live + still)
#'
#' @return Stillbirth rate per 1000
#' @export
stillbirth_rate <- function(stillbirths, total_births) {
  stillbirths / total_births * 1000
}

#' Perinatal Mortality Rate
#'
#' @param stillbirths Number of stillbirths
#' @param early_neonatal Deaths in first 7 days
#' @param total_births Total births
#'
#' @return PMR per 1000
#' @export
perinatal_mr <- function(stillbirths, early_neonatal, total_births) {
  (stillbirths + early_neonatal) / total_births * 1000
}

#' Age-Standardized Death Rate
#'
#' @param deaths Vector of deaths by age group
#' @param population Vector of population by age group
#' @param standard Vector of standard population by age group
#'
#' @return Age-standardized death rate per 1000
#' @export
asdr <- function(deaths, population, standard) {
  
  # Age-specific rates
  asmr <- deaths / population
  
  # Standardized rate
  std_rate <- sum(asmr * standard) / sum(standard) * 1000
  
  return(std_rate)
}

#' Years of Life Lost
#'
#' @param deaths Vector of deaths by age
#' @param ages Vector of ages at death
#' @param life_expectancy Reference life expectancy (default 80)
#'
#' @return Total YLL
#' @export
yll <- function(deaths, ages, life_expectancy = 80) {
  sum(deaths * pmax(0, life_expectancy - ages))
}

#' Print method for life_table
#' @export
print.life_table <- function(x, ...) {
  cat("\n========================================\n")
  cat("           LIFE TABLE\n")
  cat("========================================\n\n")
  
  cat("Life expectancy at birth (e0):", x$ex[1], "years\n\n")
  
  print(as.data.frame(x), row.names = FALSE)
}
