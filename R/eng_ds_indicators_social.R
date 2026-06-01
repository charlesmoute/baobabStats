#' @title Social Indicators
#' @description Functions for migration, education, employment, disability and gender indicators
#' @name indicators_social
NULL

# ============================================================
# MIGRATION INDICATORS
# ============================================================

#' Migration Rates
#'
#' Calculate migration indicators
#'
#' @param data Data frame with migration data
#' @param migration_var Variable indicating migration status
#' @param origin_var Variable for place of origin
#' @param destination_var Variable for place of destination
#' @param duration_var Duration of residence variable
#' @param age_var Age variable
#' @param sex_var Sex variable
#' @param weight_var Weight variable
#' @param total_pop Total population (for rates)
#'
#' @return List of migration indicators
#' @export
migration_rates <- function(data, migration_var = "migrant",
                             origin_var = NULL, destination_var = NULL,
                             duration_var = NULL, age_var = "age",
                             sex_var = "sexe", weight_var = NULL,
                             total_pop = NULL) {
  
  # Calculate weights
  if (!is.null(weight_var) && weight_var %in% names(data)) {
    w <- data[[weight_var]]
  } else {
    w <- rep(1, nrow(data))
  }
  
  if (is.null(total_pop)) {
    total_pop <- sum(w, na.rm = TRUE)
  }
  
  # Migration status
  migrants <- data[[migration_var]] == 1 | data[[migration_var]] == TRUE |
    tolower(as.character(data[[migration_var]])) %in% c("yes", "oui", "migrant")
  
  n_migrants <- sum(w[migrants], na.rm = TRUE)
  migration_rate <- n_migrants / total_pop * 1000
  
  # By duration if available
  duration_dist <- NULL
  if (!is.null(duration_var) && duration_var %in% names(data)) {
    duration_dist <- tapply(w[migrants], data[[duration_var]][migrants], 
                            sum, na.rm = TRUE)
  }
  
  # By age and sex
  data$sex_std <- standardize_sex(data[[sex_var]])
  
  migration_by_age_sex <- aggregate(
    w[migrants],
    by = list(age_group = ds_create_age_groups(data[[age_var]][migrants]),
              sex = data$sex_std[migrants]),
    FUN = sum, na.rm = TRUE
  )
  names(migration_by_age_sex)[3] <- "n_migrants"
  
  # Calculate rates
  pop_by_age_sex <- aggregate(
    w,
    by = list(age_group = ds_create_age_groups(data[[age_var]]),
              sex = data$sex_std),
    FUN = sum, na.rm = TRUE
  )
  names(pop_by_age_sex)[3] <- "population"
  
  migration_by_age_sex <- merge(migration_by_age_sex, pop_by_age_sex,
                                 by = c("age_group", "sex"), all = TRUE)
  migration_by_age_sex$rate <- migration_by_age_sex$n_migrants / 
    migration_by_age_sex$population * 1000
  
  list(
    total_migrants = n_migrants,
    total_population = total_pop,
    migration_rate = migration_rate,
    percent_migrants = n_migrants / total_pop * 100,
    by_age_sex = migration_by_age_sex,
    by_duration = duration_dist
  )
}

#' Net Migration
#'
#' Calculate net migration from in and out migration
#'
#' @param in_migrants Number of in-migrants
#' @param out_migrants Number of out-migrants
#' @param population Mid-period population
#'
#' @return List with net migration and rate
#' @export
net_migration <- function(in_migrants, out_migrants, population) {
  net <- in_migrants - out_migrants
  rate <- net / population * 1000
  
  list(
    in_migrants = in_migrants,
    out_migrants = out_migrants,
    net_migration = net,
    net_migration_rate = rate,
    gross_migration = in_migrants + out_migrants,
    migration_effectiveness = abs(net) / (in_migrants + out_migrants) * 100
  )
}

# ============================================================
# EDUCATION INDICATORS
# ============================================================

#' Education Indicators
#'
#' Calculate education-related indicators
#'
#' @param data Data frame with education data
#' @param literacy_var Literacy variable
#' @param education_var Education level variable
#' @param attendance_var School attendance variable
#' @param age_var Age variable
#' @param sex_var Sex variable
#' @param weight_var Weight variable
#'
#' @return List of education indicators
#' @export
education_indicators <- function(data, literacy_var = "alphabetise",
                                  education_var = "niveau_instruction",
                                  attendance_var = "frequentation",
                                  age_var = "age", sex_var = "sexe",
                                  weight_var = NULL) {
  
  results <- list()
  
  # Weights
  if (!is.null(weight_var) && weight_var %in% names(data)) {
    w <- data[[weight_var]]
  } else {
    w <- rep(1, nrow(data))
  }
  
  data$sex_std <- standardize_sex(data[[sex_var]])
  
  # Literacy rate (15+)
  if (literacy_var %in% names(data)) {
    adults <- data[[age_var]] >= 15
    literate <- data[[literacy_var]] == 1 | data[[literacy_var]] == TRUE |
      tolower(as.character(data[[literacy_var]])) %in% c("yes", "oui", "literate")
    
    results$literacy <- list(
      total = sum(w[adults & literate], na.rm = TRUE) / 
        sum(w[adults], na.rm = TRUE) * 100,
      male = sum(w[adults & literate & data$sex_std == "Male"], na.rm = TRUE) /
        sum(w[adults & data$sex_std == "Male"], na.rm = TRUE) * 100,
      female = sum(w[adults & literate & data$sex_std == "Female"], na.rm = TRUE) /
        sum(w[adults & data$sex_std == "Female"], na.rm = TRUE) * 100
    )
    results$literacy$gender_gap <- results$literacy$male - results$literacy$female
  }
  
  # Education level distribution
  if (education_var %in% names(data)) {
    edu_dist <- tapply(w, data[[education_var]], sum, na.rm = TRUE)
    edu_dist <- edu_dist / sum(edu_dist) * 100
    results$education_distribution <- edu_dist
    
    # By sex
    results$education_by_sex <- list(
      male = tapply(w[data$sex_std == "Male"], 
                    data[[education_var]][data$sex_std == "Male"],
                    sum, na.rm = TRUE),
      female = tapply(w[data$sex_std == "Female"],
                      data[[education_var]][data$sex_std == "Female"],
                      sum, na.rm = TRUE)
    )
  }
  
  # School attendance
  if (attendance_var %in% names(data)) {
    results$attendance <- school_attendance(data, attendance_var, age_var, 
                                             sex_var, weight_var)
  }
  
  return(results)
}

#' Literacy Rate
#'
#' @param literate Number of literate persons
#' @param population Total population (usually 15+)
#'
#' @return Literacy rate as percentage
#' @export
literacy_rate <- function(literate, population) {
  literate / population * 100
}

#' School Attendance Rates
#'
#' Calculate gross and net enrollment/attendance rates
#'
#' @param data Data frame
#' @param attendance_var Attendance variable
#' @param age_var Age variable
#' @param sex_var Sex variable
#' @param weight_var Weight variable
#' @param primary_ages Age range for primary (default 6-11)
#' @param secondary_ages Age range for secondary (default 12-17)
#'
#' @return List of attendance rates
#' @export
school_attendance <- function(data, attendance_var = "frequentation",
                               age_var = "age", sex_var = "sexe",
                               weight_var = NULL,
                               primary_ages = c(6, 11),
                               secondary_ages = c(12, 17)) {
  
  if (!is.null(weight_var) && weight_var %in% names(data)) {
    w <- data[[weight_var]]
  } else {
    w <- rep(1, nrow(data))
  }
  
  data$sex_std <- standardize_sex(data[[sex_var]])
  
  attending <- data[[attendance_var]] == 1 | data[[attendance_var]] == TRUE |
    tolower(as.character(data[[attendance_var]])) %in% c("yes", "oui")
  
  # Primary school age
  primary_age <- data[[age_var]] >= primary_ages[1] & 
    data[[age_var]] <= primary_ages[2]
  
  # Secondary school age
  secondary_age <- data[[age_var]] >= secondary_ages[1] & 
    data[[age_var]] <= secondary_ages[2]
  
  # Net attendance rates
  nar_primary <- sum(w[primary_age & attending], na.rm = TRUE) /
    sum(w[primary_age], na.rm = TRUE) * 100
  
  nar_secondary <- sum(w[secondary_age & attending], na.rm = TRUE) /
    sum(w[secondary_age], na.rm = TRUE) * 100
  
  # By sex
  nar_primary_m <- sum(w[primary_age & attending & data$sex_std == "Male"], na.rm = TRUE) /
    sum(w[primary_age & data$sex_std == "Male"], na.rm = TRUE) * 100
  nar_primary_f <- sum(w[primary_age & attending & data$sex_std == "Female"], na.rm = TRUE) /
    sum(w[primary_age & data$sex_std == "Female"], na.rm = TRUE) * 100
  
  nar_secondary_m <- sum(w[secondary_age & attending & data$sex_std == "Male"], na.rm = TRUE) /
    sum(w[secondary_age & data$sex_std == "Male"], na.rm = TRUE) * 100
  nar_secondary_f <- sum(w[secondary_age & attending & data$sex_std == "Female"], na.rm = TRUE) /
    sum(w[secondary_age & data$sex_std == "Female"], na.rm = TRUE) * 100
  
  # Gender Parity Index
  gpi_primary <- nar_primary_f / nar_primary_m
  gpi_secondary <- nar_secondary_f / nar_secondary_m
  
  list(
    primary = list(
      total = nar_primary,
      male = nar_primary_m,
      female = nar_primary_f,
      gpi = gpi_primary
    ),
    secondary = list(
      total = nar_secondary,
      male = nar_secondary_m,
      female = nar_secondary_f,
      gpi = gpi_secondary
    )
  )
}

# ============================================================
# EMPLOYMENT INDICATORS
# ============================================================

#' Employment Indicators
#'
#' Calculate employment-related indicators
#'
#' @param data Data frame with employment data
#' @param activity_var Activity status variable
#' @param occupation_var Occupation variable
#' @param sector_var Economic sector variable
#' @param age_var Age variable
#' @param sex_var Sex variable
#' @param weight_var Weight variable
#' @param working_age Working age range (default 15-64)
#'
#' @return List of employment indicators
#' @export
employment_indicators <- function(data, activity_var = "situation_activite",
                                   occupation_var = NULL, sector_var = NULL,
                                   age_var = "age", sex_var = "sexe",
                                   weight_var = NULL, working_age = c(15, 64)) {
  
  if (!is.null(weight_var) && weight_var %in% names(data)) {
    w <- data[[weight_var]]
  } else {
    w <- rep(1, nrow(data))
  }
  
  data$sex_std <- standardize_sex(data[[sex_var]])
  
  # Working age population
  wap <- data[[age_var]] >= working_age[1] & data[[age_var]] <= working_age[2]
  
  # Define activity categories (adapt to your coding)
  activity <- tolower(as.character(data[[activity_var]]))
  employed <- activity %in% c("employed", "occupe", "working", "1", "travaille")
  unemployed <- activity %in% c("unemployed", "chomeur", "seeking", "2", "chomage")
  inactive <- activity %in% c("inactive", "inactif", "3", "student", "retired", "etudiant")
  
  # If numeric coding
  if (is.numeric(data[[activity_var]])) {
    employed <- data[[activity_var]] == 1
    unemployed <- data[[activity_var]] == 2
    inactive <- data[[activity_var]] >= 3 | is.na(data[[activity_var]])
  }
  
  # Labor force
  labor_force <- employed | unemployed
  
  # Total working age population
  total_wap <- sum(w[wap], na.rm = TRUE)
  total_employed <- sum(w[wap & employed], na.rm = TRUE)
  total_unemployed <- sum(w[wap & unemployed], na.rm = TRUE)
  total_lf <- sum(w[wap & labor_force], na.rm = TRUE)
  
  # Rates
  activity_rate <- total_lf / total_wap * 100
  employment_rate <- total_employed / total_wap * 100
  unemployment_rate <- total_unemployed / total_lf * 100
  
  # By sex
  male_wap <- wap & data$sex_std == "Male"
  female_wap <- wap & data$sex_std == "Female"
  
  results <- list(
    working_age_population = total_wap,
    labor_force = total_lf,
    employed = total_employed,
    unemployed = total_unemployed,
    
    activity_rate = list(
      total = activity_rate,
      male = sum(w[male_wap & labor_force], na.rm = TRUE) / 
        sum(w[male_wap], na.rm = TRUE) * 100,
      female = sum(w[female_wap & labor_force], na.rm = TRUE) / 
        sum(w[female_wap], na.rm = TRUE) * 100
    ),
    
    employment_rate = list(
      total = employment_rate,
      male = sum(w[male_wap & employed], na.rm = TRUE) / 
        sum(w[male_wap], na.rm = TRUE) * 100,
      female = sum(w[female_wap & employed], na.rm = TRUE) / 
        sum(w[female_wap], na.rm = TRUE) * 100
    ),
    
    unemployment_rate = list(
      total = unemployment_rate,
      male = sum(w[male_wap & unemployed], na.rm = TRUE) / 
        sum(w[male_wap & labor_force], na.rm = TRUE) * 100,
      female = sum(w[female_wap & unemployed], na.rm = TRUE) / 
        sum(w[female_wap & labor_force], na.rm = TRUE) * 100
    )
  )
  
  # By sector if available
  if (!is.null(sector_var) && sector_var %in% names(data)) {
    results$by_sector <- tapply(w[wap & employed], 
                                 data[[sector_var]][wap & employed],
                                 sum, na.rm = TRUE)
  }
  
  # By occupation if available
  if (!is.null(occupation_var) && occupation_var %in% names(data)) {
    results$by_occupation <- tapply(w[wap & employed],
                                     data[[occupation_var]][wap & employed],
                                     sum, na.rm = TRUE)
  }
  
  return(results)
}

#' Activity Rate
#'
#' @param labor_force Labor force size
#' @param working_age_pop Working age population
#'
#' @return Activity rate as percentage
#' @export
activity_rate <- function(labor_force, working_age_pop) {
  labor_force / working_age_pop * 100
}

#' Unemployment Rate
#'
#' @param unemployed Number of unemployed
#' @param labor_force Labor force size
#'
#' @return Unemployment rate as percentage
#' @export
unemployment_rate <- function(unemployed, labor_force) {
  unemployed / labor_force * 100
}

# ============================================================
# DISABILITY INDICATORS
# ============================================================

#' Disability Prevalence
#'
#' Calculate disability prevalence rates
#'
#' @param data Data frame with disability data
#' @param disability_var Disability variable(s)
#' @param age_var Age variable
#' @param sex_var Sex variable
#' @param weight_var Weight variable
#' @param domains Washington Group domains (optional)
#'
#' @return List of disability indicators
#' @export
disability_prevalence <- function(data, disability_var = "handicap",
                                   age_var = "age", sex_var = "sexe",
                                   weight_var = NULL, domains = NULL) {
  
  if (!is.null(weight_var) && weight_var %in% names(data)) {
    w <- data[[weight_var]]
  } else {
    w <- rep(1, nrow(data))
  }
  
  data$sex_std <- standardize_sex(data[[sex_var]])
  total_pop <- sum(w, na.rm = TRUE)
  
  # Overall disability
  if (is.character(disability_var) && length(disability_var) == 1) {
    disabled <- data[[disability_var]] == 1 | data[[disability_var]] == TRUE |
      tolower(as.character(data[[disability_var]])) %in% c("yes", "oui", "handicape")
    
    # If numeric scale (Washington Group style: 1-4)
    if (is.numeric(data[[disability_var]])) {
      # "A lot of difficulty" or "Cannot do at all" (3 or 4)
      disabled <- data[[disability_var]] >= 3
    }
  } else {
    # Multiple domains
    disabled <- rep(FALSE, nrow(data))
    for (var in disability_var) {
      if (var %in% names(data)) {
        if (is.numeric(data[[var]])) {
          disabled <- disabled | (data[[var]] >= 3)
        } else {
          disabled <- disabled | (data[[var]] == 1 | data[[var]] == TRUE)
        }
      }
    }
  }
  
  # Overall prevalence
  overall <- sum(w[disabled], na.rm = TRUE) / total_pop * 100
  
  # By sex
  male_prev <- sum(w[disabled & data$sex_std == "Male"], na.rm = TRUE) /
    sum(w[data$sex_std == "Male"], na.rm = TRUE) * 100
  female_prev <- sum(w[disabled & data$sex_std == "Female"], na.rm = TRUE) /
    sum(w[data$sex_std == "Female"], na.rm = TRUE) * 100
  
  # By age group
  data$age_group <- ds_create_age_groups(data[[age_var]])
  by_age <- tapply(w[disabled], data$age_group[disabled], sum, na.rm = TRUE)
  pop_by_age <- tapply(w, data$age_group, sum, na.rm = TRUE)
  prev_by_age <- by_age / pop_by_age * 100
  
  # By domain if Washington Group questions
  by_domain <- NULL
  if (!is.null(domains) && all(domains %in% names(data))) {
    by_domain <- sapply(domains, function(d) {
      if (is.numeric(data[[d]])) {
        sum(w[data[[d]] >= 3], na.rm = TRUE) / total_pop * 100
      } else {
        sum(w[data[[d]] == 1 | data[[d]] == TRUE], na.rm = TRUE) / total_pop * 100
      }
    })
  }
  
  list(
    total_disabled = sum(w[disabled], na.rm = TRUE),
    total_population = total_pop,
    prevalence = list(
      total = overall,
      male = male_prev,
      female = female_prev
    ),
    by_age = prev_by_age,
    by_domain = by_domain
  )
}

# ============================================================
# GENDER INDICATORS
# ============================================================

#' Gender Indicators
#'
#' Calculate gender-related indicators
#'
#' @param data Data frame
#' @param indicators List of indicators to calculate
#' @param sex_var Sex variable
#' @param age_var Age variable
#' @param weight_var Weight variable
#' @param ... Additional variables for specific indicators
#'
#' @return List of gender indicators
#' @export
gender_indicators <- function(data, indicators = c("sex_ratio", "gpi", "lfp_gap"),
                               sex_var = "sexe", age_var = "age",
                               weight_var = NULL, ...) {
  
  results <- list()
  args <- list(...)
  
  if (!is.null(weight_var) && weight_var %in% names(data)) {
    w <- data[[weight_var]]
  } else {
    w <- rep(1, nrow(data))
  }
  
  data$sex_std <- standardize_sex(data[[sex_var]])
  
  # Sex ratio
  if ("sex_ratio" %in% indicators) {
    males <- sum(w[data$sex_std == "Male"], na.rm = TRUE)
    females <- sum(w[data$sex_std == "Female"], na.rm = TRUE)
    results$sex_ratio <- males / females * 100
    results$percent_female <- females / (males + females) * 100
  }
  
  # Gender Parity Index for education
  if ("gpi" %in% indicators && !is.null(args$attendance_var)) {
    att <- school_attendance(data, args$attendance_var, age_var, sex_var, weight_var)
    results$gpi_primary <- att$primary$gpi
    results$gpi_secondary <- att$secondary$gpi
  }
  
  # Labor force participation gap
  if ("lfp_gap" %in% indicators && !is.null(args$activity_var)) {
    emp <- employment_indicators(data, args$activity_var, age_var = age_var,
                                  sex_var = sex_var, weight_var = weight_var)
    results$lfp_male <- emp$activity_rate$male
    results$lfp_female <- emp$activity_rate$female
    results$lfp_gap <- emp$activity_rate$male - emp$activity_rate$female
  }
  
  # Female-headed households
  if ("fhh" %in% indicators && !is.null(args$head_var) && !is.null(args$hh_id)) {
    # Get household heads
    heads <- data[data[[args$head_var]] == 1, ]
    if (!is.null(weight_var)) {
      wh <- heads[[weight_var]]
    } else {
      wh <- rep(1, nrow(heads))
    }
    heads$sex_std <- standardize_sex(heads[[sex_var]])
    
    results$female_headed_hh <- sum(wh[heads$sex_std == "Female"], na.rm = TRUE) /
      sum(wh, na.rm = TRUE) * 100
  }
  
  return(results)
}

#' Gender Parity Index
#'
#' @param female_value Female indicator value
#' @param male_value Male indicator value
#'
#' @return GPI value
#' @export
gpi <- function(female_value, male_value) {
  female_value / male_value
}
