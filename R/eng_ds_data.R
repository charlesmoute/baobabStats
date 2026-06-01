#' Census Demonstration Data
#'
#' A simulated census dataset for demonstration purposes.
#'
#' @format A data frame with 5000 rows and 17 variables:
#' \describe{
#'   \item{census_id}{Unique identifier}
#'   \item{ea_code}{Enumeration area code}
#'   \item{region}{Geographic region (Nord, Sud, Est, Ouest, Centre)}
#'   \item{milieu}{Urban/Rural (Urbain, Rural)}
#'   \item{nom}{Family name}
#'   \item{prenom}{First name}
#'   \item{sexe}{Sex (M, F)}
#'   \item{age}{Age in years}
#'   \item{etat_matrimonial}{Marital status}
#'   \item{niveau_instruction}{Education level}
#'   \item{alphabetise}{Literacy status (0/1)}
#'   \item{situation_activite}{Activity status}
#'   \item{handicap}{Disability status (0/1)}
#'   \item{migrant}{Migration status (0/1)}
#'   \item{naissances_12m}{Births in last 12 months}
#'   \item{deces_12m}{Deaths in last 12 months}
#' }
#' @source Simulated data for demonstration
"census_demo"

#' PES Demonstration Data
#'
#' A simulated Post Enumeration Survey dataset for demonstration purposes.
#' Contains records from a subset of enumeration areas plus some omissions.
#'
#' @format A data frame with approximately 2000 rows and 18 variables:
#' \describe{
#'   \item{pes_id}{Unique PES identifier}
#'   \item{census_id}{Corresponding census ID (NA for omissions)}
#'   \item{ea_code}{Enumeration area code}
#'   \item{region}{Geographic region}
#'   \item{milieu}{Urban/Rural}
#'   \item{nom}{Family name}
#'   \item{prenom}{First name}
#'   \item{sexe}{Sex (M, F)}
#'   \item{age}{Age in years}
#'   \item{etat_matrimonial}{Marital status}
#'   \item{niveau_instruction}{Education level}
#'   \item{alphabetise}{Literacy status (0/1)}
#'   \item{situation_activite}{Activity status}
#'   \item{handicap}{Disability status (0/1)}
#'   \item{migrant}{Migration status (0/1)}
#'   \item{naissances_12m}{Births in last 12 months}
#'   \item{deces_12m}{Deaths in last 12 months}
#' }
#' @source Simulated data for demonstration
"pes_demo"

#' Back-check Demonstration Data
#'
#' A simulated back-check dataset for quality control demonstration.
#' Contains re-interviews with some intentional discrepancies.
#'
#' @format A data frame with 500 rows and 8 variables:
#' \describe{
#'   \item{census_id}{Corresponding census ID}
#'   \item{ea_code}{Enumeration area code}
#'   \item{nom}{Family name}
#'   \item{prenom}{First name}
#'   \item{sexe}{Sex (M, F)}
#'   \item{age}{Age in years}
#'   \item{niveau_instruction}{Education level}
#'   \item{enumerator}{Enumerator ID}
#' }
#' @source Simulated data for demonstration
"backcheck_demo"
