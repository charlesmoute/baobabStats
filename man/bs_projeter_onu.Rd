% Generated for baobabStats 1.1.0
\name{bs_projeter_onu}
\alias{bs_projeter_onu}
\title{Projection par la methode des cohortes-composantes (ONU)}
\usage{
bs_projeter_onu(population, survie = NULL, fecondite = NULL, migration = NULL,
  horizon = 10, pas = 5, sex_ratio_naissance = 1.03,
  var_age = "age", var_sexe = "sexe", var_effectif = "effectif")
}
\arguments{
\item{population}{Population de base par age et sexe (ou microdonnees).}
\item{survie, fecondite, migration}{Composantes demographiques (schemas types par defaut).}
\item{horizon}{Nombre d'annees a projeter.}
\item{pas}{Intervalle (1 ou 5 ans).}
\item{sex_ratio_naissance}{Rapport de masculinite a la naissance.}
\item{var_age, var_sexe, var_effectif}{Noms de colonnes.}
}
\value{Objet bs_projection_onu : trajectoire par annee/age/sexe et resume.}
\description{
Methode des composantes par cohorte (standard Nations Unies / World Population
Prospects). Applique separement fecondite, mortalite (survie) et migration, pas
a pas. Complete la fonction bs_projeter_population() existante.
}
