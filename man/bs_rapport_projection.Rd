% Generated manually for baobabStats 1.0.1
\name{bs_rapport_projection}
\alias{bs_rapport_projection}
\title{Rapport de projection de la population (Word/Excel/HTML, sans interpretation)}
\usage{
bs_rapport_projection(data, dossier = "rapports_thematiques",
  mapping = NULL, formats = c("word", "excel", "html"), horizon = 10,
  date_collecte = Sys.Date(), niveau_fin = NULL, methode = "cohort")
}
\arguments{
\item{data}{data.frame harmonise.}
\item{dossier}{Dossier de sortie.}
\item{mapping}{Liste role -> colonne.}
\item{formats}{Formats a produire.}
\item{horizon}{Nombre d'annees projetees (defaut 10).}
\item{date_collecte}{Date de collecte (origine ; defaut : aujourd'hui).}
\item{niveau_fin}{Variable d'unite administrative la plus fine.}
\item{methode}{Methode de projection ("cohort" par defaut).}
}
\value{Vecteur nomme des chemins produits (invisible).}
\description{
Projette la population par sexe et par l'unite administrative la plus fine sur
\code{horizon} annees a partir de la date de collecte, et produit les livrables.
Conformement a la specification, aucune interpretation dynamique n'est inseree
dans ce rapport.
}
\seealso{\code{\link{bs_rapports_thematiques}}}
