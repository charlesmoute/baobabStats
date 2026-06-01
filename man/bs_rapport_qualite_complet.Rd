% Generated manually for baobabStats 1.0.1
\name{bs_rapport_qualite_complet}
\alias{bs_rapport_qualite_complet}
\title{Rapport d'evaluation de la qualite des donnees (Word/Excel/HTML)}
\usage{
bs_rapport_qualite_complet(data, dossier = "rapports_thematiques",
  mapping = NULL, post = NULL, formats = c("word", "excel", "html"))
}
\arguments{
\item{data}{data.frame harmonise.}
\item{dossier}{Dossier de sortie.}
\item{mapping}{Liste role -> colonne.}
\item{post}{Resultat de \code{bs_post_censitaire()} (couverture PES + redressement).}
\item{formats}{Formats a produire.}
}
\value{Vecteur nomme des chemins produits (invisible).}
\description{
Rapport d'evaluation de la qualite : indices d'attraction d'age (Whipple, Myers,
Bachi), completude des variables, couverture post-censitaire et coefficients de
redressement. Les sorties Word et HTML integrent des interpretations dynamiques.
}
