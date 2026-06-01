% Generated for baobabStats 1.1.0
\name{bs_sae}
\alias{bs_sae}
\alias{bs_sae_agreger}
\title{Estimation sur petits domaines (Small Area Estimation)}
\usage{
bs_sae(data, domaine, approche = c("top_down", "bottom_up"), indicateur = NULL,
  total_superieur = NULL, niveau_superieur = NULL, poids_aux = NULL,
  par = c("age", "sexe", "handicap"), var_age = "age", var_sexe = "sexe",
  var_handicap = "handicap", caler = NULL, mapping = NULL)
bs_sae_agreger(sae, par = NULL)
}
\arguments{
\item{data}{Microdonnees ou estimations directes.}
\item{domaine}{Variable du petit domaine cible.}
\item{approche}{"top_down" ou "bottom_up".}
\item{indicateur}{Variable a estimer (NULL = effectifs).}
\item{total_superieur, niveau_superieur}{Total fiable de niveau superieur.}
\item{poids_aux}{Poids auxiliaire pour la repartition top-down.}
\item{par}{Axes de desagregation : age, sexe, handicap (les trois par defaut).}
\item{var_age, var_sexe, var_handicap}{Noms de colonnes des axes.}
\item{caler}{Caler les estimations bottom-up sur le total superieur.}
\item{mapping}{Liste role -> colonne.}
\item{sae}{Objet bs_sae a recombiner.}
}
\value{Objet bs_sae : estimations par domaine et croisements d'axes.}
\description{
Estimation sur petits domaines avec approches top-down (repartition) et
bottom-up (agregation + calage). Desagregation/agregation par age, sexe et
handicap (un axe, plusieurs, ou les trois actifs par defaut). Configurable via
la feuille Excel SAE.
}
