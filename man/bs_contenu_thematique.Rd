% Generated manually for baobabStats 1.0.1
\name{bs_contenu_thematique}
\alias{bs_contenu_thematique}
\title{Construire le contenu analytique d'une thematique}
\usage{
bs_contenu_thematique(data, theme, mapping = NULL, coefficients = NULL,
  var_strate = NULL, niveau = NULL, interpreter = TRUE)
}
\arguments{
\item{data}{data.frame harmonise.}
\item{theme}{Code de la thematique.}
\item{mapping}{Liste nommee role -> colonne.}
\item{coefficients}{Coefficients de redressement (optionnel).}
\item{var_strate}{Variable de strate pour le redressement (optionnel).}
\item{niveau}{Variable d'unite administrative pour la ventilation (optionnel).}
\item{interpreter}{Logique : produire les interpretations dynamiques.}
}
\value{Liste : titre, sections (chacune : titre, tableaux, graphiques, texte).}
\description{
Calcule les tableaux et graphiques d'une thematique et, pour les sorties
Word/HTML, les interpretations dynamiques associees.
}
