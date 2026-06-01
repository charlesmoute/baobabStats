% Generated manually for baobabStats 1.0.1
\name{bs_rapport_thematique}
\alias{bs_rapport_thematique}
\title{Generer un rapport thematique aux formats Word, Excel et HTML}
\usage{
bs_rapport_thematique(data, theme, dossier = "rapports_thematiques",
  mapping = NULL, coefficients = NULL, var_strate = NULL,
  formats = c("word", "excel", "html"), interpreter = TRUE)
}
\arguments{
\item{data}{data.frame harmonise.}
\item{theme}{Code de la thematique (voir \code{bs_themes_recensement()}).}
\item{dossier}{Dossier de sortie.}
\item{mapping}{Liste role -> colonne.}
\item{coefficients}{Coefficients de redressement (optionnel).}
\item{var_strate}{Variable de strate pour le redressement (optionnel).}
\item{formats}{Sous-ensemble de \code{c("word","excel","html")}.}
\item{interpreter}{Logique : interpretations dynamiques (Word/HTML). Force a FALSE
pour le theme \code{"projection"}.}
}
\value{Vecteur nomme des chemins produits (invisible).}
\description{
Produit, pour une thematique donnee, trois fichiers distincts (.docx, .xlsx,
.html). Les sorties Word et HTML integrent les interpretations dynamiques ;
l'Excel contient les tableaux. Les effectifs sont redresses si des coefficients
sont fournis.
}
\seealso{\code{\link{bs_rapports_thematiques}}, \code{\link{bs_themes_recensement}}}
