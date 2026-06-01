% Generated manually for baobabStats 1.0.1
\name{bs_rapports_thematiques}
\alias{bs_rapports_thematiques}
\title{Generer l'ensemble des rapports thematiques (et speciaux)}
\usage{
bs_rapports_thematiques(data, themes = NULL,
  dossier = "rapports_thematiques", mapping = NULL, post = NULL,
  formats = c("word", "excel", "html"), horizon_projection = 10,
  date_collecte = Sys.Date(), niveau_fin = NULL)
}
\arguments{
\item{data}{data.frame harmonise des microdonnees.}
\item{themes}{Vecteur de codes thematiques (defaut : toutes les thematiques).}
\item{dossier}{Dossier de sortie.}
\item{mapping}{Liste nommee role -> colonne (issue de la feuille Variables).}
\item{post}{Resultat de \code{bs_post_censitaire()} fournissant les coefficients
de redressement (optionnel).}
\item{formats}{Sous-ensemble de \code{c("word","excel","html")}.}
\item{horizon_projection}{Nombre d'annees de projection (defaut 10).}
\item{date_collecte}{Date de collecte (origine de la projection).}
\item{niveau_fin}{Variable d'unite administrative la plus fine pour la projection.}
}
\value{Liste nommee (par theme) des vecteurs de chemins produits (invisible).}
\description{
Boucle sur toutes les thematiques demandees et produit pour chacune les fichiers
Word, Excel et HTML. Gere aussi le rapport de qualite et le rapport de projection.
Reutilise les coefficients de redressement issus de l'enquete post-censitaire
lorsqu'ils existent. Les sorties Word et HTML integrent des interpretations
dynamiques ; le rapport de projection en est exempte.
}
\examples{
\dontrun{
data <- bs_collecter("individus.csv")
post <- bs_post_censitaire(data, pes = bs_collecter("pes.csv"))
bs_rapports_thematiques(data, post = post, dossier = "sorties")
}
}
\seealso{\code{\link{bs_rapport_thematique}}, \code{\link{bs_post_censitaire}}}
