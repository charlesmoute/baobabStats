% Generated for baobabStats 1.1.0
\name{bs_agreger}
\alias{bs_agreger}
\alias{bs_as_dt}
\alias{bs_as_sortie}
\alias{bs_fread}
\alias{bs_moteur_calcul}
\title{Architecture hybride data.table / tibble}
\usage{
bs_agreger(data, by, mesure = NULL, fun = NULL, poids = NULL)
bs_as_dt(x)
bs_as_sortie(x)
bs_fread(chemin, ...)
bs_moteur_calcul()
}
\arguments{
\item{data}{data.frame a agreger.}
\item{by}{Vecteur de noms de variables de regroupement.}
\item{mesure}{Variable a agreger (NULL = comptage).}
\item{fun}{"somme", "moyenne" ou "n".}
\item{poids}{Variable de ponderation (optionnel).}
\item{x}{Objet a convertir.}
\item{chemin}{Chemin du fichier a lire.}
\item{...}{Arguments transmis a fread/read.csv.}
}
\value{Un tibble agrege (bs_agreger) ; un data.table (bs_as_dt) ; un tibble
(bs_as_sortie) ; un data.frame (bs_fread) ; une liste de diagnostic (bs_moteur_calcul).}
\description{
Architecture hybride : data.table en interne pour les calculs lourds (gros
volumes de recensement), tibble en sortie pour la compatibilite tidyverse.
Repli automatique sur base/dplyr si data.table est absent.
}
