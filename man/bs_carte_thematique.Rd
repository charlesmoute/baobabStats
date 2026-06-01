% Generated for baobabStats 1.1.0
\name{bs_carte_thematique}
\alias{bs_carte_thematique}
\alias{bs_lire_shapefile}
\alias{bs_cartes_config}
\title{Cartes thematiques (choropleths)}
\usage{
bs_carte_thematique(donnees, shapefile, var_fusion, var_fusion_carte = NULL,
  indicateur, titre = NULL, palette = "sequentiel", classes = 5)
bs_lire_shapefile(chemin, couche = NULL)
bs_cartes_config(data, carto, dossier = "cartes", mapping = NULL)
}
\arguments{
\item{donnees}{data.frame avec variable de fusion et indicateur.}
\item{shapefile}{Dossier/fichier du fond de carte, ou objet sf.}
\item{var_fusion}{Variable de jointure cote donnees.}
\item{var_fusion_carte}{Variable de jointure cote shapefile.}
\item{indicateur}{Variable a representer.}
\item{titre, palette, classes}{Options de rendu.}
\item{chemin, couche}{Chemin et couche du fond de carte.}
\item{data, carto, dossier, mapping}{Production en lot pilotee par Excel.}
}
\value{Un objet ggplot (carte), un objet sf (lecture), ou les chemins produits.}
\description{
Cartes choroplethes a partir d'un fond de carte (shapefile/GeoPackage) et d'un
indicateur agrege par unite administrative. Dossier du shapefile et variable de
fusion configurables via la feuille Excel Cartographie.
}
