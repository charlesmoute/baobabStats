% Generated manually for baobabStats 1.0.1
\name{bs_post_censitaire}
\alias{bs_post_censitaire}
\alias{print.bs_post_censitaire}
\title{Evaluer la qualite et calculer les coefficients de redressement depuis la PES}
\usage{
bs_post_censitaire(recensement, pes = NULL, var_strate = "region",
  config_appariement = list(), plafond = 3, interpreter = TRUE)
}
\arguments{
\item{recensement}{data.frame des microdonnees du recensement (deja harmonise).}
\item{pes}{data.frame des donnees de l'enquete post-censitaire, ou \code{NULL}.}
\item{var_strate}{Nom de la variable de stratification (defaut \code{"region"}).}
\item{config_appariement}{Liste d'options transmise a \code{bs_apparier_pes()}.}
\item{plafond}{Plafond de securite du coefficient (defaut 3).}
\item{interpreter}{Logique : ajouter les interpretations dynamiques (defaut TRUE).}
}
\value{Un objet \code{bs_post_censitaire} : liste contenant \code{appariement},
\code{dse}, \code{coefficients} (tibble par strate), \code{qualite} et
\code{interpretation}. \code{NULL} si \code{pes} absente.}
\description{
Point d'entree unique pour exploiter une enquete post-censitaire (PES) : apparie
les donnees PES et recensement, realise l'estimation par systeme dual (DSE) et
derive les coefficients de redressement par strate, reutilisables par les rapports
thematiques. Si aucune PES n'est fournie, renvoie \code{NULL} sans erreur.
}
\seealso{\code{\link{bs_apparier_pes}}, \code{\link{bs_estimer_dse}},
\code{\link{bs_coefficients_redressement}}}
