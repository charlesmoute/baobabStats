% Generated manually for baobabStats 1.0.2
\name{bs_evaluer_concordance}
\alias{bs_evaluer_concordance}
\alias{print.bs_concordance}
\title{Evaluer la concordance entre collecte et controle (backcheck ou PES)}
\usage{
bs_evaluer_concordance(donnees, controle, mode = c("enquete", "recensement"),
  id_var = "id", enum_var = NULL, t1 = NULL, t2 = NULL, t3 = NULL,
  okrange = NULL, var_strate = "region", interpreter = TRUE)
}
\arguments{
\item{donnees}{data.frame de la collecte principale.}
\item{controle}{data.frame du backcheck (enquete) ou de la PES (recensement).}
\item{mode}{"enquete" (backcheck) ou "recensement" (post-censitaire).}
\item{id_var}{Variable identifiant commune aux deux fichiers.}
\item{enum_var}{Variable agent enqueteur (optionnel ; statistiques par agent).}
\item{t1, t2, t3}{Vecteurs de variables critiques / moderees / mineures.}
\item{okrange}{Liste nommee variable -> tolerance numerique (variables T2).}
\item{var_strate}{Variable de strate (mode recensement : redressement).}
\item{interpreter}{Logique : interpretation dynamique (defaut TRUE).}
}
\value{Objet \code{bs_concordance} : resultat bcstats enrichi. \code{resume} est
un tibble (concordance + Kappa de Cohen par variable) ; en mode recensement,
\code{omission} et \code{coefficients} sont egalement fournis.}
\description{
Point d'entree unique, pilotable par configuration, pour evaluer la qualite des
donnees par comparaison avec une source de controle : un backcheck (mode enquete)
ou une enquete post-censitaire (mode recensement). La logique bcstats compare les
variables T1/T2/T3 et calcule les taux de concordance, taux d'erreur par type et
le coefficient Kappa de Cohen. En mode recensement, le taux d'omission (systeme
dual de Petersen-Lincoln) et les coefficients de redressement sont derives.
}
\seealso{\code{\link{bs_qualite_backcheck}}, \code{\link{bs_post_censitaire}}}
