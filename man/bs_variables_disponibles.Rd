% Generated manually for baobabStats 1.0.2
\name{bs_variables_disponibles}
\alias{bs_variables_disponibles}
\alias{bs_variables_requises}
\alias{bs_tableaux_ignores}
\title{Controle de disponibilite des variables pour la production des tableaux}
\usage{
bs_variables_disponibles(roles, mapping, data, tableau = "Tableau")
bs_variables_requises(theme = NULL)
bs_tableaux_ignores(reset = FALSE)
}
\arguments{
\item{roles}{Vecteur des roles de variables requis (ex. \code{c("age","sexe")}).}
\item{mapping}{Liste nommee role -> colonne (feuille Variables).}
\item{data}{data.frame des donnees.}
\item{tableau}{Libelle du tableau concerne (pour le message).}
\item{theme}{Code de thematique pour filtrer (\code{bs_variables_requises}).}
\item{reset}{Vider le journal apres lecture (\code{bs_tableaux_ignores}).}
}
\value{\code{bs_variables_disponibles} : TRUE si toutes les variables sont
disponibles, FALSE sinon (avec message et journalisation).
\code{bs_variables_requises} : tibble (theme, tableau, roles_requis).
\code{bs_tableaux_ignores} : tibble des tableaux non produits.}
\description{
Avant de produire un tableau d'un plan d'analyse, baobabStats verifie que toutes
les variables necessaires sont configurees. Si une variable manque, le tableau
n'est pas produit, un message explicite est emis et l'evenement est journalise
(consultable via \code{bs_tableaux_ignores()}). \code{bs_variables_requises()}
documente les roles requis par chaque tableau-cle des 12 tomes (plus qualite et
projection).
}
