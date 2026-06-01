#' @title Generation de prompts pour l'interpretation par IA
#' @name bs_prompt
#' @description
#' Transforme un objet de resultats baobabStats en un \emph{prompt} structure,
#' concis et directement exploitable par un assistant IA (Claude, GPT, Gemini...)
#' pour produire une interpretation experte. Le prompt suit une structure eprouvee :
#' role, contexte, donnees (serialisees de facon compacte), tache, contraintes et
#' format de sortie attendu. L'objectif est l'efficacite (peu de jetons) et
#' l'efficience (interpretation actionnable).
NULL

#' Generer un prompt d'interpretation
#'
#' @param objet Resultat produit par une fonction baobabStats.
#' @param type Type de resultat (defaut : detection automatique).
#' @param contexte Texte libre decrivant le contexte (pays, operation, annee...).
#'   Defaut construit a partir des options \code{baobabstats.pays}.
#' @param public Public cible de l'interpretation : "technique" (defaut),
#'   "decideur" ou "grand_public" ; ajuste le niveau de langage demande.
#' @param langue Langue de sortie attendue (defaut "francais").
#' @param inclure_donnees Logique : serialiser les chiffres cles dans le prompt
#'   (defaut TRUE). Si FALSE, ne genere que le squelette.
#' @return Un objet \code{bs_prompt} (chaine de caracteres) imprimable et copiable.
#' @examples
#' \dontrun{
#' d <- bs_estimer_dse(n_pes = 5000, n_recensement = 48000, n_apparies = 4600)
#' cat(bs_prompt(d, public = "decideur"))
#' }
#' @export
bs_prompt <- function(objet, type = NULL, contexte = NULL,
                      public = c("technique", "decideur", "grand_public"),
                      langue = "francais", inclure_donnees = TRUE) {
  public <- match.arg(public)
  if (is.null(type)) type <- .bs_detecter_type(objet)
  if (is.null(contexte))
    contexte <- sprintf("Recensement/enquete, pays = %s.", getOption("baobabstats.pays", "?"))

  role <- "Tu es un demographe senior specialiste de l'evaluation de la qualite des donnees de recensement et d'enquete en Afrique."
  niveau <- switch(public,
    technique    = "Adopte un registre technique precis (terminologie demographique, seuils de reference).",
    decideur     = "Adopte un registre synthetique oriente decision : 3 messages cles, implications operationnelles, pas de jargon inutile.",
    grand_public = "Adopte un registre pedagogique accessible : evite le jargon, illustre par des ordres de grandeur.")

  donnees <- if (inclure_donnees) .bs_serialiser(objet, type) else "(donnees a coller ici)"

  tache <- switch(type,
    dse = "Interprete ces resultats d'estimation par systeme dual (DSE) : qualifie le niveau de couverture/omission, signale les risques, et recommande la strategie de redressement.",
    backcheck = "Interprete ces resultats de controle de terrain (backcheck) : identifie les variables et enqueteurs a risque, et propose des mesures correctives.",
    qualite_intrinseque = "Interprete ce diagnostic de qualite intrinseque : commente l'attraction d'age (Whipple/Myers/Bachi), le rapport de masculinite et la completude, puis recommande d'eventuels lissages.",
    redressement = "Interprete ces coefficients de redressement : juge leur ampleur et leur heterogeneite, et conseille sur leur application aux effectifs publies.",
    projection = "Interprete cette projection de population : commente la trajectoire, le rythme de croissance et la sensibilite aux hypotheses.",
    "Interprete ces resultats demographiques de maniere rigoureuse."
  )

  contraintes <- paste(
    "- Compare systematiquement aux seuils de reference (Nations Unies, manuels de recensement).",
    "- Distingue clairement les faits (chiffres) des recommandations.",
    "- Si une valeur est aberrante ou implausible, dis-le explicitement.",
    "- Sois concis : pas de remplissage.", sep = "\n")

  sortie <- switch(public,
    decideur = "Format : (1) Verdict en une phrase ; (2) 3 points cles ; (3) 2 recommandations operationnelles.",
    grand_public = "Format : un paragraphe clair + une analogie chiffree.",
    "Format : diagnostic structure par theme, avec verdict, justification chiffree et recommandation pour chaque theme.")

  prompt <- glue::glue(
    "## ROLE\n{role}\n\n",
    "## CONTEXTE\n{contexte}\n\n",
    "## DONNEES (resultats baobabStats, type = {type})\n{donnees}\n\n",
    "## TACHE\n{tache}\n\n",
    "## CONTRAINTES\n{contraintes}\n{niveau}\nLangue de sortie : {langue}.\n\n",
    "## FORMAT DE SORTIE ATTENDU\n{sortie}\n"
  )
  structure(as.character(prompt), class = "bs_prompt", type = type)
}

# Serialisation compacte des chiffres cles selon le type ----------------------
.bs_serialiser <- function(objet, type) {
  extraire <- function(champs) {
    vals <- lapply(champs, function(ch) {
      v <- tryCatch(objet[[ch]], error = function(e) NULL)
      if (is.null(v) || length(v) != 1 || !is.numeric(v)) return(NULL)
      sprintf("- %s : %s", ch, format(round(v, 3), big.mark = " "))
    })
    paste(Filter(Negate(is.null), vals), collapse = "\n")
  }
  res <- switch(type,
    dse = extraire(c("n_pes", "n_census", "n_matched", "n_erroneous",
                     "true_population", "coverage_rate", "omission_rate",
                     "erroneous_inclusion_rate", "net_coverage_error_rate")),
    backcheck = extraire(c("n_compared", "n_survey", "n_backcheck", "overall_error_rate")),
    qualite_intrinseque = {
      ag <- objet$age_quality
      paste(c(
        if (!is.null(objet$global_score)) sprintf("- score_global : %.1f/100", objet$global_score),
        if (!is.null(ag$whipple_combined)) sprintf("- whipple : %.0f", ag$whipple_combined),
        if (!is.null(ag$myers)) sprintf("- myers : %.1f", ag$myers),
        if (!is.null(ag$bachi)) sprintf("- bachi : %.1f", ag$bachi)
      ), collapse = "\n")
    },
    redressement = {
      if ("coef_redressement" %in% names(objet)) {
        utils::capture.output(print(utils::head(
          objet[, intersect(c("strate", "taux_omission", "coef_redressement"), names(objet))], 15)))
      } else "(coefficients indisponibles)"
    },
    {
      # Generique : JSON tronque
      txt <- tryCatch(jsonlite::toJSON(objet, auto_unbox = TRUE, null = "null"),
                      error = function(e) "")
      substr(as.character(txt), 1, 1200)
    }
  )
  if (is.null(res) || !nzchar(res)) res <- "(aucune valeur numerique extraite ; coller les resultats manuellement)"
  res
}

#' Copier le dernier prompt genere dans le presse-papiers (si disponible)
#' @param prompt Objet \code{bs_prompt}.
#' @export
bs_prompt_copier <- function(prompt) {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    tryCatch({
      rstudioapi::insertText(text = as.character(prompt))
      cli::cli_alert_success("Prompt insere dans l'editeur RStudio.")
      return(invisible(TRUE))
    }, error = function(e) NULL)
  }
  cat(prompt)
  invisible(FALSE)
}

#' @export
print.bs_prompt <- function(x, ...) {
  cat(x, sep = "")
  invisible(x)
}
