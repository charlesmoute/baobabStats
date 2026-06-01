#' @title Collecte : importation des donnees de terrain
#' @name bs_collecte
#' @description
#' Fonctions d'importation harmonisees pour les principales plateformes de
#' collecte utilisees par les INS africains (CSPro, KoboToolbox, ODK) ainsi que
#' pour les formats standards (CSV, Excel, SPSS, Stata). Toutes les fonctions
#' retournent un \code{tibble} enrichi d'attributs de tracabilite (source, date,
#' nombre d'enregistrements).
NULL

# Helper interne : pose les attributs de tracabilite ----------------------------
.bs_tag_source <- function(data, source, chemin = NA_character_) {
  data <- tibble::as_tibble(data)
  attr(data, "bs_source")    <- source
  attr(data, "bs_chemin")    <- chemin
  attr(data, "bs_import_at") <- Sys.time()
  attr(data, "bs_n")         <- nrow(data)
  class(data) <- unique(c("bs_data", class(data)))
  data
}

#' Importer des donnees generiques (detection automatique du format)
#'
#' @param chemin Chemin du fichier (.csv, .xlsx, .sav, .dta, .json).
#' @param ... Arguments transmis au lecteur sous-jacent.
#' @return Un \code{tibble} de classe \code{bs_data}.
#' @export
bs_collecter <- function(chemin, ...) {
  stopifnot(is.character(chemin), length(chemin) == 1)
  if (!file.exists(chemin)) cli::cli_abort("Fichier introuvable : {.path {chemin}}")
  ext <- tolower(tools::file_ext(chemin))
  data <- switch(ext,
    csv  = as.data.frame(bs_fread(chemin, ...), stringsAsFactors = FALSE),
    tsv  = utils::read.delim(chemin, stringsAsFactors = FALSE, fileEncoding = "UTF-8", ...),
    xlsx = readxl::read_excel(chemin, ...),
    xls  = readxl::read_excel(chemin, ...),
    sav  = haven::read_sav(chemin, ...),
    dta  = haven::read_dta(chemin, ...),
    json = tibble::as_tibble(jsonlite::fromJSON(chemin, flatten = TRUE)),
    cli::cli_abort("Format non pris en charge : {.val {ext}}")
  )
  .bs_tag_source(data, paste0("fichier:", ext), chemin)
}

#' Importer des donnees CSPro
#'
#' @param chemin_data Chemin du fichier de donnees (.csdb, .dat ou export texte).
#' @param dictionnaire Chemin du dictionnaire CSPro (.dcf), optionnel si le fichier
#'   est deja delimite.
#' @param sep Separateur si import en texte delimite.
#' @return Un \code{tibble} de classe \code{bs_data}.
#' @details Lorsque le fichier est un export delimite, l'import est direct. Pour les
#'   fichiers positionnels (.dat) accompagnes d'un .dcf, le dictionnaire est lu pour
#'   reconstituer les colonnes (positions de debut/longueur).
#' @export
bs_collecter_cspro <- function(chemin_data, dictionnaire = NULL, sep = ",") {
  if (!file.exists(chemin_data)) cli::cli_abort("Donnees CSPro introuvables : {.path {chemin_data}}")
  ext <- tolower(tools::file_ext(chemin_data))
  if (ext %in% c("csv", "txt", "tsv") || is.null(dictionnaire)) {
    data <- utils::read.delim(chemin_data, sep = sep, stringsAsFactors = FALSE,
                              fileEncoding = "UTF-8")
  } else {
    dico <- .bs_lire_dcf(dictionnaire)
    lignes <- readLines(chemin_data, encoding = "UTF-8", warn = FALSE)
    data <- as.data.frame(
      lapply(seq_len(nrow(dico)), function(i) {
        trimws(substr(lignes, dico$start[i], dico$start[i] + dico$len[i] - 1))
      }), stringsAsFactors = FALSE
    )
    names(data) <- dico$name
  }
  .bs_tag_source(data, "cspro", chemin_data)
}

# Lecture minimaliste d'un dictionnaire CSPro (.dcf)
.bs_lire_dcf <- function(dcf) {
  txt <- readLines(dcf, encoding = "UTF-8", warn = FALSE)
  items <- which(grepl("^\\[Item\\]", txt, ignore.case = TRUE))
  parse_bloc <- function(start) {
    bloc <- txt[start:min(length(txt), start + 12)]
    get <- function(cle) {
      l <- grep(paste0("^", cle, "="), bloc, ignore.case = TRUE, value = TRUE)
      if (length(l)) trimws(sub(".*=", "", l[1])) else NA
    }
    data.frame(name = get("Label"), start = as.integer(get("Start")),
               len = as.integer(get("Len")), stringsAsFactors = FALSE)
  }
  do.call(rbind, lapply(items, parse_bloc))
}

#' Importer des donnees KoboToolbox via l'API
#'
#' @param asset_id Identifiant de l'asset (formulaire) Kobo.
#' @param token Jeton d'API Kobo. A defaut, lu depuis \code{Sys.getenv("KOBO_TOKEN")}.
#' @param base_url URL de base du serveur (defaut : serveur humanitaire Kobo).
#' @return Un \code{tibble} de classe \code{bs_data}.
#' @details Necessite le package \pkg{httr}. En contexte hors-ligne, exporter d'abord
#'   les donnees depuis Kobo puis utiliser \code{bs_collecter()}.
#' @export
bs_collecter_kobo <- function(asset_id, token = Sys.getenv("KOBO_TOKEN"),
                              base_url = "https://kf.kobotoolbox.org") {
  if (!requireNamespace("httr", quietly = TRUE))
    cli::cli_abort("Le package {.pkg httr} est requis pour l'API Kobo.")
  if (!nzchar(token)) cli::cli_abort("Jeton Kobo absent (definir KOBO_TOKEN).")
  url <- sprintf("%s/api/v2/assets/%s/data/?format=json", base_url, asset_id)
  rep <- httr::GET(url, httr::add_headers(Authorization = paste("Token", token)))
  httr::stop_for_status(rep)
  contenu <- jsonlite::fromJSON(httr::content(rep, "text", encoding = "UTF-8"),
                                flatten = TRUE)
  data <- tibble::as_tibble(contenu$results)
  .bs_tag_source(data, "kobotoolbox", asset_id)
}

#' Importer des donnees ODK (Central) via OData
#'
#' @param projet_id Identifiant du projet ODK Central.
#' @param formulaire Identifiant (xmlFormId) du formulaire.
#' @param base_url URL du serveur ODK Central.
#' @param email,mot_de_passe Identifiants. A defaut variables d'environnement
#'   \code{ODK_EMAIL} / \code{ODK_PWD}.
#' @return Un \code{tibble} de classe \code{bs_data}.
#' @export
bs_collecter_odk <- function(projet_id, formulaire, base_url,
                             email = Sys.getenv("ODK_EMAIL"),
                             mot_de_passe = Sys.getenv("ODK_PWD")) {
  if (!requireNamespace("httr", quietly = TRUE))
    cli::cli_abort("Le package {.pkg httr} est requis pour l'API ODK.")
  tok <- httr::POST(paste0(base_url, "/v1/sessions"),
                    body = list(email = email, password = mot_de_passe),
                    encode = "json")
  httr::stop_for_status(tok)
  jeton <- httr::content(tok)$token
  url <- sprintf("%s/v1/projects/%s/forms/%s.svc/Submissions",
                 base_url, projet_id, formulaire)
  rep <- httr::GET(url, httr::add_headers(Authorization = paste("Bearer", jeton)))
  httr::stop_for_status(rep)
  contenu <- jsonlite::fromJSON(httr::content(rep, "text", encoding = "UTF-8"),
                                flatten = TRUE)
  data <- tibble::as_tibble(contenu$value)
  .bs_tag_source(data, "odk", formulaire)
}

#' Verifier le taux de valeurs manquantes
#'
#' @param data Un data.frame / tibble.
#' @param seuil Seuil d'alerte sur la proportion de NA par variable (defaut 0.15).
#' @return Un \code{tibble} : variable, n_na, prop_na, alerte.
#' @export
bs_controler_na <- function(data, seuil = 0.15) {
  res <- tibble::tibble(
    variable = names(data),
    n_na     = vapply(data, function(x) sum(is.na(x) | (is.character(x) & !nzchar(trimws(x)))), integer(1)),
    prop_na  = NA_real_
  )
  res$prop_na <- res$n_na / nrow(data)
  res$alerte  <- res$prop_na > seuil
  res <- res[order(-res$prop_na), ]
  if (any(res$alerte))
    cli::cli_warn("{sum(res$alerte)} variable(s) depassent le seuil de {scales::percent(seuil)} de valeurs manquantes.")
  res
}
