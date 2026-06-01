#' @title Configuration pilotee par fichier Excel
#' @name bs_config_excel
#' @description
#' baobabStats peut etre entierement pilote par un classeur Excel decrivant chaque
#' etape du cycle statistique : collecte, mappage des variables, traitement,
#' qualite, visualisation et diffusion. Cela permet aux utilisateurs non
#' programmeurs de parametrer un traitement reproductible sans ecrire de R.
#'
#' \itemize{
#'   \item \code{bs_config_modele()} : genere un classeur modele documente.
#'   \item \code{bs_config_lire()} : lit un classeur en une configuration structuree.
#'   \item \code{bs_pipeline()} : execute le pipeline decrit par la configuration.
#' }
NULL

# Definition des feuilles du modele (cle = nom de feuille) ----------------------
.bs_config_feuilles <- function() {
  list(
    Projet = data.frame(
      Parametre = c("nom_projet", "pays_code", "annee", "langue", "dossier_sortie"),
      Valeur    = c("RGPH 2026", "CM", "2026", "fr", "sorties_baobabstats"),
      Aide      = c("Libelle du projet", "Code ISO-2 du pays (ex. CM)",
                    "Annee de l'operation", "Langue des sorties (fr/en)",
                    "Dossier ou ecrire les livrables"),
      stringsAsFactors = FALSE),

    Collecte = data.frame(
      Parametre = c("source", "format", "chemin", "dictionnaire", "seuil_na"),
      Valeur    = c("fichier", "csv", "donnees/individus.csv", "", "0.15"),
      Aide      = c("fichier / cspro / kobo / odk", "csv/xlsx/sav/dta/json",
                    "Chemin du fichier de donnees", "Dictionnaire CSPro (.dcf) si besoin",
                    "Seuil d'alerte de valeurs manquantes"),
      stringsAsFactors = FALSE),

    Variables = data.frame(
      Role     = c("id", "age", "sexe", "region", "departement", "unite_fine", "menage", "ponderation",
                   "situation_matrimoniale", "alphabetisation", "niveau_instruction",
                   "frequentation", "activite", "handicap",
                   "naissances_recentes", "parite", "deces", "deces_maternel",
                   "lieu_naissance", "residence_anterieure",
                   "eau", "assainissement", "energie", "murs",
                   "biens_durables", "transferts", "peuple_autochtone",
                   "activite_agricole", "elevage"),
      Colonne  = c("id_ind", "age", "sexe", "region", "departement", "arrondissement", "id_men", "poids",
                   "etat_matri", "alphabet", "niveau_instr",
                   "frequentation", "activite", "handicap",
                   "naiss_12m", "parite", "deces_12m", "deces_mat",
                   "lieu_naiss", "res_anter",
                   "source_eau", "type_aisance", "eclairage", "mur",
                   "biens", "transferts_diaspora", "p17",
                   "men_agri", "men_ele"),
      Obligatoire = c("oui", "oui", "oui", "non", "non", "non", "non", "non",
                      "non", "non", "non",
                      "non", "non", "non",
                      "non", "non", "non", "non",
                      "non", "non",
                      "non", "non", "non", "non",
                      "non", "non", "non",
                      "non", "non"),
      stringsAsFactors = FALSE),

    Traitement = data.frame(
      Parametre = c("harmoniser_regions", "imputation", "methode_imputation",
                    "detecter_doublons", "appliquer_contraintes"),
      Valeur    = c("oui", "oui", "auto", "oui", "oui"),
      Aide      = c("Harmoniser les libelles de region (oui/non)",
                    "Imputer les manquants (oui/non)", "auto/mice/missforest",
                    "Detecter et traiter les doublons (oui/non)",
                    "Appliquer les contraintes demographiques (oui/non)"),
      stringsAsFactors = FALSE),

    Qualite = data.frame(
      Parametre = c("controle_intrinseque", "backcheck", "chemin_backcheck",
                    "pes", "chemin_pes", "calcul_redressement",
                    "id_var", "enum_var", "var_strate"),
      Valeur    = c("oui", "non", "", "non", "", "oui", "id", "", "region"),
      Aide      = c("Indices d'age, masculinite, completude (oui/non)",
                    "Controle de terrain bcstats / re-interview (oui/non)",
                    "Fichier des donnees de re-interview (backcheck)",
                    "Enquete post-censitaire / appariement (oui/non)",
                    "Fichier des donnees PES",
                    "Calculer les coefficients de redressement (oui/non)",
                    "Variable identifiant commune collecte / controle",
                    "Variable agent enqueteur (stats par agent ; optionnel)",
                    "Variable de strate (redressement post-censitaire)"),
      stringsAsFactors = FALSE),

    Backcheck = data.frame(
      Variable = c("sexe", "age", "situation_matrimoniale", "region", "niveau_instruction"),
      Type     = c("T1", "T2", "T1", "T1", "T3"),
      Okrange  = c("", "2", "", "", ""),
      Aide     = c(
        "T1 = critique (comparaison exacte) ; T2 = moderee (tolerance numerique Okrange) ; T3 = mineure",
        "Okrange = 2 : un ecart d'age <= 2 ans est tolere (pas une erreur)",
        "Concordance + Kappa de Cohen calcules pour chaque variable",
        "Memes variables utilisees pour le backcheck (enquete) et la PES (recensement)",
        "Laisser Okrange vide pour les variables categorielles (T1/T3)"),
      stringsAsFactors = FALSE),

    Visualisation = data.frame(
      Graphique = c("pyramide_ages", "carte_region", "barres_alphabetisation",
                    "courbe_fecondite"),
      Produire  = c("oui", "non", "oui", "oui"),
      Format    = c("png", "png", "png", "png"),
      Titre     = c("Pyramide des ages", "Repartition par region",
                    "Taux d'alphabetisation", "Taux de fecondite par age"),
      stringsAsFactors = FALSE),

    Diffusion = data.frame(
      Livrable = c("tableaux", "rapport_synthese", "rapport_qualite", "export_sdmx"),
      Produire = c("oui", "oui", "oui", "non"),
      Format   = c("xlsx", "word", "html", "sdmx"),
      Niveau   = c("national", "region", "national", "national"),
      stringsAsFactors = FALSE),

    Rapports = data.frame(
      Theme    = c("structure", "nuptialite", "education", "emploi", "fecondite",
                   "mortalite", "migration", "handicap", "habitat", "equipements",
                   "autochtones", "agriculture", "qualite", "projection"),
      Produire = c(rep("oui", 12), "oui", "oui"),
      Word     = rep("oui", 14),
      Excel    = rep("oui", 14),
      HTML     = rep("oui", 14),
      Aide     = c(
        "Structure et repartition spatiale",
        "Nuptialite (situation matrimoniale, SMAM)",
        "Education, scolarisation, alphabetisation",
        "Emploi et activites economiques",
        "Fecondite (naissances, ISF)",
        "Mortalite generale et maternelle",
        "Migration interne et internationale",
        "Handicap (prevalence, comparaisons)",
        "Habitat et conditions de logement",
        "Equipements des menages et bien-etre",
        "Peuples autochtones (Pygmees, Mbororo)",
        "Agriculture, elevage, aquaculture, peche",
        "Rapport d'evaluation de la qualite des donnees",
        "Projection de la population (10 ans, sans interpretation)"),
      stringsAsFactors = FALSE),

    Projection = data.frame(
      Parametre = c("horizon_annees", "date_collecte", "unite_fine", "methode",
                    "pas", "sex_ratio_naissance"),
      Valeur    = c("10", format(Sys.Date(), "%Y-%m-%d"), "unite_fine", "onu",
                    "5", "1.03"),
      Aide      = c("Nombre d'annees a projeter a partir de la date de collecte",
                    "Date de collecte (origine de la projection, AAAA-MM-JJ)",
                    "Role de l'unite administrative la plus fine (cf. feuille Variables)",
                    "Methode : onu (cohortes-composantes) / cohort / microsimulation",
                    "Pas de projection en annees (1 ou 5) pour la methode ONU",
                    "Rapport de masculinite a la naissance (defaut 1.03)"),
      stringsAsFactors = FALSE),

    Cartographie = data.frame(
      Indicateur = c("alphabetisation", "activite", "handicap"),
      Produire   = c("oui", "non", "non"),
      Titre      = c("Taux d'alphabetisation", "Taux d'activite", "Prevalence du handicap"),
      Shapefile  = c("shapes/", "", ""),
      Variable_fusion = c("region", "", ""),
      Variable_fusion_carte = c("NOM_REGION", "", ""),
      Aide = c("Dossier du shapefile + variable de fusion (1re ligne = parametres globaux)",
               "Mettre 'oui' pour produire la carte de cet indicateur",
               "Variable_fusion = cle cote donnees ; Variable_fusion_carte = cle cote shapefile"),
      stringsAsFactors = FALSE),

    SAE = data.frame(
      Parametre = c("activer", "approche", "domaine", "niveau_superieur",
                    "indicateur", "par_age", "par_sexe", "par_handicap",
                    "poids_auxiliaire", "caler"),
      Valeur    = c("non", "top_down", "arrondissement", "region",
                    "", "oui", "oui", "oui", "", "oui"),
      Aide      = c("Activer l'estimation sur petits domaines (oui/non)",
                    "Approche : top_down (repartition) ou bottom_up (agregation + calage)",
                    "Variable du petit domaine cible (ex. arrondissement)",
                    "Variable du niveau superieur fiable (ex. region)",
                    "Indicateur a estimer (vide = effectifs/comptage)",
                    "Desagreger par age (oui/non) - actif par defaut",
                    "Desagreger par sexe (oui/non) - actif par defaut",
                    "Desagreger par handicap (oui/non) - actif par defaut",
                    "Variable de poids auxiliaire pour la repartition top-down (optionnel)",
                    "Caler les estimations bottom-up sur le total superieur (oui/non)"),
      stringsAsFactors = FALSE)
  )
}

#' Generer un classeur Excel de configuration modele
#' @param chemin Chemin de sortie (.xlsx).
#' @param ouvrir Logique : ouvrir le fichier apres creation (defaut FALSE).
#' @return Le chemin du fichier cree (invisible).
#' @export
bs_config_modele <- function(chemin = "baobabstats_config.xlsx", ouvrir = FALSE) {
  if (!requireNamespace("openxlsx", quietly = TRUE))
    cli::cli_abort("Le package {.pkg openxlsx} est requis.")
  feuilles <- .bs_config_feuilles()
  wb <- openxlsx::createWorkbook()
  style_titre <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#4A3826",
                                       fontColour = "#FBF6EC", halign = "left")
  style_aide  <- openxlsx::createStyle(textDecoration = "italic", fontColour = "#73592F")

  # Feuille d'accueil
  openxlsx::addWorksheet(wb, "Lisez-moi")
  intro <- data.frame(baobabStats = c(
    "Classeur de configuration baobabStats",
    "Renseignez chaque feuille puis : bs_pipeline(bs_config_lire('ce_fichier.xlsx'))",
    "Onglets : Projet, Collecte, Variables, Traitement, Qualite, Visualisation, Diffusion, Rapports, Projection.",
    "Onglet Rapports : produire les rapports thematiques (Word/Excel/HTML) par theme.",
    "Onglet Projection : horizon (annees), date de collecte, unite administrative la plus fine.",
    "Valeurs oui/non = activer/desactiver une etape. Ne pas renommer les colonnes."),
    stringsAsFactors = FALSE)
  openxlsx::writeData(wb, "Lisez-moi", intro, headerStyle = style_titre)
  openxlsx::setColWidths(wb, "Lisez-moi", cols = 1, widths = 95)

  for (nm in names(feuilles)) {
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, feuilles[[nm]], headerStyle = style_titre)
    openxlsx::setColWidths(wb, nm, cols = seq_len(ncol(feuilles[[nm]])),
                           widths = "auto")
    if ("Aide" %in% names(feuilles[[nm]])) {
      ca <- which(names(feuilles[[nm]]) == "Aide")
      openxlsx::addStyle(wb, nm, style_aide, rows = 2:(nrow(feuilles[[nm]]) + 1),
                         cols = ca, gridExpand = TRUE)
    }
  }
  openxlsx::saveWorkbook(wb, chemin, overwrite = TRUE)
  cli::cli_alert_success("Modele de configuration cree : {.path {chemin}}")
  if (ouvrir && interactive()) utils::browseURL(chemin)
  invisible(chemin)
}

#' Lire un classeur de configuration baobabStats
#' @param chemin Chemin du classeur (.xlsx).
#' @return Une liste structuree de classe \code{bs_config}.
#' @export
bs_config_lire <- function(chemin) {
  if (!file.exists(chemin)) cli::cli_abort("Configuration introuvable : {.path {chemin}}")
  feuilles <- readxl::excel_sheets(chemin)
  lire_kv <- function(f) {
    d <- readxl::read_excel(chemin, sheet = f)
    if (all(c("Parametre", "Valeur") %in% names(d)))
      stats::setNames(as.list(d$Valeur), d$Parametre) else d
  }
  cfg <- list()
  for (f in setdiff(feuilles, "Lisez-moi")) cfg[[tolower(f)]] <- lire_kv(f)
  cfg$chemin_config <- chemin
  class(cfg) <- "bs_config"
  .baobabstats$config <- cfg
  cfg
}

#' Executer le pipeline decrit par une configuration
#'
#' @param config Objet \code{bs_config} (issu de \code{bs_config_lire()}) ou chemin
#'   d'un classeur.
#' @param etapes Sous-ensemble d'etapes a executer (defaut : toutes les etapes
#'   activees dans la configuration).
#' @param verbeux Logique : afficher la progression (defaut TRUE).
#' @return Une liste de classe \code{bs_resultats_pipeline} contenant les sorties
#'   de chaque etape.
#' @details Les noms de colonnes du jeu de donnees sont renommes selon la feuille
#'   \emph{Variables} (role -> colonne) afin que les moteurs trouvent les variables
#'   attendues. Chaque etape produit un resultat interprete et, si demande, des
#'   livrables ecrits dans \code{dossier_sortie}.
#' @export
bs_pipeline <- function(config, etapes = NULL, verbeux = TRUE) {
  if (is.character(config)) config <- bs_config_lire(config)
  stopifnot(inherits(config, "bs_config"))
  msg <- function(...) if (verbeux) cli::cli_alert_info(paste0(...))

  pays   <- config$projet$pays_code %||% getOption("baobabstats.pays")
  sortie <- config$projet$dossier_sortie %||% getOption("baobabstats.sortie")
  if (!dir.exists(sortie)) dir.create(sortie, recursive = TRUE)
  options(baobabstats.pays = pays, baobabstats.sortie = sortie)

  res <- list(config = config, sorties = list())
  actif <- function(x) isTRUE(tolower(as.character(x)) %in% c("oui", "yes", "true", "1"))

  # 1. COLLECTE -----------------------------------------------------------------
  msg("Etape 1/7 : collecte des donnees")
  col <- config$collecte
  data <- switch(tolower(col$source %||% "fichier"),
    cspro = bs_collecter_cspro(col$chemin, if (nzchar(col$dictionnaire %||% "")) col$dictionnaire else NULL),
    kobo  = bs_collecter_kobo(col$chemin),
    odk   = bs_collecter_odk(NULL, col$chemin, base_url = ""),
    bs_collecter(col$chemin)
  )
  # Mappage des variables (role -> colonne reelle)
  vmap <- config$variables
  if (is.data.frame(vmap) && all(c("Role", "Colonne") %in% names(vmap))) {
    for (i in seq_len(nrow(vmap))) {
      colreel <- vmap$Colonne[i]; role <- vmap$Role[i]
      if (!is.na(colreel) && colreel %in% names(data) && !identical(colreel, role))
        names(data)[names(data) == colreel] <- role
    }
  }
  res$donnees_brutes <- data
  res$sorties$controle_na <- bs_controler_na(data, as.numeric(col$seuil_na %||% 0.15))

  # 2. TRAITEMENT ---------------------------------------------------------------
  tr <- config$traitement
  if (actif(tr$harmoniser_regions) && "region" %in% names(data)) {
    msg("Etape 2/7 : harmonisation des regions")
    data <- bs_harmoniser_regions(data, "region", code_pays = pays)
  }
  if (actif(tr$imputation) || actif(tr$detecter_doublons)) {
    msg("Etape 2/7 : nettoyage / imputation / doublons")
    nettoye <- tryCatch(bs_nettoyer(
      data,
      methode_imputation = if (actif(tr$imputation)) (tr$methode_imputation %||% "auto") else "none",
      remove_duplicates = actif(tr$detecter_doublons),
      apply_constraints = actif(tr$appliquer_contraintes)),
      error = function(e) { cli::cli_warn("Nettoyage ignore : {conditionMessage(e)}"); NULL })
    if (!is.null(nettoye)) {
      data <- if (!is.null(nettoye$cleaned_data)) nettoye$cleaned_data else
              if (!is.null(nettoye$data)) nettoye$data else data
      res$sorties$nettoyage <- nettoye
    }
  }
  res$donnees_traitees <- data

  # 3. QUALITE ------------------------------------------------------------------
  q <- config$qualite
  if (actif(q$controle_intrinseque)) {
    msg("Etape 3/7 : controle qualite intrinseque")
    res$sorties$qualite_intrinseque <- tryCatch(
      bs_qualite_intrinseque(data, var_age = "age", var_sexe = "sexe"),
      error = function(e) { cli::cli_warn(conditionMessage(e)); NULL })
  }
  # Lire la table Backcheck (T1/T2/T3 + okrange) si presente
  .bs_lire_types <- function() {
    bc <- config$backcheck
    if (!is.data.frame(bc) || !all(c("Variable", "Type") %in% names(bc)))
      return(NULL)
    t1 <- bc$Variable[toupper(bc$Type) == "T1"]
    t2 <- bc$Variable[toupper(bc$Type) == "T2"]
    t3 <- bc$Variable[toupper(bc$Type) == "T3"]
    okrange <- NULL
    if ("Okrange" %in% names(bc)) {
      for (i in seq_len(nrow(bc))) {
        v <- bc$Okrange[i]
        if (!is.na(v) && nzchar(as.character(v))) {
          okrange[[bc$Variable[i]]] <- as.numeric(v)
        }
      }
    }
    list(t1 = t1, t2 = t2, t3 = t3, okrange = okrange)
  }
  idv  <- q$id_var %||% "id"
  enumv <- if (nzchar(q$enum_var %||% "")) q$enum_var else NULL
  strate_v <- q$var_strate %||% "region"

  if (actif(q$backcheck) && nzchar(q$chemin_backcheck %||% "")) {
    msg("Etape 3/7 : controle de terrain (backcheck) + concordance/Kappa")
    bc <- tryCatch(bs_collecter(q$chemin_backcheck), error = function(e) NULL)
    types <- .bs_lire_types()
    if (!is.null(bc) && !is.null(types)) {
      res$sorties$backcheck <- tryCatch(
        bs_evaluer_concordance(data, bc, mode = "enquete", id_var = idv,
          enum_var = enumv, t1 = types$t1, t2 = types$t2, t3 = types$t3,
          okrange = types$okrange),
        error = function(e) { cli::cli_warn("Backcheck : {conditionMessage(e)}"); NULL })
    } else if (!is.null(bc)) {
      res$sorties$backcheck <- tryCatch(
        bs_qualite_backcheck(data, bc, id_var = idv), error = function(e) NULL)
    }
  }
  if (actif(q$pes) && nzchar(q$chemin_pes %||% "")) {
    msg("Etape 3/7 : enquete post-censitaire (PES) : concordance + omission + redressement")
    pes <- tryCatch(bs_collecter(q$chemin_pes), error = function(e) NULL)
    types <- .bs_lire_types()
    if (!is.null(pes) && !is.null(types)) {
      conc <- tryCatch(
        bs_evaluer_concordance(data, pes, mode = "recensement", id_var = idv,
          enum_var = enumv, t1 = types$t1, t2 = types$t2, t3 = types$t3,
          okrange = types$okrange, var_strate = strate_v),
        error = function(e) { cli::cli_warn("PES : {conditionMessage(e)}"); NULL })
      if (!is.null(conc)) {
        res$sorties$pes <- conc
        res$sorties$dse <- conc
        if (actif(q$calcul_redressement) && !is.null(conc$coefficients))
          res$sorties$redressement <- conc$coefficients
      }
    }
    if (is.null(res$sorties$pes) && !is.null(pes)) {
      # Repli : appariement DSE historique
      m <- tryCatch(bs_apparier_pes(pes, data), error = function(e) NULL)
      if (!is.null(m)) {
        res$sorties$pes <- m
        res$sorties$dse <- tryCatch(bs_estimer_dse(match_result = m), error = function(e) NULL)
        if (actif(q$calcul_redressement) && !is.null(res$sorties$dse))
          res$sorties$redressement <- tryCatch(
            bs_coefficients_redressement(dse = res$sorties$dse), error = function(e) NULL)
      }
    }
  }

  # 4. TABLEAUX -----------------------------------------------------------------
  msg("Etape 4/7 : tabulations standards")
  res$sorties$tableaux <- tryCatch(bs_tableaux(data), error = function(e) {
    cli::cli_warn("Tableaux ignores : {conditionMessage(e)}"); NULL })

  # 5. VISUALISATION ------------------------------------------------------------
  vis <- config$visualisation
  if (is.data.frame(vis)) {
    msg("Etape 5/7 : visualisations")
    res$sorties$graphiques <- bs_visualiser_config(data, vis, dossier = sortie)
  }

  # Construire le mapping role -> colonne (reutilise par cartes, SAE, rapports)
  vmap_p <- config$variables
  mapping_p <- NULL
  if (is.data.frame(vmap_p) && all(c("Role", "Colonne") %in% names(vmap_p))) {
    mapping_p <- stats::setNames(as.list(vmap_p$Role), vmap_p$Role)
  }

  # 5b. CARTES THEMATIQUES ------------------------------------------------------
  carto <- config$cartographie
  if (is.data.frame(carto) && nrow(carto) > 0) {
    msg("Etape 5/7 : cartes thematiques")
    res$sorties$cartes <- tryCatch(
      bs_cartes_config(data, carto, dossier = file.path(sortie, "cartes"), mapping = mapping_p),
      error = function(e) { cli::cli_warn("Cartes thematiques : {conditionMessage(e)}"); NULL })
  }

  # 5c. SMALL AREA ESTIMATION ---------------------------------------------------
  sae_cfg <- config$sae
  if (is.data.frame(sae_cfg) && "Parametre" %in% names(sae_cfg)) {
    getp <- function(nom, def = NULL) {
      v <- sae_cfg$Valeur[sae_cfg$Parametre == nom]
      if (length(v) && !is.na(v[1]) && nzchar(as.character(v[1]))) as.character(v[1]) else def
    }
    if (actif(getp("activer", "non"))) {
      msg("Etape 5/7 : estimation sur petits domaines (SAE)")
      axes <- c(if (actif(getp("par_age", "oui"))) "age",
                if (actif(getp("par_sexe", "oui"))) "sexe",
                if (actif(getp("par_handicap", "oui"))) "handicap")
      axes <- axes[!vapply(axes, is.null, logical(1))]
      if (length(axes) == 0) axes <- c("age", "sexe", "handicap")
      ind <- getp("indicateur"); if (!is.null(ind) && !nzchar(ind)) ind <- NULL
      res$sorties$sae <- tryCatch(
        bs_sae(data, domaine = getp("domaine", "arrondissement"),
               approche = getp("approche", "top_down"),
               indicateur = ind,
               niveau_superieur = getp("niveau_superieur", "region"),
               poids_aux = { p <- getp("poids_auxiliaire"); if (!is.null(p) && nzchar(p)) p else NULL },
               par = axes,
               caler = actif(getp("caler", "oui")),
               mapping = mapping_p),
        error = function(e) { cli::cli_warn("SAE : {conditionMessage(e)}"); NULL })
    }
  }

  # 6. DIFFUSION ----------------------------------------------------------------
  diff <- config$diffusion
  if (is.data.frame(diff)) {
    msg("Etape 6/7 : diffusion des livrables")
    res$sorties$diffusion <- bs_diffuser_config(data, res, diff, dossier = sortie)
  }

  # 7. RAPPORTS THEMATIQUES MULTI-FORMAT ---------------------------------------
  rap <- config$rapports
  if (is.data.frame(rap) && "Theme" %in% names(rap)) {
    msg("Etape 7/7 : rapports thematiques (Word / Excel / HTML)")
    mapping <- mapping_p
    # Coefficients de redressement issus de l'enquete post-censitaire
    post <- NULL
    if (!is.null(res$sorties$redressement)) {
      post <- list(coefficients = res$sorties$redressement,
                   dse = res$sorties$dse,
                   var_strate = if ("region" %in% names(data)) "region" else NULL,
                   interpretation = attr(res$sorties$redressement, "bs_interpretation"))
      class(post) <- c("bs_post_censitaire", "list")
    }
    # Parametres de projection
    pj <- config$projection %||% list()
    horizon <- as.integer(pj$horizon_annees %||% 10)
    date_col <- tryCatch(as.Date(pj$date_collecte), error = function(e) Sys.Date())
    if (is.na(date_col)) date_col <- Sys.Date()
    unite_fine <- pj$unite_fine %||% "unite_fine"
    if (!unite_fine %in% names(data)) unite_fine <- if ("region" %in% names(data)) "region" else NULL

    actifs <- rap[actif(rap$Produire), , drop = FALSE]
    themes_demandes <- actifs$Theme
    # Formats demandes (au moins un actif globalement)
    fmts <- c(if (any(actif(rap$Word))) "word",
              if (any(actif(rap$Excel)) || any(actif(rap$Excel))) "excel",
              if (any(actif(rap$HTML))) "html")
    fmts <- fmts[!vapply(fmts, is.null, logical(1))]
    if (length(fmts) == 0) fmts <- c("word", "excel", "html")

    res$sorties$rapports_thematiques <- tryCatch(
      bs_rapports_thematiques(
        data, themes = themes_demandes,
        dossier = file.path(sortie, "rapports_thematiques"),
        mapping = mapping, post = post, formats = fmts,
        horizon_projection = horizon, date_collecte = date_col,
        niveau_fin = unite_fine,
        methode_projection = pj$methode %||% "onu"),
      error = function(e) { cli::cli_warn("Rapports thematiques : {conditionMessage(e)}"); NULL })
  }

  class(res) <- "bs_resultats_pipeline"
  .baobabstats$last_results$pipeline <- res
  cli::cli_alert_success("Pipeline termine. Livrables dans {.path {sortie}}")
  res
}

#' @export
print.bs_resultats_pipeline <- function(x, ...) {
  cli::cli_h2("Resultats du pipeline baobabStats")
  cli::cli_li("Donnees traitees : {nrow(x$donnees_traitees)} lignes, {ncol(x$donnees_traitees)} variables")
  cli::cli_li("Sorties produites : {paste(names(x$sorties), collapse = ', ')}")
  invisible(x)
}
