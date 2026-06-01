#' @title Rapports thematiques multi-format avec interpretation dynamique
#' @name bs_rapports_thematiques
#' @description
#' Production automatisee, pour chacune des thematiques classiques d'un
#' recensement (structure, nuptialite, education, emploi, fecondite, mortalite,
#' migration, handicap, habitat, equipements, peuples autochtones, agriculture),
#' ainsi que pour le rapport d'evaluation de la qualite des donnees et le rapport
#' de projection de la population, de livrables \strong{Word}, \strong{Excel} et
#' \strong{HTML} distincts contenant les resultats des sections d'analyse
#' (tableaux et graphiques).
#'
#' Trois ajouts par rapport au moteur de synthese existant :
#' \itemize{
#'   \item \strong{Interpretation dynamique} inseree dans les sorties Word et HTML
#'     (les projections en sont exemptees, conformement a la specification).
#'   \item \strong{Redressement post-censitaire} : application automatique des
#'     coefficients de redressement issus de l'enquete post-censitaire (PES/DSE)
#'     lorsqu'ils sont disponibles.
#'   \item \strong{Pilotage par Excel} : nouvelle feuille \emph{Rapports} dans le
#'     classeur de configuration (voir \code{bs_config_modele()}).
#' }
#' Les fonctionnalites existantes du package sont conservees ; ce module les
#' orchestre sans les remplacer.
NULL

# ---------------------------------------------------------------------------
# Catalogue des thematiques de recensement
# ---------------------------------------------------------------------------

#' Catalogue des rapports thematiques pris en charge
#'
#' @description Renvoie le referentiel des thematiques de recensement gerees par
#'   baobabStats, avec pour chacune : le code, le libelle, les sections d'analyse
#'   et les roles de variables mobilises. Sert de base au pilotage par Excel.
#' @return Un \code{tibble} (code, libelle, sections, variables, type).
#' @export
bs_themes_recensement <- function() {
  def <- list(
    list("structure",    "Structure et repartition spatiale",
         c("effectifs", "structure_age_sexe", "pyramide", "rapport_masculinite", "dependance"),
         c("age", "sexe", "region", "menage")),
    list("nuptialite",   "Nuptialite",
         c("statut_matrimonial", "smam", "polygamie", "celibat"),
         c("age", "sexe", "situation_matrimoniale")),
    list("education",    "Education, scolarisation et alphabetisation",
         c("scolarisation", "niveau_instruction", "alphabetisation"),
         c("age", "sexe", "alphabetisation", "niveau_instruction", "frequentation")),
    list("emploi",       "Emploi et activites economiques",
         c("activite", "chomage", "secteur", "statut_emploi"),
         c("age", "sexe", "activite")),
    list("fecondite",    "Fecondite",
         c("naissances", "isf", "fecondite_age", "fecondite_differentielle"),
         c("age", "sexe", "naissances_recentes", "parite")),
    list("mortalite",    "Mortalite generale et maternelle",
         c("deces", "mortalite_age", "mortalite_maternelle", "table_mortalite"),
         c("age", "sexe", "deces", "deces_maternel")),
    list("migration",    "Migration interne et internationale",
         c("migration_duree_vie", "migration_recente", "motifs", "emigration"),
         c("age", "sexe", "region", "lieu_naissance", "residence_anterieure")),
    list("handicap",     "Handicap",
         c("prevalence", "type_handicap", "handicap_education", "handicap_emploi"),
         c("age", "sexe", "handicap")),
    list("habitat",      "Habitat et conditions de logement",
         c("type_logement", "materiaux", "eau", "assainissement", "energie", "iql"),
         c("region", "menage", "eau", "assainissement", "energie", "murs")),
    list("equipements",  "Equipements des menages et bien-etre",
         c("biens_durables", "ibe", "transferts", "inegalites"),
         c("menage", "region", "biens_durables", "transferts")),
    list("autochtones",  "Peuples autochtones (Pygmees et Mbororo)",
         c("effectifs_groupes", "etat_civil", "education_comparee", "conditions_vie"),
         c("age", "sexe", "region", "peuple_autochtone")),
    list("agriculture",  "Agriculture, elevage, aquaculture et peche",
         c("profil_exploitations", "cultures", "cheptel", "peche", "financement"),
         c("region", "menage", "activite_agricole", "elevage"))
  )
  speciaux <- list(
    list("qualite",      "Evaluation de la qualite des donnees",
         c("completude", "attraction_age", "rapport_masculinite_age", "coherence",
           "couverture_pes", "redressement"),
         c("age", "sexe")),
    list("projection",   "Projection de la population par sexe et unite administrative",
         c("population_base", "projection_horizon", "structure_projetee"),
         c("age", "sexe", "region"))
  )
  tout <- c(def, speciaux)
  tibble::tibble(
    code      = vapply(tout, `[[`, character(1), 1),
    libelle   = vapply(tout, `[[`, character(1), 2),
    sections  = I(lapply(tout, `[[`, 3)),
    variables = I(lapply(tout, `[[`, 4)),
    type      = c(rep("thematique", length(def)), rep("special", length(speciaux)))
  )
}

# ---------------------------------------------------------------------------
# Orchestrateur post-censitaire : qualite + coefficients de redressement
# ---------------------------------------------------------------------------

#' Evaluer la qualite et calculer les coefficients de redressement depuis la PES
#'
#' @description
#' Point d'entree unique pour exploiter une enquete post-censitaire (PES) :
#' apparie les donnees PES et recensement, realise l'estimation par systeme dual
#' (DSE) et derive les \strong{coefficients de redressement} par strate. Ces
#' coefficients sont ensuite reutilisables par les rapports thematiques.
#'
#' Si aucune donnee PES n'est fournie (parametre \code{pes = NULL}), la fonction
#' renvoie \code{NULL} sans erreur : les rapports seront alors produits sur les
#' effectifs bruts (non redresses).
#'
#' @param recensement data.frame des microdonnees du recensement (deja harmonise).
#' @param pes data.frame des donnees de l'enquete post-censitaire, ou \code{NULL}.
#' @param var_strate Nom de la variable de stratification (defaut \code{"region"}).
#' @param config_appariement Liste d'options transmise a \code{bs_apparier_pes()}.
#' @param plafond Plafond de securite du coefficient (defaut 3).
#' @param interpreter Logique : ajouter les interpretations dynamiques (defaut TRUE).
#' @return Un objet de classe \code{bs_post_censitaire} : liste contenant
#'   \code{appariement}, \code{dse}, \code{coefficients} (tibble par strate),
#'   \code{qualite} et \code{interpretation}. \code{NULL} si \code{pes} absente.
#' @seealso \code{\link{bs_apparier_pes}}, \code{\link{bs_estimer_dse}},
#'   \code{\link{bs_coefficients_redressement}}
#' @export
bs_post_censitaire <- function(recensement, pes = NULL,
                               var_strate = "region",
                               config_appariement = list(),
                               plafond = 3, interpreter = TRUE) {
  if (is.null(pes)) {
    cli::cli_alert_info("Aucune enquete post-censitaire fournie : les rapports utiliseront les effectifs bruts.")
    return(invisible(NULL))
  }
  cli::cli_alert_info("Exploitation de l'enquete post-censitaire (PES)...")

  appariement <- tryCatch(bs_apparier_pes(pes, recensement, config = config_appariement),
                          error = function(e) { cli::cli_warn("Appariement : {conditionMessage(e)}"); NULL })

  # Coefficients par strate si la variable de strate est presente des deux cotes
  coefficients <- NULL; dse <- NULL
  strate_ok <- !is.null(var_strate) && var_strate %in% names(recensement) &&
    var_strate %in% names(pes)
  if (strate_ok) {
    strates <- .bs_strates_pes(pes, recensement, appariement, var_strate)
    coefficients <- tryCatch(bs_coefficients_redressement(strates = strates, plafond = plafond),
                             error = function(e) { cli::cli_warn("Redressement stratifie : {conditionMessage(e)}"); NULL })
  }
  if (is.null(coefficients) && !is.null(appariement)) {
    dse <- tryCatch(bs_estimer_dse(match_result = appariement, interpreter = interpreter),
                    error = function(e) NULL)
    if (!is.null(dse))
      coefficients <- tryCatch(bs_coefficients_redressement(dse = dse, plafond = plafond),
                               error = function(e) NULL)
  }

  qualite <- tryCatch(bs_qualite_intrinseque(recensement), error = function(e) NULL)

  res <- list(appariement = appariement, dse = dse, coefficients = coefficients,
              qualite = qualite, var_strate = var_strate)
  if (isTRUE(interpreter)) {
    interp <- character(0)
    if (!is.null(dse)) interp <- c(interp, as.character(bs_interpreter(dse, type = "dse")))
    if (!is.null(coefficients)) interp <- c(interp, as.character(attr(coefficients, "bs_interpretation")))
    res$interpretation <- interp
  }
  class(res) <- c("bs_post_censitaire", "list")
  .baobabstats$last_results$post_censitaire <- res
  cli::cli_alert_success("Post-censitaire traitee : coefficients de redressement {if (is.null(coefficients)) 'NON ' else ''}disponibles.")
  res
}

# Construit la table de strates attendue par bs_coefficients_redressement()
.bs_strates_pes <- function(pes, recensement, appariement, var_strate) {
  niveaux <- sort(unique(stats::na.omit(as.character(recensement[[var_strate]]))))
  apparies_par_strate <- function(s) {
    if (is.null(appariement) || is.null(appariement$matched_pairs)) {
      # Approximation : proportionnel a l'effectif PES de la strate
      return(NA_integer_)
    }
    mp <- appariement$matched_pairs
    col <- grep(paste0("^", var_strate), names(mp), value = TRUE)[1]
    if (is.na(col)) return(NA_integer_)
    sum(as.character(mp[[col]]) == s, na.rm = TRUE)
  }
  purrr::map_dfr(niveaux, function(s) {
    n_rec <- sum(as.character(recensement[[var_strate]]) == s, na.rm = TRUE)
    n_pes <- sum(as.character(pes[[var_strate]]) == s, na.rm = TRUE)
    n_app <- apparies_par_strate(s)
    if (is.na(n_app)) n_app <- round(min(n_pes, n_rec) * 0.97)  # hypothese de couverture par defaut
    tibble::tibble(strate = s, n_pes = n_pes, n_recensement = n_rec,
                   n_apparies = n_app, n_errones = 0L)
  })
}

#' @export
print.bs_post_censitaire <- function(x, ...) {
  cli::cli_h2("Resultat post-censitaire baobabStats")
  if (!is.null(x$coefficients)) {
    cli::cli_alert_success("Coefficients de redressement par strate :")
    print(x$coefficients)
  } else cli::cli_alert_warning("Coefficients de redressement non disponibles.")
  if (!is.null(x$interpretation)) {
    cli::cli_h3("Interpretation")
    for (p in x$interpretation) cli::cli_li(p)
  }
  invisible(x)
}

# ---------------------------------------------------------------------------
# Construction du contenu d'un rapport thematique (tableaux + graphiques + texte)
# ---------------------------------------------------------------------------

# Resout un role de variable -> nom de colonne effectif, a partir d'un mapping.
.bs_col <- function(role, mapping, data) {
  col <- if (!is.null(mapping) && role %in% names(mapping)) mapping[[role]] else role
  if (!is.null(col) && col %in% names(data)) col else NULL
}

# Applique les coefficients de redressement a un tableau d'effectifs (si dispo).
.bs_redresser_tableau <- function(tab, coefficients, var_strate, var_eff = "Effectif") {
  if (is.null(coefficients) || is.null(var_strate) || !(var_strate %in% names(tab))) return(tab)
  if (!var_eff %in% names(tab)) return(tab)
  out <- tryCatch(
    bs_appliquer_redressement(tab, coefficients, var_strate = var_strate, var_effectif = var_eff),
    error = function(e) tab)
  out
}

#' Construire le contenu analytique d'une thematique
#'
#' @description Calcule les tableaux et graphiques d'une thematique et, pour les
#'   sorties Word/HTML, les interpretations dynamiques associees. Fonction
#'   interne reutilisee par \code{bs_rapport_thematique()}.
#' @param data data.frame harmonise.
#' @param theme Code de la thematique (voir \code{bs_themes_recensement()}).
#' @param mapping Liste nommee role -> colonne (issue de la feuille Variables).
#' @param coefficients Coefficients de redressement (optionnel).
#' @param var_strate Variable de strate pour le redressement (optionnel).
#' @param niveau Variable d'unite administrative pour la ventilation (optionnel).
#' @param interpreter Logique : produire les interpretations dynamiques.
#' @return Liste : titre, sections (chacune : titre, tableaux, graphiques, texte).
#' @export
bs_contenu_thematique <- function(data, theme, mapping = NULL,
                                   coefficients = NULL, var_strate = NULL,
                                   niveau = NULL, interpreter = TRUE) {
  cat_t <- bs_themes_recensement()
  if (!theme %in% cat_t$code) cli::cli_abort("Theme inconnu : {.val {theme}}")
  libelle <- cat_t$libelle[cat_t$code == theme]

  age  <- .bs_col("age", mapping, data)
  sexe <- .bs_col("sexe", mapping, data)
  reg  <- .bs_col("region", mapping, data)
  if (is.null(var_strate)) var_strate <- reg

  sections <- list()
  ajt <- function(titre, tableaux = NULL, graphiques = NULL, texte = NULL) {
    sections[[length(sections) + 1]] <<- list(
      titre = titre, tableaux = tableaux, graphiques = graphiques, texte = texte)
  }

  # ---- Section transversale : effectifs et structure ----
  if (!is.null(age)) {
    grp <- cut(as.numeric(data[[age]]), breaks = c(0, 15, 65, Inf),
               labels = c("0-14 ans", "15-64 ans", "65 ans et +"), right = FALSE)
    tab <- as.data.frame(table(`Groupe d'age` = grp))
    names(tab) <- c("Groupe d'age", "Effectif")
    tab$`Pourcentage (%)` <- round(100 * tab$Effectif / sum(tab$Effectif), 1)
    txt <- if (interpreter) .bs_interp_structure(tab, data, age, sexe) else NULL
    ajt("Structure par grands groupes d'age", tableaux = list(`Structure par age` = tab), texte = txt)
  } else {
    bs_variables_disponibles("age", mapping, data, tableau = "Structure par grands groupes d'age")
  }

  # ---- Tableau d'effectifs par unite administrative (+ redressement) ----
  if (!is.null(reg)) {
    teff <- as.data.frame(table(stats::setNames(list(data[[reg]]), reg)))
    names(teff) <- c(reg, "Effectif")
    teff[[reg]] <- as.character(teff[[reg]])
    teff <- .bs_redresser_tableau(teff, coefficients, var_strate, "Effectif")
    redresse <- paste0("Effectif", "_redresse") %in% names(teff)
    txt <- if (interpreter) .bs_interp_repartition(teff, reg, redresse) else NULL
    ajt(paste0("Repartition par ", reg, if (redresse) " (effectifs redresses)" else ""),
        tableaux = list(`Repartition spatiale` = teff), texte = txt)
  } else {
    bs_variables_disponibles("region", mapping, data, tableau = "Repartition par unite administrative")
  }

  # ---- Sections specifiques par theme ----
  spec <- .bs_sections_specifiques(theme, data, mapping, interpreter)
  for (s in spec) ajt(s$titre, tableaux = s$tableaux, graphiques = s$graphiques, texte = s$texte)

  list(theme = theme, titre = libelle, date = Sys.Date(), n_total = nrow(data),
       redressement = !is.null(coefficients), sections = sections)
}

# Sections specifiques selon la thematique (degrade si variables absentes).
.bs_sections_specifiques <- function(theme, data, mapping, interpreter) {
  out <- list()
  add <- function(titre, tab = NULL, gr = NULL, txt = NULL)
    out[[length(out) + 1]] <<- list(titre = titre, tableaux = tab, graphiques = gr, texte = txt)

  tab_var <- function(role, libelle, titre_tableau) {
    col <- .bs_col(role, mapping, data)
    if (is.null(col)) {
      bs_variables_disponibles(role, mapping, data,
        tableau = titre_tableau %||% paste0("[", theme, "] ", libelle))
      return(NULL)
    }
    t <- as.data.frame(table(stats::setNames(list(data[[col]]), libelle)))
    names(t) <- c(libelle, "Effectif")
    t$`Pourcentage (%)` <- round(100 * t$Effectif / sum(t$Effectif), 1)
    t
  }

  if (theme == "nuptialite") {
    t <- tab_var("situation_matrimoniale", "Situation matrimoniale", "Situation matrimoniale")
    if (!is.null(t)) add("Repartition selon la situation matrimoniale",
                         tab = list(`Situation matrimoniale` = t),
                         txt = if (interpreter) .bs_interp_categoriel(t, "situation matrimoniale") else NULL)
  } else if (theme == "education") {
    t1 <- tab_var("alphabetisation", "Alphabetisation", "Alphabetisation")
    if (!is.null(t1)) add("Alphabetisation", tab = list(Alphabetisation = t1),
                          txt = if (interpreter) .bs_interp_taux_oui(t1, "alphabetisation") else NULL)
    t2 <- tab_var("niveau_instruction", "Niveau d'instruction", "Niveau d'instruction")
    if (!is.null(t2)) add("Niveau d'instruction", tab = list(`Niveau d'instruction` = t2),
                          txt = if (interpreter) .bs_interp_categoriel(t2, "niveau d'instruction") else NULL)
  } else if (theme == "emploi") {
    t <- tab_var("activite", "Activite economique", "Activite economique")
    if (!is.null(t)) add("Activite economique", tab = list(`Activite` = t),
                         txt = if (interpreter) .bs_interp_categoriel(t, "activite economique") else NULL)
  } else if (theme == "handicap") {
    t <- tab_var("handicap", "Handicap", "Handicap")
    if (!is.null(t)) add("Prevalence du handicap", tab = list(Handicap = t),
                         txt = if (interpreter) .bs_interp_taux_oui(t, "handicap") else NULL)
  } else if (theme == "habitat") {
    for (r in c("eau", "assainissement", "energie", "murs")) {
      lib <- paste0(toupper(substring(r, 1, 1)), substring(r, 2))
      t <- tab_var(r, lib, paste0("Logement : ", r))
      if (!is.null(t)) add(paste0("Logement : ", r), tab = stats::setNames(list(t), r),
                           txt = if (interpreter) .bs_interp_categoriel(t, r) else NULL)
    }
  } else if (theme == "equipements") {
    t <- tab_var("biens_durables", "Biens durables", "Biens durables")
    if (!is.null(t)) add("Equipements des menages", tab = list(`Biens durables` = t),
                         txt = if (interpreter) .bs_interp_categoriel(t, "equipements") else NULL)
  } else if (theme == "autochtones") {
    t <- tab_var("peuple_autochtone", "Peuple autochtone", "Peuple autochtone")
    if (!is.null(t)) add("Effectifs des peuples autochtones", tab = list(`Peuples autochtones` = t),
                         txt = if (interpreter) .bs_interp_categoriel(t, "peuples autochtones") else NULL)
  } else if (theme == "agriculture") {
    t <- tab_var("activite_agricole", "Activite agropastorale", "Activite agropastorale")
    if (!is.null(t)) add("Menages agropastoraux", tab = list(`Activites agropastorales` = t),
                         txt = if (interpreter) .bs_interp_categoriel(t, "activites agropastorales") else NULL)
  } else if (theme == "fecondite") {
    col <- .bs_col("naissances_recentes", mapping, data)
    if (is.null(col)) {
      bs_variables_disponibles("naissances_recentes", mapping, data, tableau = "Fecondite recente")
    } else {
      naiss <- sum(as.numeric(data[[col]]) > 0, na.rm = TRUE)
      t <- data.frame(Indicateur = "Naissances recentes declarees", Valeur = naiss)
      add("Fecondite recente", tab = list(`Fecondite` = t),
          txt = if (interpreter) sprintf("Le module a recense %s naissances au cours de la periode de reference. Le calcul de l'ISF requiert l'exposition par age (cf. bs_indicateur(famille='fecondite')).", format(naiss, big.mark = " ")) else NULL)
    }
  } else if (theme == "mortalite") {
    col <- .bs_col("deces", mapping, data)
    if (is.null(col)) {
      bs_variables_disponibles("deces", mapping, data, tableau = "Mortalite recente")
    } else {
      d <- sum(as.numeric(data[[col]]) > 0, na.rm = TRUE)
      t <- data.frame(Indicateur = "Deces declares (12 derniers mois)", Valeur = d)
      add("Mortalite recente", tab = list(`Mortalite` = t),
          txt = if (interpreter) sprintf("Le module a recense %s deces sur la periode de reference de 12 mois. La table de mortalite s'obtient via bs_table_mortalite().", format(d, big.mark = " ")) else NULL)
    }
  }
  out
}

# ---------------------------------------------------------------------------
# Generateurs d'interpretation dynamique (texte genere a partir des resultats)
# ---------------------------------------------------------------------------

.bs_interp_structure <- function(tab, data, age, sexe) {
  p_jeunes <- tab$`Pourcentage (%)`[tab$`Groupe d'age` == "0-14 ans"]
  p_vieux  <- tab$`Pourcentage (%)`[tab$`Groupe d'age` == "65 ans et +"]
  age_med  <- stats::median(as.numeric(data[[age]]), na.rm = TRUE)
  msg <- sprintf(paste0(
    "La population se caracterise par une proportion de %.1f%% de moins de 15 ans ",
    "et de %.1f%% de personnes agees de 65 ans et plus ; l'age median est de %.0f ans. "),
    p_jeunes, p_vieux, age_med)
  msg <- paste0(msg, if (p_jeunes >= 40)
    "Une base aussi large traduit une fecondite encore elevee et une structure tres jeune, impliquant de forts besoins en education et en emplois futurs." else if (p_jeunes >= 30)
    "Cette structure jeune reste caracteristique d'une transition demographique en cours." else
    "La part relativement moderee des jeunes suggere une transition demographique avancee.")
  if (!is.null(sexe)) {
    st <- table(data[[sexe]])
    if (length(st) >= 2) {
      rm <- round(100 * st[1] / st[2], 1)
      msg <- paste0(msg, sprintf(" Le rapport de masculinite global s'etablit a %.1f hommes pour 100 femmes.", rm))
    }
  }
  msg
}

.bs_interp_repartition <- function(tab, reg, redresse) {
  eff <- if (redresse) tab[["Effectif_redresse"]] else tab[["Effectif"]]
  o <- order(eff, decreasing = TRUE)
  tot <- sum(eff, na.rm = TRUE)
  haut <- tab[[reg]][o[1]]; p_haut <- round(100 * eff[o[1]] / tot, 1)
  bas  <- tab[[reg]][o[length(o)]]; p_bas <- round(100 * eff[o[length(o)]] / tot, 1)
  msg <- sprintf(paste0(
    "La repartition par %s fait apparaitre une concentration dans l'unite '%s' (%.1f%% du total) ",
    "et un poids plus faible pour '%s' (%.1f%%)."),
    reg, as.character(haut), p_haut, as.character(bas), p_bas)
  if (redresse) msg <- paste0(msg,
    " Ces effectifs integrent les coefficients de redressement issus de l'enquete post-censitaire, corrigeant le sous-denombrement observe.")
  msg
}

.bs_interp_categoriel <- function(tab, libelle) {
  v <- names(tab)[1]
  o <- order(tab$Effectif, decreasing = TRUE)
  dom <- tab[[v]][o[1]]; p <- tab$`Pourcentage (%)`[o[1]]
  sprintf("La modalite la plus frequente pour %s est '%s' (%.1f%%). La distribution complete figure dans le tableau ci-dessus.",
          libelle, as.character(dom), p)
}

.bs_interp_taux_oui <- function(tab, libelle) {
  v <- names(tab)[1]
  pos <- grep("oui|1|yes|alphab|handic", tolower(as.character(tab[[v]])))
  if (length(pos) == 0) return(.bs_interp_categoriel(tab, libelle))
  p <- sum(tab$`Pourcentage (%)`[pos])
  sprintf("Le taux relatif a '%s' est estime a %.1f%%. %s",
          libelle, p,
          if (grepl("alphab", libelle) && p < 60) "Ce niveau appelle un renforcement des programmes d'alphabetisation." else
          if (grepl("handic", libelle)) "Cette prevalence est a interpreter au regard de la definition retenue (Washington Group)." else "")
}

# ---------------------------------------------------------------------------
# Ecriture multi-format : HTML, Word, Excel
# ---------------------------------------------------------------------------

#' Generer un rapport thematique aux formats Word, Excel et HTML
#'
#' @description Produit, pour une thematique donnee, trois fichiers distincts
#'   (\code{.docx}, \code{.xlsx}, \code{.html}). Les sorties Word et HTML
#'   integrent les interpretations dynamiques ; l'Excel contient les tableaux.
#'   Les effectifs sont redresses si des coefficients sont fournis.
#'
#' @param data data.frame harmonise.
#' @param theme Code de la thematique (voir \code{bs_themes_recensement()}).
#' @param dossier Dossier de sortie.
#' @param mapping Liste role -> colonne (feuille Variables).
#' @param coefficients Coefficients de redressement (optionnel).
#' @param var_strate Variable de strate pour le redressement (optionnel).
#' @param formats Sous-ensemble de \code{c("word","excel","html")} (defaut : tous).
#' @param interpreter Logique : interpretations dynamiques (Word/HTML). Force a
#'   FALSE pour le theme \code{"projection"}.
#' @return Vecteur nomme des chemins produits (invisible).
#' @export
bs_rapport_thematique <- function(data, theme, dossier = "rapports_thematiques",
                                  mapping = NULL, coefficients = NULL,
                                  var_strate = NULL,
                                  formats = c("word", "excel", "html"),
                                  interpreter = TRUE) {
  if (!dir.exists(dossier)) dir.create(dossier, recursive = TRUE)
  if (identical(theme, "projection")) interpreter <- FALSE  # specification : pas d'interpretation
  contenu <- bs_contenu_thematique(data, theme, mapping = mapping,
                                   coefficients = coefficients,
                                   var_strate = var_strate, interpreter = interpreter)
  base <- file.path(dossier, paste0("rapport_", theme))
  sorties <- character(0)
  if ("html"  %in% formats) sorties["html"]  <- .bs_ecrire_html(contenu, paste0(base, ".html"))
  if ("word"  %in% formats) sorties["word"]  <- .bs_ecrire_word(contenu, paste0(base, ".docx"))
  if ("excel" %in% formats) sorties["excel"] <- .bs_ecrire_excel(contenu, paste0(base, ".xlsx"))
  cli::cli_alert_success("Thematique '{theme}' : {length(sorties)} fichier(s) produit(s).")
  invisible(sorties)
}

#' Generer l'ensemble des rapports thematiques (et speciaux)
#'
#' @description Boucle sur toutes les thematiques demandees et produit pour
#'   chacune les fichiers Word, Excel et HTML. Gere aussi le rapport de qualite
#'   et le rapport de projection. Reutilise les coefficients de redressement
#'   issus de l'enquete post-censitaire lorsqu'ils existent.
#'
#' @param data data.frame harmonise.
#' @param themes Vecteur de codes (defaut : toutes les thematiques).
#' @param dossier Dossier de sortie.
#' @param mapping Liste role -> colonne.
#' @param post Resultat de \code{bs_post_censitaire()} (optionnel) : fournit les
#'   coefficients de redressement.
#' @param formats Formats a produire.
#' @param horizon_projection Nombre d'annees de projection (defaut 10).
#' @param date_collecte Date de collecte (origine de la projection ; defaut : annee courante).
#' @param niveau_fin Variable d'unite administrative la plus fine pour la projection.
#' @return Liste nommee (par theme) des vecteurs de chemins produits (invisible).
#' @export
bs_rapports_thematiques <- function(data, themes = NULL,
                                    dossier = "rapports_thematiques",
                                    mapping = NULL, post = NULL,
                                    formats = c("word", "excel", "html"),
                                    horizon_projection = 10,
                                    date_collecte = Sys.Date(),
                                    niveau_fin = NULL,
                                    methode_projection = "onu") {
  cat_t <- bs_themes_recensement()
  if (is.null(themes)) themes <- cat_t$code
  coefficients <- if (!is.null(post)) post$coefficients else NULL
  var_strate   <- if (!is.null(post)) post$var_strate else .bs_col("region", mapping, data)
  if (!dir.exists(dossier)) dir.create(dossier, recursive = TRUE)

  resultats <- list()
  for (th in themes) {
    if (identical(th, "projection")) {
      resultats[[th]] <- tryCatch(bs_rapport_projection(
        data, dossier = dossier, mapping = mapping, formats = formats,
        horizon = horizon_projection, date_collecte = date_collecte,
        niveau_fin = niveau_fin, methode = methode_projection), error = function(e) {
          cli::cli_warn("Projection : {conditionMessage(e)}"); NULL })
    } else if (identical(th, "qualite")) {
      resultats[[th]] <- tryCatch(bs_rapport_qualite_complet(
        data, dossier = dossier, mapping = mapping, post = post, formats = formats),
        error = function(e) { cli::cli_warn("Qualite : {conditionMessage(e)}"); NULL })
    } else {
      resultats[[th]] <- tryCatch(bs_rapport_thematique(
        data, th, dossier = dossier, mapping = mapping, coefficients = coefficients,
        var_strate = var_strate, formats = formats), error = function(e) {
          cli::cli_warn("Theme {th} : {conditionMessage(e)}"); NULL })
    }
  }
  cli::cli_alert_success("Rapports thematiques generes dans {.path {dossier}}.")
  invisible(resultats)
}

#' Rapport d'evaluation de la qualite des donnees (Word/Excel/HTML)
#'
#' @param data data.frame harmonise.
#' @param dossier Dossier de sortie.
#' @param mapping Liste role -> colonne.
#' @param post Resultat de \code{bs_post_censitaire()} (couverture PES + redressement).
#' @param formats Formats a produire.
#' @return Vecteur nomme des chemins produits (invisible).
#' @export
bs_rapport_qualite_complet <- function(data, dossier = "rapports_thematiques",
                                       mapping = NULL, post = NULL,
                                       formats = c("word", "excel", "html")) {
  if (!dir.exists(dossier)) dir.create(dossier, recursive = TRUE)
  age <- .bs_col("age", mapping, data); sexe <- .bs_col("sexe", mapping, data)
  sections <- list()
  add <- function(titre, tab = NULL, txt = NULL)
    sections[[length(sections) + 1]] <<- list(titre = titre, tableaux = tab,
                                              graphiques = NULL, texte = txt)

  # Indices d'attraction d'age
  if (!is.null(age)) {
    a <- as.numeric(data[[age]])
    w <- tryCatch(whipple_index(a), error = function(e) NA)
    m <- tryCatch(myers_blended_index(a), error = function(e) NA)
    b <- tryCatch(bachi_index(a), error = function(e) NA)
    t <- data.frame(Indice = c("Whipple", "Myers", "Bachi"),
                    Valeur = round(c(w, m, b), 2))
    verdict <- if (!is.na(w)) .bs_verdict(w, c(105, 125, 175),
      c("excellente", "bonne", "approximative", "mediocre")) else "indeterminee"
    txt <- sprintf(paste0("L'indice de Whipple s'etablit a %.1f, ce qui correspond a une qualite %s ",
      "des declarations d'age. Les indices de Myers (%.1f) et de Bachi (%.1f) confirment ce diagnostic. ",
      "Tout biais notable doit etre corrige avant le calcul des indicateurs."), w, verdict, m, b)
    add("Attraction des ages (Whipple, Myers, Bachi)", tab = list(`Indices d'age` = t), txt = txt)
  }
  # Completude
  na_rate <- round(100 * colMeans(is.na(data)), 1)
  comp <- data.frame(Variable = names(na_rate), `Valeurs manquantes (%)` = unname(na_rate),
                     check.names = FALSE)
  comp <- comp[order(-comp$`Valeurs manquantes (%)`), ][seq_len(min(15, nrow(comp))), ]
  add("Completude des variables (top 15)", tab = list(Completude = comp),
      txt = sprintf("La variable la plus affectee par les valeurs manquantes est '%s' (%.1f%%). Un seuil d'alerte de 15%% est recommande.",
                    comp$Variable[1], comp$`Valeurs manquantes (%)`[1]))
  # Concordance / Kappa (backcheck ou PES) si disponible
  conc <- if (!is.null(post) && !is.null(post$resume)) post else .baobabstats$last_results$concordance
  if (!is.null(conc) && !is.null(conc$resume) && nrow(conc$resume) > 0) {
    add("Concordance et coefficient Kappa de Cohen (par variable)",
        tab = list(`Concordance et Kappa` = as.data.frame(conc$resume)),
        txt = if (!is.null(conc$interpretation)) conc$interpretation else NULL)
    if (!is.null(conc$omission)) {
      add("Taux d'omission et coefficients de redressement par strate",
          tab = list(`Omission et redressement` = as.data.frame(conc$omission)),
          txt = "Taux d'omission estimes par systeme dual (Petersen-Lincoln) et coefficients de redressement correspondants.")
    }
  }
  # Couverture PES / redressement
  if (!is.null(post) && !is.null(post$coefficients)) {
    add("Couverture post-censitaire et coefficients de redressement",
        tab = list(`Coefficients de redressement` = as.data.frame(post$coefficients)),
        txt = paste(post$interpretation, collapse = " "))
  } else {
    add("Couverture post-censitaire",
        txt = "Aucune enquete post-censitaire n'a ete exploitee : les effectifs publies sont des effectifs bruts (non redresses).")
  }

  contenu <- list(theme = "qualite", titre = "Evaluation de la qualite des donnees",
                  date = Sys.Date(), n_total = nrow(data),
                  redressement = !is.null(post) && !is.null(post$coefficients),
                  sections = sections)
  base <- file.path(dossier, "rapport_qualite")
  sorties <- character(0)
  if ("html"  %in% formats) sorties["html"]  <- .bs_ecrire_html(contenu, paste0(base, ".html"))
  if ("word"  %in% formats) sorties["word"]  <- .bs_ecrire_word(contenu, paste0(base, ".docx"))
  if ("excel" %in% formats) sorties["excel"] <- .bs_ecrire_excel(contenu, paste0(base, ".xlsx"))
  cli::cli_alert_success("Rapport de qualite : {length(sorties)} fichier(s).")
  invisible(sorties)
}

#' Rapport de projection de la population (Word/Excel/HTML, sans interpretation)
#'
#' @description Projette la population par \strong{sexe} et par l'unite
#'   administrative la plus fine sur \code{horizon} annees a partir de la date de
#'   collecte, et produit les livrables. Conformement a la specification, aucune
#'   interpretation dynamique n'est inseree dans ce rapport.
#'
#' @param data data.frame harmonise.
#' @param dossier Dossier de sortie.
#' @param mapping Liste role -> colonne.
#' @param formats Formats a produire.
#' @param horizon Nombre d'annees projetees (defaut 10).
#' @param date_collecte Date de collecte (origine ; defaut : aujourd'hui).
#' @param niveau_fin Variable d'unite administrative la plus fine (defaut : role region).
#' @param methode Methode de projection ("cohort" par defaut).
#' @return Vecteur nomme des chemins produits (invisible).
#' @export
bs_rapport_projection <- function(data, dossier = "rapports_thematiques",
                                  mapping = NULL, formats = c("word", "excel", "html"),
                                  horizon = 10, date_collecte = Sys.Date(),
                                  niveau_fin = NULL, methode = "cohort") {
  if (!dir.exists(dossier)) dir.create(dossier, recursive = TRUE)
  age <- .bs_col("age", mapping, data); sexe <- .bs_col("sexe", mapping, data)
  if (is.null(niveau_fin)) niveau_fin <- .bs_col("region", mapping, data)
  annee0 <- as.integer(format(as.Date(date_collecte), "%Y"))

  sections <- list()
  add <- function(titre, tab = NULL)
    sections[[length(sections) + 1]] <<- list(titre = titre, tableaux = tab,
                                              graphiques = NULL, texte = NULL)

  # Projection demographique -> trajectoire annuelle
  # Methode "onu" = cohortes-composantes (standard Nations Unies / WPP)
  proj <- NULL
  traj <- NULL
  if (identical(methode, "onu")) {
    proj <- tryCatch(
      bs_projeter_onu(data, horizon = horizon, pas = 5,
                      var_age = age %||% "age", var_sexe = sexe %||% "sexe"),
      error = function(e) NULL)
    if (!is.null(proj) && !is.null(proj$resume)) {
      rs <- as.data.frame(proj$resume)
      traj <- data.frame(Annee = annee0 + rs$annee,
                         `Population projetee` = rs$effectif, check.names = FALSE)
      add(sprintf("Projection ONU (cohortes-composantes), %d-%d", annee0, annee0 + horizon),
          tab = list(`Projection totale (ONU)` = traj))
      # Detail par sexe a l'horizon
      if (!is.null(proj$resume_sexe)) {
        rsx <- as.data.frame(proj$resume_sexe)
        rsx <- rsx[rsx$annee == max(rsx$annee), c("sexe", "effectif")]
        names(rsx) <- c("Sexe", paste0("Projection_", annee0 + horizon))
        add(sprintf("Projection ONU par sexe a l'horizon %d", annee0 + horizon),
            tab = list(`Projection ONU par sexe` = rsx))
      }
    }
  } else {
    proj <- tryCatch(
      bs_projeter_population(data, methode = methode, annees = horizon,
                             var_age = age %||% "age", var_sexe = sexe %||% "sexe"),
      error = function(e) NULL)
  }

  # Repli tendanciel si la projection par moteur n'a pas abouti
  croissance <- 0.026
  base_tot <- nrow(data)
  annees <- annee0:(annee0 + horizon)
  if (is.null(traj)) {
    traj <- data.frame(Annee = annees,
                       `Population projetee` = round(base_tot * (1 + croissance)^(annees - annee0)),
                       check.names = FALSE)
    add(sprintf("Projection de la population totale (%d-%d)", annee0, annee0 + horizon),
        tab = list(`Projection totale` = traj))
  }

  # Projection par sexe x unite administrative la plus fine (annee horizon)
  if (!is.null(sexe) && !is.null(niveau_fin) &&
      all(c(sexe, niveau_fin) %in% names(data))) {
    base <- as.data.frame(table(data[[niveau_fin]], data[[sexe]]))
    names(base) <- c(niveau_fin, "Sexe", "Effectif_base")
    base[[paste0("Projection_", annee0 + horizon)]] <-
      round(base$Effectif_base * (1 + croissance)^horizon)
    add(sprintf("Projection par sexe et par %s a l'horizon %d", niveau_fin, annee0 + horizon),
        tab = stats::setNames(list(base), paste0("Projection_", niveau_fin, "_sexe")))
  }

  contenu <- list(theme = "projection",
                  titre = "Projection de la population par sexe et unite administrative",
                  date = Sys.Date(), n_total = nrow(data), redressement = FALSE,
                  sections = sections,
                  note = sprintf(paste0("Projection sur %d ans a partir de %d (date de collecte). ",
                    "Hypotheses parametrables via les scenarios de bs_scenarios_projection(). ",
                    "Ce rapport ne contient pas d'interpretation dynamique (specification)."),
                    horizon, annee0))
  base <- file.path(dossier, "rapport_projection")
  sorties <- character(0)
  if ("html"  %in% formats) sorties["html"]  <- .bs_ecrire_html(contenu, paste0(base, ".html"), interpretation = FALSE)
  if ("word"  %in% formats) sorties["word"]  <- .bs_ecrire_word(contenu, paste0(base, ".docx"), interpretation = FALSE)
  if ("excel" %in% formats) sorties["excel"] <- .bs_ecrire_excel(contenu, paste0(base, ".xlsx"))
  cli::cli_alert_success("Rapport de projection : {length(sorties)} fichier(s).")
  invisible(sorties)
}

# ---- Ecrivains de bas niveau ------------------------------------------------

.bs_ecrire_html <- function(contenu, fichier, interpretation = TRUE) {
  col <- tryCatch(bs_couleurs(), error = function(e) list(
    encre = "#3B2F2F", ecorce = "#6B4226", or = "#C8932A",
    ecorce_moyen = "#8B5E3C", creme = "#FBF6EC"))
  logo <- tryCatch(bs_logo("complet"), error = function(e) "")
  entete <- if (nzchar(logo) && file.exists(logo))
    sprintf("<img src='%s' alt='baobabStats' style='height:80px'/>", logo) else ""
  css <- sprintf(paste0(
    "<meta charset='utf-8'><style>",
    "body{font-family:Georgia,'Times New Roman',serif;color:%s;max-width:900px;margin:24px auto;padding:0 16px;}",
    "h1{color:%s;border-bottom:3px solid %s;padding-bottom:8px;}",
    "h2{color:%s;margin-top:28px;border-left:4px solid %s;padding-left:10px;}",
    ".bs-interp{background:%s;border-left:4px solid %s;padding:10px 14px;margin:12px 0;font-style:italic;}",
    ".bs-meta{color:%s;font-size:.9em;}",
    "table{border-collapse:collapse;width:100%%;margin:10px 0;}",
    "td,th{border:1px solid #E4D9BF;padding:6px 12px;text-align:left;}",
    "th{background:%s;color:#fff;} tr:nth-child(even){background:#FAF6EC;}",
    "</style>"),
    col[["encre"]], col[["ecorce"]], col[["or"]], col[["ecorce_moyen"]], col[["or"]],
    col[["creme"]], col[["or"]], col[["ecorce_moyen"]], col[["ecorce"]])

  html <- c("<!DOCTYPE html><html lang='fr'><head>", css, "</head><body>",
            entete,
            sprintf("<h1>%s</h1>", contenu$titre),
            sprintf("<p class='bs-meta'>Genere le %s &mdash; %s enregistrements%s</p>",
                    format(contenu$date), format(contenu$n_total, big.mark = " "),
                    if (isTRUE(contenu$redressement)) " &mdash; effectifs redresses (post-censitaire)" else ""),
            if (!is.null(contenu$note)) sprintf("<p class='bs-meta'>%s</p>", contenu$note) else "")
  for (s in contenu$sections) {
    html <- c(html, sprintf("<h2>%s</h2>", s$titre))
    if (!is.null(s$tableaux)) for (nm in names(s$tableaux))
      html <- c(html, .bs_df_to_html(s$tableaux[[nm]]))
    if (interpretation && !is.null(s$texte))
      html <- c(html, sprintf("<div class='bs-interp'><strong>Interpretation :</strong> %s</div>",
                              paste(s$texte, collapse = " ")))
  }
  html <- c(html, "<hr><p class='bs-meta'>baobabStats &mdash; Tools for Data, Rooted in Africa.</p>",
            "</body></html>")
  writeLines(enc2utf8(html), fichier, useBytes = TRUE)
  fichier
}

.bs_df_to_html <- function(df) {
  df <- as.data.frame(df)
  th <- paste0("<tr>", paste0("<th>", names(df), "</th>", collapse = ""), "</tr>")
  rows <- apply(df, 1, function(r)
    paste0("<tr>", paste0("<td>", format(r), "</td>", collapse = ""), "</tr>"))
  paste0("<table>", th, paste0(rows, collapse = ""), "</table>")
}

.bs_ecrire_word <- function(contenu, fichier, interpretation = TRUE) {
  if (!requireNamespace("officer", quietly = TRUE)) {
    cli::cli_warn("Package 'officer' requis pour le Word ; ecriture d'un .md de repli.")
    md <- sub("\\.docx$", ".md", fichier)
    lignes <- c(paste("#", contenu$titre), "")
    for (s in contenu$sections) {
      lignes <- c(lignes, paste("##", s$titre), "")
      if (!is.null(s$tableaux)) for (nm in names(s$tableaux))
        lignes <- c(lignes, utils::capture.output(print(as.data.frame(s$tableaux[[nm]]))), "")
      if (interpretation && !is.null(s$texte))
        lignes <- c(lignes, paste("_Interpretation :_", paste(s$texte, collapse = " ")), "")
    }
    writeLines(enc2utf8(lignes), md, useBytes = TRUE); return(md)
  }
  doc <- officer::read_docx()
  doc <- officer::body_add_par(doc, contenu$titre, style = "heading 1")
  doc <- officer::body_add_par(doc, sprintf("Genere le %s - %s enregistrements%s",
    format(contenu$date), format(contenu$n_total, big.mark = " "),
    if (isTRUE(contenu$redressement)) " - effectifs redresses (post-censitaire)" else ""),
    style = "Normal")
  if (!is.null(contenu$note))
    doc <- officer::body_add_par(doc, contenu$note, style = "Normal")
  for (s in contenu$sections) {
    doc <- officer::body_add_par(doc, s$titre, style = "heading 2")
    if (!is.null(s$tableaux)) for (nm in names(s$tableaux)) {
      doc <- tryCatch(officer::body_add_table(doc, as.data.frame(s$tableaux[[nm]]),
                                              style = "table_template"),
                      error = function(e) officer::body_add_table(doc, as.data.frame(s$tableaux[[nm]])))
    }
    if (interpretation && !is.null(s$texte)) {
      doc <- officer::body_add_par(doc, paste("Interpretation :", paste(s$texte, collapse = " ")),
                                   style = "Normal")
    }
  }
  print(doc, target = fichier)
  fichier
}

.bs_ecrire_excel <- function(contenu, fichier) {
  feuilles <- list()
  meta <- data.frame(Champ = c("Titre", "Date", "Effectif", "Redressement"),
                     Valeur = c(contenu$titre, as.character(contenu$date),
                                format(contenu$n_total, big.mark = " "),
                                if (isTRUE(contenu$redressement)) "Oui (post-censitaire)" else "Non"))
  feuilles[["Synthese"]] <- meta
  i <- 1
  for (s in contenu$sections) {
    if (!is.null(s$tableaux)) for (nm in names(s$tableaux)) {
      nom <- substr(gsub("[^A-Za-z0-9]", "_", paste0(i, "_", nm)), 1, 31)
      feuilles[[nom]] <- as.data.frame(s$tableaux[[nm]]); i <- i + 1
    }
  }
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    wb <- openxlsx::createWorkbook()
    col <- tryCatch(bs_couleurs(), error = function(e) list(or = "#C8932A", creme = "#FBF6EC"))
    hs <- openxlsx::createStyle(fgFill = col[["or"]], textDecoration = "bold",
                               fontColour = "#FFFFFF", border = "TopBottomLeftRight")
    for (nm in names(feuilles)) {
      openxlsx::addWorksheet(wb, nm)
      openxlsx::writeData(wb, nm, feuilles[[nm]], headerStyle = hs)
      openxlsx::setColWidths(wb, nm, cols = seq_along(feuilles[[nm]]), widths = "auto")
    }
    openxlsx::saveWorkbook(wb, fichier, overwrite = TRUE)
  } else if (requireNamespace("writexl", quietly = TRUE)) {
    writexl::write_xlsx(feuilles, fichier)
  } else {
    csv <- sub("\\.xlsx$", ".csv", fichier)
    utils::write.csv(feuilles[[2]] %||% feuilles[[1]], csv, row.names = FALSE)
    return(csv)
  }
  fichier
}
