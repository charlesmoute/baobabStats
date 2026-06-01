#' @title Génération de fiches synthétiques
#' @description Module pour la création de fiches synthétiques par unité administrative
#' @name synthetic_reports

#' Générer une fiche synthétique pour une unité administrative
#'
#' @param data Data frame contenant les données de recensement
#' @param admin_var Nom de la variable d'unité administrative
#' @param admin_value Valeur de l'unité administrative (NULL pour toutes)
#' @param age_var Nom de la variable d'âge
#' @param sex_var Nom de la variable de sexe
#' @param output_format Format de sortie ("html", "pdf", "word", "excel")
#' @param output_file Chemin du fichier de sortie
#' @param sections Sections à inclure dans le rapport
#' @param language Langue du rapport ("fr" ou "en")
#' @param include_charts Inclure les graphiques
#' @param logo_path Chemin vers un logo à inclure
#'
#' @return Chemin du fichier généré
#' @export
#'
#' @examples
#' \dontrun{
#' generate_synthetic_report(
#'   data = census_data,
#'   admin_var = "region",
#'   admin_value = "Nord",
#'   output_format = "pdf",
#'   output_file = "fiche_nord.pdf"
#' )
#' }
generate_synthetic_report <- function(data,
                                      admin_var = NULL,
                                      admin_value = NULL,
                                      age_var = "age",
                                      sex_var = "sex",
                                      output_format = c("html", "pdf", "word", "excel"),
                                      output_file = NULL,
                                      sections = c("summary", "structure", "pyramid", 
                                                  "indicators", "nuptiality", "fertility",
                                                  "mortality", "migration", "disability", "gender"),
                                      language = "fr",
                                      include_charts = TRUE,
                                      logo_path = NULL) {
  
  output_format <- match.arg(output_format)
  
  # Filtrer les données si nécessaire
  if (!is.null(admin_var) && !is.null(admin_value)) {
    data <- data[data[[admin_var]] == admin_value, ]
  }
  
  # Préparer le titre
  title <- if (!is.null(admin_value)) {
    paste("Fiche synthétique -", admin_value)
  } else {
    "Fiche synthétique - Population totale"
  }
  
  # Collecter les statistiques
  report_content <- list(
    title = title,
    date = Sys.Date(),
    n_total = nrow(data),
    admin_unit = admin_value
  )
  
  # Section Résumé
  if ("summary" %in% sections) {
    report_content$summary <- list(
      population = nrow(data),
      n_households = if ("household_id" %in% names(data)) {
        length(unique(data$household_id))
      } else NA,
      avg_household_size = if ("household_id" %in% names(data)) {
        round(nrow(data) / length(unique(data$household_id)), 2)
      } else NA
    )
  }
  
  # Section Structure
  if ("structure" %in% sections && age_var %in% names(data)) {
    data$age_group_broad <- cut(
      data[[age_var]],
      breaks = c(0, 15, 65, Inf),
      labels = c("0-14 ans", "15-64 ans", "65+ ans"),
      right = FALSE
    )
    
    structure_table <- as.data.frame(table(data$age_group_broad))
    names(structure_table) <- c("Groupe d'âge", "Effectif")
    structure_table$Pourcentage <- round(structure_table$Effectif / sum(structure_table$Effectif) * 100, 1)
    
    report_content$structure <- structure_table
  }
  
  # Section Pyramide
  if ("pyramid" %in% sections && all(c(age_var, sex_var) %in% names(data))) {
    data$age_group_5 <- cut(
      data[[age_var]],
      breaks = c(seq(0, 85, 5), Inf),
      labels = c(paste0(seq(0, 80, 5), "-", seq(4, 84, 5)), "85+"),
      right = FALSE
    )
    
    pyramid_data <- as.data.frame(table(data$age_group_5, data[[sex_var]]))
    names(pyramid_data) <- c("age_group", "sex", "effectif")
    
    report_content$pyramid <- pyramid_data
  }
  
  # Section Indicateurs
  if ("indicators" %in% sections) {
    indicators <- list()
    
    if (age_var %in% names(data)) {
      indicators$age_moyen <- round(mean(data[[age_var]], na.rm = TRUE), 1)
      indicators$age_median <- median(data[[age_var]], na.rm = TRUE)
      
      # Ratio de dépendance
      pop_0_14 <- sum(data[[age_var]] < 15, na.rm = TRUE)
      pop_65_plus <- sum(data[[age_var]] >= 65, na.rm = TRUE)
      pop_15_64 <- sum(data[[age_var]] >= 15 & data[[age_var]] < 65, na.rm = TRUE)
      
      if (pop_15_64 > 0) {
        indicators$ratio_dependance <- round((pop_0_14 + pop_65_plus) / pop_15_64 * 100, 1)
        indicators$ratio_jeunes <- round(pop_0_14 / pop_15_64 * 100, 1)
        indicators$ratio_vieux <- round(pop_65_plus / pop_15_64 * 100, 1)
      }
    }
    
    if (sex_var %in% names(data)) {
      sex_table <- table(data[[sex_var]])
      if (length(sex_table) >= 2) {
        indicators$rapport_masculinite <- round(sex_table[1] / sex_table[2] * 100, 1)
        indicators$pct_hommes <- round(sex_table[1] / sum(sex_table) * 100, 1)
        indicators$pct_femmes <- round(sex_table[2] / sum(sex_table) * 100, 1)
      }
    }
    
    report_content$indicators <- indicators
  }
  
  # Section Nuptialité
  if ("nuptiality" %in% sections && "marital_status" %in% names(data)) {
    nuptiality_table <- as.data.frame(table(data$marital_status))
    names(nuptiality_table) <- c("Situation matrimoniale", "Effectif")
    nuptiality_table$Pourcentage <- round(nuptiality_table$Effectif / sum(nuptiality_table$Effectif) * 100, 1)
    
    report_content$nuptiality <- nuptiality_table
  }
  
  # Section Fécondité
  if ("fertility" %in% sections && "children_born" %in% names(data)) {
    # Femmes en âge de procréer
    if (all(c(age_var, sex_var) %in% names(data))) {
      women_fertile <- data[data[[sex_var]] %in% c(2, "F", "Female", "Femme") &
                           data[[age_var]] >= 15 & data[[age_var]] < 50, ]
      
      if (nrow(women_fertile) > 0) {
        report_content$fertility <- list(
          n_women_fertile = nrow(women_fertile),
          avg_children = round(mean(women_fertile$children_born, na.rm = TRUE), 2),
          total_children = sum(women_fertile$children_born, na.rm = TRUE)
        )
      }
    }
  }
  
  # Section Handicap
  if ("disability" %in% sections && "disability" %in% names(data)) {
    n_disabled <- sum(data$disability %in% c(1, "Yes", "Oui", TRUE), na.rm = TRUE)
    
    report_content$disability <- list(
      n_disabled = n_disabled,
      taux_handicap = round(n_disabled / nrow(data) * 100, 2)
    )
  }
  
  # Section Genre
  if ("gender" %in% sections && sex_var %in% names(data)) {
    gender_stats <- list()
    
    if ("education_level" %in% names(data)) {
      gender_education <- as.data.frame(table(data[[sex_var]], data$education_level))
      names(gender_education) <- c("Sexe", "Niveau", "Effectif")
      gender_stats$education <- gender_education
    }
    
    if ("employment_status" %in% names(data)) {
      gender_employment <- as.data.frame(table(data[[sex_var]], data$employment_status))
      names(gender_employment) <- c("Sexe", "Statut", "Effectif")
      gender_stats$employment <- gender_employment
    }
    
    report_content$gender <- gender_stats
  }
  
  # Générer le fichier de sortie
  if (is.null(output_file)) {
    admin_name <- if (!is.null(admin_value)) gsub(" ", "_", admin_value) else "total"
    output_file <- paste0("fiche_", admin_name, "_", Sys.Date(), ".", output_format)
  }
  
  # Générer selon le format
  if (output_format == "html") {
    generate_html_report(report_content, output_file, include_charts, logo_path, language)
  } else if (output_format == "excel") {
    generate_excel_report(report_content, output_file)
  } else if (output_format == "pdf") {
    # Générer HTML puis convertir en PDF si possible
    html_file <- tempfile(fileext = ".html")
    generate_html_report(report_content, html_file, include_charts, logo_path, language)
    
    if (requireNamespace("pagedown", quietly = TRUE)) {
      pagedown::chrome_print(html_file, output_file)
    } else {
      warning("Package 'pagedown' non disponible. Génération HTML à la place.")
      file.copy(html_file, gsub("\\.pdf$", ".html", output_file))
      output_file <- gsub("\\.pdf$", ".html", output_file)
    }
  } else if (output_format == "word") {
    # Utiliser officer si disponible
    if (requireNamespace("officer", quietly = TRUE)) {
      generate_word_report(report_content, output_file, logo_path, language)
    } else {
      warning("Package 'officer' non disponible. Génération Excel à la place.")
      generate_excel_report(report_content, gsub("\\.docx$", ".xlsx", output_file))
      output_file <- gsub("\\.docx$", ".xlsx", output_file)
    }
  }
  
  message("Rapport généré: ", output_file)
  return(invisible(output_file))
}

#' Générer un rapport HTML
#' @keywords internal
generate_html_report <- function(content, output_file, include_charts, logo_path, language) {
  
  # Labels selon la langue
  labels <- if (language == "fr") {
    list(
      title = "Fiche Synthétique",
      date = "Date",
      population = "Population totale",
      structure = "Structure de la population",
      pyramid = "Pyramide des âges",
      indicators = "Indicateurs démographiques",
      nuptiality = "Nuptialité",
      fertility = "Fécondité",
      disability = "Handicap",
      gender = "Genre"
    )
  } else {
    list(
      title = "Synthetic Report",
      date = "Date",
      population = "Total population",
      structure = "Population structure",
      pyramid = "Age pyramid",
      indicators = "Demographic indicators",
      nuptiality = "Nuptiality",
      fertility = "Fertility",
      disability = "Disability",
      gender = "Gender"
    )
  }
  
  # Construire le HTML
  html <- paste0(
    "<!DOCTYPE html>\n",
    "<html lang='", language, "'>\n",
    "<head>\n",
    "  <meta charset='UTF-8'>\n",
    "  <meta name='viewport' content='width=device-width, initial-scale=1.0'>\n",
    "  <title>", content$title, "</title>\n",
    "  <style>\n",
    "    body { font-family: 'Segoe UI', Arial, sans-serif; margin: 40px; line-height: 1.6; color: #333; }\n",
    "    h1 { color: #2c3e50; border-bottom: 3px solid #3498db; padding-bottom: 10px; }\n",
    "    h2 { color: #34495e; margin-top: 30px; }\n",
    "    table { border-collapse: collapse; width: 100%; margin: 20px 0; }\n",
    "    th, td { border: 1px solid #ddd; padding: 12px; text-align: left; }\n",
    "    th { background-color: #3498db; color: white; }\n",
    "    tr:nth-child(even) { background-color: #f9f9f9; }\n",
    "    .summary-box { background: #ecf0f1; padding: 20px; border-radius: 8px; margin: 20px 0; }\n",
    "    .indicator { display: inline-block; margin: 10px; padding: 15px; background: #fff; border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); }\n",
    "    .indicator-value { font-size: 24px; font-weight: bold; color: #3498db; }\n",
    "    .indicator-label { font-size: 12px; color: #7f8c8d; }\n",
    "    .footer { margin-top: 40px; padding-top: 20px; border-top: 1px solid #ddd; font-size: 12px; color: #7f8c8d; }\n",
    "    @media print { body { margin: 20px; } }\n",
    "  </style>\n",
    "</head>\n",
    "<body>\n"
  )
  
  # Logo si fourni
  if (!is.null(logo_path) && file.exists(logo_path)) {
    html <- paste0(html, "  <img src='", logo_path, "' alt='Logo' style='max-height: 80px; float: right;'>\n")
  }
  
  # Titre et date
  html <- paste0(
    html,
    "  <h1>", content$title, "</h1>\n",
    "  <p><strong>", labels$date, ":</strong> ", content$date, "</p>\n",
    "  <p><strong>", labels$population, ":</strong> ", format(content$n_total, big.mark = " "), "</p>\n"
  )
  
  # Résumé
  if (!is.null(content$summary)) {
    html <- paste0(
      html,
      "  <div class='summary-box'>\n",
      "    <h2>Résumé</h2>\n",
      "    <div class='indicator'>\n",
      "      <div class='indicator-value'>", format(content$summary$population, big.mark = " "), "</div>\n",
      "      <div class='indicator-label'>Population</div>\n",
      "    </div>\n"
    )
    
    if (!is.na(content$summary$n_households)) {
      html <- paste0(
        html,
        "    <div class='indicator'>\n",
        "      <div class='indicator-value'>", format(content$summary$n_households, big.mark = " "), "</div>\n",
        "      <div class='indicator-label'>Ménages</div>\n",
        "    </div>\n",
        "    <div class='indicator'>\n",
        "      <div class='indicator-value'>", content$summary$avg_household_size, "</div>\n",
        "      <div class='indicator-label'>Taille moyenne ménage</div>\n",
        "    </div>\n"
      )
    }
    
    html <- paste0(html, "  </div>\n")
  }
  
  # Structure
  if (!is.null(content$structure)) {
    html <- paste0(
      html,
      "  <h2>", labels$structure, "</h2>\n",
      "  <table>\n",
      "    <tr><th>Groupe d'âge</th><th>Effectif</th><th>Pourcentage</th></tr>\n"
    )
    
    for (i in 1:nrow(content$structure)) {
      html <- paste0(
        html,
        "    <tr><td>", content$structure[i, 1], "</td>",
        "<td>", format(content$structure[i, 2], big.mark = " "), "</td>",
        "<td>", content$structure[i, 3], "%</td></tr>\n"
      )
    }
    
    html <- paste0(html, "  </table>\n")
  }
  
  # Indicateurs
  if (!is.null(content$indicators)) {
    html <- paste0(html, "  <h2>", labels$indicators, "</h2>\n  <div>\n")
    
    ind <- content$indicators
    
    if (!is.null(ind$age_moyen)) {
      html <- paste0(html, "    <div class='indicator'><div class='indicator-value'>", 
                    ind$age_moyen, "</div><div class='indicator-label'>Âge moyen</div></div>\n")
    }
    
    if (!is.null(ind$rapport_masculinite)) {
      html <- paste0(html, "    <div class='indicator'><div class='indicator-value'>", 
                    ind$rapport_masculinite, "</div><div class='indicator-label'>Rapport de masculinité</div></div>\n")
    }
    
    if (!is.null(ind$ratio_dependance)) {
      html <- paste0(html, "    <div class='indicator'><div class='indicator-value'>", 
                    ind$ratio_dependance, "%</div><div class='indicator-label'>Ratio de dépendance</div></div>\n")
    }
    
    html <- paste0(html, "  </div>\n")
  }
  
  # Nuptialité
  if (!is.null(content$nuptiality)) {
    html <- paste0(
      html,
      "  <h2>", labels$nuptiality, "</h2>\n",
      "  <table>\n",
      "    <tr><th>Situation matrimoniale</th><th>Effectif</th><th>Pourcentage</th></tr>\n"
    )
    
    for (i in 1:nrow(content$nuptiality)) {
      html <- paste0(
        html,
        "    <tr><td>", content$nuptiality[i, 1], "</td>",
        "<td>", format(content$nuptiality[i, 2], big.mark = " "), "</td>",
        "<td>", content$nuptiality[i, 3], "%</td></tr>\n"
      )
    }
    
    html <- paste0(html, "  </table>\n")
  }
  
  # Handicap
  if (!is.null(content$disability)) {
    html <- paste0(
      html,
      "  <h2>", labels$disability, "</h2>\n",
      "  <p>Personnes en situation de handicap: <strong>", 
      format(content$disability$n_disabled, big.mark = " "), "</strong> (",
      content$disability$taux_handicap, "%)</p>\n"
    )
  }
  
  # Footer
  html <- paste0(
    html,
    "  <div class='footer'>\n",
    "    <p>Généré par CensusAnalytics - ", Sys.time(), "</p>\n",
    "  </div>\n",
    "</body>\n",
    "</html>\n"
  )
  
  writeLines(html, output_file)
}

#' Générer un rapport Excel
#' @keywords internal
generate_excel_report <- function(content, output_file) {
  
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' requis pour l'export Excel")
  }
  
  wb <- openxlsx::createWorkbook()
  
  # Feuille Résumé
  openxlsx::addWorksheet(wb, "Résumé")
  summary_df <- data.frame(
    Indicateur = c("Titre", "Date", "Population totale"),
    Valeur = c(content$title, as.character(content$date), content$n_total)
  )
  openxlsx::writeData(wb, "Résumé", summary_df, startRow = 1)
  
  # Structure
  if (!is.null(content$structure)) {
    openxlsx::addWorksheet(wb, "Structure")
    openxlsx::writeData(wb, "Structure", content$structure)
  }
  
  # Pyramide
  if (!is.null(content$pyramid)) {
    openxlsx::addWorksheet(wb, "Pyramide")
    openxlsx::writeData(wb, "Pyramide", content$pyramid)
  }
  
  # Indicateurs
  if (!is.null(content$indicators)) {
    openxlsx::addWorksheet(wb, "Indicateurs")
    ind_df <- data.frame(
      Indicateur = names(content$indicators),
      Valeur = unlist(content$indicators)
    )
    openxlsx::writeData(wb, "Indicateurs", ind_df)
  }
  
  # Nuptialité
  if (!is.null(content$nuptiality)) {
    openxlsx::addWorksheet(wb, "Nuptialité")
    openxlsx::writeData(wb, "Nuptialité", content$nuptiality)
  }
  
  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
}

#' Générer un rapport Word
#' @keywords internal
generate_word_report <- function(content, output_file, logo_path, language) {
  
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("Package 'officer' requis pour l'export Word")
  }
  
  doc <- officer::read_docx()
  
  # Titre
  doc <- officer::body_add_par(doc, content$title, style = "heading 1")
  doc <- officer::body_add_par(doc, paste("Date:", content$date))
  doc <- officer::body_add_par(doc, paste("Population totale:", format(content$n_total, big.mark = " ")))
  
  # Structure
  if (!is.null(content$structure)) {
    doc <- officer::body_add_par(doc, "Structure de la population", style = "heading 2")
    doc <- officer::body_add_table(doc, content$structure, style = "table_template")
  }
  
  # Indicateurs
  if (!is.null(content$indicators)) {
    doc <- officer::body_add_par(doc, "Indicateurs démographiques", style = "heading 2")
    ind_df <- data.frame(
      Indicateur = names(content$indicators),
      Valeur = unlist(content$indicators)
    )
    doc <- officer::body_add_table(doc, ind_df, style = "table_template")
  }
  
  print(doc, target = output_file)
}

#' Générer des fiches synthétiques pour toutes les unités administratives
#'
#' @param data Data frame contenant les données de recensement
#' @param admin_var Nom de la variable d'unité administrative
#' @param output_dir Répertoire de sortie
#' @param output_format Format de sortie
#' @param ... Arguments supplémentaires passés à generate_synthetic_report
#'
#' @return Vecteur des chemins des fichiers générés
#' @export
generate_all_reports <- function(data,
                                 admin_var,
                                 output_dir = "reports",
                                 output_format = "html",
                                 ...) {
  
  # Créer le répertoire si nécessaire
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Obtenir les unités uniques
  units <- unique(data[[admin_var]])
  units <- units[!is.na(units)]
  
  message("Génération de ", length(units), " fiches...")
  
  files <- character(length(units))
  
  for (i in seq_along(units)) {
    unit <- units[i]
    message("  [", i, "/", length(units), "] ", unit)
    
    output_file <- file.path(
      output_dir,
      paste0("fiche_", gsub("[^a-zA-Z0-9]", "_", unit), ".", output_format)
    )
    
    files[i] <- generate_synthetic_report(
      data = data,
      admin_var = admin_var,
      admin_value = unit,
      output_format = output_format,
      output_file = output_file,
      ...
    )
  }
  
  message("Terminé. ", length(files), " fiches générées dans ", output_dir)
  return(invisible(files))
}

#' Créer un rapport comparatif entre unités administratives
#'
#' @param data Data frame contenant les données de recensement
#' @param admin_var Nom de la variable d'unité administrative
#' @param admin_values Vecteur des unités à comparer
#' @param output_file Chemin du fichier de sortie
#'
#' @return Chemin du fichier généré
#' @export
generate_comparison_report <- function(data,
                                       admin_var,
                                       admin_values,
                                       output_file = "comparison_report.html") {
  
  # Calculer les indicateurs pour chaque unité
  results <- lapply(admin_values, function(unit) {
    subset_data <- data[data[[admin_var]] == unit, ]
    
    list(
      unit = unit,
      population = nrow(subset_data),
      age_moyen = if ("age" %in% names(subset_data)) {
        round(mean(subset_data$age, na.rm = TRUE), 1)
      } else NA,
      pct_0_14 = if ("age" %in% names(subset_data)) {
        round(sum(subset_data$age < 15, na.rm = TRUE) / nrow(subset_data) * 100, 1)
      } else NA,
      pct_65_plus = if ("age" %in% names(subset_data)) {
        round(sum(subset_data$age >= 65, na.rm = TRUE) / nrow(subset_data) * 100, 1)
      } else NA
    )
  })
  
  # Créer le tableau comparatif
  comparison_df <- do.call(rbind, lapply(results, as.data.frame))
  
  # Générer le HTML
  html <- paste0(
    "<!DOCTYPE html>\n<html>\n<head>\n",
    "<meta charset='UTF-8'>\n",
    "<title>Rapport comparatif</title>\n",
    "<style>\n",
    "body { font-family: Arial, sans-serif; margin: 40px; }\n",
    "table { border-collapse: collapse; width: 100%; }\n",
    "th, td { border: 1px solid #ddd; padding: 12px; text-align: right; }\n",
    "th { background-color: #3498db; color: white; }\n",
    "td:first-child { text-align: left; font-weight: bold; }\n",
    "</style>\n</head>\n<body>\n",
    "<h1>Rapport comparatif</h1>\n",
    "<table>\n",
    "<tr><th>Unité</th><th>Population</th><th>Âge moyen</th><th>% 0-14 ans</th><th>% 65+ ans</th></tr>\n"
  )
  
  for (i in 1:nrow(comparison_df)) {
    html <- paste0(
      html,
      "<tr><td>", comparison_df$unit[i], "</td>",
      "<td>", format(comparison_df$population[i], big.mark = " "), "</td>",
      "<td>", comparison_df$age_moyen[i], "</td>",
      "<td>", comparison_df$pct_0_14[i], "%</td>",
      "<td>", comparison_df$pct_65_plus[i], "%</td></tr>\n"
    )
  }
  
  html <- paste0(html, "</table>\n</body>\n</html>\n")
  
  writeLines(html, output_file)
  message("Rapport comparatif généré: ", output_file)
  
  return(invisible(output_file))
}
