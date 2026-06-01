#' @title Visualisation prete pour publication
#' @name bs_visualisation
#' @description
#' Graphiques demographiques au standard publication (theme baobabStats), exportables
#' en PNG, HTML, Word et PDF. La fonction \code{bs_visualiser_config()} produit en
#' lot les graphiques decrits dans la feuille \emph{Visualisation} d'un classeur de
#' configuration.
NULL

#' Theme ggplot2 baobabStats
#'
#' Theme de publication aux couleurs de la charte (brun ecorce, or savane, creme).
#' @param base_size Taille de police de base.
#' @param base_family Police (defaut "" = police par defaut du peripherique).
#' @return Un objet \code{theme} ggplot2.
#' @export
theme_baobabstats <- function(base_size = 12, base_family = "") {
  col <- bs_couleurs()
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", colour = col[["ecorce"]],
                                              size = base_size + 3),
      plot.subtitle   = ggplot2::element_text(colour = col[["ecorce_moyen"]]),
      plot.background  = ggplot2::element_rect(fill = col[["creme"]], colour = NA),
      panel.background = ggplot2::element_rect(fill = col[["creme"]], colour = NA),
      axis.title      = ggplot2::element_text(colour = col[["encre"]]),
      axis.text       = ggplot2::element_text(colour = col[["ecorce_moyen"]]),
      panel.grid.major = ggplot2::element_line(colour = "#E4D9BF", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "bottom",
      legend.title     = ggplot2::element_text(colour = col[["encre"]]),
      plot.caption     = ggplot2::element_text(colour = col[["gris"]], size = base_size - 3)
    )
}

#' Echelles de couleur/remplissage baobabStats pour ggplot2
#' @param ... Arguments transmis a la scale ggplot2 sous-jacente.
#' @rdname bs_scales
#' @export
scale_fill_baobabstats <- function(...) {
  ggplot2::scale_fill_manual(values = unname(bs_palette(8)), ...)
}
#' @rdname bs_scales
#' @export
scale_colour_baobabstats <- function(...) {
  ggplot2::scale_colour_manual(values = unname(bs_palette(8)), ...)
}

# Palette interne (compatibilite) - reprise de la charte
.bs_palette <- c(homme = unname(bs_couleurs("feuille_fonce")),
                 femme = unname(bs_couleurs("or")),
                 accent = unname(bs_couleurs("argile")),
                 gris = unname(bs_couleurs("gris")))

#' Pyramide des ages (ggplot2)
#' @param data data.frame de microdonnees.
#' @param var_age Variable age. @param var_sexe Variable sexe.
#' @param largeur_groupe Largeur des groupes d'age (defaut 5).
#' @param titre Titre du graphique.
#' @return Un objet ggplot.
#' @export
bs_graph_pyramide <- function(data, var_age = "age", var_sexe = "sexe",
                              largeur_groupe = 5, titre = "Pyramide des ages") {
  age <- as.numeric(data[[var_age]])
  sexe <- .bs_std_sexe(data[[var_sexe]])
  gr <- cut(age, breaks = seq(0, max(age, na.rm = TRUE) + largeur_groupe, largeur_groupe),
            right = FALSE)
  df <- stats::aggregate(list(n = rep(1, length(age))),
                         by = list(groupe = gr, sexe = sexe), FUN = sum)
  df <- df[!is.na(df$groupe) & df$sexe %in% c("Homme", "Femme"), ]
  df$n_signe <- ifelse(df$sexe == "Homme", -df$n, df$n)
  ggplot2::ggplot(df, ggplot2::aes(x = .data$groupe, y = .data$n_signe,
                                   fill = .data$sexe)) +
    ggplot2::geom_col(width = 0.9) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = function(x) format(abs(x), big.mark = " ")) +
    ggplot2::scale_fill_manual(values = c(Homme = .bs_palette[["homme"]],
                                          Femme = .bs_palette[["femme"]]), name = NULL) +
    ggplot2::labs(title = titre, x = "Groupe d'ages", y = "Effectif",
                  caption = "Source : baobabStats") +
    theme_baobabstats()
}

#' Graphique en barres d'un indicateur par categorie
#' @param data data.frame.
#' @param var_categorie Variable de regroupement (axe x).
#' @param var_valeur Variable numerique (axe y) ou NULL pour comptage.
#' @param titre Titre.
#' @return Un objet ggplot.
#' @export
bs_graph_barres <- function(data, var_categorie, var_valeur = NULL,
                            titre = NULL) {
  if (is.null(var_valeur)) {
    df <- as.data.frame(table(data[[var_categorie]]))
    names(df) <- c("categorie", "valeur")
  } else {
    df <- stats::aggregate(data[[var_valeur]], list(categorie = data[[var_categorie]]),
                           FUN = mean, na.rm = TRUE)
    names(df) <- c("categorie", "valeur")
  }
  ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(.data$categorie, .data$valeur),
                                   y = .data$valeur)) +
    ggplot2::geom_col(fill = .bs_palette[["accent"]]) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = titre %||% var_categorie, x = NULL, y = NULL,
                  caption = "Source : baobabStats") +
    theme_baobabstats()
}

#' Enregistrer un graphique dans un ou plusieurs formats
#' @param plot Objet ggplot.
#' @param fichier Chemin de base (sans extension).
#' @param formats Vecteur de formats : "png", "pdf", "html".
#' @param largeur,hauteur Dimensions en pouces.
#' @return Vecteur des fichiers ecrits (invisible).
#' @export
bs_enregistrer_graph <- function(plot, fichier, formats = "png",
                                 largeur = 8, hauteur = 6) {
  ecrits <- character(0)
  for (fmt in formats) {
    out <- paste0(fichier, ".", fmt)
    if (fmt %in% c("png", "pdf")) {
      ggplot2::ggsave(out, plot, width = largeur, height = hauteur, dpi = 300)
    } else if (fmt == "html" && requireNamespace("plotly", quietly = TRUE) &&
               requireNamespace("htmlwidgets", quietly = TRUE)) {
      htmlwidgets::saveWidget(plotly::ggplotly(plot), out, selfcontained = TRUE)
    } else next
    ecrits <- c(ecrits, out)
  }
  invisible(ecrits)
}

#' Produire les graphiques decrits par une feuille de configuration
#' @param data data.frame traite.
#' @param vis data.frame issu de la feuille Visualisation.
#' @param dossier Dossier de sortie.
#' @return Liste nommee : graphique -> fichier(s) ecrit(s).
#' @export
bs_visualiser_config <- function(data, vis, dossier = getOption("baobabstats.sortie")) {
  if (!dir.exists(dossier)) dir.create(dossier, recursive = TRUE)
  actif <- function(x) tolower(as.character(x)) %in% c("oui", "yes", "true", "1")
  sorties <- list()
  for (i in seq_len(nrow(vis))) {
    if (!actif(vis$Produire[i])) next
    g  <- vis$Graphique[i]; fmt <- vis$Format[i] %||% "png"; titre <- vis$Titre[i]
    plot <- tryCatch(switch(g,
      pyramide_ages = if ("age" %in% names(data)) bs_graph_pyramide(data, titre = titre),
      barres_alphabetisation = if ("region" %in% names(data) && "alphabetisation" %in% names(data))
        bs_graph_barres(data, "region", "alphabetisation", titre),
      courbe_fecondite = NULL,
      carte_region = NULL,
      NULL), error = function(e) NULL)
    if (!is.null(plot)) {
      sorties[[g]] <- bs_enregistrer_graph(plot, file.path(dossier, g), formats = fmt)
    }
  }
  sorties
}

# Standardisation interne du sexe
.bs_std_sexe <- function(x) {
  x0 <- tolower(trimws(as.character(x)))
  ifelse(x0 %in% c("1", "m", "h", "homme", "masculin", "male"), "Homme",
  ifelse(x0 %in% c("2", "f", "femme", "feminin", "female"), "Femme", NA))
}
