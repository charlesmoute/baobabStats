# Guide d'installation et de déploiement — baobabStats

## 1. Prérequis

- **R ≥ 4.1.0** (Windows, macOS ou Linux).
- **RStudio Desktop ou RStudio Server** (recommandé pour les addins et Shiny).

## 2. Dépendances R

baobabStats sépare volontairement les dépendances **essentielles** (Imports,
légères) des dépendances **optionnelles** (Suggests, plus lourdes) afin de
faciliter le déploiement en contexte de connectivité limitée.

```r
# Dépendances essentielles (installées automatiquement)
install.packages(c("dplyr","tidyr","stringr","stringdist","purrr","tibble",
                   "rlang","glue","cli","ggplot2","scales","readxl","writexl",
                   "openxlsx","haven","jsonlite"))

# Dépendances optionnelles selon les fonctions utilisées
install.packages(c("shiny","shinydashboard","shinyWidgets","DT","plotly",
                   "flextable","gtsummary","officer","rmarkdown","knitr"))   # Shiny + sorties
install.packages(c("mice","missForest","dbscan","cluster"))                  # imputation/doublons
install.packages(c("sf","leaflet","survey"))                                 # cartes + sondage
```

> Les fonctions qui requièrent une dépendance optionnelle affichent un message
> clair si celle-ci est absente ; le reste de la suite continue de fonctionner.

## 3. Dépendances système (export Word / PDF)

Pour les rapports Word et PDF :

```bash
# Debian / Ubuntu
sudo apt-get install -y pandoc libreoffice
# Pour le PDF via LaTeX (alternative légère)
# R : tinytex::install_tinytex()
```

## 4. Installation du package

```r
install.packages("baobabStats_1.0.0.tar.gz", repos = NULL, type = "source")
library(baobabStats)
bs_app()   # vérifie l'installation en lançant l'interface
```

## 5. Déploiement sur RStudio Server / Posit Connect

1. Installer le package dans la bibliothèque partagée (`.libPaths()`).
2. Les addins apparaissent automatiquement dans le menu **Addins** de chaque session.
3. Pour publier l'application Shiny de façon permanente :

```r
# Copier l'application déployable
file.copy(system.file("shiny", package = "baobabStats"),
          "/srv/shiny-server/baobabstats", recursive = TRUE)
```

   puis y ajouter un `app.R` minimal :

```r
library(baobabStats); shiny::shinyAppDir(system.file("shiny", package = "baobabStats"))
```

## 6. Vérification rapide

```r
library(baobabStats)
bs_catalogue()                 # liste les fonctions
d <- bs_estimer_dse(n_pes = 5000, n_recensement = 48000, n_apparies = 4600)
print(d$interpretation)        # doit afficher une interprétation en français
```

## 7. Désinstallation

```r
remove.packages("baobabStats")
```
