# baobabStats <img src="man/figures/logo.png" align="right" height="130" alt="baobabStats" />

> **Tools for Data — Rooted in Africa**

**Suite intégrée R pour l'analyse des recensements et enquêtes en Afrique.**

baobabStats réunit de manière harmonieuse **trois moteurs expérimentaux
complémentaires** sous une API unique, cohérente et entièrement en français :

| Moteur d'origine | Apport principal conservé |
|---|---|
| **DemoStats** | Appariement post-censitaire (PES), estimation par système dual (DSE), contrôle de terrain `bcstats`, indicateurs démographiques, projections par cohorte. |
| **CensusAnalytics** | Contrôle qualité intrinsèque (Whipple, Myers, Bachi), nettoyage et imputation (MICE/missForest), dédoublonnage par apprentissage, tabulations thématiques, microsimulation, rapports. |
| **statAfrikR** | Collecte (CSPro/Kobo/ODK), référentiels géographiques africains, plans de sondage, diffusion (SDMX/DDI). |

baobabStats ne se contente pas de juxtaposer ces outils : il résout les
recouvrements (choix de la meilleure implémentation pour chaque besoin), unifie
la nomenclature (préfixe `bs_`, verbes français) et **ajoute trois innovations** :

1. **Configuration pilotée par Excel** — paramétrer un traitement reproductible
   sans écrire de R (`bs_config_modele()`, `bs_pipeline()`).
2. **Interprétation dynamique** des résultats, calibrée sur les seuils de
   référence des Nations Unies (`bs_interpreter()`).
3. **Génération de prompts** pour l'interprétation assistée par IA (`bs_prompt()`).
4. **Rapports thématiques multi-format** — pour chaque thématique de
   recensement (et pour la qualité et la projection), production de fichiers
   **Word, Excel et HTML distincts** avec interprétations dynamiques et
   redressement post-censitaire (`bs_rapports_thematiques()`).

Le tout est accessible via une **application Shiny** et des **modules
complémentaires RStudio (addins)** installés automatiquement avec le package.

---

## Installation

```r
# Depuis l'archive fournie
install.packages("baobabStats_1.0.1.tar.gz", repos = NULL, type = "source")

# Ou en développement
# remotes::install_github("baobabstats/baobabStats")
```

Voir `INSTALLATION.md` pour les dépendances système (export Word/PDF) et le
déploiement sur RStudio Server / Posit Connect.

---

## Démarrage en 30 secondes

```r
library(baobabStats)

# 1. Lancer l'interface graphique (aucune ligne de code ensuite)
bs_app()

# 2. Ou en script : charger les données de démonstration
chemin <- system.file("extdata", "demo_individus.csv", package = "baobabStats")
indiv  <- bs_collecter(chemin)

# 3. Contrôle qualité intrinsèque + interprétation automatique
q <- bs_qualite_intrinseque(indiv, var_age = "age", var_sexe = "sexe")
print(q$interpretation)

# 4. Couverture post-censitaire et coefficients de redressement
d   <- bs_estimer_dse(n_pes = 5000, n_recensement = 48000, n_apparies = 4600)
coef <- bs_coefficients_redressement(dse = d)

# 5. Générer un prompt pour faire interpréter par une IA
cat(bs_prompt(d, public = "decideur"))
```

---

## Le cycle statistique en sept étapes

```
Collecte → Traitement → Qualité → Analyse → Projection → Visualisation → Diffusion
   │           │           │          │          │             │             │
bs_collecter bs_nettoyer bs_qualite_* bs_tableau bs_projeter bs_graph_*  bs_rapport
bs_collecter_cspro/      bs_apparier_pes  bs_indicateur          bs_visualiser_config bs_exporter_sdmx
kobo/odk   bs_harmoniser_  bs_estimer_dse
           regions        bs_coefficients_redressement
```

`bs_catalogue()` liste l'ensemble des fonctions par étape.

## Pilotage par fichier Excel

```r
bs_config_modele("ma_config.xlsx")        # crée un classeur documenté
# … renseigner les onglets dans Excel …
res <- bs_pipeline("ma_config.xlsx")       # exécute tout le pipeline
```

Un modèle prêt à l'emploi est fourni :
`system.file("config", "baobabstats_config_template.xlsx", package = "baobabStats")`.

## Modules RStudio (addins)

Après installation, le menu **Addins** de RStudio propose : lancer l'application,
créer une configuration Excel, exécuter un pipeline, interpréter le dernier
résultat et générer un prompt IA.

---

## Pourquoi baobabStats pour l'Afrique ?

- **Français d'abord** : interfaces, messages et documentation.
- **Hors-ligne** : fonctionne sans connexion après installation (souveraineté des données).
- **Qualité au cœur** : contrôle *intrinsèque* (attraction d'âge) **et** *a posteriori*
  (backcheck, PES/DSE) avec calcul des coefficients de redressement par strate.
- **Référentiels géographiques africains** intégrés, dont le **Cameroun**.
- **Prise en main sans code** via Shiny et Excel, pour les INS et les non-initiés.

## Identité visuelle

L'identité de baobabStats est dérivée du logo (un baobab enraciné dans une carte
d'Afrique) : brun écorce (dominante), or savane, vert feuille, sur fonds crème et
sable. Elle est appliquée de façon cohérente à toutes les sorties — graphiques
(`theme_baobabstats()`, `scale_fill_baobabstats()`), application Shiny, classeurs
Excel et rapports. Les couleurs sont accessibles via `bs_couleurs()` et `bs_palette()`,
et les différentes versions du logo via `bs_logo()`.

## Licence

MIT © 2026 **Charles Mouté** (charles.moute@gmail.com).
Moteurs d'origine intégrés : DemoStats, CensusAnalytics, statAfrikR.

---

## Rapports thématiques (nouveau en 1.0.1)

Production, pour chaque thématique classique d'un recensement, de trois livrables
**distincts** (`.docx`, `.xlsx`, `.html`). Les sorties **Word** et **HTML**
contiennent des **interprétations dynamiques** ; le **rapport de projection** en
est exempté (conformément à la pratique de diffusion).

```r
library(baobabStats)

# 1) Données harmonisées
individus <- bs_collecter("donnees/individus.csv")

# 2) Exploiter l'enquête post-censitaire : qualité + coefficients de redressement
post <- bs_post_censitaire(
  recensement = individus,
  pes         = bs_collecter("donnees/pes.csv"),
  var_strate  = "region"
)
print(post)   # coefficients de redressement par strate + interprétation

# 3) Générer tous les rapports thématiques (Word + Excel + HTML)
bs_rapports_thematiques(
  data    = individus,
  post    = post,                    # effectifs redressés automatiquement
  dossier = "sorties/rapports",
  formats = c("word", "excel", "html"),
  horizon_projection = 10,           # projection sur 10 ans
  date_collecte      = as.Date("2026-05-01"),
  niveau_fin         = "arrondissement"
)
```

Tout est également pilotable **sans écrire de R**, via le classeur de
configuration (onglets **Rapports** et **Projection**) :

```r
bs_config_modele("config.xlsx")   # créer le modèle, puis l'éditer dans Excel
bs_pipeline("config.xlsx")        # exécute collecte → ... → rapports thématiques
```

### Thématiques couvertes

structure · nuptialité · éducation · emploi · fécondité · mortalité · migration ·
handicap · habitat · équipements · peuples autochtones · agriculture · **qualité
des données** · **projection de la population**.
