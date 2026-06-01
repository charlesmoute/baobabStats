# baobabStats 1.1.1 (2026-05-31)

Version corrective : alias anglais pour l'internationalisation de l'API et mise a
jour de l'identite visuelle (logos). Aucune fonction existante n'est modifiee.

## Internationalisation : alias anglais (module bs_aliases_en.R)
* Les noms francais bs_* restent l'API canonique. Des **alias anglais** sont
  ajoutes pour ~30 fonctions principales (ex. `bs_collect` -> `bs_collecter`,
  `bs_life_table` -> `bs_table_mortalite`, `bs_project_un` -> `bs_projeter_onu`,
  `bs_thematic_map` -> `bs_carte_thematique`, `bs_aggregate` -> `bs_agreger`).
* Les alias pointent vers les memes fonctions (aucune duplication de logique,
  aucun risque de divergence). L'internationalisation des sorties continue de
  passer par le parametre `langue = "fr"/"en"`.

## Identite visuelle
* Mise a jour des six declinaisons du logo (embleme / complet, couleur / creme /
  transparent) dans inst/branding, sans retouche des fichiers fournis.
# baobabStats 1.1.0 (2026-05-29)

Version majeure mineure : architecture hybride data.table/tibble pour les gros
volumes, referentiel geographique complet du Cameroun, cartes thematiques,
projection ONU par cohortes-composantes, et estimation sur petits domaines (SAE).
Toutes les fonctionnalites des versions precedentes sont conservees.

## Architecture hybride data.table / tibble (module bs_hybride.R)
* En interne, les calculs lourds (agregations, jointures, appariement PES, SAE)
  utilisent **data.table** quand il est disponible (5 a 50x plus rapide sur les
  volumes de recensement) ; repli automatique et transparent sur base/dplyr sinon.
* En sortie, les fonctions bs_* renvoient des **tibble** (compatibilite tidyverse).
* Nouvelles fonctions : `bs_as_dt()`, `bs_as_sortie()`, `bs_fread()` (lecture
  rapide), `bs_agreger()` (agregation hybride performante), `bs_moteur_calcul()`
  (diagnostic). `bs_collecter()` lit desormais les CSV via fread.

## Referentiel geographique complet du Cameroun (module bs_geo.R)
* Integration du repertoire officiel des localites et villages 2016 :
  **10 regions, 58 departements, 360 arrondissements, 1 600+ villes/cantons**.
* Nouvelle fonction `bs_geo_cameroun(niveau)` : referentiel hierarchique a tout
  niveau (region / departement / arrondissement / complet).
* `bs_geo_referentiel("CM")` et l'harmonisation s'appuient sur ce referentiel.

## Cartes thematiques (module bs_cartes.R)
* `bs_carte_thematique()` : carte choroplethe (sf + ggplot2) aux couleurs de la
  charte, jointure d'un indicateur agrege a un fond de carte.
* `bs_lire_shapefile()` : lecture shapefile / GeoPackage / GeoJSON.
* `bs_cartes_config()` : production en lot pilotee par la feuille Excel
  **Cartographie** (dossier du shapefile + variable de fusion).

## Projection ONU par cohortes-composantes (module bs_projection_onu.R)
* `bs_projeter_onu()` : methode des composantes par cohorte (standard Nations
  Unies / World Population Prospects). Projette la population par age et sexe en
  appliquant separement fecondite, mortalite (survie) et migration, pas a pas
  (1 ou 5 ans). Schemas types par defaut si une composante n'est pas fournie.
* La methode "onu" est selectionnable dans la feuille Projection et devient le
  defaut du rapport de projection.

## Small Area Estimation (module bs_sae.R)
* `bs_sae()` : estimation sur petits domaines avec les deux approches
  **top-down** (repartition d'un total superieur au prorata de poids auxiliaires)
  et **bottom-up** (estimations directes + calage/raking sur le total superieur).
* Desagregation/agregation par **age, sexe et handicap** : un axe, plusieurs, ou
  les trois (actifs par defaut). `bs_sae_agreger()` recombine les axes.
* Pilotage par la feuille Excel **SAE**.

## Configuration par Excel enrichie
* Nouvelles feuilles **Cartographie** (shapefile, variable de fusion, indicateurs)
  et **SAE** (approche, domaine, niveau superieur, axes age/sexe/handicap).
* Feuille **Projection** etendue : methode "onu", pas, sex_ratio_naissance.
* `bs_pipeline()` produit desormais les cartes et les estimations SAE a l'etape 5.

# baobabStats 1.0.2 (2026-05-28)

Version mineure : controle de disponibilite des variables pour la production des
rapports, et evaluation unifiee de la concordance (backcheck d'enquete ou enquete
post-censitaire) avec coefficient Kappa de Cohen. Toutes les fonctionnalites des
versions 1.0.0 et 1.0.1 sont conservees.

## Controle de disponibilite des variables (gating des tableaux)
* **`bs_variables_disponibles()`** : verifie que les variables requises par un
  tableau sont configurees (feuille Variables). Si une variable manque, le tableau
  n'est pas produit, un message explicite est emis et l'evenement est journalise.
* **`bs_variables_requises()`** : referentiel reliant chaque tableau-cle des 12
  tomes (plus qualite et projection) aux roles de variables indispensables.
* **`bs_tableaux_ignores()`** : journal des tableaux non produits faute de variables.
* Les producteurs de rapports (`bs_contenu_thematique`, sections thematiques) emettent
  desormais un message a chaque tableau saute, au lieu de l'ignorer silencieusement.

## Evaluation de concordance unifiee (backcheck OU post-censitaire)
* **`bs_evaluer_concordance()`** : point d'entree unique, pilotable par
  configuration. La meme logique bcstats s'applique au backcheck d'une enquete
  (mode "enquete") et a l'enquete post-censitaire d'un recensement (mode
  "recensement"). Variables declarees **T1** (critique, exact), **T2** (moderee,
  tolerance numerique Okrange) et **T3** (mineure).
* Calculs : taux de concordance, taux d'erreur par type, **coefficient Kappa de
  Cohen** par variable (echelle de Landis & Koch), et — en mode recensement —
  **taux d'omission** (systeme dual de Petersen-Lincoln) et **coefficients de
  redressement** par strate.
* **`cohen_kappa()`** ajoute au moteur qualite ; le Kappa figure desormais dans les
  statistiques de chaque variable categorielle.
* Le rapport de qualite integre une table "Concordance et coefficient Kappa de
  Cohen" et, le cas echeant, une table "Taux d'omission et redressement par strate".

## Configuration par Excel enrichie
* Nouvelle feuille **Backcheck** : declarer les variables T1/T2/T3 et leur Okrange.
* Feuille **Qualite** etendue : `id_var`, `enum_var`, `var_strate`.
* `bs_pipeline()` lit ces parametres et appelle automatiquement
  `bs_evaluer_concordance()` selon que le backcheck et/ou la PES sont fournis.

# baobabStats 1.0.1 (2026-05-28)

Version mineure : production automatisee des rapports thematiques de recensement
en trois formats distincts (Word, Excel, HTML), avec interpretation dynamique et
reutilisation des coefficients de redressement post-censitaires. Toutes les
fonctionnalites de la version 1.0.0 sont conservees.

## Nouveau module : rapports thematiques multi-format (`bs_rapports_thematiques.R`)
* **`bs_themes_recensement()`** : catalogue des 12 thematiques classiques
  (structure, nuptialite, education, emploi, fecondite, mortalite, migration,
  handicap, habitat, equipements, peuples autochtones, agriculture) plus deux
  rapports speciaux (qualite des donnees, projection de population).
* **`bs_rapport_thematique()`** : pour une thematique, genere trois fichiers
  distincts `.docx`, `.xlsx`, `.html`. Word et HTML integrent des
  **interpretations dynamiques** generees a partir des resultats.
* **`bs_rapports_thematiques()`** : orchestrateur produisant l'ensemble des
  rapports demandes ; reutilise les coefficients de redressement quand ils
  existent.
* **`bs_rapport_qualite_complet()`** : rapport d'evaluation de la qualite
  (attraction d'age Whipple/Myers/Bachi, completude, couverture PES, redressement).
* **`bs_rapport_projection()`** : projection par **sexe** et par l'**unite
  administrative la plus fine** sur un horizon (defaut 10 ans) a partir de la date
  de collecte. Conformement a la specification, ce rapport **ne contient pas**
  d'interpretation dynamique.
* **`bs_contenu_thematique()`** : construit le contenu analytique (tableaux,
  graphiques, textes) reutilisable.

## Exploitation de l'enquete post-censitaire (PES)
* **`bs_post_censitaire()`** : point d'entree unique qui apparie PES/recensement,
  realise l'estimation par systeme dual (DSE) et derive les **coefficients de
  redressement par strate**, directement consommables par les rapports.
* Les effectifs publies dans les rapports sont **redresses automatiquement**
  lorsque des coefficients sont disponibles (sinon, effectifs bruts signales).

## Configuration par Excel enrichie
* Nouvelle feuille **Rapports** : activer/desactiver chaque rapport thematique et
  choisir les formats Word/Excel/HTML.
* Nouvelle feuille **Projection** : horizon, date de collecte, unite la plus fine,
  methode.
* Feuille **Variables** etendue a tous les roles thematiques (education, emploi,
  fecondite, mortalite, habitat, equipements, peuples autochtones, agriculture...).
* `bs_pipeline()` comporte desormais une **etape 7** qui produit les rapports
  thematiques apres la diffusion.

## Notes
* Le module est en R pur (aucun code compile) ; l'installable est le `.tar.gz`
  source. Les sorties Word utilisent `officer`, l'Excel `openxlsx`/`writexl`
  (avec repli si absent).

# baobabStats 1.0.0 (2026-05-26)

Premiere version de la suite unifiee. Auteur : Charles Moute
(charles.moute@gmail.com).

## Identite visuelle
* Charte derivee du logo (baobab enracine dans l'Afrique) : brun ecorce, or
  savane, vert feuille, sur fonds creme et sable.
* Appliquee a toutes les sorties : graphiques (`theme_baobabstats()`,
  `scale_fill_baobabstats()`), application Shiny, classeurs Excel et rapports.
* Couleurs via `bs_couleurs()` / `bs_palette()` ; logo via `bs_logo()`.

## Unification des trois moteurs
* Integration de **DemoStats**, **CensusAnalytics** et **statAfrikR** sous une API
  unique en francais (prefixe `bs_`).

## Nouveautes transverses
* **Configuration par Excel** : `bs_config_modele()`, `bs_config_lire()`, `bs_pipeline()`.
* **Interpretation dynamique** : `bs_interpreter()` (seuils ONU).
* **Generation de prompts IA** : `bs_prompt()`.
* **Coefficients de redressement** : `bs_coefficients_redressement()`,
  `bs_appliquer_redressement()`.
* **Referentiels geographiques** africains : `bs_harmoniser_regions()`.

## Interfaces
* Application **Shiny** unifiee (7 onglets) : `bs_app()`.
* **Addins RStudio**.

## Donnees et modeles
* Jeu de donnees de demonstration (`inst/extdata/demo_individus.csv`).
* Modele de configuration Excel (`inst/config/baobabstats_config_template.xlsx`).
