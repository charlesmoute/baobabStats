# ============================================================================
# baobabStats - Exemple de flux de travail complet
# Recensement / enquete post-censitaire en contexte africain
# ============================================================================

library(baobabStats)

# ---------------------------------------------------------------------------
# 1. COLLECTE
# ---------------------------------------------------------------------------
chemin <- system.file("extdata", "demo_individus.csv", package = "baobabStats")
indiv  <- bs_collecter(chemin)

# Diagnostic des valeurs manquantes (alerte au-dela de 15 %)
bs_controler_na(indiv, seuil = 0.15)

# ---------------------------------------------------------------------------
# 2. TRAITEMENT : harmonisation des regions (Cameroun)
# ---------------------------------------------------------------------------
indiv <- bs_harmoniser_regions(indiv, var_region = "region", code_pays = "CM")
# Les variantes "Extreme Nord" / "Adamawa" sont rapprochees du referentiel.
table(indiv$region_std, useNA = "ifany")

# ---------------------------------------------------------------------------
# 3a. QUALITE INTRINSEQUE (attraction d'age, masculinite, completude)
# ---------------------------------------------------------------------------
q <- bs_qualite_intrinseque(indiv, var_age = "age", var_sexe = "sexe")
print(q$interpretation)            # interpretation dynamique en francais
bs_whipple(indiv$age)              # indice de Whipple
bs_myers(indiv$age)                # indice de Myers

# ---------------------------------------------------------------------------
# 3b. QUALITE A POSTERIORI : PES + systeme dual + redressement
# ---------------------------------------------------------------------------
# (a) avec des comptages agreges connus
dse <- bs_estimer_dse(n_pes = 5000, n_recensement = 48000,
                      n_apparies = 4600, n_errones = 200)
print(dse$interpretation)

# (b) coefficients de redressement par strate (urbain / rural)
strates <- data.frame(
  strate        = c("Urbain", "Rural"),
  n_pes         = c(3000, 2000),
  n_recensement = c(30000, 18000),
  n_apparies    = c(2850, 1750),
  n_errones     = c(120, 80)
)
coef <- bs_coefficients_redressement(strates = strates)
print(coef)

# Application aux effectifs publies
effectifs <- data.frame(strate = c("Urbain", "Rural"), population = c(30000, 18000))
bs_appliquer_redressement(effectifs, coef, "strate", "population")

# ---------------------------------------------------------------------------
# 4. INDICATEURS & TABLEAUX
# ---------------------------------------------------------------------------
bs_indicateur(indiv, "rapport_masc", sex_var = "sexe")
tableaux <- bs_tableaux(indiv, config = list(age_var = "age", sex_var = "sexe"))

# ---------------------------------------------------------------------------
# 5. VISUALISATION (PNG / PDF / HTML)
# ---------------------------------------------------------------------------
g <- bs_graph_pyramide(indiv, var_age = "age", var_sexe = "sexe")
bs_enregistrer_graph(g, "pyramide_ages", formats = c("png", "pdf"))

# ---------------------------------------------------------------------------
# 6. INTERPRETATION ASSISTEE PAR IA
# ---------------------------------------------------------------------------
prompt <- bs_prompt(dse, public = "decideur")
cat(prompt)            # a copier dans Claude / GPT / Gemini
# bs_prompt_copier(prompt)   # insere directement dans l'editeur RStudio

# ---------------------------------------------------------------------------
# 7. PILOTAGE COMPLET PAR EXCEL (sans code)
# ---------------------------------------------------------------------------
# bs_config_modele("ma_config.xlsx")          # creer le classeur
# res <- bs_pipeline("ma_config.xlsx")         # tout executer
# print(res)
