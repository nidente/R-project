# CLAUDE.md

Guide de travail pour Claude Code sur ce depot. Lire aussi [README.md](README.md) (doc utilisateur).

## Projet

Application R/Shiny mono-page "Analyse des Notes d'Etudiants": saisie/import de notes (nom, classe, note /20, temps de travail, absences), statistiques descriptives, correlations avec p-value, graphiques plotly, filtre par classe, export CSV et rapport HTML (R Markdown).

- Deployee sur shinyapps.io: https://landoulsi-app-etudiant.shinyapps.io/analyse-notes-etudiants/ (compte `landoulsi-app-etudiant`, app `analyse-notes-etudiants`, cf. `rsconnect/.../*.dcf`)
- Licence MIT. Auteur git: `nidente`.
- Langue: interface, messages, commentaires et noms de variables en **francais sans accents** (ASCII: "Reinitialiser", "Mediane", "Non classe"...). Respecter cette convention partout (UI, notifications, tests, README).

## Commandes

```bash
R -q -e "renv::restore()"            # installer les packages figes (renv.lock, R 4.6.1)
R -q -e "shiny::runApp('app.R')"     # lancer l'app en local
Rscript test_app_progress.R          # tests (doit finir par "TOUS LES TESTS SONT VALIDES")
R -q -e "renv::snapshot()"           # apres ajout/suppression d'un package
SHINYAPPS_NAME=... SHINYAPPS_TOKEN=... SHINYAPPS_SECRET=... Rscript deploy_shinyapps.R
```

`.Rprofile` source `renv/activate.R`: toute session R lancee depuis la racine utilise la librairie renv du projet. Toujours lancer les commandes depuis la racine (chemins relatifs `helpers.R`, `report_template.Rmd`).

## Architecture

| Fichier | Role |
|---|---|
| `app.R` | UI (`bslib::page_sidebar`, theme flatly, Poppins/Space Grotesk, mode sombre, CSS inline `app_css`) + `server`. Se termine par `shinyApp(ui, server)`. |
| `helpers.R` | Toute la logique pure, partagee par l'app ET le rapport: `safe_cor`, `safe_cor_pvalue`, `format_metric`, `format_pvalue`, `empty_df`, `validate_entry`, `read_csv_auto`, `read_import_file`, `write_export_csv`, `parse_num`, `prepare_import`, `compute_stats`, `classe_summary`, `build_interpretation`, `plot_histogram`, `plot_scatter_temps`, `plot_scatter_absences`, `plot_boxplot_classe`, `generate_report`. |
| `report_template.Rmd` | Rapport HTML autonome (params `data`, `full_data`, `classe_label`). Fait `source("helpers.R")`. Graphiques ggplot2 statiques. |
| `test_app_progress.R` | Script de tests maison (pas testthat): `assert_ok()` + `shiny::testServer(server, ...)`. |
| `deploy_shinyapps.R` | Deploiement via `rsconnect` a partir de variables d'environnement. |
| `.rscignore` | Fichiers exclus du bundle deploye (`.claude/`, `CLAUDE.md`). |

### Flux de donnees du serveur

- `rv$df` (reactiveValues): donnees courantes editees (table DT, selection simple).
- `analyzed_data()` (reactiveVal): **instantane** de `rv$df` pris au clic sur "Lancer l'analyse". Les modifications ulterieures n'affectent les stats qu'apres un nouveau clic. `reset_data` le remet a `NULL`.
- `active_classe()`: valeur valide du filtre (retombe sur `"Toutes les classes"` si l'input est perime). `filtered_data()`: `analyzed_data()` filtre par `active_classe()` (valeur sentinelle `"Toutes les classes"`).
- `stats()` = `compute_stats(filtered_data())`.
- Regle metier du filtre: stats, histogramme, 2 scatter plots et interpretation suivent le filtre; **boxplot par classe et tableau de comparaison utilisent toujours `analyzed_data()` complet**. Le rapport applique la meme regle (`data` filtre, `full_data` complet).
- Le selecteur de classe (`output$classe_filter_ui`) n'apparait qu'avec >= 2 classes.
- Classe vide -> `"Non classe"` (saisie et import).

### Schema des donnees

Colonnes fixes et dans cet ordre: `nom` (chr), `classe` (chr), `note` (num 0-20), `temps_travail` (num >= 0), `absences` (num >= 0). Ce schema est duplique dans: `empty_df()`, `prepare_import()` (`required_cols`), noms de colonnes DT dans `app.R`, `knitr::kable` du rapport, README. Toute modification du schema doit toucher tous ces endroits + les tests.

## Conventions

- Mettre la logique dans `helpers.R` (testable, reutilisee par le rapport); `app.R` ne fait que l'orchestration reactive et l'affichage.
- Les fonctions de stats renvoient `NA_real_` plutot que d'echouer (n < 2, n < 3 pour p-value, variance nulle); l'affichage passe par `format_metric()` / `format_pvalue()` qui rendent `"N/A"`.
- Chaque nouvelle fonctionnalite: ajouter des `assert_ok` dans `test_app_progress.R` (unitaire sur helpers + scenario `testServer`) et mettre a jour le README.
- `generate_report()` copie template + helpers dans un `tempfile()` isole avant `rmarkdown::render` (concurrence entre sessions): ne pas rendre directement dans le dossier du projet.
- Nouveau package: l'ajouter en `library()` dans `app.R` (ou `helpers.R`) puis `renv::snapshot()` et commiter `renv.lock`. Ne jamais commiter `renv/library/`.

## Securite / donnees

- Jamais de token/secret shinyapps dans le depot (uniquement variables d'env).
- Les donnees eleves sont personnelles: ne pas commiter de CSV/Excel/rapports exportes.

### Pieges deja rencontres (corriges, couverts par les tests)

- Ne jamais lire `input$classe_filter` directement: passer par `active_classe()`, car l'input garde sa derniere valeur quand le selecteur disparait (< 2 classes) ou que la classe n'existe plus apres re-analyse.
- `nzchar(NA)` vaut `TRUE`: tester `is.na()` explicitement pour les champs texte importes (cellules Excel vides).
- Import CSV: passer par `read_csv_auto()` (separateur `,`/`;`, encodage UTF-8/BOM/latin1) et `parse_num()` (virgule decimale). Export: `write_export_csv()` (UTF-8 avec BOM).

## Points connus (non corriges)

1. `validate_entry`: un argument `NULL` est absorbe par `c()` et non detecte (sans impact via Shiny, `numericInput` vide donne `NA`).
2. R systeme 4.6.2 vs lockfile 4.6.1 (sans consequence constatee; tests OK).
3. A chaque re-analyse, le selecteur de classe revient a "Toutes les classes" (la selection precedente n'est pas conservee).
