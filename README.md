# Analyse des Notes d'Etudiants (Shiny)

Application web R/Shiny pour saisir des notes d'etudiants et generer automatiquement statistiques + graphiques.

## Fonctionnalites

- Saisie manuelle: note, temps de travail, absences
- Actions: ajouter, supprimer, reinitialiser, analyser
- Statistiques: moyenne, mediane, min/max, ecart-type, correlations
- Graphiques ggplot2: histogramme + 2 scatter plots avec regression
- Interface responsive avec bslib

## Fichiers

- app.R: application principale
- test_app_progress.R: tests progressifs
- deploy_shinyapps.R: deploiement shinyapps.io

## Lancement local

```bash
R -q -e "install.packages(c('shiny','bslib','ggplot2','dplyr','DT'), repos='https://cloud.r-project.org')"
R -q -e "setwd('/home/landoulsi/R-project'); shiny::runApp('app.R')"
```

## Tests

```bash
Rscript test_app_progress.R
```

## Deploiement shinyapps.io

```bash
cd /home/landoulsi/R-project
export SHINYAPPS_NAME="votre_name"
export SHINYAPPS_TOKEN="votre_token"
export SHINYAPPS_SECRET="votre_secret"
Rscript deploy_shinyapps.R
```

## Securite

- Ne jamais publier token/secret
- Regenerer un token s'il est expose
