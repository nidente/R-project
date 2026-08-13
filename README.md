# Analyse des Notes d'Etudiants (Shiny)

Application web R/Shiny pour saisir des notes d'etudiants (par nom et par classe) et generer automatiquement statistiques + graphiques.

## Fonctionnalites

- Saisie manuelle: nom, classe, note, temps de travail, absences
- Actions: ajouter, modifier la ligne selectionnee, supprimer, reinitialiser, analyser
- Import CSV ou Excel (.csv, .xlsx, .xls) et export CSV des donnees (ajout ou remplacement)
- Export d'un rapport d'analyse au format HTML (statistiques, graphiques, interpretation)
- Filtre par classe: une fois l'analyse lancee, restreint les statistiques et les graphiques principaux a une classe (des lors qu'au moins 2 classes existent)
- Statistiques: effectif, moyenne, mediane, quartiles (Q1/Q3), min/max, ecart-type, correlations avec p-value
- Comparaison par classe: tableau moyenne/mediane/ecart-type sur toutes les classes (non affecte par le filtre), des lors qu'au moins 2 classes sont presentes
- Graphiques interactifs (plotly): histogramme, 2 scatter plots avec regression, boxplot des notes par classe
- Interface moderne: layout avec barre laterale (bslib `page_sidebar`), bascule clair/sombre, cartes agrandissables (`full_screen`)
- Dependances figees avec renv pour des installations reproductibles

## Fichiers

- app.R: application principale
- helpers.R: logique partagee (statistiques, graphiques, validation, import) utilisee par l'app et le rapport
- report_template.Rmd: gabarit du rapport d'analyse HTML
- test_app_progress.R: tests progressifs
- deploy_shinyapps.R: deploiement shinyapps.io
- renv.lock / renv/: versions figees des packages R

## Lancement local

Le projet utilise [renv](https://rstudio.github.io/renv/) pour figer les versions des packages. A la premiere ouverture d'une session R dans le dossier, `renv/activate.R` est charge automatiquement (via `.Rprofile`).

```bash
cd /home/landoulsi/R-project
R -q -e "renv::restore()"          # installe les packages figes dans renv.lock
R -q -e "shiny::runApp('app.R')"
```

Si vous modifiez les dependances du projet, mettez a jour le lockfile avec `R -q -e "renv::snapshot()"`.

## Import / Export

- Le bouton "Telecharger les donnees (CSV)" exporte les lignes actuelles (colonnes: nom, classe, note, temps_travail, absences).
- Le champ d'import accepte un fichier CSV (.csv) ou Excel (.xlsx, .xls) avec exactement ces colonnes (seule la premiere feuille est lue pour un fichier Excel). Les lignes invalides (nom vide, note hors de 0-20, temps/absences negatifs) sont ignorees et comptabilisees dans la notification.
- La case "Remplacer les donnees existantes" bascule entre ajout (par defaut) et remplacement complet du jeu de donnees.

## Rapport d'analyse

- Le bouton "Telecharger le rapport (HTML)" genere un fichier HTML autonome (statistiques, tableau de donnees, 4 graphiques, comparaison par classe, interpretation) a partir des dernieres donnees analysees (bouton "Lancer l'analyse").
- Si un filtre par classe est actif, le rapport applique le meme filtre aux statistiques/graphiques principaux (le boxplot et la comparaison par classe restent toujours sur l'ensemble des donnees) et indique le filtre applique en tete de rapport.
- Sans analyse lancee au prealable, le fichier telecharge contient un message l'indiquant plutot que d'echouer silencieusement.
- Le rapport reutilise les memes fonctions de calcul/graphiques que l'application (`helpers.R`), garantissant la coherence entre l'ecran et le rapport telecharge (les graphiques du rapport restent des images statiques ggplot2, contrairement aux graphiques interactifs de l'application).

## Filtre par classe

- Des qu'une analyse contient au moins 2 classes distinctes, un selecteur "Filtre" apparait au-dessus des statistiques principales.
- Choisir une classe restreint l'effectif, les statistiques (moyenne, mediane, quartiles, ecart-type, correlations) et les 3 premiers graphiques (histogramme, 2 scatter plots) a cette classe.
- Le boxplot "Notes par classe" et le tableau "Comparaison par classe" affichent toujours toutes les classes, quel que soit le filtre, puisque leur role est justement la comparaison globale.

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
- Les donnees saisies (nom, classe, note) sont des donnees personnelles d'eleves: ne pas commiter de fichiers CSV exportes dans le depot, et supprimer les fichiers temporaires apres usage
