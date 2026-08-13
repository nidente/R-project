#!/usr/bin/env Rscript

# Test progressif de l'application Shiny
# Objectif: montrer une verification au fur et a mesure du developpement.

cat("===== DEMARRAGE DES TESTS PROGRESSIFS =====\n")

assert_ok <- function(condition, message_ok, message_ko) {
  if (!isTRUE(condition)) {
    stop(message_ko, call. = FALSE)
  }
  cat(paste0("[OK] ", message_ok, "\n"))
}

# Etape 1: chargement du code
source("app.R")
assert_ok(exists("safe_cor"), "Fonction safe_cor chargee", "safe_cor introuvable")
assert_ok(exists("safe_cor_pvalue"), "Fonction safe_cor_pvalue chargee", "safe_cor_pvalue introuvable")
assert_ok(exists("format_metric"), "Fonction format_metric chargee", "format_metric introuvable")
assert_ok(exists("format_pvalue"), "Fonction format_pvalue chargee", "format_pvalue introuvable")
assert_ok(exists("validate_entry"), "Fonction validate_entry chargee", "validate_entry introuvable")
assert_ok(exists("prepare_import"), "Fonction prepare_import chargee", "prepare_import introuvable")
assert_ok(exists("read_import_file"), "Fonction read_import_file chargee", "read_import_file introuvable")
assert_ok(exists("classe_summary"), "Fonction classe_summary chargee", "classe_summary introuvable")
assert_ok(exists("compute_stats"), "Fonction compute_stats chargee", "compute_stats introuvable")
assert_ok(exists("build_interpretation"), "Fonction build_interpretation chargee", "build_interpretation introuvable")
assert_ok(exists("generate_report"), "Fonction generate_report chargee", "generate_report introuvable")
assert_ok(exists("ui"), "Objet ui charge", "ui introuvable")
assert_ok(exists("server"), "Fonction server chargee", "server introuvable")

# Etape 2: tests unitaires sur safe_cor / safe_cor_pvalue
x <- c(2, 4, 6, 8)
y <- c(1, 2, 3, 4)
val_cor <- safe_cor(x, y)
assert_ok(!is.na(val_cor), "safe_cor retourne une valeur numerique", "safe_cor retourne NA de facon inattendue")
assert_ok(abs(val_cor - 1) < 1e-12, "safe_cor calcule correctement la correlation parfaite", "safe_cor ne calcule pas correctement la correlation")
assert_ok(is.na(safe_cor(c(1), c(1))), "safe_cor gere le cas n < 2", "safe_cor devrait retourner NA si n < 2")
assert_ok(is.na(safe_cor(c(1, 1, 1), c(2, 3, 4))), "safe_cor gere la variance nulle", "safe_cor devrait retourner NA avec variance nulle")

assert_ok(is.na(safe_cor_pvalue(c(1, 2), c(1, 2))), "safe_cor_pvalue gere le cas n < 3", "safe_cor_pvalue devrait retourner NA si n < 3")
pval <- safe_cor_pvalue(c(1, 2, 3, 4, 5), c(2, 4, 5, 4, 5))
assert_ok(!is.na(pval) && pval >= 0 && pval <= 1, "safe_cor_pvalue retourne une p-value valide", "safe_cor_pvalue ne retourne pas une p-value valide")

# Etape 3: tests unitaires sur format_metric / format_pvalue
assert_ok(format_metric(12.3456) == "12.35", "format_metric arrondit correctement", "format_metric n'arrondit pas correctement")
assert_ok(format_metric(NA_real_) == "N/A", "format_metric gere NA", "format_metric devrait retourner N/A pour NA")
assert_ok(format_pvalue(NA_real_) == "N/A", "format_pvalue gere NA", "format_pvalue devrait retourner N/A pour NA")
assert_ok(format_pvalue(0.0001) == "< 0.001", "format_pvalue gere les tres petites valeurs", "format_pvalue devrait afficher < 0.001")
assert_ok(format_pvalue(0.04321) == "0.043", "format_pvalue arrondit correctement", "format_pvalue n'arrondit pas correctement")

# Etape 4: tests unitaires sur validate_entry
assert_ok(is.null(validate_entry("Alice", 15, 5, 1)), "validate_entry accepte une saisie valide", "validate_entry rejette une saisie valide")
assert_ok(!is.null(validate_entry("", 15, 5, 1)), "validate_entry rejette un nom vide", "validate_entry devrait rejeter un nom vide")
assert_ok(!is.null(validate_entry("Alice", 25, 5, 1)), "validate_entry rejette une note hors intervalle", "validate_entry devrait rejeter une note > 20")
assert_ok(!is.null(validate_entry("Alice", 15, -1, 1)), "validate_entry rejette un temps de travail negatif", "validate_entry devrait rejeter un temps negatif")
assert_ok(!is.null(validate_entry("Alice", NA_real_, 5, 1)), "validate_entry rejette une valeur NA", "validate_entry devrait rejeter NA")

# Etape 5: tests unitaires sur prepare_import
csv_ok <- data.frame(
  nom = c("Bob", "Chloe", ""),
  classe = c("A", "", "B"),
  note = c(14, 25, 10),
  temps_travail = c(4, 3, 2),
  absences = c(0, 1, 1),
  stringsAsFactors = FALSE
)
res_ok <- prepare_import(csv_ok)
assert_ok(res_ok$ok, "prepare_import traite un data.frame avec les bonnes colonnes", "prepare_import devrait reussir avec les bonnes colonnes")
assert_ok(nrow(res_ok$data) == 1, "prepare_import filtre les lignes invalides (note hors intervalle, nom vide)", "prepare_import n'a pas filtre les lignes invalides comme attendu")
assert_ok(res_ok$n_invalid == 2, "prepare_import compte correctement les lignes invalides", "prepare_import ne compte pas correctement les lignes invalides")
assert_ok(res_ok$data$classe[1] == "A", "prepare_import conserve la classe fournie", "prepare_import ne conserve pas la classe fournie")

csv_missing_cols <- data.frame(nom = "Bob", note = 12, stringsAsFactors = FALSE)
res_missing <- prepare_import(csv_missing_cols)
assert_ok(!res_missing$ok, "prepare_import detecte les colonnes manquantes", "prepare_import devrait echouer si des colonnes sont manquantes")

csv_empty_classe <- data.frame(
  nom = "Dan", classe = "", note = 12, temps_travail = 3, absences = 0,
  stringsAsFactors = FALSE
)
res_empty_classe <- prepare_import(csv_empty_classe)
assert_ok(res_empty_classe$data$classe[1] == "Non classe", "prepare_import remplace une classe vide par 'Non classe'", "prepare_import ne remplace pas une classe vide")

# Etape 5b: tests unitaires sur read_import_file (CSV et Excel)
tmp_csv_read <- tempfile(fileext = ".csv")
write.csv(data.frame(nom = "Ivy", classe = "A", note = 15, temps_travail = 4, absences = 0), tmp_csv_read, row.names = FALSE)
csv_read <- read_import_file(tmp_csv_read, "roster.csv")
assert_ok(is.data.frame(csv_read) && nrow(csv_read) == 1, "read_import_file lit correctement un CSV", "read_import_file ne lit pas correctement un CSV")

xlsx_example <- readxl::readxl_example("datasets.xlsx")
xlsx_read <- read_import_file(xlsx_example, "datasets.xlsx")
assert_ok(is.data.frame(xlsx_read) && nrow(xlsx_read) > 0, "read_import_file lit correctement un fichier Excel", "read_import_file ne lit pas correctement un fichier Excel")
res_xlsx_missing <- prepare_import(xlsx_read)
assert_ok(!res_xlsx_missing$ok, "prepare_import detecte les colonnes manquantes sur un import Excel non conforme", "prepare_import devrait detecter les colonnes manquantes sur ce fichier Excel")

# Etape 6: tests unitaires sur classe_summary
df_one_classe <- tibble::tibble(classe = c("A", "A"), note = c(10, 12))
assert_ok(is.null(classe_summary(df_one_classe)), "classe_summary retourne NULL avec une seule classe", "classe_summary devrait retourner NULL avec une seule classe")

df_two_classes <- tibble::tibble(
  classe = c("A", "A", "B", "B"),
  note = c(10, 12, 16, 18)
)
summary_two <- classe_summary(df_two_classes)
assert_ok(!is.null(summary_two), "classe_summary retourne un resultat avec deux classes", "classe_summary devrait retourner un resultat avec deux classes")
assert_ok(nrow(summary_two) == 2, "classe_summary agrege une ligne par classe", "classe_summary devrait agreger une ligne par classe")
assert_ok(summary_two$classe[1] == "B", "classe_summary trie par moyenne decroissante", "classe_summary devrait trier par moyenne decroissante")

# Etape 7: test d'integration serveur (ajout, modification, suppression, import, analyse, reset)
shiny::testServer(server, {
  # Ajout de deux lignes
  session$setInputs(nom = "Alice", classe = "Terminale S1", note = 14, temps = 5, absences = 1, add_row = 1)
  session$setInputs(nom = "Bob", classe = "Terminale S1", note = 10, temps = 2, absences = 3, add_row = 2)

  assert_ok(nrow(rv$df) == 2, "Ajout de lignes fonctionne", "Le nombre de lignes attendues apres ajout est incorrect")
  assert_ok(rv$df$nom[1] == "Alice", "Le nom est bien enregistre", "Le nom n'est pas correctement enregistre")

  # Modification de la ligne selectionnee
  session$setInputs(data_table_rows_selected = 1)
  session$setInputs(nom = "Alice Martin", classe = "Terminale S1", note = 16, temps = 6, absences = 0, update_row = 1)
  assert_ok(rv$df$nom[1] == "Alice Martin", "Modification du nom fonctionne", "La modification du nom a echoue")
  assert_ok(abs(rv$df$note[1] - 16) < 1e-12, "Modification de la note fonctionne", "La modification de la note a echoue")
  assert_ok(nrow(rv$df) == 2, "La modification ne change pas le nombre de lignes", "La modification a change le nombre de lignes de facon inattendue")

  # Import CSV (ajout)
  tmp_csv <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(
      nom = c("Chloe", "David"),
      classe = c("Terminale S2", "Terminale S2"),
      note = c(18, 8),
      temps_travail = c(7, 1),
      absences = c(0, 4),
      stringsAsFactors = FALSE
    ),
    tmp_csv, row.names = FALSE
  )
  session$setInputs(replace_import = FALSE)
  session$setInputs(import_file = data.frame(
    name = "import.csv", size = file.info(tmp_csv)$size, type = "text/csv", datapath = tmp_csv,
    stringsAsFactors = FALSE
  ))
  assert_ok(nrow(rv$df) == 4, "Import CSV en mode ajout fonctionne", "L'import CSV en mode ajout a echoue")

  # Import CSV (remplacement)
  tmp_csv2 <- tempfile(fileext = ".csv")
  write.csv(
    data.frame(
      nom = "Eve", classe = "Terminale S3", note = 12, temps_travail = 4, absences = 2,
      stringsAsFactors = FALSE
    ),
    tmp_csv2, row.names = FALSE
  )
  session$setInputs(replace_import = TRUE)
  session$setInputs(import_file = data.frame(
    name = "import2.csv", size = file.info(tmp_csv2)$size, type = "text/csv", datapath = tmp_csv2,
    stringsAsFactors = FALSE
  ))
  assert_ok(nrow(rv$df) == 1, "Import CSV en mode remplacement fonctionne", "L'import CSV en mode remplacement a echoue")
  assert_ok(rv$df$nom[1] == "Eve", "Import CSV en mode remplacement contient les bonnes donnees", "L'import CSV en mode remplacement n'a pas les bonnes donnees")

  # Ajout de donnees supplementaires pour tester l'analyse et la comparaison par classe
  session$setInputs(nom = "Frank", classe = "Terminale S1", note = 15, temps = 5, absences = 1, add_row = 2)

  # Lancement analyse
  session$setInputs(analyze = 1)
  s <- stats()

  assert_ok(s$n_etudiants == 2, "Effectif calcule correctement", "Effectif incorrect")
  assert_ok(!is.na(s$q1) && !is.na(s$q3), "Quartiles calcules", "Quartiles non calcules")
  assert_ok(!is.na(s$pval_temps_note) || is.na(s$pval_temps_note), "p-value temps-note accessible", "p-value temps-note inaccessible")

  cs <- classe_summary(analyzed_data())
  assert_ok(!is.null(cs), "Comparaison par classe disponible avec plusieurs classes", "La comparaison par classe devrait etre disponible")

  # Filtre par classe: le filtre doit s'afficher et restreindre les stats/graphiques,
  # sans affecter la comparaison globale par classe
  assert_ok(!is.null(output$classe_filter_ui), "Le filtre de classe s'affiche avec plusieurs classes", "Le filtre de classe devrait s'afficher avec plusieurs classes")

  classes_dispo <- sort(unique(analyzed_data()$classe))
  session$setInputs(classe_filter = classes_dispo[1])
  s_filtre <- stats()
  attendu <- dplyr::filter(analyzed_data(), classe == classes_dispo[1])
  assert_ok(s_filtre$n_etudiants == nrow(attendu), "Le filtre de classe restreint correctement l'effectif", "Le filtre de classe ne restreint pas correctement l'effectif")
  assert_ok(abs(s_filtre$moyenne - mean(attendu$note)) < 1e-9, "Le filtre de classe restreint correctement la moyenne", "Le filtre de classe ne restreint pas correctement la moyenne")

  cs_apres_filtre <- classe_summary(analyzed_data())
  assert_ok(!is.null(cs_apres_filtre) && nrow(cs_apres_filtre) == length(classes_dispo), "La comparaison par classe reste inchangee malgre le filtre", "La comparaison par classe ne devrait pas etre affectee par le filtre")

  # Generation du rapport HTML (donnees filtrees + donnees completes pour la comparaison)
  tmp_report <- tempfile(fileext = ".html")
  generate_report(attendu, tmp_report, full_df = analyzed_data(), classe_label = classes_dispo[1])
  assert_ok(file.exists(tmp_report) && file.info(tmp_report)$size > 0, "generate_report produit un fichier HTML non vide", "generate_report n'a pas produit de fichier valide")
  report_content <- paste(readLines(tmp_report, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  assert_ok(grepl("Rapport d.analyse", report_content), "Le rapport contient le titre attendu", "Le rapport ne contient pas le titre attendu")
  assert_ok(grepl("Comparaison par classe", report_content), "Le rapport contient la section comparaison par classe", "Le rapport ne contient pas la section comparaison par classe")
  assert_ok(grepl(classes_dispo[1], report_content, fixed = TRUE), "Le rapport mentionne le filtre applique", "Le rapport ne mentionne pas le filtre applique")

  session$setInputs(classe_filter = "Toutes les classes")

  # Suppression d'une ligne selectionnee
  session$setInputs(data_table_rows_selected = 1, delete_row = 1)
  assert_ok(nrow(rv$df) == 1, "Suppression de ligne fonctionne", "La suppression de ligne a echoue")

  # Reinitialisation
  session$setInputs(reset_data = 1)
  assert_ok(nrow(rv$df) == 0, "Reinitialisation des donnees fonctionne", "La reinitialisation a echoue")
  assert_ok(is.null(analyzed_data()), "Etat d'analyse reinitialise", "L'etat d'analyse n'a pas ete reinitialise")
})

cat("===== TOUS LES TESTS SONT VALIDES =====\n")
