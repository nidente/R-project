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
assert_ok(exists("format_metric"), "Fonction format_metric chargee", "format_metric introuvable")
assert_ok(exists("ui"), "Objet ui charge", "ui introuvable")
assert_ok(exists("server"), "Fonction server chargee", "server introuvable")

# Etape 2: tests unitaires sur safe_cor
x <- c(2, 4, 6, 8)
y <- c(1, 2, 3, 4)
val_cor <- safe_cor(x, y)
assert_ok(!is.na(val_cor), "safe_cor retourne une valeur numerique", "safe_cor retourne NA de facon inattendue")
assert_ok(abs(val_cor - 1) < 1e-12, "safe_cor calcule correctement la correlation parfaite", "safe_cor ne calcule pas correctement la correlation")
assert_ok(is.na(safe_cor(c(1), c(1))), "safe_cor gere le cas n < 2", "safe_cor devrait retourner NA si n < 2")
assert_ok(is.na(safe_cor(c(1, 1, 1), c(2, 3, 4))), "safe_cor gere la variance nulle", "safe_cor devrait retourner NA avec variance nulle")

# Etape 3: tests unitaires sur format_metric
assert_ok(format_metric(12.3456) == "12.35", "format_metric arrondit correctement", "format_metric n'arrondit pas correctement")
assert_ok(format_metric(NA_real_) == "N/A", "format_metric gere NA", "format_metric devrait retourner N/A pour NA")

# Etape 4: test d'integration serveur (ajout, analyse, suppression, reset)
shiny::testServer(server, {
  # Ajout de deux lignes
  session$setInputs(note = 14, temps = 5, absences = 1, add_row = 1)
  session$setInputs(note = 10, temps = 2, absences = 3, add_row = 2)

  assert_ok(nrow(rv$df) == 2, "Ajout de lignes fonctionne", "Le nombre de lignes attendues apres ajout est incorrect")

  # Lancement analyse
  session$setInputs(analyze = 1)
  s <- stats()

  assert_ok(abs(s$moyenne - 12) < 1e-12, "Calcul moyenne correct", "Moyenne incorrecte")
  assert_ok(abs(s$mediane - 12) < 1e-12, "Calcul mediane correct", "Mediane incorrecte")
  assert_ok(abs(s$minimum - 10) < 1e-12, "Calcul minimum correct", "Minimum incorrect")
  assert_ok(abs(s$maximum - 14) < 1e-12, "Calcul maximum correct", "Maximum incorrect")
  assert_ok(!is.na(s$corr_temps_note), "Correlation temps-note calculee", "Correlation temps-note non calculee")
  assert_ok(!is.na(s$corr_absences_note), "Correlation absences-note calculee", "Correlation absences-note non calculee")

  # Suppression d'une ligne selectionnee
  session$setInputs(data_table_rows_selected = 1, delete_row = 1)
  assert_ok(nrow(rv$df) == 1, "Suppression de ligne fonctionne", "La suppression de ligne a echoue")

  # Reinitialisation
  session$setInputs(reset_data = 1)
  assert_ok(nrow(rv$df) == 0, "Reinitialisation des donnees fonctionne", "La reinitialisation a echoue")
  assert_ok(is.null(analyzed_data()), "Etat d'analyse reinitialise", "L'etat d'analyse n'a pas ete reinitialise")
})

cat("===== TOUS LES TESTS SONT VALIDES =====\n")
