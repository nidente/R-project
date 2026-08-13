library(ggplot2)
library(dplyr)

safe_cor <- function(x, y) {
  if (length(x) < 2 || length(y) < 2) return(NA_real_)
  if (sd(x, na.rm = TRUE) == 0 || sd(y, na.rm = TRUE) == 0) return(NA_real_)
  suppressWarnings(cor(x, y, use = "complete.obs"))
}

safe_cor_pvalue <- function(x, y) {
  if (length(x) < 3 || length(y) < 3) return(NA_real_)
  if (sd(x, na.rm = TRUE) == 0 || sd(y, na.rm = TRUE) == 0) return(NA_real_)
  res <- tryCatch(suppressWarnings(cor.test(x, y)), error = function(e) NULL)
  if (is.null(res)) return(NA_real_)
  res$p.value
}

format_metric <- function(x, digits = 2) {
  if (is.na(x) || is.nan(x) || is.infinite(x)) return("N/A")
  format(round(x, digits), nsmall = digits)
}

format_pvalue <- function(p) {
  if (is.na(p)) return("N/A")
  if (p < 0.001) return("< 0.001")
  format(round(p, 3), nsmall = 3)
}

empty_df <- function() {
  tibble(
    nom = character(),
    classe = character(),
    note = numeric(),
    temps_travail = numeric(),
    absences = numeric()
  )
}

# Valide une saisie manuelle (formulaire). Retourne NULL si valide, sinon un message d'erreur.
validate_entry <- function(nom, note, temps, absences) {
  if (is.null(nom) || !nzchar(trimws(nom))) {
    return("Le nom de l'etudiant est obligatoire.")
  }

  values <- c(note, temps, absences)
  if (any(is.null(values)) || any(is.na(values)) || any(!is.finite(values))) {
    return("Veuillez remplir tous les champs avec des valeurs numeriques valides.")
  }

  if (note < 0 || note > 20) {
    return("La note doit etre comprise entre 0 et 20.")
  }

  if (temps < 0 || absences < 0) {
    return("Le temps de travail et les absences doivent etre >= 0.")
  }

  NULL
}

# Lit un fichier importe (CSV ou Excel) en data.frame, selon son extension.
read_import_file <- function(datapath, filename) {
  ext <- tolower(tools::file_ext(filename))

  if (ext %in% c("xlsx", "xls")) {
    as.data.frame(readxl::read_excel(datapath, sheet = 1))
  } else {
    read.csv(datapath, stringsAsFactors = FALSE)
  }
}

# Normalise et valide un data.frame importe (CSV ou Excel).
# Retourne list(ok, error) ou list(ok, data, n_invalid).
prepare_import <- function(imported_df) {
  required_cols <- c("nom", "classe", "note", "temps_travail", "absences")
  missing_cols <- setdiff(required_cols, names(imported_df))

  if (length(missing_cols) > 0) {
    return(list(
      ok = FALSE,
      error = paste0("Colonnes manquantes dans le fichier: ", paste(missing_cols, collapse = ", "))
    ))
  }

  imported <- imported_df %>%
    transmute(
      nom = as.character(nom),
      classe = as.character(classe),
      note = suppressWarnings(as.numeric(note)),
      temps_travail = suppressWarnings(as.numeric(temps_travail)),
      absences = suppressWarnings(as.numeric(absences))
    )

  valid_mask <- !is.na(imported$note) & !is.na(imported$temps_travail) & !is.na(imported$absences) &
    nzchar(trimws(imported$nom)) &
    imported$note >= 0 & imported$note <= 20 &
    imported$temps_travail >= 0 & imported$absences >= 0

  n_invalid <- sum(!valid_mask)
  valid_rows <- imported[valid_mask, , drop = FALSE]

  if (nrow(valid_rows) > 0) {
    empty_classe <- !nzchar(trimws(valid_rows$classe))
    valid_rows$classe[empty_classe] <- "Non classe"
  }

  list(ok = TRUE, data = valid_rows, n_invalid = n_invalid)
}

# Statistiques principales calculees sur les donnees analysees.
compute_stats <- function(df) {
  tibble(
    n_etudiants = nrow(df),
    moyenne = mean(df$note),
    mediane = median(df$note),
    minimum = min(df$note),
    maximum = max(df$note),
    q1 = if (nrow(df) > 1) quantile(df$note, 0.25, names = FALSE) else NA_real_,
    q3 = if (nrow(df) > 1) quantile(df$note, 0.75, names = FALSE) else NA_real_,
    ecart_type = if (nrow(df) > 1) sd(df$note) else NA_real_,
    corr_temps_note = safe_cor(df$temps_travail, df$note),
    corr_absences_note = safe_cor(df$absences, df$note),
    pval_temps_note = safe_cor_pvalue(df$temps_travail, df$note),
    pval_absences_note = safe_cor_pvalue(df$absences, df$note)
  )
}

# Statistiques moyenne/mediane/effectif par classe, triees par moyenne decroissante.
# Retourne NULL si moins de deux classes distinctes (comparaison non pertinente).
classe_summary <- function(df) {
  if (n_distinct(df$classe) < 2) return(NULL)

  df %>%
    group_by(classe) %>%
    summarise(
      n = n(),
      moyenne = mean(note),
      mediane = median(note),
      ecart_type = if (n() > 1) sd(note) else NA_real_,
      .groups = "drop"
    ) %>%
    arrange(desc(moyenne))
}

# Phrases d'interpretation des correlations, partagees entre l'app et le rapport.
build_interpretation <- function(s) {
  corr_t <- s$corr_temps_note
  corr_a <- s$corr_absences_note
  pval_t <- s$pval_temps_note
  pval_a <- s$pval_absences_note

  text_t <- if (is.na(corr_t)) {
    "Correlation temps-note non interpretable (donnees insuffisantes ou variance nulle)."
  } else {
    base_t <- if (corr_t > 0.3) {
      "Correlation positive entre temps de travail et note : plus le temps de travail augmente, plus la note tend a augmenter."
    } else if (corr_t < -0.3) {
      "Correlation negative entre temps de travail et note : tendance inverse observee."
    } else {
      "Correlation faible entre temps de travail et note : relation lineaire peu marquee."
    }
    paste0(base_t, " (r = ", format_metric(corr_t), ", p = ", format_pvalue(pval_t), ")")
  }

  text_a <- if (is.na(corr_a)) {
    "Correlation absences-note non interpretable (donnees insuffisantes ou variance nulle)."
  } else {
    base_a <- if (corr_a < -0.3) {
      "Correlation negative entre absences et note : davantage d'absences est associe a une baisse des notes."
    } else if (corr_a > 0.3) {
      "Correlation positive entre absences et note : resultat atypique a verifier selon le contexte."
    } else {
      "Correlation faible entre absences et note : impact lineaire peu net dans cet echantillon."
    }
    paste0(base_a, " (r = ", format_metric(corr_a), ", p = ", format_pvalue(pval_a), ")")
  }

  list(text_t = text_t, text_a = text_a)
}

plot_histogram <- function(df) {
  ggplot(df, aes(x = note)) +
    geom_histogram(binwidth = 1, fill = "#0F766E", color = "white", alpha = 0.9) +
    labs(x = "Note", y = "Frequence") +
    theme_minimal(base_size = 13)
}

plot_scatter_temps <- function(df) {
  ggplot(df, aes(x = temps_travail, y = note)) +
    geom_point(size = 3, color = "#2563EB", alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE, color = "#DC2626", linewidth = 1) +
    labs(x = "Temps de travail (h)", y = "Note") +
    theme_minimal(base_size = 13)
}

plot_scatter_absences <- function(df) {
  ggplot(df, aes(x = absences, y = note)) +
    geom_point(size = 3, color = "#F59E0B", alpha = 0.85) +
    geom_smooth(method = "lm", se = FALSE, color = "#DC2626", linewidth = 1) +
    labs(x = "Nombre d'absences", y = "Note") +
    theme_minimal(base_size = 13)
}

plot_boxplot_classe <- function(df) {
  ggplot(df, aes(x = classe, y = note, fill = classe)) +
    geom_boxplot(alpha = 0.85, show.legend = FALSE) +
    scale_fill_brewer(palette = "Set2") +
    labs(x = "Classe", y = "Note") +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
}

# Genere le rapport HTML d'analyse dans output_file, a partir du template Rmd.
# df: donnees (eventuellement filtrees par classe) utilisees pour les statistiques et graphiques principaux.
# full_df: donnees completes (toutes classes) utilisees pour le boxplot et la comparaison par classe.
# Rend dans un dossier temporaire isole pour eviter les conflits entre generations concurrentes.
generate_report <- function(df, output_file, full_df = df, classe_label = "Toutes les classes", template = "report_template.Rmd") {
  report_dir <- tempfile("rapport_")
  dir.create(report_dir)
  file.copy(template, file.path(report_dir, "report_template.Rmd"), overwrite = TRUE)
  file.copy("helpers.R", file.path(report_dir, "helpers.R"), overwrite = TRUE)

  rmarkdown::render(
    input = file.path(report_dir, "report_template.Rmd"),
    output_file = output_file,
    params = list(data = df, full_data = full_df, classe_label = classe_label),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )

  invisible(output_file)
}
