library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(DT)

safe_cor <- function(x, y) {
  if (length(x) < 2 || length(y) < 2) return(NA_real_)
  if (sd(x, na.rm = TRUE) == 0 || sd(y, na.rm = TRUE) == 0) return(NA_real_)
  suppressWarnings(cor(x, y, use = "complete.obs"))
}

format_metric <- function(x, digits = 2) {
  if (is.na(x) || is.nan(x) || is.infinite(x)) return("N/A")
  format(round(x, digits), nsmall = digits)
}

ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#0F766E",
    secondary = "#F59E0B",
    base_font = font_google("Poppins"),
    heading_font = font_google("Space Grotesk")
  ),
  tags$head(
    tags$style(HTML("\n      body {\n        background: radial-gradient(circle at 15% 20%, #e6fffa 0%, #f0fdfa 30%, #f8fafc 75%, #ffffff 100%);\n      }\n      .app-title {\n        font-weight: 700;\n        letter-spacing: 0.2px;\n        margin-bottom: 0.4rem;\n      }\n      .app-subtitle {\n        color: #334155;\n        margin-bottom: 1.2rem;\n      }\n      .card-soft {\n        border: 1px solid #dbeafe;\n        border-radius: 16px;\n        box-shadow: 0 10px 30px rgba(15, 23, 42, 0.06);\n      }\n      .btn-row .btn {\n        width: 100%;\n        margin-bottom: 0.5rem;\n      }\n      .small-note {\n        color: #475569;\n        font-size: 0.9rem;\n      }\n    "))
  ),

  div(
    h1("Analyse des Notes d'Etudiants", class = "app-title"),
    p(
      "Saisissez vos propres donnees (note, temps de travail, absences), puis lancez l'analyse pour obtenir automatiquement les statistiques et visualisations.",
      class = "app-subtitle"
    )
  ),

  layout_columns(
    col_widths = c(4, 8),

    card(
      class = "card-soft",
      card_header("Saisie des donnees"),
      numericInput("note", "Note (sur 20)", value = 12, min = 0, max = 20, step = 0.25),
      numericInput("temps", "Temps de travail (heures)", value = 5, min = 0, step = 0.5),
      numericInput("absences", "Nombre d'absences", value = 0, min = 0, step = 1),
      div(
        class = "btn-row",
        actionButton("add_row", "Ajouter une ligne", class = "btn-primary"),
        actionButton("delete_row", "Supprimer la ligne selectionnee", class = "btn-outline-secondary"),
        actionButton("reset_data", "Reinitialiser les donnees", class = "btn-outline-danger"),
        actionButton("analyze", "Lancer l'analyse", class = "btn-success")
      ),
      p("Selectionnez une ligne du tableau pour la supprimer.", class = "small-note")
    ),

    card(
      class = "card-soft",
      card_header("Donnees saisies"),
      DTOutput("data_table")
    )
  ),

  card(
    class = "card-soft",
    card_header("Statistiques principales"),
    uiOutput("stats_boxes")
  ),

  layout_columns(
    col_widths = c(4, 4, 4),
    card(
      class = "card-soft",
      card_header("Histogramme des notes"),
      plotOutput("hist_plot", height = "280px")
    ),
    card(
      class = "card-soft",
      card_header("Note vs Temps de travail"),
      plotOutput("scatter_temps", height = "280px")
    ),
    card(
      class = "card-soft",
      card_header("Note vs Absences"),
      plotOutput("scatter_absences", height = "280px")
    )
  ),

  card(
    class = "card-soft",
    card_header("Interpretation des resultats"),
    uiOutput("interpretation")
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    df = tibble(
      note = numeric(),
      temps_travail = numeric(),
      absences = numeric()
    )
  )

  analyzed_data <- reactiveVal(NULL)

  observeEvent(input$add_row, {
    values <- c(input$note, input$temps, input$absences)

    if (any(is.null(values)) || any(is.na(values)) || any(!is.finite(values))) {
      showNotification("Veuillez remplir tous les champs avec des valeurs numeriques valides.", type = "error")
      return()
    }

    if (input$note < 0 || input$note > 20) {
      showNotification("La note doit etre comprise entre 0 et 20.", type = "error")
      return()
    }

    if (input$temps < 0 || input$absences < 0) {
      showNotification("Le temps de travail et les absences doivent etre >= 0.", type = "error")
      return()
    }

    rv$df <- bind_rows(
      rv$df,
      tibble(
        note = as.numeric(input$note),
        temps_travail = as.numeric(input$temps),
        absences = as.numeric(input$absences)
      )
    )

    showNotification("Ligne ajoutee avec succes.", type = "message")
  })

  observeEvent(input$delete_row, {
    selected <- input$data_table_rows_selected

    if (length(selected) == 0) {
      showNotification("Selectionnez une ligne a supprimer.", type = "warning")
      return()
    }

    rv$df <- rv$df[-selected, , drop = FALSE]
    showNotification("Ligne supprimee.", type = "message")
  })

  observeEvent(input$reset_data, {
    rv$df <- tibble(
      note = numeric(),
      temps_travail = numeric(),
      absences = numeric()
    )
    analyzed_data(NULL)
    showNotification("Donnees reinitialisees.", type = "message")
  })

  observeEvent(input$analyze, {
    if (nrow(rv$df) == 0) {
      showNotification("Ajoutez au moins une ligne avant de lancer l'analyse.", type = "error")
      return()
    }

    analyzed_data(rv$df)
    showNotification("Analyse mise a jour.", type = "message")
  })

  output$data_table <- renderDT({
    datatable(
      rv$df,
      rownames = FALSE,
      selection = "single",
      options = list(pageLength = 6, dom = "tp")
    )
  })

  stats <- reactive({
    df <- analyzed_data()
    req(df)

    tibble(
      moyenne = mean(df$note),
      mediane = median(df$note),
      minimum = min(df$note),
      maximum = max(df$note),
      ecart_type = if (nrow(df) > 1) sd(df$note) else NA_real_,
      corr_temps_note = safe_cor(df$temps_travail, df$note),
      corr_absences_note = safe_cor(df$absences, df$note)
    )
  })

  output$stats_boxes <- renderUI({
    s <- stats()

    layout_column_wrap(
      width = 1 / 4,
      value_box(title = "Moyenne", value = format_metric(s$moyenne), showcase = icon("calculator")),
      value_box(title = "Mediane", value = format_metric(s$mediane), showcase = icon("ruler")),
      value_box(title = "Min / Max", value = paste0(format_metric(s$minimum), " / ", format_metric(s$maximum)), showcase = icon("arrows-left-right")),
      value_box(title = "Ecart-type", value = format_metric(s$ecart_type), showcase = icon("chart-column")),
      value_box(title = "Corr Temps-Note", value = format_metric(s$corr_temps_note), showcase = icon("arrow-trend-up")),
      value_box(title = "Corr Absences-Note", value = format_metric(s$corr_absences_note), showcase = icon("arrow-trend-down"))
    )
  })

  output$hist_plot <- renderPlot({
    df <- analyzed_data()
    req(df)

    ggplot(df, aes(x = note)) +
      geom_histogram(binwidth = 1, fill = "#0F766E", color = "white", alpha = 0.9) +
      labs(x = "Note", y = "Frequence") +
      theme_minimal(base_size = 13)
  })

  output$scatter_temps <- renderPlot({
    df <- analyzed_data()
    req(df)

    ggplot(df, aes(x = temps_travail, y = note)) +
      geom_point(size = 3, color = "#2563EB", alpha = 0.8) +
      geom_smooth(method = "lm", se = FALSE, color = "#DC2626", linewidth = 1) +
      labs(x = "Temps de travail (h)", y = "Note") +
      theme_minimal(base_size = 13)
  })

  output$scatter_absences <- renderPlot({
    df <- analyzed_data()
    req(df)

    ggplot(df, aes(x = absences, y = note)) +
      geom_point(size = 3, color = "#F59E0B", alpha = 0.85) +
      geom_smooth(method = "lm", se = FALSE, color = "#DC2626", linewidth = 1) +
      labs(x = "Nombre d'absences", y = "Note") +
      theme_minimal(base_size = 13)
  })

  output$interpretation <- renderUI({
    s <- stats()

    corr_t <- s$corr_temps_note
    corr_a <- s$corr_absences_note

    text_t <- if (is.na(corr_t)) {
      "Correlation temps-note non interpretable (donnees insuffisantes ou variance nulle)."
    } else if (corr_t > 0.3) {
      "Correlation positive entre temps de travail et note : plus le temps de travail augmente, plus la note tend a augmenter."
    } else if (corr_t < -0.3) {
      "Correlation negative entre temps de travail et note : tendance inverse observee."
    } else {
      "Correlation faible entre temps de travail et note : relation lineaire peu marquee."
    }

    text_a <- if (is.na(corr_a)) {
      "Correlation absences-note non interpretable (donnees insuffisantes ou variance nulle)."
    } else if (corr_a < -0.3) {
      "Correlation negative entre absences et note : davantage d'absences est associe a une baisse des notes."
    } else if (corr_a > 0.3) {
      "Correlation positive entre absences et note : resultat atypique a verifier selon le contexte."
    } else {
      "Correlation faible entre absences et note : impact lineaire peu net dans cet echantillon."
    }

    tagList(
      p(paste0("Moyenne: ", format_metric(s$moyenne), " | Mediane: ", format_metric(s$mediane), " | Ecart-type: ", format_metric(s$ecart_type), ".")),
      tags$ul(
        tags$li(text_t),
        tags$li(text_a),
        tags$li("Utilisez ces indicateurs pour comparer des classes, ajuster les habitudes de travail et suivre l'evolution des performances.")
      )
    )
  })
}

shinyApp(ui, server)
