library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(DT)
library(readxl)
library(rmarkdown)
library(plotly)

source("helpers.R")

app_css <- "
  body {
    background: radial-gradient(circle at 15% 20%, #e6fffa 0%, #f0fdfa 30%, #f8fafc 75%, #ffffff 100%);
    transition: background 0.3s ease;
  }
  html[data-bs-theme='dark'] body {
    background: radial-gradient(circle at 15% 20%, #10202b 0%, #13233a 30%, #0b1220 75%, #05070d 100%);
  }
  .app-title {
    font-weight: 700;
    letter-spacing: 0.2px;
    margin-bottom: 0;
  }
  .app-subtitle {
    color: #334155;
    margin-bottom: 1.2rem;
  }
  html[data-bs-theme='dark'] .app-subtitle { color: #cbd5e1; }
  .card-soft {
    border: 1px solid #dbeafe;
    border-radius: 16px;
    box-shadow: 0 10px 30px rgba(15, 23, 42, 0.06);
    transition: transform 0.15s ease, box-shadow 0.15s ease;
  }
  .card-soft:hover {
    transform: translateY(-2px);
    box-shadow: 0 14px 34px rgba(15, 23, 42, 0.12);
  }
  html[data-bs-theme='dark'] .card-soft {
    border-color: rgba(255, 255, 255, 0.08);
    box-shadow: 0 10px 30px rgba(0, 0, 0, 0.45);
  }
  html[data-bs-theme='dark'] .card-soft:hover {
    box-shadow: 0 14px 34px rgba(0, 0, 0, 0.6);
  }
  .btn, .form-control, .form-select {
    border-radius: 10px;
  }
  .btn-row .btn {
    width: 100%;
    margin-bottom: 0.5rem;
  }
  .small-note {
    color: #475569;
    font-size: 0.9rem;
  }
  html[data-bs-theme='dark'] .small-note { color: #94a3b8; }
  .comparison-table th, .comparison-table td {
    padding: 0.4rem 0.6rem;
  }
  .app-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    width: 100%;
    gap: 1rem;
  }
"

ui <- page_sidebar(
  fillable = FALSE,
  title = div(
    class = "app-header",
    h1("Analyse des Notes d'Etudiants", class = "app-title"),
    input_dark_mode(id = "dark_mode", mode = "light")
  ),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#0F766E",
    secondary = "#F59E0B",
    base_font = font_google("Poppins"),
    heading_font = font_google("Space Grotesk")
  ),
  tags$head(tags$style(HTML(app_css))),

  sidebar = sidebar(
    width = 360,
    title = "Saisie des donnees",
    textInput("nom", "Nom de l'etudiant", placeholder = "ex: Alice Martin"),
    textInput("classe", "Classe", placeholder = "ex: Terminale S1 (optionnel)"),
    numericInput("note", "Note (sur 20)", value = 12, min = 0, max = 20, step = 0.25),
    numericInput("temps", "Temps de travail (heures)", value = 5, min = 0, step = 0.5),
    numericInput("absences", "Nombre d'absences", value = 0, min = 0, step = 1),
    div(
      class = "btn-row",
      actionButton("add_row", "Ajouter une ligne", class = "btn-primary"),
      actionButton("update_row", "Modifier la ligne selectionnee", class = "btn-outline-primary"),
      actionButton("delete_row", "Supprimer la ligne selectionnee", class = "btn-outline-secondary"),
      actionButton("reset_data", "Reinitialiser les donnees", class = "btn-outline-danger"),
      actionButton("analyze", "Lancer l'analyse", class = "btn-success")
    ),
    p("Selectionnez une ligne du tableau pour la modifier ou la supprimer.", class = "small-note"),
    hr(),
    p(strong("Import / Export"), class = "small-note"),
    fileInput("import_file", "Importer un fichier (CSV ou Excel)", accept = c(".csv", ".xlsx", ".xls")),
    checkboxInput("replace_import", "Remplacer les donnees existantes lors de l'import", value = FALSE),
    downloadButton("export_csv", "Telecharger les donnees (CSV)", class = "btn-outline-primary"),
    p("Colonnes attendues: nom, classe, note, temps_travail, absences.", class = "small-note"),
    hr(),
    p(strong("Rapport d'analyse"), class = "small-note"),
    downloadButton("export_report", "Telecharger le rapport (HTML)", class = "btn-outline-success"),
    p("Necessite d'avoir lance une analyse au prealable.", class = "small-note")
  ),

  p(
    "Saisissez vos propres donnees (nom, classe, note, temps de travail, absences), puis lancez l'analyse pour obtenir automatiquement les statistiques et visualisations.",
    class = "app-subtitle"
  ),

  card(
    class = "card-soft",
    card_header("Donnees saisies"),
    DTOutput("data_table")
  ),

  uiOutput("classe_filter_ui"),

  card(
    class = "card-soft",
    card_header("Statistiques principales"),
    uiOutput("stats_boxes")
  ),

  layout_columns(
    col_widths = c(6, 6, 6, 6),
    card(
      class = "card-soft",
      full_screen = TRUE,
      card_header("Histogramme des notes"),
      plotlyOutput("hist_plot", height = "420px")
    ),
    card(
      class = "card-soft",
      full_screen = TRUE,
      card_header("Note vs Temps de travail"),
      plotlyOutput("scatter_temps", height = "420px")
    ),
    card(
      class = "card-soft",
      full_screen = TRUE,
      card_header("Note vs Absences"),
      plotlyOutput("scatter_absences", height = "420px")
    ),
    card(
      class = "card-soft",
      full_screen = TRUE,
      card_header("Notes par classe (toutes classes)"),
      plotlyOutput("boxplot_classe", height = "420px")
    )
  ),

  card(
    class = "card-soft",
    card_header("Comparaison par classe (toutes classes)"),
    uiOutput("classe_comparison")
  ),

  card(
    class = "card-soft",
    card_header("Interpretation des resultats"),
    uiOutput("interpretation")
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(df = empty_df())

  analyzed_data <- reactiveVal(NULL)

  observeEvent(input$data_table_rows_selected, {
    selected <- input$data_table_rows_selected
    if (length(selected) == 1) {
      row <- rv$df[selected, ]
      updateTextInput(session, "nom", value = row$nom)
      updateTextInput(session, "classe", value = row$classe)
      updateNumericInput(session, "note", value = row$note)
      updateNumericInput(session, "temps", value = row$temps_travail)
      updateNumericInput(session, "absences", value = row$absences)
    }
  })

  observeEvent(input$add_row, {
    err <- validate_entry(input$nom, input$note, input$temps, input$absences)
    if (!is.null(err)) {
      showNotification(err, type = "error")
      return()
    }

    classe_val <- if (nzchar(trimws(input$classe))) input$classe else "Non classe"

    rv$df <- bind_rows(
      rv$df,
      tibble(
        nom = input$nom,
        classe = classe_val,
        note = as.numeric(input$note),
        temps_travail = as.numeric(input$temps),
        absences = as.numeric(input$absences)
      )
    )

    showNotification("Ligne ajoutee avec succes.", type = "message")
  })

  observeEvent(input$update_row, {
    selected <- input$data_table_rows_selected

    if (length(selected) != 1) {
      showNotification("Selectionnez une seule ligne a modifier.", type = "warning")
      return()
    }

    err <- validate_entry(input$nom, input$note, input$temps, input$absences)
    if (!is.null(err)) {
      showNotification(err, type = "error")
      return()
    }

    classe_val <- if (nzchar(trimws(input$classe))) input$classe else "Non classe"

    rv$df[selected, ] <- tibble(
      nom = input$nom,
      classe = classe_val,
      note = as.numeric(input$note),
      temps_travail = as.numeric(input$temps),
      absences = as.numeric(input$absences)
    )

    showNotification("Ligne modifiee avec succes.", type = "message")
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
    rv$df <- empty_df()
    analyzed_data(NULL)
    showNotification("Donnees reinitialisees.", type = "message")
  })

  observeEvent(input$import_file, {
    req(input$import_file)

    imported_raw <- tryCatch(
      read_import_file(input$import_file$datapath, input$import_file$name),
      error = function(e) NULL
    )

    if (is.null(imported_raw)) {
      showNotification("Impossible de lire le fichier importe.", type = "error")
      return()
    }

    result <- prepare_import(imported_raw)

    if (!result$ok) {
      showNotification(result$error, type = "error")
      return()
    }

    if (nrow(result$data) == 0) {
      showNotification("Aucune ligne valide trouvee dans le fichier importe.", type = "error")
      return()
    }

    rv$df <- if (isTRUE(input$replace_import)) {
      result$data
    } else {
      bind_rows(rv$df, result$data)
    }

    msg <- paste0(nrow(result$data), " ligne(s) importee(s).")
    if (result$n_invalid > 0) {
      msg <- paste0(msg, " ", result$n_invalid, " ligne(s) ignoree(s) (donnees invalides).")
    }
    showNotification(msg, type = "message")
  })

  output$export_csv <- downloadHandler(
    filename = function() paste0("notes_etudiants_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      write.csv(rv$df, file, row.names = FALSE)
    }
  )

  observeEvent(input$analyze, {
    if (nrow(rv$df) == 0) {
      showNotification("Ajoutez au moins une ligne avant de lancer l'analyse.", type = "error")
      return()
    }

    analyzed_data(rv$df)
    showNotification("Analyse mise a jour.", type = "message")
  })

  filtered_data <- reactive({
    df <- analyzed_data()
    req(df)

    sel <- input$classe_filter
    if (is.null(sel) || sel == "Toutes les classes") {
      df
    } else {
      df %>% filter(classe == sel)
    }
  })

  output$classe_filter_ui <- renderUI({
    df <- analyzed_data()
    req(df)

    classes <- sort(unique(df$classe))
    if (length(classes) < 2) return(NULL)

    card(
      class = "card-soft",
      card_header("Filtre"),
      div(
        style = "max-width: 480px;",
        selectInput(
          "classe_filter",
          "Afficher les statistiques et graphiques pour :",
          choices = c("Toutes les classes", classes),
          selected = "Toutes les classes",
          width = "100%"
        )
      )
    )
  })

  output$export_report <- downloadHandler(
    filename = function() paste0("rapport_analyse_", format(Sys.Date(), "%Y%m%d"), ".html"),
    content = function(file) {
      base_df <- analyzed_data()

      if (is.null(base_df) || nrow(base_df) == 0) {
        writeLines(
          "<html><body><h2>Aucune analyse disponible</h2><p>Ajoutez des donnees et cliquez sur \"Lancer l'analyse\" avant de telecharger le rapport.</p></body></html>",
          file
        )
        return()
      }

      df <- filtered_data()
      classe_label <- if (is.null(input$classe_filter) || input$classe_filter == "Toutes les classes") {
        "Toutes les classes"
      } else {
        input$classe_filter
      }

      generate_report(df, file, full_df = base_df, classe_label = classe_label)
    }
  )

  output$data_table <- renderDT({
    datatable(
      rv$df,
      rownames = FALSE,
      selection = "single",
      colnames = c(
        "Nom" = "nom",
        "Classe" = "classe",
        "Note" = "note",
        "Temps travail (h)" = "temps_travail",
        "Absences" = "absences"
      ),
      options = list(pageLength = 6, dom = "tp")
    )
  })

  stats <- reactive({
    df <- filtered_data()
    req(df)
    compute_stats(df)
  })

  output$stats_boxes <- renderUI({
    s <- stats()

    corr_t_theme <- if (is.na(s$corr_temps_note)) {
      "secondary"
    } else if (s$corr_temps_note > 0.3) {
      "success"
    } else if (s$corr_temps_note < -0.3) {
      "danger"
    } else {
      "secondary"
    }

    corr_a_theme <- if (is.na(s$corr_absences_note)) {
      "secondary"
    } else if (s$corr_absences_note < -0.3) {
      "success"
    } else if (s$corr_absences_note > 0.3) {
      "danger"
    } else {
      "secondary"
    }

    layout_column_wrap(
      width = "220px",
      heights_equal = "row",
      value_box(title = "Nb etudiants", value = s$n_etudiants, showcase = icon("users"), theme = "primary"),
      value_box(title = "Moyenne", value = format_metric(s$moyenne), showcase = icon("calculator"), theme = "success"),
      value_box(title = "Mediane", value = format_metric(s$mediane), showcase = icon("ruler")),
      value_box(title = "Min / Max", value = paste0(format_metric(s$minimum), " / ", format_metric(s$maximum)), showcase = icon("arrows-left-right")),
      value_box(title = "Q1 / Q3", value = paste0(format_metric(s$q1), " / ", format_metric(s$q3)), showcase = icon("chart-simple")),
      value_box(title = "Ecart-type", value = format_metric(s$ecart_type), showcase = icon("chart-column")),
      value_box(title = "Corr Temps-Note", value = format_metric(s$corr_temps_note), showcase = icon("arrow-trend-up"), theme = corr_t_theme),
      value_box(title = "Corr Absences-Note", value = format_metric(s$corr_absences_note), showcase = icon("arrow-trend-down"), theme = corr_a_theme)
    )
  })

  output$hist_plot <- renderPlotly({
    df <- filtered_data()
    req(df)
    plotly::ggplotly(plot_histogram(df)) %>% plotly::config(displaylogo = FALSE)
  })

  output$scatter_temps <- renderPlotly({
    df <- filtered_data()
    req(df)
    plotly::ggplotly(plot_scatter_temps(df)) %>% plotly::config(displaylogo = FALSE)
  })

  output$scatter_absences <- renderPlotly({
    df <- filtered_data()
    req(df)
    plotly::ggplotly(plot_scatter_absences(df)) %>% plotly::config(displaylogo = FALSE)
  })

  output$boxplot_classe <- renderPlotly({
    df <- analyzed_data()
    req(df)
    plotly::ggplotly(plot_boxplot_classe(df)) %>% plotly::config(displaylogo = FALSE)
  })

  output$classe_comparison <- renderUI({
    df <- analyzed_data()
    req(df)

    summary_df <- classe_summary(df)

    if (is.null(summary_df)) {
      return(p("Ajoutez des etudiants dans au moins deux classes differentes pour afficher une comparaison.", class = "small-note"))
    }

    tags$table(
      class = "table table-sm comparison-table",
      tags$thead(
        tags$tr(
          tags$th("Classe"), tags$th("Effectif"), tags$th("Moyenne"), tags$th("Mediane"), tags$th("Ecart-type")
        )
      ),
      tags$tbody(
        lapply(seq_len(nrow(summary_df)), function(i) {
          tags$tr(
            tags$td(summary_df$classe[i]),
            tags$td(summary_df$n[i]),
            tags$td(format_metric(summary_df$moyenne[i])),
            tags$td(format_metric(summary_df$mediane[i])),
            tags$td(format_metric(summary_df$ecart_type[i]))
          )
        })
      )
    )
  })

  output$interpretation <- renderUI({
    s <- stats()
    interp <- build_interpretation(s)

    tagList(
      p(paste0(
        "Effectif: ", s$n_etudiants,
        " | Moyenne: ", format_metric(s$moyenne),
        " | Mediane: ", format_metric(s$mediane),
        " | Q1-Q3: ", format_metric(s$q1), "-", format_metric(s$q3),
        " | Ecart-type: ", format_metric(s$ecart_type), "."
      )),
      tags$ul(
        tags$li(interp$text_t),
        tags$li(interp$text_a),
        tags$li("Un p < 0.05 indique une correlation statistiquement significative sur cet echantillon."),
        tags$li("Utilisez ces indicateurs pour comparer des classes, ajuster les habitudes de travail et suivre l'evolution des performances.")
      )
    )
  })
}

shinyApp(ui, server)
