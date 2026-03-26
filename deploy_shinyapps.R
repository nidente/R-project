#!/usr/bin/env Rscript

# Deploiement Shiny vers shinyapps.io
# Usage (Linux/macOS):
# SHINYAPPS_NAME="..." SHINYAPPS_TOKEN="..." SHINYAPPS_SECRET="..." Rscript deploy_shinyapps.R

required_env <- c("SHINYAPPS_NAME", "SHINYAPPS_TOKEN", "SHINYAPPS_SECRET")
missing <- required_env[Sys.getenv(required_env) == ""]

if (length(missing) > 0) {
  stop(
    paste0(
      "Variables d'environnement manquantes: ",
      paste(missing, collapse = ", "),
      "\nAjoutez-les puis relancez le script."
    ),
    call. = FALSE
  )
}

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect", repos = "https://cloud.r-project.org")
}

library(rsconnect)

app_dir <- normalizePath(".")
app_name <- Sys.getenv("SHINYAPPS_APPNAME", unset = "analyse-notes-etudiants")

rsconnect::setAccountInfo(
  name = Sys.getenv("SHINYAPPS_NAME"),
  token = Sys.getenv("SHINYAPPS_TOKEN"),
  secret = Sys.getenv("SHINYAPPS_SECRET")
)

message("Verification locale du fichier app.R...")
source(file.path(app_dir, "app.R"))

message("Deploiement en cours sur shinyapps.io...")
res <- rsconnect::deployApp(
  appDir = app_dir,
  appName = app_name,
  appPrimaryDoc = "app.R",
  forceUpdate = TRUE,
  launch.browser = FALSE
)

message("Deploiement termine.")
print(res)
