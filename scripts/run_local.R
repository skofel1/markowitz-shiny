# scripts/run_local.R
# Lance l'app via modulr
# À exécuter depuis la racine du repo (working dir = markowitz-shiny)

# ---------------------------------------------------------------------------
# Dev comfort
# ---------------------------------------------------------------------------
options(shiny.autoreload = TRUE)

# ---------------------------------------------------------------------------
# Fix env (souvent utile en environnements type Nix / pods):
# purge des deps web temporaires (bslib + selectize) pour éviter Permission denied
# ---------------------------------------------------------------------------
cleanup_shiny_tmp <- function() {
  td <- tempdir()
  x  <- list.files(td, full.names = TRUE)
  rm <- x[grepl("^(bslib-|selectize)", basename(x))]
  unlink(rm, recursive = TRUE, force = TRUE)
}
cleanup_shiny_tmp()

# (Optionnel) si tu veux forcer un TMP "maison" quand tu es sur pod/Nix
# dir.create("tmp", showWarnings = FALSE)
# Sys.setenv(TMPDIR = normalizePath("tmp", winslash = "/", mustWork = TRUE))

# ---------------------------------------------------------------------------
# Build + run
# ---------------------------------------------------------------------------
AppBuilder_ <- modulr::make("tool/MarkowitzShiny/markowitz_shiny_app_provider")
app <- AppBuilder_()

shiny::runApp(app, host = "0.0.0.0", port = 3838)
