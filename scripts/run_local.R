# scripts/run_local.R
# Lance l'app via modulr
# À exécuter depuis la racine du repo (working dir = markowitz-shiny)

options(shiny.autoreload = TRUE)

AppBuilder_ <- modulr::make("tool/MarkowitzShiny/markowitz_shiny_app_provider")()
app <- AppBuilder_()

shiny::runApp(app, host = "0.0.0.0", port = 3838)
