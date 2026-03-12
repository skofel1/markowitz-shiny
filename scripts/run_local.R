# scripts/run_local.R
# Lance l'app via modulr
# Utiliser via: bash scripts/run_local.sh (recommande, fixe le TMPDIR)
#          ou: TMPDIR=.tmp/shiny Rscript scripts/run_local.R

# ---------------------------------------------------------------------------
# Dev comfort
# ---------------------------------------------------------------------------
# Ajouter le user library (packages installes manuellement)
user_lib <- Sys.getenv("R_LIBS_USER")
if (nzchar(user_lib) && dir.exists(path.expand(user_lib))) {
  .libPaths(c(path.expand(user_lib), .libPaths()))
}

options(shiny.autoreload = TRUE)

# ---------------------------------------------------------------------------
# Purger les caches bslib/selectize dans le tempdir courant
# (evite les "Permission denied" sous Nix)
# ---------------------------------------------------------------------------
td <- tempdir()
stale <- list.files(td, full.names = TRUE,
                    pattern = "^(bslib|selectize)")
unlink(stale, recursive = TRUE, force = TRUE)

cat("[Markowitz] tempdir() =", td, "\n")

# ---------------------------------------------------------------------------
# Build + run
# ---------------------------------------------------------------------------
shiny::runApp(
  appDir = ".",
  host   = "127.0.0.1",
  port   = httpuv::randomPort()
)
