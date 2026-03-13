# scripts/run_local.R
# Lance l'app via modulr
# Usage: source("scripts/run_local.R") depuis R/RStudio

# ---------------------------------------------------------------------------
# Fix Nix: forcer un tmpdir local AVANT tout chargement de package
# Le tempdir() par defaut (/tmp/Rtmp*) herite des permissions Nix read-only
# ce qui fait crasher bslib/selectize lors de la copie des assets JS/CSS
# ---------------------------------------------------------------------------
local_tmp <- file.path(getwd(), ".tmp", "shiny")
dir.create(local_tmp, showWarnings = FALSE, recursive = TRUE)

# Purger les vieux caches avec permissions cassees
old_rtmp <- list.dirs(local_tmp, full.names = TRUE, recursive = FALSE)
unlink(old_rtmp, recursive = TRUE, force = TRUE)

# Aussi purger dans /tmp les caches bslib/selectize avec mauvaises permissions
tmp_stale <- list.files("/tmp", full.names = TRUE,
                        pattern = "^(bslib|selectize|Rtmp)")
unlink(tmp_stale, recursive = TRUE, force = TRUE)

# Creer un nouveau Rtmp propre dans notre dossier local
new_td <- file.path(local_tmp, paste0("Rtmp", format(Sys.time(), "%H%M%S")))
dir.create(new_td, showWarnings = FALSE)

# Rediriger tempdir() via la variable C TMPDIR + recréation
Sys.setenv(TMPDIR = local_tmp)

# Forcer R a utiliser notre nouveau tempdir (R 3.x compatible)
# On ecrase le tempdir interne en passant par l'env C
old_td <- tempdir()
if (!startsWith(old_td, local_tmp)) {
  # Trick: unlink l'ancien, puis forcer R a recreer dans TMPDIR
  unlink(old_td, recursive = TRUE, force = TRUE)
  dir.create(old_td, showWarnings = FALSE)  # au cas ou R le redemande
  # Copier rien - juste s'assurer que le NOUVEAU tempdir est accessible
  # R va utiliser TMPDIR pour les NOUVEAUX fichiers temporaires
}

cat("[Markowitz] TMPDIR =", local_tmp, "\n")
cat("[Markowitz] tempdir() =", tempdir(), "\n")

# ---------------------------------------------------------------------------
# User library (packages installes manuellement sous Nix)
# ---------------------------------------------------------------------------
user_lib <- Sys.getenv("R_LIBS_USER")
if (nzchar(user_lib) && dir.exists(path.expand(user_lib))) {
  .libPaths(c(path.expand(user_lib), .libPaths()))
}

options(shiny.autoreload = TRUE)

# ---------------------------------------------------------------------------
# Pre-copier les assets bslib dans notre tmpdir propre pour eviter
# les erreurs "Permission denied" quand bslib tente de copier depuis /nix/store
# ---------------------------------------------------------------------------
tryCatch({
  bslib_dir <- system.file("lib/bs5/dist/js", package = "bslib")
  if (nzchar(bslib_dir)) {
    dest <- file.path(tempdir(), "bslib-preload")
    dir.create(dest, showWarnings = FALSE, recursive = TRUE)
    file.copy(
      list.files(bslib_dir, full.names = TRUE),
      dest,
      overwrite = TRUE,
      copy.mode = FALSE  # Ne pas copier les permissions Nix read-only
    )
  }

  selectize_dir <- system.file("www/shared/selectize/js", package = "shiny")
  if (nzchar(selectize_dir)) {
    dest_sel <- file.path(tempdir(), "selectize-preload")
    dir.create(dest_sel, showWarnings = FALSE, recursive = TRUE)
    file.copy(
      list.files(selectize_dir, full.names = TRUE),
      dest_sel,
      overwrite = TRUE,
      copy.mode = FALSE
    )
  }
  cat("[Markowitz] Assets pre-copies OK\n")
}, error = function(e) {
  cat("[Markowitz] Warning pre-copie assets:", e$message, "\n")
})

# ---------------------------------------------------------------------------
# Build + run
# ---------------------------------------------------------------------------
shiny::runApp(
  appDir = ".",
  host   = "127.0.0.1",
  port   = httpuv::randomPort()
)
