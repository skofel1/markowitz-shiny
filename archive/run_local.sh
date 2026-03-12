#!/bin/bash
# run_local.sh — Lance l'app Markowitz en local
# Fixe le tmpdir AVANT le demarrage de R pour eviter les conflits Nix
#
# Usage: bash scripts/run_local.sh
#        ou: ./scripts/run_local.sh

cd "$(dirname "$0")/.." || exit 1

# Creer un tmpdir local propre
LOCAL_TMP="$(pwd)/.tmp/shiny"
mkdir -p "$LOCAL_TMP"

# Purger les vieux caches bslib/selectize
rm -rf "$LOCAL_TMP"/bslib* "$LOCAL_TMP"/selectize* "$LOCAL_TMP"/Rtmp*

echo "[Markowitz] TMPDIR = $LOCAL_TMP"
echo "[Markowitz] Lancement sur http://localhost:3838"

# Lancer R avec TMPDIR pointe vers notre dossier local
TMPDIR="$LOCAL_TMP" Rscript scripts/run_local.R
