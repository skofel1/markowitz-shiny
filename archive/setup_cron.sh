#!/bin/bash
# setup_cron.sh — Configure les taches cron pour le monitoring Markowitz
#
# A executer sur une machine avec crontab (ton PC local, un serveur, etc.)
# Usage: bash scripts/setup_cron.sh
#
# Prerequis:
#   - R installe avec les packages necessaires
#   - ~/.markowitz_smtp configure avec SMTP_USER et SMTP_PASS

PROJECT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
RSCRIPT="$(which Rscript)"

if [ -z "$RSCRIPT" ]; then
  echo "[ERREUR] Rscript non trouve dans le PATH"
  exit 1
fi

echo "=== Configuration des crons Markowitz ==="
echo "Projet: $PROJECT_DIR"
echo "Rscript: $RSCRIPT"
echo ""

# Sauvegarder le crontab existant
EXISTING=$(crontab -l 2>/dev/null || true)

# Supprimer les anciennes entrees Markowitz
CLEANED=$(echo "$EXISTING" | grep -v "portfolio_monitor.R" || true)

# Ajouter les nouvelles entrees
NEW_CRON="$CLEANED

# === Markowitz Portfolio Monitor ===
# Check alerte drawdown — tous les jours a 18h15 (lun-ven)
15 18 * * 1-5 cd $PROJECT_DIR && $RSCRIPT scripts/portfolio_monitor.R --alert >> logs/monitor.log 2>&1

# Recap hebdomadaire — chaque lundi a 18h30
30 18 * * 1 cd $PROJECT_DIR && $RSCRIPT scripts/portfolio_monitor.R --check >> logs/monitor.log 2>&1

# Rappel rebalancement — le 1er de chaque trimestre (jan, avr, jul, oct)
0 9 1 1,4,7,10 * cd $PROJECT_DIR && $RSCRIPT scripts/portfolio_monitor.R --rebalance >> logs/monitor.log 2>&1
"

echo "$NEW_CRON" | crontab -

echo "[OK] Crontab mis a jour. Entrees :"
crontab -l | grep -A1 "Markowitz"
echo ""
echo "Planning :"
echo "  - Alerte drawdown  : lun-ven 18h15"
echo "  - Recap hebdo      : lundi 18h30"
echo "  - Rappel rebalance : 1er jan/avr/jul/oct a 9h"
