#!/usr/bin/env python3
"""
scheduler.py — Scheduler leger pour le monitoring Markowitz
Remplace crontab quand celui-ci n'est pas disponible.

Usage:
    python3 scripts/scheduler.py          # Lance en foreground
    python3 scripts/scheduler.py &        # Lance en background
    nohup python3 scripts/scheduler.py &  # Persiste apres deconnexion

Planning:
    - Alerte drawdown  : lun-ven a 18h15
    - Recap hebdo      : lundi a 18h30
    - Rappel rebalance : 1er jan/avr/jul/oct a 9h00
    - Claude Code news : jeudi a 9h00
"""

import subprocess
import time
import os
import sys
from datetime import datetime, timedelta
from pathlib import Path

PROJECT_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
RSCRIPT = "Rscript"
LOG_FILE = os.path.join(PROJECT_DIR, "logs", "monitor.log")
CLAUDE_NEWS_SCRIPT = str(Path.home() / ".claude" / "skills" / "claude-news" / "scripts" / "claude_news.py")

# S'assurer que le dossier logs existe
os.makedirs(os.path.join(PROJECT_DIR, "logs"), exist_ok=True)


def log(msg):
    ts = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    line = f"[{ts}] {msg}"
    print(line, flush=True)
    try:
        with open(LOG_FILE, "a") as f:
            f.write(line + "\n")
    except Exception:
        pass


def run_monitor(mode):
    """Execute portfolio_monitor.R avec le mode donne."""
    cmd = [RSCRIPT, os.path.join(PROJECT_DIR, "scripts", "portfolio_monitor.R"), f"--{mode}"]
    log(f"Lancement: {' '.join(cmd)}")
    try:
        result = subprocess.run(
            cmd, capture_output=True, text=True, timeout=300, cwd=PROJECT_DIR
        )
        if result.stdout:
            for line in result.stdout.strip().split("\n"):
                log(f"  {line}")
        if result.returncode != 0 and result.stderr:
            for line in result.stderr.strip().split("\n")[-5:]:
                log(f"  [ERR] {line}")
    except subprocess.TimeoutExpired:
        log(f"  [TIMEOUT] {mode} a depasse 5 minutes")
    except Exception as e:
        log(f"  [ERROR] {e}")


def run_claude_news():
    """Execute le script claude_news.py --send."""
    cmd = [sys.executable, CLAUDE_NEWS_SCRIPT, "--send"]
    log(f"Lancement: {' '.join(cmd)}")
    try:
        result = subprocess.run(
            cmd, capture_output=True, text=True, timeout=120,
        )
        if result.stdout:
            for line in result.stdout.strip().split("\n"):
                log(f"  {line}")
        if result.returncode != 0 and result.stderr:
            for line in result.stderr.strip().split("\n")[-5:]:
                log(f"  [ERR] {line}")
    except subprocess.TimeoutExpired:
        log("  [TIMEOUT] claude-news a depasse 2 minutes")
    except Exception as e:
        log(f"  [ERROR] claude-news: {e}")


def main():
    log("=== Markowitz Scheduler demarre ===")
    log(f"  Projet    : {PROJECT_DIR}")
    log(f"  Log       : {LOG_FILE}")
    log(f"  Planning  :")
    log(f"    - Alerte drawdown  : lun-ven 18h15")
    log(f"    - Recap hebdo      : lundi 18h30")
    log(f"    - Rappel rebalance : 1er jan/avr/jul/oct 9h00")
    log(f"    - Claude Code news : jeudi 9h00")
    log("")

    # Tracking: eviter de relancer la meme tache dans la meme minute
    last_run = {}

    while True:
        now = datetime.now()
        weekday = now.weekday()  # 0=lundi, 6=dimanche
        hour = now.hour
        minute = now.minute
        day = now.day
        month = now.month

        # Cle unique pour cette minute
        time_key = now.strftime("%Y%m%d%H%M")

        # Alerte drawdown: lun-ven a 18h15
        if weekday <= 4 and hour == 18 and minute == 15:
            key = f"alert-{time_key}"
            if key not in last_run:
                last_run[key] = True
                run_monitor("alert")

        # Recap hebdo: lundi a 18h30
        if weekday == 0 and hour == 18 and minute == 30:
            key = f"check-{time_key}"
            if key not in last_run:
                last_run[key] = True
                run_monitor("check")

        # Rappel rebalancement: 1er du trimestre a 9h00
        if day == 1 and month in (1, 4, 7, 10) and hour == 9 and minute == 0:
            key = f"rebalance-{time_key}"
            if key not in last_run:
                last_run[key] = True
                run_monitor("rebalance")

        # Claude Code news: jeudi a 9h00
        if weekday == 3 and hour == 9 and minute == 0:
            key = f"claude-news-{time_key}"
            if key not in last_run:
                last_run[key] = True
                run_claude_news()

        # Nettoyer les vieilles cles (garder 24h)
        if len(last_run) > 1000:
            last_run.clear()

        # Dormir 30 secondes (precision suffisante)
        time.sleep(30)


if __name__ == "__main__":
    # Si argument --test, lancer un test immediat
    if len(sys.argv) > 1 and sys.argv[1] == "--test":
        log("=== TEST: lancement immediat des 3 modes ===")
        run_monitor("alert")
        run_monitor("check")
        log("=== TEST termine ===")
    else:
        main()
