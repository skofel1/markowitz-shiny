#!/usr/bin/env Rscript
# portfolio_monitor.R — Monitoring automatique du portefeuille
#
# Usage:
#   Rscript scripts/portfolio_monitor.R --check        # Check hebdomadaire
#   Rscript scripts/portfolio_monitor.R --alert        # Check alerte drawdown (quotidien)
#   Rscript scripts/portfolio_monitor.R --rebalance    # Rappel trimestriel
#
# Envoi d'email via la commande `mail` du système ou via gmailr si configuré.

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------
EMAIL_TO       <- "simonkofel@gmail.com"
ALERT_DROP_PCT <- 10   # Alerte si un titre chute de X% sur 5 jours
REBAL_MONTHS   <- 3    # Rappel tous les X mois

TICKERS <- c("V", "AAPL", "MSFT", "KO", "ABBN.SW", "NESN.SW", "NOVN.SW",
             "JNJ", "MCD", "XOM")

TICKER_LABELS <- c(
  "V" = "Visa", "AAPL" = "Apple", "MSFT" = "Microsoft",
  "KO" = "Coca-Cola", "ABBN.SW" = "ABB", "NESN.SW" = "Nestle",
  "NOVN.SW" = "Novartis", "JNJ" = "J&J", "MCD" = "McDonald's",
  "XOM" = "ExxonMobil"
)

# ---------------------------------------------------------------------------
# Dependencies
# ---------------------------------------------------------------------------
user_lib <- Sys.getenv("R_LIBS_USER")
if (nzchar(user_lib) && dir.exists(path.expand(user_lib))) {
  .libPaths(c(path.expand(user_lib), .libPaths()))
}

suppressPackageStartupMessages({
  library(quantmod)
  library(xts)
})

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
send_email <- function(subject, body) {
  # Envoie un email via Gmail SMTP (Python3 smtplib)
  # Necessite le fichier ~/.markowitz_smtp avec:
  #   SMTP_USER=ton.email@gmail.com
  #   SMTP_PASS=ton_app_password
  #
  # Pour creer un App Password Gmail:
  #   1. https://myaccount.google.com/security
  #   2. Verification en 2 etapes (activer si pas fait)
  #   3. Mots de passe d'application -> Generer
  #   4. Copier le mot de passe dans ~/.markowitz_smtp

  smtp_file <- path.expand("~/.markowitz_smtp")

  if (!file.exists(smtp_file)) {
    cat("[WARN] Fichier", smtp_file, "introuvable.\n")
    cat("[WARN] Email NON envoye. Contenu qui aurait ete envoye:\n")
    cat("  Sujet:", subject, "\n")
    cat(body, "\n")
    return(invisible(FALSE))
  }

  # Lire les credentials
  creds <- readLines(smtp_file, warn = FALSE)
  smtp_user <- sub("^SMTP_USER=", "", grep("^SMTP_USER=", creds, value = TRUE))
  smtp_pass <- sub("^SMTP_PASS=", "", grep("^SMTP_PASS=", creds, value = TRUE))

  if (length(smtp_user) == 0 || length(smtp_pass) == 0) {
    cat("[WARN] SMTP_USER ou SMTP_PASS manquant dans", smtp_file, "\n")
    return(invisible(FALSE))
  }

  # Echapper les caracteres speciaux pour Python
  body_escaped <- gsub("\\\\", "\\\\\\\\", body)
  body_escaped <- gsub('"', '\\\\"', body_escaped)
  subject_escaped <- gsub('"', '\\\\"', subject)

  py_script <- sprintf('
import smtplib
from email.mime.text import MIMEText

msg = MIMEText("""%s""")
msg["Subject"] = "%s"
msg["From"] = "%s"
msg["To"] = "%s"

try:
    with smtplib.SMTP_SSL("smtp.gmail.com", 465) as s:
        s.login("%s", "%s")
        s.send_message(msg)
    print("[OK] Email envoye a %s")
except Exception as e:
    print(f"[ERROR] Echec envoi: {e}")
', body_escaped, subject_escaped, smtp_user, EMAIL_TO,
  smtp_user, smtp_pass, EMAIL_TO)

  tmp <- tempfile(fileext = ".py")
  writeLines(py_script, tmp)
  on.exit(unlink(tmp))

  system(paste("python3", tmp))
}

get_prices <- function(tickers, days = 30) {
  prices <- list()
  for (tk in tickers) {
    tryCatch({
      data <- getSymbols(tk, src = "yahoo", auto.assign = FALSE,
                         from = Sys.Date() - days, to = Sys.Date())
      prices[[tk]] <- Ad(data)
    }, error = function(e) {
      cat("[WARN] Impossible de charger", tk, ":", e$message, "\n")
    })
  }

  if (length(prices) == 0) return(NULL)
  result <- do.call(merge, prices)
  # Clean column names (remove .Adjusted suffix from Yahoo)
  colnames(result) <- sub("\\.Adjusted$", "", colnames(result))
  result
}

format_pct <- function(x) sprintf("%+.1f%%", x * 100)

label <- function(tk) {
  if (tk %in% names(TICKER_LABELS)) TICKER_LABELS[[tk]] else tk
}

# ---------------------------------------------------------------------------
# Mode: --check (recap hebdomadaire)
# ---------------------------------------------------------------------------
run_check <- function() {
  cat("=== Check hebdomadaire du portefeuille ===\n")

  prices <- get_prices(TICKERS, days = 10)
  if (is.null(prices)) {
    cat("[ERROR] Aucun prix recupere.\n")
    return(invisible())
  }

  last <- as.numeric(tail(prices, 1))
  prev <- as.numeric(tail(prices, 2)[1, ])
  names(last) <- names(prev) <- colnames(prices)

  weekly_ret <- (last - prev) / prev

  # Build email body
  lines <- c(
    "MARKOWITZ PORTFOLIO — Recap Hebdomadaire",
    paste0("Date: ", Sys.Date()),
    paste(rep("-", 55), collapse = ""),
    "",
    sprintf("%-12s %10s %10s", "Ticker", "Prix", "Var 1 sem"),
    paste(rep("-", 35), collapse = "")
  )

  for (tk in colnames(prices)) {
    lines <- c(lines, sprintf("%-12s %10.2f %10s",
                               label(tk), last[tk], format_pct(weekly_ret[tk])))
  }

  # Alertes
  drops <- weekly_ret[weekly_ret < -0.05]
  if (length(drops) > 0) {
    lines <- c(lines, "", "*** ATTENTION — Baisses significatives cette semaine ***")
    for (tk in names(drops)) {
      lines <- c(lines, sprintf("  %s : %s", label(tk), format_pct(drops[tk])))
    }
  }

  lines <- c(lines, "", paste(rep("-", 55), collapse = ""),
             "Genere automatiquement par Markowitz Portfolio Monitor")

  body <- paste(lines, collapse = "\n")
  cat(body, "\n\n")

  subject <- sprintf("[Markowitz] Recap hebdo — %s", format(Sys.Date(), "%d/%m/%Y"))
  send_email(subject, body)
}

# ---------------------------------------------------------------------------
# Mode: --alert (alerte drawdown quotidienne)
# ---------------------------------------------------------------------------
run_alert <- function() {
  cat("=== Check alerte drawdown ===\n")

  prices <- get_prices(TICKERS, days = 10)
  if (is.null(prices)) {
    cat("[ERROR] Aucun prix recupere.\n")
    return(invisible())
  }

  n <- nrow(prices)
  if (n < 5) {
    cat("[WARN] Pas assez de jours de donnees (", n, ").\n")
    return(invisible())
  }

  last    <- as.numeric(tail(prices, 1))
  five_ago <- as.numeric(prices[n - 4, ])
  names(last) <- names(five_ago) <- colnames(prices)

  ret_5d <- (last - five_ago) / five_ago
  threshold <- -ALERT_DROP_PCT / 100

  alerts <- ret_5d[ret_5d < threshold]

  if (length(alerts) == 0) {
    cat("[OK] Aucune alerte — tous les titres sont stables (seuil:", ALERT_DROP_PCT, "% sur 5j)\n")
    return(invisible())
  }

  # ALERTE !
  lines <- c(
    "*** ALERTE MARKOWITZ — CHUTE SIGNIFICATIVE ***",
    paste0("Date: ", Sys.Date()),
    paste0("Seuil: -", ALERT_DROP_PCT, "% sur 5 jours de bourse"),
    "",
    "Titres en alerte :"
  )

  for (tk in names(alerts)) {
    lines <- c(lines, sprintf(
      "  %s (%s) : %s sur 5 jours  (prix: %.2f)",
      label(tk), tk, format_pct(alerts[tk]), last[tk]
    ))
  }

  lines <- c(lines, "",
    "ACTION RECOMMANDEE :",
    "  1. Lancer l'app Markowitz (Rscript scripts/run_local.R)",
    "  2. Verifier le drift dans l'onglet Holdings",
    "  3. Si drift > 5%, rebalancer le portefeuille",
    "  4. Si la baisse est fondamentale, envisager un remplacement",
    "",
    "Genere automatiquement par Markowitz Portfolio Monitor")

  body <- paste(lines, collapse = "\n")
  cat(body, "\n\n")

  subject <- sprintf("[ALERTE Markowitz] Chute detectee — %s",
                     paste(sapply(names(alerts), label), collapse = ", "))
  send_email(subject, body)
}

# ---------------------------------------------------------------------------
# Mode: --rebalance (rappel trimestriel)
# ---------------------------------------------------------------------------
run_rebalance <- function() {
  cat("=== Rappel de rebalancement trimestriel ===\n")

  lines <- c(
    "MARKOWITZ PORTFOLIO — Rappel de Rebalancement",
    paste0("Date: ", Sys.Date()),
    paste(rep("-", 55), collapse = ""),
    "",
    "Il est temps de re-optimiser ton portefeuille !",
    "",
    "Etapes :",
    "  1. Lancer l'app : Rscript scripts/run_local.R",
    "  2. Charger tes parametres sauvegardes (bouton Charger)",
    "  3. Relancer l'optimisation (les prix/correlations ont change)",
    "  4. Comparer les nouveaux poids avec tes positions actuelles",
    "  5. Si drift > 5%, executer les ordres de rebalancement",
    "  6. Sauvegarder les nouveaux parametres",
    "",
    sprintf("Prochain rappel : %s", format(Sys.Date() + 30 * REBAL_MONTHS, "%d/%m/%Y")),
    "",
    paste(rep("-", 55), collapse = ""),
    "Genere automatiquement par Markowitz Portfolio Monitor"
  )

  body <- paste(lines, collapse = "\n")
  cat(body, "\n\n")

  subject <- sprintf("[Markowitz] Rappel rebalancement — %s", format(Sys.Date(), "%d/%m/%Y"))
  send_email(subject, body)
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  cat("Usage: Rscript scripts/portfolio_monitor.R [--check|--alert|--rebalance]\n")
  cat("  --check      Recap hebdomadaire (lundi)\n")
  cat("  --alert      Alerte drawdown (quotidien)\n")
  cat("  --rebalance  Rappel trimestriel\n")
  quit(status = 1)
}

switch(args[1],
  "--check"     = run_check(),
  "--alert"     = run_alert(),
  "--rebalance" = run_rebalance(),
  {
    cat("Option inconnue:", args[1], "\n")
    quit(status = 1)
  }
)
