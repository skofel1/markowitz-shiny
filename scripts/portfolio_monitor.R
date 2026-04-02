#!/usr/bin/env Rscript
# portfolio_monitor.R — Monitoring automatique du portefeuille
#
# Usage:
#   Rscript scripts/portfolio_monitor.R --check        # Check hebdomadaire
#   Rscript scripts/portfolio_monitor.R --alert        # Check alerte drawdown (quotidien)
#   Rscript scripts/portfolio_monitor.R --rebalance    # Rappel trimestriel
#   Rscript scripts/portfolio_monitor.R --drift        # Check drift vs poids cibles
#
# Envoi d'email via la commande `mail` du système ou via gmailr si configuré.

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------
EMAIL_TO       <- "simonkofel@gmail.com"
ALERT_DROP_PCT <- 10   # Alerte si un titre chute de X% sur 5 jours
REBAL_MONTHS   <- 3    # Rappel tous les X mois
DRIFT_THRESHOLD <- 5   # Alerte drift si un ticker depasse X% d'ecart vs cible
PROJECT_DIR <- tryCatch({
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    normalizePath(file.path(dirname(sub("^--file=", "", file_arg[1])), ".."), mustWork = FALSE)
  } else {
    "/home/endreas/markowitz-shiny"
  }
}, error = function(e) "/home/endreas/markowitz-shiny")
SETTINGS_FILE  <- file.path(PROJECT_DIR, "data", "user_settings.json")

TICKERS <- c("GOOGL", "V", "AAPL", "MSFT", "KO", "ABBN.SW", "UBSG.SW",
             "CFR.SW", "NOVN.SW", "JNJ", "MCD", "XOM")

TICKER_LABELS <- c(
  "GOOGL" = "Alphabet", "V" = "Visa", "AAPL" = "Apple", "MSFT" = "Microsoft",
  "KO" = "Coca-Cola", "ABBN.SW" = "ABB", "UBSG.SW" = "UBS",
  "CFR.SW" = "Richemont", "NOVN.SW" = "Novartis", "JNJ" = "J&J",
  "MCD" = "McDonald's", "XOM" = "ExxonMobil"
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
  library(jsonlite)
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

  tmp <- tempfile(tmpdir = "/tmp", fileext = ".py")
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
# Mode: --drift (check drift vs poids cibles)
# ---------------------------------------------------------------------------
run_drift <- function() {
  cat("=== Check drift portefeuille ===\n")

  # Lire les settings
  if (!file.exists(SETTINGS_FILE)) {
    cat("[ERROR] Fichier settings introuvable:", SETTINGS_FILE, "\n")
    return(invisible())
  }

  settings <- fromJSON(SETTINGS_FILE)

  # Parser les holdings (format "TICKER:QTY:PRICE\nTICKER:QTY:PRICE")
  if (is.null(settings$holdings) || !nzchar(settings$holdings)) {
    cat("[WARN] Aucun holding dans user_settings.json\n")
    return(invisible())
  }

  holdings_raw <- strsplit(trimws(settings$holdings), "\n")[[1]]
  holdings <- data.frame(
    ticker = character(0), qty = numeric(0), cost = numeric(0),
    stringsAsFactors = FALSE
  )
  for (line in holdings_raw) {
    parts <- strsplit(trimws(line), ":")[[1]]
    if (length(parts) >= 2) {
      holdings <- rbind(holdings, data.frame(
        ticker = parts[1],
        qty = as.numeric(parts[2]),
        cost = if (length(parts) >= 3) as.numeric(parts[3]) else NA,
        stringsAsFactors = FALSE
      ))
    }
  }

  if (nrow(holdings) == 0) {
    cat("[WARN] Aucun holding parsable.\n")
    return(invisible())
  }

  # Recuperer les prix actuels
  prices <- get_prices(holdings$ticker, days = 5)
  if (is.null(prices)) {
    cat("[ERROR] Aucun prix recupere.\n")
    return(invisible())
  }

  last_prices <- as.numeric(tail(prices, 1))
  names(last_prices) <- colnames(prices)

  # Calculer la valeur de marche de chaque position
  holdings$price <- last_prices[holdings$ticker]
  holdings$value <- holdings$qty * holdings$price
  total_value <- sum(holdings$value, na.rm = TRUE)
  holdings$weight_actual <- holdings$value / total_value * 100

  # Lire les poids cibles depuis la derniere optimisation
  # On utilise wmin/wmax comme reference, mais les poids cibles reels
  # viennent de l'optimisation. Comme on n'a pas les poids exacts sauvegardes,
  # on calcule l'equiponderation corrigee comme approximation.
  # Pour les poids cibles exacts, il faudrait les sauvegarder dans settings.
  # En attendant, on detecte le drift entre positions (ecart max - min).

  # Lire les poids cibles
  if (!is.null(settings$target_weights)) {
    tw <- data.frame(
      ticker = names(settings$target_weights),
      target = as.numeric(unlist(settings$target_weights)),
      stringsAsFactors = FALSE
    )
  } else {
    # Fallback: equi-poids
    n <- nrow(holdings)
    tw <- data.frame(
      ticker = holdings$ticker,
      target = rep(100 / n, n),
      stringsAsFactors = FALSE
    )
    cat("[INFO] Pas de poids cibles sauvegardes, utilisation equi-poids comme reference.\n")
  }

  # Merge — les tickers sans poids cible (ex: GOOGL hors optimisation) sont ignores
  holdings <- merge(holdings, tw, by = "ticker", all.x = TRUE)

  # Exclure les tickers sans poids cible
  holdings_with_target <- holdings[!is.na(holdings$target), ]
  holdings_no_target <- holdings[is.na(holdings$target), ]

  # Recalculer les poids cibles au prorata du poids reel des tickers hors optimisation
  # Ex: si GOOGL prend 25%, les poids cibles (qui totalisent 100%) doivent etre ramenes a 75%
  weight_outside <- sum(holdings_no_target$weight_actual, na.rm = TRUE)
  scale_factor <- (100 - weight_outside) / 100
  holdings_with_target$target_adj <- holdings_with_target$target * scale_factor
  holdings_with_target$drift <- holdings_with_target$weight_actual - holdings_with_target$target_adj

  # Verifier si un ticker depasse le seuil
  max_drift <- max(abs(holdings_with_target$drift), na.rm = TRUE)

  cat(sprintf("  Valeur totale portefeuille: CHF %.2f\n", total_value))
  if (nrow(holdings_no_target) > 0) {
    cat(sprintf("  Tickers hors optimisation: %s\n", paste(holdings_no_target$ticker, collapse = ", ")))
  }
  cat(sprintf("  Drift max: %.1f%% (seuil: %d%%)\n", max_drift, DRIFT_THRESHOLD))

  if (max_drift < DRIFT_THRESHOLD) {
    cat("[OK] Drift sous controle — pas de rebalancement necessaire.\n")
    return(invisible())
  }

  # ALERTE DRIFT
  lines <- c(
    "*** ALERTE MARKOWITZ — DRIFT DETECTE ***",
    paste0("Date: ", Sys.Date()),
    paste0("Seuil: ", DRIFT_THRESHOLD, "% | Drift max: ", sprintf("%.1f%%", max_drift)),
    sprintf("Valeur totale: CHF %.0f", total_value),
    "",
    sprintf("%-12s %8s %8s %8s %10s", "Ticker", "Actuel%", "Cible%", "Drift%", "Action"),
    paste(rep("-", 52), collapse = "")
  )

  if (nrow(holdings_no_target) > 0) {
    lines <- c(lines, sprintf("  (Hors optimisation: %s)",
                                paste(holdings_no_target$ticker, collapse = ", ")))
  }

  # Trier par drift absolu decroissant
  holdings_with_target <- holdings_with_target[order(-abs(holdings_with_target$drift)), ]

  for (i in seq_len(nrow(holdings_with_target))) {
    h <- holdings_with_target[i, ]
    drift_val <- h$drift
    if (abs(drift_val) >= 1) {
      action <- if (drift_val > 0) "VENDRE" else "ACHETER"
      montant <- abs(drift_val) / 100 * total_value
      action_str <- sprintf("%s ~CHF%.0f", action, montant)
    } else {
      action_str <- "OK"
    }
    flag <- if (abs(drift_val) >= DRIFT_THRESHOLD) " ***" else ""
    lines <- c(lines, sprintf("%-12s %7.1f%% %7.1f%% %+7.1f%% %10s%s",
                                label(h$ticker), h$weight_actual, h$target_adj,
                                drift_val, action_str, flag))
  }

  lines <- c(lines, "",
    "ACTION RECOMMANDEE :",
    "  1. Lancer l'app Markowitz (Rscript scripts/run_local.R)",
    "  2. Relancer l'optimisation avec les prix actuels",
    "  3. Onglet Holdings > verifier les trades proposes",
    "  4. Passer les ordres sur Swissquote",
    "",
    "Genere automatiquement par Markowitz Portfolio Monitor")

  body <- paste(lines, collapse = "\n")
  cat(body, "\n\n")

  subject <- sprintf("[ALERTE Markowitz] Drift detecte (%.1f%%) — %s",
                     max_drift, format(Sys.Date(), "%d/%m/%Y"))
  send_email(subject, body)
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  cat("Usage: Rscript scripts/portfolio_monitor.R [--check|--alert|--rebalance|--drift]\n")
  cat("  --check      Recap hebdomadaire (lundi)\n")
  cat("  --alert      Alerte drawdown (quotidien)\n")
  cat("  --rebalance  Rappel trimestriel\n")
  cat("  --drift      Check drift vs poids cibles (hebdo)\n")
  quit(status = 1)
}

switch(args[1],
  "--check"     = run_check(),
  "--alert"     = run_alert(),
  "--rebalance" = run_rebalance(),
  "--drift"     = run_drift(),
  {
    cat("Option inconnue:", args[1], "\n")
    quit(status = 1)
  }
)
