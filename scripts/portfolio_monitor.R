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
send_email <- function(subject, body_html) {
  smtp_file <- path.expand("~/.markowitz_smtp")

  if (!file.exists(smtp_file)) {
    cat("[WARN] Fichier", smtp_file, "introuvable. Email NON envoye.\n")
    return(invisible(FALSE))
  }

  creds <- readLines(smtp_file, warn = FALSE)
  smtp_user <- sub("^SMTP_USER=", "", grep("^SMTP_USER=", creds, value = TRUE))
  smtp_pass <- sub("^SMTP_PASS=", "", grep("^SMTP_PASS=", creds, value = TRUE))

  if (length(smtp_user) == 0 || length(smtp_pass) == 0) {
    cat("[WARN] SMTP_USER ou SMTP_PASS manquant dans", smtp_file, "\n")
    return(invisible(FALSE))
  }

  # Ecrire le HTML dans un fichier temporaire pour eviter les problemes d'echappement
  tmp_html <- tempfile(tmpdir = "/tmp", fileext = ".html")
  writeLines(body_html, tmp_html)

  tmp_py <- tempfile(tmpdir = "/tmp", fileext = ".py")
  py_script <- sprintf('
import smtplib
from email.mime.text import MIMEText

with open("%s", "r") as f:
    html = f.read()

msg = MIMEText(html, "html")
msg["Subject"] = """%s"""
msg["From"] = "%s"
msg["To"] = "%s"

try:
    with smtplib.SMTP_SSL("smtp.gmail.com", 465) as s:
        s.login("%s", "%s")
        s.send_message(msg)
    print("[OK] Email envoye a %s")
except Exception as e:
    print(f"[ERROR] Echec envoi: {e}")
', tmp_html, subject, smtp_user, EMAIL_TO, smtp_user, smtp_pass, EMAIL_TO)

  writeLines(py_script, tmp_py)
  on.exit({ unlink(tmp_py); unlink(tmp_html) })
  system(paste("python3", tmp_py))
}

# ---------------------------------------------------------------------------
# HTML template
# ---------------------------------------------------------------------------
html_wrap <- function(title, icon, content, accent_color = "#2C3E50") {
  sprintf('<!DOCTYPE html>
<html><head><meta charset="utf-8"></head>
<body style="margin:0;padding:0;background:#f4f5f7;font-family:-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Helvetica,Arial,sans-serif;">
<table width="100%%" cellpadding="0" cellspacing="0" style="background:#f4f5f7;padding:24px 0;">
<tr><td align="center">
<table width="600" cellpadding="0" cellspacing="0" style="background:#ffffff;border-radius:12px;overflow:hidden;box-shadow:0 2px 12px rgba(0,0,0,0.08);">
  <tr><td style="background:linear-gradient(135deg,%s 0%%,#34495E 100%%);padding:28px 32px;">
    <span style="font-size:28px;">%s</span>
    <span style="color:#ffffff;font-size:22px;font-weight:700;margin-left:12px;vertical-align:middle;">%s</span>
    <br><span style="color:rgba(255,255,255,0.7);font-size:13px;">%s</span>
  </td></tr>
  <tr><td style="padding:28px 32px;">
    %s
  </td></tr>
  <tr><td style="padding:16px 32px;background:#f8f9fa;border-top:1px solid #e9ecef;">
    <span style="color:#95a5a6;font-size:11px;">Markowitz Portfolio Monitor &mdash; Automatique</span>
  </td></tr>
</table>
</td></tr></table>
</body></html>', accent_color, icon, title, format(Sys.Date(), "%%d/%%m/%%Y"), content)
}

html_table <- function(headers, rows, highlight_col = NULL, highlight_fn = NULL) {
  th_style <- "padding:10px 14px;text-align:left;font-size:12px;font-weight:600;color:#95a5a6;text-transform:uppercase;letter-spacing:0.5px;border-bottom:2px solid #e9ecef;"
  td_style <- "padding:10px 14px;font-size:14px;border-bottom:1px solid #f0f0f0;"

  header_html <- paste0("<tr>", paste0(sprintf('<th style="%s">%s</th>', th_style, headers), collapse = ""), "</tr>")

  row_htmls <- sapply(seq_len(nrow(rows)), function(i) {
    bg <- if (i %% 2 == 0) "background:#fafbfc;" else ""
    cells <- sapply(seq_len(ncol(rows)), function(j) {
      val <- as.character(rows[i, j])
      style <- paste0(td_style, bg)
      if (!is.null(highlight_col) && !is.null(highlight_fn) && j == highlight_col) {
        style <- paste0(style, highlight_fn(val))
      }
      sprintf('<td style="%s">%s</td>', style, val)
    })
    paste0("<tr>", paste(cells, collapse = ""), "</tr>")
  })

  sprintf('<table width="100%%" cellpadding="0" cellspacing="0" style="border-collapse:collapse;margin:16px 0;">%s%s</table>',
          header_html, paste(row_htmls, collapse = ""))
}

html_badge <- function(text, color = "#2C3E50") {
  sprintf('<span style="display:inline-block;padding:3px 10px;border-radius:12px;font-size:12px;font-weight:600;color:#fff;background:%s;">%s</span>', color, text)
}

html_kpi <- function(label, value, color = "#2C3E50") {
  sprintf('<div style="display:inline-block;text-align:center;padding:12px 20px;margin:4px;background:#f8f9fa;border-radius:8px;min-width:80px;">
    <div style="font-size:22px;font-weight:700;color:%s;">%s</div>
    <div style="font-size:11px;color:#95a5a6;margin-top:4px;">%s</div>
  </div>', color, value, label)
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

  # Construire le tableau
  tbl <- data.frame(
    Ticker = sapply(colnames(prices), label),
    Prix = sprintf("%.2f", last),
    Variation = format_pct(weekly_ret),
    stringsAsFactors = FALSE
  )

  color_fn <- function(val) {
    v <- as.numeric(gsub("[%+]", "", val))
    if (!is.finite(v)) return("")
    if (v < -3) return("color:#e74c3c;font-weight:700;")
    if (v < 0) return("color:#e67e22;")
    if (v > 3) return("color:#27ae60;font-weight:700;")
    "color:#27ae60;"
  }

  table_html <- html_table(c("Titre", "Prix", "Var. 1 sem."), tbl, highlight_col = 3, highlight_fn = color_fn)

  # Alertes
  drops <- weekly_ret[weekly_ret < -0.05]
  alert_html <- ""
  if (length(drops) > 0) {
    items <- paste(sapply(names(drops), function(tk) {
      sprintf('<li><strong>%s</strong> : %s</li>', label(tk), format_pct(drops[tk]))
    }), collapse = "")
    alert_html <- sprintf('
      <div style="margin:16px 0;padding:14px 18px;background:#fdf0ed;border-left:4px solid #e74c3c;border-radius:6px;">
        <strong style="color:#e74c3c;">Baisses significatives cette semaine</strong>
        <ul style="margin:8px 0 0 0;padding-left:20px;">%s</ul>
      </div>', items)
  }

  # KPIs
  avg_ret <- mean(weekly_ret, na.rm = TRUE)
  best_tk <- names(which.max(weekly_ret))
  worst_tk <- names(which.min(weekly_ret))
  kpis <- paste0(
    '<div style="text-align:center;margin:16px 0;">',
    html_kpi("Perf. moyenne", format_pct(avg_ret), if (avg_ret >= 0) "#27ae60" else "#e74c3c"),
    html_kpi("Meilleur", sprintf("%s %s", label(best_tk), format_pct(weekly_ret[best_tk])), "#27ae60"),
    html_kpi("Pire", sprintf("%s %s", label(worst_tk), format_pct(weekly_ret[worst_tk])), "#e74c3c"),
    '</div>')

  content <- paste0(kpis, table_html, alert_html)
  body_html <- html_wrap("Recap Hebdomadaire", "&#128200;", content)

  cat("[HTML] Rapport genere.\n")
  subject <- sprintf("[Markowitz] Recap hebdo — %s", format(Sys.Date(), "%d/%m/%Y"))
  send_email(subject, body_html)
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

  # Construire l'alerte HTML
  alert_items <- paste(sapply(names(alerts), function(tk) {
    sprintf('
      <div style="display:flex;justify-content:space-between;align-items:center;padding:12px 16px;margin:8px 0;background:#fdf0ed;border-radius:8px;border-left:4px solid #e74c3c;">
        <div>
          <strong style="font-size:15px;">%s</strong>
          <span style="color:#95a5a6;margin-left:8px;">%s</span>
        </div>
        <div style="text-align:right;">
          <span style="font-size:20px;font-weight:700;color:#e74c3c;">%s</span>
          <br><span style="font-size:12px;color:#95a5a6;">Prix: %.2f</span>
        </div>
      </div>', label(tk), tk, format_pct(alerts[tk]), last[tk])
  }), collapse = "")

  actions <- '
    <div style="margin:20px 0;padding:16px 20px;background:#eef6ff;border-radius:8px;border-left:4px solid #3498db;">
      <strong style="color:#2C3E50;">Actions recommandees</strong>
      <ol style="margin:8px 0 0 0;padding-left:20px;color:#34495e;">
        <li>Lancer l\'app : <code>Rscript scripts/run_local.R</code></li>
        <li>Verifier le drift dans l\'onglet Holdings</li>
        <li>Si drift &gt; 5%, rebalancer le portefeuille</li>
        <li>Si la baisse est fondamentale, envisager un remplacement</li>
      </ol>
    </div>'

  content <- paste0(
    sprintf('<p style="font-size:14px;color:#666;">Seuil : <strong>-%d%%</strong> sur 5 jours de bourse</p>', ALERT_DROP_PCT),
    alert_items, actions)

  body_html <- html_wrap("Alerte Drawdown", "&#128680;", content, "#c0392b")

  cat("[HTML] Alerte generee pour:", paste(sapply(names(alerts), label), collapse = ", "), "\n")
  subject <- sprintf("[ALERTE Markowitz] Chute detectee — %s",
                     paste(sapply(names(alerts), label), collapse = ", "))
  send_email(subject, body_html)
}

# ---------------------------------------------------------------------------
# Mode: --rebalance (rappel trimestriel)
# ---------------------------------------------------------------------------
run_rebalance <- function() {
  cat("=== Rappel de rebalancement trimestriel ===\n")

  steps <- '
    <div style="margin:16px 0;">
      <div style="display:flex;align-items:center;padding:12px 0;border-bottom:1px solid #f0f0f0;">
        <span style="display:inline-block;width:32px;height:32px;line-height:32px;text-align:center;border-radius:50%%;background:#2C3E50;color:#fff;font-weight:700;font-size:14px;margin-right:14px;">1</span>
        <span>Lancer l\'app : <code style="background:#f0f0f0;padding:2px 6px;border-radius:4px;">Rscript scripts/run_local.R</code></span>
      </div>
      <div style="display:flex;align-items:center;padding:12px 0;border-bottom:1px solid #f0f0f0;">
        <span style="display:inline-block;width:32px;height:32px;line-height:32px;text-align:center;border-radius:50%%;background:#2C3E50;color:#fff;font-weight:700;font-size:14px;margin-right:14px;">2</span>
        <span>Charger tes parametres sauvegardes (bouton <strong>Charger</strong>)</span>
      </div>
      <div style="display:flex;align-items:center;padding:12px 0;border-bottom:1px solid #f0f0f0;">
        <span style="display:inline-block;width:32px;height:32px;line-height:32px;text-align:center;border-radius:50%%;background:#2C3E50;color:#fff;font-weight:700;font-size:14px;margin-right:14px;">3</span>
        <span>Relancer l\'optimisation (prix et correlations ont change)</span>
      </div>
      <div style="display:flex;align-items:center;padding:12px 0;border-bottom:1px solid #f0f0f0;">
        <span style="display:inline-block;width:32px;height:32px;line-height:32px;text-align:center;border-radius:50%%;background:#2C3E50;color:#fff;font-weight:700;font-size:14px;margin-right:14px;">4</span>
        <span>Onglet <strong>Holdings</strong> : comparer poids actuels vs cibles</span>
      </div>
      <div style="display:flex;align-items:center;padding:12px 0;border-bottom:1px solid #f0f0f0;">
        <span style="display:inline-block;width:32px;height:32px;line-height:32px;text-align:center;border-radius:50%%;background:#2C3E50;color:#fff;font-weight:700;font-size:14px;margin-right:14px;">5</span>
        <span>Si drift &gt; 5%%, executer les ordres sur <strong>Swissquote</strong></span>
      </div>
      <div style="display:flex;align-items:center;padding:12px 0;">
        <span style="display:inline-block;width:32px;height:32px;line-height:32px;text-align:center;border-radius:50%%;background:#27ae60;color:#fff;font-weight:700;font-size:14px;margin-right:14px;">6</span>
        <span>Sauvegarder les nouveaux parametres</span>
      </div>
    </div>'

  next_date <- format(Sys.Date() + 30 * REBAL_MONTHS, "%d/%m/%Y")
  footer <- sprintf('<p style="margin-top:16px;padding:12px 16px;background:#f0faf0;border-radius:8px;font-size:13px;color:#27ae60;">Prochain rappel : <strong>%s</strong></p>', next_date)

  content <- paste0(
    '<p style="font-size:16px;color:#2C3E50;">Il est temps de re-optimiser ton portefeuille !</p>',
    steps, footer)

  body_html <- html_wrap("Rebalancement Trimestriel", "&#128197;", content, "#27ae60")

  cat("[HTML] Rappel rebalancement genere.\n")
  subject <- sprintf("[Markowitz] Rappel rebalancement — %s", format(Sys.Date(), "%d/%m/%Y"))
  send_email(subject, body_html)
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

  # ALERTE DRIFT — HTML
  # KPIs
  kpis <- paste0(
    '<div style="text-align:center;margin:16px 0;">',
    html_kpi("Valeur totale", sprintf("CHF %.0f", total_value), "#2C3E50"),
    html_kpi("Drift max", sprintf("%.1f%%", max_drift), "#e74c3c"),
    html_kpi("Seuil", sprintf("%d%%", DRIFT_THRESHOLD), "#95a5a6"),
    '</div>')

  # Note hors optimisation
  note_html <- ""
  if (nrow(holdings_no_target) > 0) {
    note_html <- sprintf('<p style="font-size:12px;color:#95a5a6;margin:8px 0;">Hors optimisation : %s</p>',
                          paste(holdings_no_target$ticker, collapse = ", "))
  }

  # Trier par drift absolu decroissant
  holdings_with_target <- holdings_with_target[order(-abs(holdings_with_target$drift)), ]

  # Construire le tableau
  tbl <- data.frame(
    Ticker = sapply(holdings_with_target$ticker, label),
    Actuel = sprintf("%.1f%%", holdings_with_target$weight_actual),
    Cible = sprintf("%.1f%%", holdings_with_target$target_adj),
    Drift = sprintf("%+.1f%%", holdings_with_target$drift),
    Action = sapply(seq_len(nrow(holdings_with_target)), function(i) {
      d <- holdings_with_target$drift[i]
      if (abs(d) >= 1) {
        act <- if (d > 0) "VENDRE" else "ACHETER"
        montant <- abs(d) / 100 * total_value
        sprintf("%s ~CHF %.0f", act, montant)
      } else { "OK" }
    }),
    stringsAsFactors = FALSE
  )

  color_fn <- function(val) {
    v <- as.numeric(gsub("[%+]", "", val))
    if (!is.finite(v)) return("")
    if (abs(v) >= DRIFT_THRESHOLD) return("color:#e74c3c;font-weight:700;")
    if (abs(v) >= 2) return("color:#e67e22;")
    ""
  }

  table_html <- html_table(c("Titre", "Actuel", "Cible", "Drift", "Action"), tbl, highlight_col = 4, highlight_fn = color_fn)

  actions <- '
    <div style="margin:20px 0;padding:16px 20px;background:#eef6ff;border-radius:8px;border-left:4px solid #3498db;">
      <strong style="color:#2C3E50;">Actions recommandees</strong>
      <ol style="margin:8px 0 0 0;padding-left:20px;color:#34495e;">
        <li>Lancer l\'app : <code style="background:#f0f0f0;padding:2px 6px;border-radius:4px;">Rscript scripts/run_local.R</code></li>
        <li>Relancer l\'optimisation avec les prix actuels</li>
        <li>Onglet Holdings &gt; verifier les trades proposes</li>
        <li>Passer les ordres sur Swissquote</li>
      </ol>
    </div>'

  content <- paste0(kpis, note_html, table_html, actions)
  body_html <- html_wrap("Alerte Drift", "&#9888;&#65039;", content, "#e67e22")

  cat("[HTML] Alerte drift generee.\n")
  subject <- sprintf("[ALERTE Markowitz] Drift detecte (%.1f%%) — %s",
                     max_drift, format(Sys.Date(), "%d/%m/%Y"))
  send_email(subject, body_html)
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
