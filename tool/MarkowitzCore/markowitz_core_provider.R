library(modulr)

"tool/MarkowitzCore/markowitz_core_provider" %provides% {

  # ============================================================================
  # DÉPENDANCES
  # ============================================================================
  library(quantmod)
  library(PerformanceAnalytics)
  library(quadprog)
  library(Matrix)
  library(xts)
  library(jsonlite)

  # ============================================================================
  # LOGGING (V11)
  # ============================================================================

  # Source the logger module
  logger_path <- file.path(dirname(sys.frame(1)$ofile %||% "."), "logger.R")
  if (file.exists(logger_path)) {
    Logger <- source(logger_path)$value
  } else {
    # Fallback: simple logging
    Logger <- list(
      log_info = function(msg, ...) cat("[INFO]", msg, "\n"),
      log_warn = function(msg, ...) cat("[WARN]", msg, "\n"),
      log_error = function(msg, ...) cat("[ERROR]", msg, "\n"),
      log_debug = function(msg, ...) invisible(NULL),
      set_log_level = function(level) invisible(NULL),
      set_log_file = function(path) invisible(NULL)
    )
  }

  # Initialize logging
  log_dir <- file.path(getwd(), "logs")
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)
  Logger$set_log_file(file.path(log_dir, paste0("markowitz_", format(Sys.Date(), "%Y%m%d"), ".log")))
  Logger$set_log_level(Sys.getenv("SHINY_LOG_LEVEL", "INFO"))

  # ============================================================================
  # PROVIDER
  # ============================================================================
  function() {
    
    # --------------------------------------------------------------------------
    # parse_tickers : découpe une chaîne de tickers séparés par espaces/virgules
    # --------------------------------------------------------------------------
    parse_tickers <- function(x) {
      x <- paste(x, collapse = " ")
      t <- unlist(strsplit(x, "[,;[:space:]]+"))
      t <- t[nzchar(t)]
      unique(toupper(t))
    }
    
    # --------------------------------------------------------------------------
    # normalize_tickers : applique des alias (notamment CH -> .SW)
    # --------------------------------------------------------------------------
    normalize_tickers <- function(x, use_aliases = TRUE) {
      tks <- parse_tickers(x)
      
      if (isTRUE(use_aliases)) {
        map <- c(
          "ABBN"      = "ABBN.SW",
          "ABB"       = "ABBN.SW",
          "NOVARTIS"  = "NOVN.SW",
          "NOVN"      = "NOVN.SW",
          "UBS"       = "UBSG.SW",
          "UBSN"      = "UBSG.SW",
          "UBSG"      = "UBSG.SW",
          "NESTLE"    = "NESN.SW",
          "NESN"      = "NESN.SW",
          "ROCHE"     = "ROG.SW",
          "ROG"       = "ROG.SW",
          "RICHEMONT" = "CFR.SW",
          "CFR"       = "CFR.SW",
          "SWISSCOM"  = "SCMN.SW",
          "SCMN"      = "SCMN.SW",
          "ZURICH"    = "ZURN.SW",
          "ZURN"      = "ZURN.SW",
          "SWISSRE"   = "SREN.SW",
          "SREN"      = "SREN.SW",
          "LONZA"     = "LONN.SW",
          "LONN"      = "LONN.SW"
        )
        
        tks <- ifelse(tks %in% names(map), unname(map[tks]), tks)
        tks <- unique(toupper(tks))
      }
      
      tks
    }
    
    # --------------------------------------------------------------------------
    # get_prices_yahoo : télécharge les prix ajustés depuis Yahoo Finance
    # Retourne un xts avec une colonne par ticker valide.
    # --------------------------------------------------------------------------
    get_prices_yahoo <- function(tickers, from, to) {
      Logger$log_info("Fetching prices from Yahoo Finance",
                      tickers = paste(tickers, collapse = ","),
                      from = as.character(from),
                      to = as.character(to))

      lst <- lapply(tickers, function(tk) {
        tryCatch({
          x <- suppressWarnings(
            getSymbols(tk, src = "yahoo", from = from, to = to, auto.assign = FALSE)
          )
          Logger$log_debug("Successfully fetched ticker", ticker = tk)
          Ad(x)
        }, error = function(e) {
          Logger$log_warn("Failed to fetch ticker", ticker = tk, error = e$message)
          NULL
        })
      })
      names(lst) <- tickers

      lst <- lst[!vapply(lst, is.null, logical(1))]
      if (length(lst) < 2) {
        Logger$log_error("Not enough valid tickers", valid_count = length(lst))
        stop("Min 2 tickers valides requis (Yahoo).")
      }

      px <- do.call(merge, c(lst, all = FALSE))
      colnames(px) <- names(lst)

      Logger$log_info("Price fetch complete",
                      tickers_fetched = length(lst),
                      observations = nrow(px))
      px
    }
    
    # --------------------------------------------------------------------------
    # get_benchmark_stats : télécharge et calcule μ/σ pour un benchmark
    # --------------------------------------------------------------------------
    get_benchmark_stats <- function(symbol, from, to, base_ccy = "CHF", freq = 252) {
      
      result <- tryCatch({
        # Télécharger les prix
        px <- suppressWarnings(
          getSymbols(symbol, src = "yahoo", from = from, to = to, auto.assign = FALSE)
        )
        px <- Ad(px)
        
        # Détecter la devise du benchmark
        ccy <- tryCatch({
          q <- getQuote(symbol, src = "yahoo", what = yahooQF("Currency"))
          toupper(as.character(q[1, "Currency"]))
        }, error = function(e) "USD")
        
        if (is.na(ccy) || ccy == "") ccy <- "USD"
        
        # Conversion FX si nécessaire
        if (ccy != base_ccy) {
          fx_sym <- paste0(ccy, base_ccy, "=X")
          fx <- tryCatch(
            getSymbols(fx_sym, src = "yahoo", from = from, to = to, auto.assign = FALSE),
            error = function(e) {
              # Essayer l'inverse
              fx_sym2 <- paste0(base_ccy, ccy, "=X")
              fx2 <- getSymbols(fx_sym2, src = "yahoo", from = from, to = to, auto.assign = FALSE)
              1 / Ad(fx2)
            }
          )
          fx <- if (inherits(fx, "xts")) Ad(fx) else fx
          
          # Merger et convertir
          m <- merge(px, fx, all = FALSE)
          px <- m[, 1] * m[, 2]
        }
        
        # Calculer rendements log
        rets <- diff(log(px))
        rets <- na.omit(rets)
        
        if (nrow(rets) < 60) return(NULL)
        
        # Stats annualisées
        mu <- as.numeric(mean(rets, na.rm = TRUE) * freq)
        sigma <- as.numeric(sd(rets, na.rm = TRUE) * sqrt(freq))
        
        list(
          symbol = symbol,
          mu = mu,
          vol = sigma,
          currency = ccy,
          n_obs = nrow(rets)
        )
        
      }, error = function(e) NULL)
      
      result
    }
    
    # --------------------------------------------------------------------------
    # get_benchmarks : récupère plusieurs benchmarks standards
    # --------------------------------------------------------------------------
    get_benchmarks <- function(from, to, base_ccy = "CHF") {
      
      benchmarks <- list(
        "S&P 500" = "^GSPC",
        "MSCI World" = "URTH",
        "Nasdaq 100" = "^NDX"
      )
      
      results <- list()
      
      for (name in names(benchmarks)) {
        sym <- benchmarks[[name]]
        stats <- get_benchmark_stats(sym, from, to, base_ccy)
        if (!is.null(stats)) {
          stats$name <- name
          results[[name]] <- stats
        }
      }
      
      results
    }
    
    # --------------------------------------------------------------------------
    # V5: get_ticker_currency_yahoo
    # Récupère la devise "Currency" par ticker via getQuote().
    # --------------------------------------------------------------------------
    get_ticker_currency_yahoo <- function(tickers) {
      out <- stats::setNames(rep(NA_character_, length(tickers)), tickers)
      
      q <- tryCatch(
        getQuote(tickers, src = "yahoo", what = yahooQF("Currency")),
        error = function(e) NULL
      )
      if (is.null(q) || nrow(q) == 0) return(out)
      
      # Colonne Currency (ou fallback 1ère colonne)
      if ("Currency" %in% colnames(q)) {
        cur <- as.character(q[, "Currency"])
      } else {
        cur <- as.character(q[, 1])
      }
      
      sym <- rownames(q)
      names(cur) <- sym
      
      # Normalisation légère (cas fréquents)
      cur <- toupper(cur)
      cur[cur %in% c("", "NA")] <- NA_character_
      
      # Remap possible: GBp/GBX (pence) -> GBP (attention: prix peuvent être en pence)
      # Ici on ne corrige PAS le /100 automatiquement (à ajouter si tu utilises des actions UK).
      cur[cur %in% c("GBP", "GBX", "GBP.")] <- "GBP"
      cur[cur %in% c("GBp", "GBPp")] <- "GBP"
      
      out[names(cur)] <- cur
      out
    }

    # --------------------------------------------------------------------------
    # V12: get_ticker_sectors_yahoo
    # Récupère le secteur de chaque ticker via Yahoo Finance API
    # --------------------------------------------------------------------------
    get_ticker_sectors_yahoo <- function(tickers) {
      Logger$log_info("Fetching sector information", tickers = paste(tickers, collapse = ","))

      out <- stats::setNames(rep("Unknown", length(tickers)), tickers)

      for (tk in tickers) {
        tryCatch({
          # Utiliser quantmod::getQuote avec le champ approprié
          # Note: Yahoo ne fournit pas toujours le secteur via getQuote standard
          # On va utiliser une approche alternative avec l'URL directe
          url <- paste0("https://query1.finance.yahoo.com/v10/finance/quoteSummary/",
                        tk, "?modules=assetProfile")

          resp <- tryCatch({
            jsonlite::fromJSON(url)
          }, error = function(e) NULL)

          if (!is.null(resp) && !is.null(resp$quoteSummary$result[[1]]$assetProfile$sector)) {
            sector <- resp$quoteSummary$result[[1]]$assetProfile$sector
            out[tk] <- sector
            Logger$log_debug("Sector found", ticker = tk, sector = sector)
          }
        }, error = function(e) {
          Logger$log_warn("Failed to fetch sector", ticker = tk, error = e$message)
        })
      }

      out
    }

    # Mapping manuel de secours pour les tickers courants
    get_sector_fallback <- function(ticker) {
      sector_map <- list(
        # Tech
        "AAPL" = "Technology", "MSFT" = "Technology", "GOOGL" = "Technology",
        "GOOG" = "Technology", "META" = "Technology", "NVDA" = "Technology",
        "AMD" = "Technology", "INTC" = "Technology", "CRM" = "Technology",
        "ADBE" = "Technology", "ORCL" = "Technology", "CSCO" = "Technology",

        # Consumer
        "AMZN" = "Consumer Cyclical", "TSLA" = "Consumer Cyclical",
        "HD" = "Consumer Cyclical", "NKE" = "Consumer Cyclical",
        "MCD" = "Consumer Cyclical", "SBUX" = "Consumer Cyclical",

        # Healthcare
        "JNJ" = "Healthcare", "UNH" = "Healthcare", "PFE" = "Healthcare",
        "MRK" = "Healthcare", "ABBV" = "Healthcare", "LLY" = "Healthcare",

        # Financial
        "JPM" = "Financial Services", "BAC" = "Financial Services",
        "WFC" = "Financial Services", "GS" = "Financial Services",
        "MS" = "Financial Services", "BLK" = "Financial Services",
        "V" = "Financial Services", "MA" = "Financial Services",

        # Energy
        "XOM" = "Energy", "CVX" = "Energy", "COP" = "Energy",

        # Swiss stocks
        "NESN.SW" = "Consumer Defensive", "NOVN.SW" = "Healthcare",
        "ROG.SW" = "Healthcare", "UBSG.SW" = "Financial Services",
        "ABBN.SW" = "Industrials", "ZURN.SW" = "Financial Services",
        "SREN.SW" = "Financial Services", "CFR.SW" = "Consumer Cyclical",
        "LONN.SW" = "Healthcare", "SCMN.SW" = "Communication Services"
      )

      if (ticker %in% names(sector_map)) {
        return(sector_map[[ticker]])
      }
      return("Unknown")
    }

    # Fonction combinée: essaie Yahoo, puis fallback
    get_ticker_sectors <- function(tickers) {
      # D'abord essayer Yahoo
      sectors <- get_ticker_sectors_yahoo(tickers)

      # Pour les Unknown, utiliser le fallback
      for (tk in names(sectors)) {
        if (sectors[tk] == "Unknown") {
          sectors[tk] <- get_sector_fallback(tk)
        }
      }

      sectors
    }

    # --------------------------------------------------------------------------
    # V5: get_fx_series
    # Télécharge un taux FX Yahoo:
    #  - symbole direct : FROMTO=X (ex USDCHF=X)
    #  - sinon inverse et on prend 1/x
    # Retour : xts = "CHF par 1 unité de FROM"
    # --------------------------------------------------------------------------
    get_fx_series <- function(from_ccy, to_ccy, from, to) {
      from_ccy <- toupper(from_ccy)
      to_ccy   <- toupper(to_ccy)
      if (from_ccy == to_ccy) return(NULL)
      
      sym <- paste0(from_ccy, to_ccy, "=X")
      fx  <- tryCatch(getSymbols(sym, src = "yahoo", from = from, to = to, auto.assign = FALSE),
                      error = function(e) NULL)
      invert <- FALSE
      
      if (is.null(fx)) {
        sym2 <- paste0(to_ccy, from_ccy, "=X")
        fx2  <- tryCatch(getSymbols(sym2, src = "yahoo", from = from, to = to, auto.assign = FALSE),
                         error = function(e) NULL)
        if (is.null(fx2)) stop(paste0("FX introuvable: ", from_ccy, "/", to_ccy))
        fx <- fx2
        invert <- TRUE
      }
      
      fx <- tryCatch(Ad(fx), error = function(e) Cl(fx))
      if (invert) fx <- 1 / fx
      
      colnames(fx) <- paste0(from_ccy, to_ccy)
      fx
    }
    
    # --------------------------------------------------------------------------
    # V5: convert_prices_to_base
    # Convertit un xts de prix (colonnes = tickers) vers la devise base.
    # Retourne aussi:
    #   - fx_last : dernier FX utilisé par ticker
    #   - tick_ccy: devise estimée par ticker
    # --------------------------------------------------------------------------
    convert_prices_to_base <- function(px, ticker_ccy, base_ccy, from, to) {
      base_ccy <- toupper(base_ccy)
      
      fx_last <- stats::setNames(rep(1, ncol(px)), colnames(px))
      tick_ccy_out <- ticker_ccy
      
      px_list <- list()
      
      for (tk in colnames(px)) {
        cur <- toupper(ticker_ccy[[tk]])
        if (is.na(cur) || cur == "") cur <- base_ccy
        tick_ccy_out[[tk]] <- cur
        
        if (cur == base_ccy) {
          px_list[[tk]] <- px[, tk, drop = FALSE]
          fx_last[tk] <- 1
        } else {
          fx <- get_fx_series(cur, base_ccy, from = from, to = to)
          
          # Aligne les dates (intersection) -> important pour cohérence des returns
          m <- merge(px[, tk, drop = FALSE], fx, all = FALSE)
          
          # Prix converti = prix_native * (CHF par 1 unité de devise_native)
          p_conv <- m[, 1] * m[, 2]
          colnames(p_conv) <- tk
          
          px_list[[tk]] <- p_conv
          fx_last[tk] <- as.numeric(fx[nrow(fx), 1])
        }
      }
      
      px_base <- do.call(merge, c(px_list, all = FALSE))
      
      list(
        px_base  = px_base,
        fx_last  = fx_last,
        tick_ccy = tick_ccy_out,
        base_ccy = base_ccy
      )
    }
    
    # --------------------------------------------------------------------------
    # calc_log_returns : calcule les log-rendements journaliers
    # --------------------------------------------------------------------------
    calc_log_returns <- function(prices_xts) {
      r <- Return.calculate(prices_xts, method = "log")
      r <- na.omit(r)
      if (nrow(r) < 60) stop("Pas assez de données après nettoyage (min ~60 obs).")
      r
    }
    
    # --------------------------------------------------------------------------
    # estimate_mu_sigma : estime mu et Sigma annualisés
    # --------------------------------------------------------------------------
    estimate_mu_sigma <- function(rets_xts, freq = 252) {
      mu    <- colMeans(rets_xts) * freq
      Sigma <- cov(rets_xts) * freq
      Sigma <- as.matrix(nearPD(Sigma)$mat)
      list(mu = as.numeric(mu), Sigma = Sigma, tickers = colnames(rets_xts))
    }
    
    # --------------------------------------------------------------------------
    # solve_min_var_target : Markowitz (QP) avec contraintes
    # --------------------------------------------------------------------------
    solve_min_var_target <- function(mu, Sigma, target_return, w_max = 1) {
      n <- length(mu)
      Dmat <- Sigma
      dvec <- rep(0, n)

      Amat <- cbind(
        rep(1, n),  # sum(w)=1
        mu,         # mu'w=target
        diag(n),    # w>=0
        -diag(n)    # w<=w_max
      )
      bvec <- c(1, target_return, rep(0, n), rep(-w_max, n))

      sol <- tryCatch(
        solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 2),
        error = function(e) NULL
      )
      if (is.null(sol)) return(rep(NA_real_, n))

      w <- sol$solution
      w[w < 1e-10] <- 0
      w
    }

    # --------------------------------------------------------------------------
    # V12: solve_min_var_target_sectors : Markowitz avec contraintes sectorielles
    # sector_assignments: vecteur de secteurs pour chaque actif
    # sector_limits: liste nommée avec max par secteur (ex: list("Technology" = 0.30))
    # --------------------------------------------------------------------------
    solve_min_var_target_sectors <- function(mu, Sigma, target_return, w_max = 1,
                                              sector_assignments = NULL, sector_limits = NULL) {
      n <- length(mu)
      Dmat <- Sigma
      dvec <- rep(0, n)

      # Contraintes de base
      Amat <- cbind(
        rep(1, n),  # sum(w)=1
        mu,         # mu'w=target
        diag(n),    # w>=0
        -diag(n)    # w<=w_max
      )
      bvec <- c(1, target_return, rep(0, n), rep(-w_max, n))

      # Ajouter contraintes sectorielles si spécifiées
      if (!is.null(sector_assignments) && !is.null(sector_limits) && length(sector_limits) > 0) {
        sectors_unique <- unique(sector_assignments)

        for (sector in names(sector_limits)) {
          if (sector %in% sectors_unique) {
            sector_max <- sector_limits[[sector]]
            # Créer un vecteur: -1 pour les actifs du secteur, 0 sinon
            # Contrainte: -sum(w_sector) >= -sector_max  =>  sum(w_sector) <= sector_max
            sector_constraint <- rep(0, n)
            sector_constraint[sector_assignments == sector] <- -1
            Amat <- cbind(Amat, sector_constraint)
            bvec <- c(bvec, -sector_max)
          }
        }
      }

      sol <- tryCatch(
        solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 2),
        error = function(e) NULL
      )
      if (is.null(sol)) return(rep(NA_real_, n))

      w <- sol$solution
      w[w < 1e-10] <- 0
      w
    }

    # --------------------------------------------------------------------------
    # V12: compute_frontier_sectors : frontière avec contraintes sectorielles
    # --------------------------------------------------------------------------
    compute_frontier_sectors <- function(mu, Sigma, rf, n_grid = 40, w_max = 1,
                                          sector_assignments = NULL, sector_limits = NULL) {
      Logger$log_info("Computing efficient frontier with sector constraints",
                      n_assets = length(mu),
                      n_grid = n_grid,
                      w_max = w_max,
                      n_sector_constraints = length(sector_limits))

      targets <- seq(min(mu), max(mu), length.out = n_grid)

      W <- lapply(targets, function(tr) {
        solve_min_var_target_sectors(mu, Sigma, tr, w_max = w_max,
                                      sector_assignments = sector_assignments,
                                      sector_limits = sector_limits)
      })
      W <- do.call(rbind, W)

      ok <- apply(W, 1, function(x) all(is.finite(x)))
      W <- W[ok, , drop = FALSE]
      targets <- targets[ok]

      if (nrow(W) < 5) {
        Logger$log_error("Frontier with sectors failed", feasible_points = nrow(W))
        stop("Frontière non calculable avec les contraintes sectorielles (trop restrictives?).")
      }

      port_ret <- as.numeric(W %*% mu)
      port_vol <- apply(W, 1, function(w) sqrt(drop(t(w) %*% Sigma %*% w)))
      sharpe   <- (port_ret - rf) / port_vol

      max_sharpe_idx <- which.max(sharpe)
      Logger$log_info("Frontier with sectors computed",
                      feasible_points = nrow(W),
                      max_sharpe = round(sharpe[max_sharpe_idx], 3))

      list(
        targets = targets,
        W       = W,
        ret     = port_ret,
        vol     = port_vol,
        sharpe  = sharpe
      )
    }
    
    # --------------------------------------------------------------------------
    # compute_frontier : calcule la frontière efficiente complète
    # --------------------------------------------------------------------------
    compute_frontier <- function(mu, Sigma, rf, n_grid = 40, w_max = 1) {
      Logger$log_info("Computing efficient frontier",
                      n_assets = length(mu),
                      n_grid = n_grid,
                      w_max = w_max,
                      rf = rf)

      targets <- seq(min(mu), max(mu), length.out = n_grid)

      W <- lapply(targets, function(tr) solve_min_var_target(mu, Sigma, tr, w_max = w_max))
      W <- do.call(rbind, W)

      ok <- apply(W, 1, function(x) all(is.finite(x)))
      W <- W[ok, , drop = FALSE]
      targets <- targets[ok]

      if (nrow(W) < 5) {
        Logger$log_error("Frontier computation failed", feasible_points = nrow(W))
        stop("Frontière non calculable (trop de points infaisables).")
      }

      port_ret <- as.numeric(W %*% mu)
      port_vol <- apply(W, 1, function(w) sqrt(drop(t(w) %*% Sigma %*% w)))
      sharpe   <- (port_ret - rf) / port_vol

      max_sharpe_idx <- which.max(sharpe)
      Logger$log_info("Frontier computed successfully",
                      feasible_points = nrow(W),
                      max_sharpe = round(sharpe[max_sharpe_idx], 3),
                      tangent_return = round(port_ret[max_sharpe_idx], 4),
                      tangent_vol = round(port_vol[max_sharpe_idx], 4))

      list(
        targets = targets,
        W       = W,
        ret     = port_ret,
        vol     = port_vol,
        sharpe  = sharpe
      )
    }
    
    # --------------------------------------------------------------------------
    # pick_tangency : indice du max Sharpe
    # --------------------------------------------------------------------------
    pick_tangency <- function(frontier) which.max(frontier$sharpe)
    
    # --------------------------------------------------------------------------
    # Shrinkage : corrélation constante
    # --------------------------------------------------------------------------
    build_constcor_target <- function(Sigma) {
      n <- ncol(Sigma)
      sdv <- sqrt(diag(Sigma))
      sdv[!is.finite(sdv)] <- 0
      
      R <- cov2cor(Sigma)
      rho <- R[upper.tri(R)]
      rho <- rho[is.finite(rho)]
      
      avg_rho <- if (length(rho) > 0) mean(rho) else 0
      avg_rho <- max(min(avg_rho, 0.99), -0.99)
      
      R_target <- matrix(avg_rho, n, n)
      diag(R_target) <- 1
      
      (sdv %o% sdv) * R_target
    }
    
    shrink_covariance <- function(Sigma, method = c("none", "diag", "constcor"), lambda = 0.2) {
      method <- match.arg(method)
      lambda <- as.numeric(lambda)
      if (!is.finite(lambda)) lambda <- 0
      lambda <- max(min(lambda, 1), 0)
      
      if (method == "none" || lambda == 0) {
        return(as.matrix(nearPD(Sigma)$mat))
      }
      
      if (method == "diag") {
        Sigma_target <- diag(diag(Sigma))
      } else if (method == "constcor") {
        Sigma_target <- build_constcor_target(Sigma)
      } else {
        Sigma_target <- diag(diag(Sigma))
      }
      
      Sigma_shrunk <- (1 - lambda) * Sigma + lambda * Sigma_target
      as.matrix(nearPD(Sigma_shrunk)$mat)
    }
    
    # --------------------------------------------------------------------------
    # BAND REBALANCING: détection du drift entre poids actuels et cibles
    # --------------------------------------------------------------------------

    #' Calcule le drift entre les poids actuels et cibles
    #' @param w_current Vecteur des poids actuels (normalisés à 1)
    #' @param w_target Vecteur des poids cibles (normalisés à 1)
    #' @return Liste avec drift_per_asset, max_drift, total_drift
    calc_drift <- function(w_current, w_target) {
      drift <- abs(w_target - w_current)
      list(
        drift_per_asset = drift,
        max_drift = max(drift),
        total_drift = sum(drift) / 2  # turnover = half sum of abs changes
      )
    }

    #' Vérifie si un rebalancement est nécessaire selon le seuil de drift
    #' @param w_current Poids actuels
    #' @param w_target Poids cibles
    #' @param threshold Seuil de drift max (ex: 0.05 = 5%)
    #' @return TRUE si rebalancement nécessaire
    needs_rebalancing <- function(w_current, w_target, threshold = 0.05) {
      d <- calc_drift(w_current, w_target)
      d$max_drift > threshold
    }

    #' Simule l'évolution des poids après une période sans rebalancement
    #' @param w_initial Poids initiaux
    #' @param rets_period Matrice des rendements (n_days x n_assets)
    #' @return Poids actualisés après drift
    drift_weights <- function(w_initial, rets_period) {
      # Valeur relative de chaque actif après les rendements
      cumret <- apply(1 + rets_period, 2, prod)
      # Poids driftés = poids initiaux * croissance relative
      w_drifted <- w_initial * cumret
      # Normaliser
      w_drifted / sum(w_drifted)
    }

    # --------------------------------------------------------------------------
    # TURNOVER CAP: limite le turnover par période de rebalancement
    # --------------------------------------------------------------------------

    #' Applique un cap sur le turnover en scalant les changements de poids
    #' @param w_current Poids actuels
    #' @param w_target Poids cibles optimaux
    #' @param turnover_cap Cap sur le turnover (ex: 0.20 = 20%)
    #' @return Poids ajustés respectant le cap
    apply_turnover_cap <- function(w_current, w_target, turnover_cap = 0.20) {
      if (!is.finite(turnover_cap) || turnover_cap <= 0) {
        return(w_target)
      }

      # Changements demandés
      delta <- w_target - w_current
      turnover_requested <- sum(abs(delta)) / 2

      # Si le turnover demandé <= cap, pas de scaling
      if (turnover_requested <= turnover_cap) {
        return(w_target)
      }

      # Sinon, scaler les changements proportionnellement
      scale_factor <- turnover_cap / turnover_requested
      w_adjusted <- w_current + delta * scale_factor

      # Normaliser (au cas où)
      w_adjusted <- pmax(w_adjusted, 0)
      w_adjusted / sum(w_adjusted)
    }

    # --------------------------------------------------------------------------
    # HOLDINGS TRACKING: comparaison portfolio réel vs cible
    # --------------------------------------------------------------------------

    #' Calcule les transactions nécessaires pour rebalancer
    #' @param holdings Liste avec ticker, shares, price
    #' @param w_target Poids cibles
    #' @param portfolio_value Valeur totale du portfolio
    #' @param tc_bps Coûts de transaction en bps
    #' @return Dataframe avec les transactions suggérées
    calc_rebalancing_trades <- function(holdings, w_target, portfolio_value, tc_bps = 10) {
      # holdings = data.frame(ticker, shares, price)
      # ou list(ticker = list(shares=X, price=Y), ...)

      if (is.data.frame(holdings)) {
        tickers <- holdings$ticker
        current_values <- holdings$shares * holdings$price
        prices <- holdings$price
      } else {
        tickers <- names(holdings)
        current_values <- sapply(holdings, function(x) x$shares * x$price)
        prices <- sapply(holdings, function(x) x$price)
      }

      total_current <- sum(current_values)
      w_current <- current_values / total_current

      # Valeurs cibles
      target_values <- w_target * portfolio_value
      names(target_values) <- tickers

      # Différence
      delta_values <- target_values - current_values
      delta_shares <- delta_values / prices

      # Coûts estimés
      tc_rate <- tc_bps / 10000
      trade_costs <- abs(delta_values) * tc_rate

      data.frame(
        ticker = tickers,
        current_weight = round(w_current * 100, 2),
        target_weight = round(w_target * 100, 2),
        drift = round((w_target - w_current) * 100, 2),
        current_value = round(current_values, 2),
        target_value = round(target_values, 2),
        trade_value = round(delta_values, 2),
        trade_shares = round(delta_shares, 2),
        trade_cost = round(trade_costs, 2),
        stringsAsFactors = FALSE
      )
    }

    # --------------------------------------------------------------------------
    # V13: RISK METRICS - CVaR (Expected Shortfall)
    # --------------------------------------------------------------------------

    #' Calculate Value at Risk (VaR)
    #' @param returns Vector of returns
    #' @param alpha Confidence level (default 0.05 = 95% VaR)
    #' @return VaR value (negative = loss)
    calc_var <- function(returns, alpha = 0.05) {
      returns <- as.numeric(returns)
      returns <- returns[is.finite(returns)]
      if (length(returns) < 20) return(NA_real_)
      as.numeric(quantile(returns, probs = alpha, na.rm = TRUE))
    }

    #' Calculate Conditional VaR (Expected Shortfall)
    #' CVaR = average loss in the worst alpha% of cases
    #' @param returns Vector of returns
    #' @param alpha Confidence level (default 0.05 = worst 5%)
    #' @return CVaR value (negative = loss)
    calc_cvar <- function(returns, alpha = 0.05) {
      returns <- as.numeric(returns)
      returns <- returns[is.finite(returns)]
      if (length(returns) < 20) return(NA_real_)

      var_threshold <- quantile(returns, probs = alpha, na.rm = TRUE)
      tail_returns <- returns[returns <= var_threshold]

      if (length(tail_returns) == 0) return(var_threshold)
      mean(tail_returns, na.rm = TRUE)
    }

    #' Calculate annualized CVaR from daily returns
    #' @param returns Daily returns vector
    #' @param alpha Confidence level
    #' @param freq Trading days per year (default 252)
    #' @return Annualized CVaR
    calc_cvar_annual <- function(returns, alpha = 0.05, freq = 252) {
      daily_cvar <- calc_cvar(returns, alpha)
      if (is.na(daily_cvar)) return(NA_real_)
      # Annualize using sqrt(T) scaling (approximation)
      daily_cvar * sqrt(freq)
    }

    #' Calculate portfolio CVaR given weights
    #' @param rets_matrix Matrix of returns (rows = dates, cols = assets)
    #' @param weights Portfolio weights
    #' @param alpha Confidence level
    #' @return Portfolio CVaR
    calc_portfolio_cvar <- function(rets_matrix, weights, alpha = 0.05) {
      if (is.null(rets_matrix) || is.null(weights)) return(NA_real_)
      rets_matrix <- as.matrix(rets_matrix)
      weights <- as.numeric(weights)

      # Portfolio returns
      port_rets <- as.numeric(rets_matrix %*% weights)
      calc_cvar(port_rets, alpha)
    }

    # --------------------------------------------------------------------------
    # V13: STRESS TESTING - Historical Scenarios
    # --------------------------------------------------------------------------

    # Pre-defined historical crisis scenarios (approximate peak-to-trough)
    STRESS_SCENARIOS <- list(
      "GFC 2008" = list(
        description = "Global Financial Crisis (Sep-Nov 2008)",
        start = "2008-09-01",
        end = "2008-11-20",
        sp500_drawdown = -0.46,
        typical_equity = -0.50,
        typical_bond = 0.05,
        correlations_spike = TRUE
      ),
      "COVID 2020" = list(
        description = "COVID-19 Crash (Feb-Mar 2020)",
        start = "2020-02-19",
        end = "2020-03-23",
        sp500_drawdown = -0.34,
        typical_equity = -0.35,
        typical_bond = 0.03,
        correlations_spike = TRUE
      ),
      "Dot-com 2000" = list(
        description = "Tech Bubble Burst (Mar 2000 - Oct 2002)",
        start = "2000-03-10",
        end = "2002-10-09",
        sp500_drawdown = -0.49,
        typical_equity = -0.45,
        typical_bond = 0.15,
        correlations_spike = FALSE
      ),
      "Euro Crisis 2011" = list(
        description = "European Debt Crisis (Jul-Oct 2011)",
        start = "2011-07-01",
        end = "2011-10-04",
        sp500_drawdown = -0.19,
        typical_equity = -0.20,
        typical_bond = 0.05,
        correlations_spike = TRUE
      ),
      "Flash Crash 2010" = list(
        description = "Flash Crash (May 6, 2010)",
        start = "2010-05-06",
        end = "2010-05-06",
        sp500_drawdown = -0.09,
        typical_equity = -0.10,
        typical_bond = 0.01,
        correlations_spike = TRUE
      ),
      "Custom -20%" = list(
        description = "Hypothetical -20% equity shock",
        start = NA,
        end = NA,
        sp500_drawdown = -0.20,
        typical_equity = -0.20,
        typical_bond = 0.02,
        correlations_spike = FALSE
      ),
      "Custom -30%" = list(
        description = "Hypothetical -30% equity shock",
        start = NA,
        end = NA,
        sp500_drawdown = -0.30,
        typical_equity = -0.30,
        typical_bond = 0.03,
        correlations_spike = TRUE
      )
    )

    #' Run stress test on portfolio using historical scenario
    #' @param weights Portfolio weights
    #' @param rets_xts Historical returns xts object
    #' @param scenario_name Name of scenario from STRESS_SCENARIOS
    #' @param portfolio_value Current portfolio value
    #' @return List with stress test results
    run_stress_test <- function(weights, rets_xts, scenario_name, portfolio_value = 10000) {
      scenario <- STRESS_SCENARIOS[[scenario_name]]
      if (is.null(scenario)) {
        return(list(error = paste("Unknown scenario:", scenario_name)))
      }

      weights <- as.numeric(weights)
      n_assets <- length(weights)

      # Try to use actual historical data if available
      actual_loss <- NA_real_
      if (!is.na(scenario$start) && !is.na(scenario$end)) {
        tryCatch({
          start_date <- as.Date(scenario$start)
          end_date <- as.Date(scenario$end)

          # Check if we have data for this period
          if (min(zoo::index(rets_xts)) <= start_date && max(zoo::index(rets_xts)) >= end_date) {
            period_rets <- rets_xts[paste0(start_date, "/", end_date)]
            if (nrow(period_rets) > 0) {
              # Calculate cumulative return for the period
              port_rets <- as.numeric(as.matrix(period_rets) %*% weights)
              cum_ret <- prod(1 + port_rets) - 1
              actual_loss <- cum_ret
            }
          }
        }, error = function(e) NULL)
      }

      # If no actual data, estimate using typical scenario impact
      estimated_loss <- scenario$typical_equity

      # Use actual if available, otherwise estimated
      portfolio_loss <- if (!is.na(actual_loss)) actual_loss else estimated_loss
      loss_amount <- portfolio_value * abs(portfolio_loss)

      list(
        scenario = scenario_name,
        description = scenario$description,
        period = if (!is.na(scenario$start)) paste(scenario$start, "to", scenario$end) else "Hypothetical",
        portfolio_return = portfolio_loss,
        loss_amount = loss_amount,
        remaining_value = portfolio_value * (1 + portfolio_loss),
        data_source = if (!is.na(actual_loss)) "Historical" else "Estimated",
        sp500_benchmark = scenario$sp500_drawdown,
        correlations_spike = scenario$correlations_spike
      )
    }

    #' Run all stress tests on portfolio
    #' @param weights Portfolio weights
    #' @param rets_xts Historical returns
    #' @param portfolio_value Portfolio value
    #' @return Data frame with all stress test results
    run_all_stress_tests <- function(weights, rets_xts, portfolio_value = 10000) {
      results <- lapply(names(STRESS_SCENARIOS), function(name) {
        res <- run_stress_test(weights, rets_xts, name, portfolio_value)
        data.frame(
          Scenario = res$scenario,
          Description = res$description,
          Portfolio_Return = res$portfolio_return,
          Loss_CHF = res$loss_amount,
          Remaining_CHF = res$remaining_value,
          SP500_Return = res$sp500_benchmark,
          Data_Source = res$data_source,
          stringsAsFactors = FALSE
        )
      })
      do.call(rbind, results)
    }

    # --------------------------------------------------------------------------
    # PERSISTENCE: Save/Load user settings
    # --------------------------------------------------------------------------

    #' Get the default settings file path
    get_settings_path <- function(filename = "user_settings.json") {
      data_dir <- file.path(getwd(), "data")
      if (!dir.exists(data_dir)) {
        dir.create(data_dir, recursive = TRUE)
      }
      file.path(data_dir, filename)
    }

    #' Save user settings to JSON file
    #' @param settings List of settings to save
    #' @param filepath Path to save file (default: data/user_settings.json)
    #' @return TRUE if successful, FALSE otherwise
    save_settings <- function(settings, filepath = NULL) {
      if (is.null(filepath)) {
        filepath <- get_settings_path()
      }

      tryCatch({
        settings$last_saved <- Sys.time()
        jsonlite::write_json(settings, filepath, pretty = TRUE, auto_unbox = TRUE)
        TRUE
      }, error = function(e) {
        warning("Failed to save settings: ", e$message)
        FALSE
      })
    }

    #' Load user settings from JSON file
    #' @param filepath Path to load file (default: data/user_settings.json)
    #' @return List of settings or NULL if file doesn't exist
    load_settings <- function(filepath = NULL) {
      if (is.null(filepath)) {
        filepath <- get_settings_path()
      }

      if (!file.exists(filepath)) {
        return(NULL)
      }

      tryCatch({
        jsonlite::read_json(filepath, simplifyVector = TRUE)
      }, error = function(e) {
        warning("Failed to load settings: ", e$message)
        NULL
      })
    }

    #' Get default settings
    get_default_settings <- function() {
      list(
        tickers = "AAPL MSFT AMZN GOOGL NVDA",
        use_aliases = TRUE,
        date_start = as.character(Sys.Date() - 365 * 5),
        date_end = as.character(Sys.Date()),
        capital = 10000,
        rf = 0.02,
        wmax = 0.40,
        ngrid = 60,
        shrink_method = "constcor",
        shrink_lambda = 0.20,
        mu_method = "none",
        mu_winsor = 0.02,
        mu_lambda = 0.30,
        bt_train_years = 3,
        bt_rebal_months = 3,
        bt_tc_bps = 10,
        bt_drift_threshold = 5,
        bt_turnover_cap = 100,
        bt_pick_mode = "tangency",
        holdings = ""
      )
    }

    # --------------------------------------------------------------------------
    # EXPORT : API du module Core
    # --------------------------------------------------------------------------
    list(
      parse_tickers             = parse_tickers,
      normalize_tickers         = normalize_tickers,
      get_prices_yahoo          = get_prices_yahoo,
      get_benchmark_stats       = get_benchmark_stats,
      get_benchmarks            = get_benchmarks,


      # V5 FX / devises
      get_ticker_currency_yahoo = get_ticker_currency_yahoo,
      get_fx_series             = get_fx_series,
      convert_prices_to_base    = convert_prices_to_base,

      # Stats Markowitz
      calc_log_returns          = calc_log_returns,
      estimate_mu_sigma         = estimate_mu_sigma,
      compute_frontier          = compute_frontier,
      compute_frontier_sectors  = compute_frontier_sectors,
      pick_tangency             = pick_tangency,

      # Secteurs
      get_ticker_sectors        = get_ticker_sectors,
      get_ticker_sectors_yahoo  = get_ticker_sectors_yahoo,
      get_sector_fallback       = get_sector_fallback,

      # Robustesse
      build_constcor_target     = build_constcor_target,
      shrink_covariance         = shrink_covariance,

      # Band rebalancing & drift
      calc_drift                = calc_drift,
      needs_rebalancing         = needs_rebalancing,
      drift_weights             = drift_weights,

      # Turnover cap
      apply_turnover_cap        = apply_turnover_cap,

      # Holdings tracking
      calc_rebalancing_trades   = calc_rebalancing_trades,

      # V13: Risk metrics (CVaR)
      calc_var                  = calc_var,
      calc_cvar                 = calc_cvar,
      calc_cvar_annual          = calc_cvar_annual,
      calc_portfolio_cvar       = calc_portfolio_cvar,

      # V13: Stress testing
      STRESS_SCENARIOS          = STRESS_SCENARIOS,
      run_stress_test           = run_stress_test,
      run_all_stress_tests      = run_all_stress_tests,

      # Persistence
      save_settings             = save_settings,
      load_settings             = load_settings,
      get_default_settings      = get_default_settings,
      get_settings_path         = get_settings_path
    )
  }
}
