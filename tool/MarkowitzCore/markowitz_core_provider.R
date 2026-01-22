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
  
  # ============================================================================
  # PROVIDER : renvoie une fonction qui, une fois appelée, retourne la liste
  # des fonctions "métier" du module Core.
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
    # Retourne un objet xts avec une colonne par ticker valide.
    # --------------------------------------------------------------------------
    get_prices_yahoo <- function(tickers, from, to) {
      lst <- lapply(tickers, function(tk) {
        tryCatch({
          x <- suppressWarnings(
            getSymbols(tk, src = "yahoo", from = from, to = to, auto.assign = FALSE)
          )
          Ad(x)
        }, error = function(e) NULL)
      })
      names(lst) <- tickers
      
      lst <- lst[!vapply(lst, is.null, logical(1))]
      if (length(lst) < 2) stop("Min 2 tickers valides requis (Yahoo).")
      
      px <- do.call(merge, c(lst, all = FALSE))
      colnames(px) <- names(lst)
      px
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
      
      # Selon version, la colonne peut s'appeler "Currency" ou autre
      if ("Currency" %in% colnames(q)) {
        cur <- as.character(q[, "Currency"])
      } else {
        cur <- as.character(q[, 1])
      }
      names(cur) <- rownames(q)
      
      out[names(cur)] <- cur
      out
    }
    
    # --------------------------------------------------------------------------
    # V5: get_fx_series
    # Télécharge un taux FX Yahoo:
    #  - symbole direct : FROMTO=X (ex USDCHF=X)
    #  - sinon inverse et on prend 1/x
    # Retour : xts (Adj Close) = "TO par 1 FROM"
    # --------------------------------------------------------------------------
    get_fx_series <- function(from_ccy, to_ccy, from, to) {
      from_ccy <- toupper(from_ccy)
      to_ccy   <- toupper(to_ccy)
      if (from_ccy == to_ccy) return(NULL)
      
      sym <- paste0(from_ccy, to_ccy, "=X")
      fx  <- tryCatch(getSymbols(sym, src="yahoo", from=from, to=to, auto.assign=FALSE), error=function(e) NULL)
      invert <- FALSE
      
      if (is.null(fx)) {
        sym2 <- paste0(to_ccy, from_ccy, "=X")
        fx2  <- tryCatch(getSymbols(sym2, src="yahoo", from=from, to=to, auto.assign=FALSE), error=function(e) NULL)
        if (is.null(fx2)) stop(paste0("FX introuvable: ", from_ccy, "/", to_ccy))
        fx <- fx2
        invert <- TRUE
      }
      
      fx <- tryCatch(Ad(fx), error=function(e) Cl(fx))
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
          fx <- get_fx_series(cur, base_ccy, from=from, to=to)
          
          # aligne les dates (intersection)
          m <- merge(px[, tk, drop = FALSE], fx, all = FALSE)
          
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
        rep(1, n),
        mu,
        diag(n),
        -diag(n)
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
    # compute_frontier : calcule la frontière efficiente complète
    # --------------------------------------------------------------------------
    compute_frontier <- function(mu, Sigma, rf, n_grid = 40, w_max = 1) {
      targets <- seq(min(mu), max(mu), length.out = n_grid)
      
      W <- lapply(targets, function(tr) {
        solve_min_var_target(mu, Sigma, tr, w_max = w_max)
      })
      W <- do.call(rbind, W)
      
      ok <- apply(W, 1, function(x) all(is.finite(x)))
      W <- W[ok, , drop = FALSE]
      targets <- targets[ok]
      
      if (nrow(W) < 5) stop("Frontière non calculable (trop de points infaisables).")
      
      port_ret <- as.numeric(W %*% mu)
      port_vol <- apply(W, 1, function(w) sqrt(drop(t(w) %*% Sigma %*% w)))
      sharpe   <- (port_ret - rf) / port_vol
      
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
    # V6: build_constcor_target
    # Construit une matrice-cible à corrélation constante (stable).
    # --------------------------------------------------------------------------
    build_constcor_target <- function(Sigma) {
      n <- ncol(Sigma)
      sdv <- sqrt(diag(Sigma))
      sdv[!is.finite(sdv)] <- 0
      
      # Corrélations
      R <- cov2cor(Sigma)
      rho <- R[upper.tri(R)]
      rho <- rho[is.finite(rho)]
      
      # moyenne des corrélations (fallback 0 si NaN)
      avg_rho <- if (length(rho) > 0) mean(rho) else 0
      avg_rho <- max(min(avg_rho, 0.99), -0.99)
      
      R_target <- matrix(avg_rho, n, n)
      diag(R_target) <- 1
      
      # Σ_target = D * R_target * D
      Sigma_target <- (sdv %o% sdv) * R_target
      Sigma_target
    }
    
    # --------------------------------------------------------------------------
    # V6: shrink_covariance
    # Shrinkage simple : Σ_shrunk = (1-λ)Σ + λΣ_target
    # method:
    #  - none     : pas de shrink
    #  - diag     : shrink vers diag(diag(Σ))
    #  - constcor : shrink vers corrélation constante
    # --------------------------------------------------------------------------
    shrink_covariance <- function(Sigma, method = c("none","diag","constcor"), lambda = 0.2) {
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
      
      # stabilité numérique
      Sigma_shrunk <- as.matrix(nearPD(Sigma_shrunk)$mat)
      Sigma_shrunk
    }
    # --------------------------------------------------------------------------
    # V6: build_constcor_target
    # Construit une matrice-cible à corrélation constante (stable).
    # --------------------------------------------------------------------------
    build_constcor_target <- function(Sigma) {
      n <- ncol(Sigma)
      sdv <- sqrt(diag(Sigma))
      sdv[!is.finite(sdv)] <- 0
      
      # Corrélations
      R <- cov2cor(Sigma)
      rho <- R[upper.tri(R)]
      rho <- rho[is.finite(rho)]
      
      # moyenne des corrélations (fallback 0 si NaN)
      avg_rho <- if (length(rho) > 0) mean(rho) else 0
      avg_rho <- max(min(avg_rho, 0.99), -0.99)
      
      R_target <- matrix(avg_rho, n, n)
      diag(R_target) <- 1
      
      # Σ_target = D * R_target * D
      Sigma_target <- (sdv %o% sdv) * R_target
      Sigma_target
    }
    
    # --------------------------------------------------------------------------
    # V6: shrink_covariance
    # Shrinkage simple : Σ_shrunk = (1-λ)Σ + λΣ_target
    # method:
    #  - none     : pas de shrink
    #  - diag     : shrink vers diag(diag(Σ))
    #  - constcor : shrink vers corrélation constante
    # --------------------------------------------------------------------------
    shrink_covariance <- function(Sigma, method = c("none","diag","constcor"), lambda = 0.2) {
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
      
      # stabilité numérique
      Sigma_shrunk <- as.matrix(nearPD(Sigma_shrunk)$mat)
      Sigma_shrunk
    }
    
    # -------------------------------------------------------------------------
    # EXPORT : liste des fonctions publiques du module
    # -------------------------------------------------------------------------
    list(
      parse_tickers             = parse_tickers,
      normalize_tickers         = normalize_tickers,
      get_prices_yahoo          = get_prices_yahoo,
      get_ticker_currency_yahoo = get_ticker_currency_yahoo,
      get_fx_series             = get_fx_series,
      convert_prices_to_base    = convert_prices_to_base,
      calc_log_returns          = calc_log_returns,
      estimate_mu_sigma         = estimate_mu_sigma,
      compute_frontier          = compute_frontier,
      pick_tangency             = pick_tangency,
      build_constcor_target     = build_constcor_target,
      shrink_covariance         = shrink_covariance
    )
  }
}
