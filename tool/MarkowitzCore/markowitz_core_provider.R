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
      t <- unlist(strsplit(x, "[,;\\s]+"))
      t <- t[nchar(t) > 0]
      unique(toupper(t))
    }
    
    # --------------------------------------------------------------------------
    # get_prices_yahoo : télécharge les prix ajustés depuis Yahoo Finance
    # Retourne un objet xts avec une colonne par ticker valide.
    # --------------------------------------------------------------------------
    get_prices_yahoo <- function(tickers, from, to) {
      lst <- lapply(tickers, function(tk) {
        tryCatch({
          # Télécharge les données OHLCV + ajusté
          x <- suppressWarnings(
            getSymbols(tk, src = "yahoo", from = from, to = to, auto.assign = FALSE)
          )
          # On ne garde que le prix ajusté (dividendes + splits)
          Ad(x)
        }, error = function(e) NULL)
      })
      names(lst) <- tickers
      
      # Retire les tickers qui ont échoué (NULL)
      lst <- lst[!vapply(lst, is.null, logical(1))]
      if (length(lst) < 2) stop("Min 2 tickers valides requis (Yahoo).")
      
      # Fusionne toutes les séries ; all=FALSE => intersection des dates
      px <- do.call(merge, c(lst, all = FALSE))
      colnames(px) <- names(lst)
      px
    }
    
    # --------------------------------------------------------------------------
    # calc_log_returns : calcule les log-rendements journaliers
    # Nettoie les NA et vérifie qu'on a assez d'observations.
    # --------------------------------------------------------------------------
    calc_log_returns <- function(prices_xts) {
      r <- Return.calculate(prices_xts, method = "log")
      r <- na.omit(r)
      if (nrow(r) < 60) stop("Pas assez de données après nettoyage (min ~60 obs).")
      r
    }
    
    # --------------------------------------------------------------------------
    # estimate_mu_sigma : estime le vecteur de rendements espérés (mu)
    # et la matrice de covariance (Sigma), annualisés.
    # On utilise nearPD() pour garantir que Sigma est semi-définie positive.
    # --------------------------------------------------------------------------
    estimate_mu_sigma <- function(rets_xts, freq = 252) {
      # Moyenne des rendements journaliers x 252 => rendement annuel
      mu <- colMeans(rets_xts) * freq
      # Covariance journalière x 252 => covariance annuelle
      Sigma <- cov(rets_xts) * freq
      # Projette sur la matrice SDP la plus proche (stabilité numérique)
      Sigma <- as.matrix(nearPD(Sigma)$mat)
      list(mu = as.numeric(mu), Sigma = Sigma, tickers = colnames(rets_xts))
    }
    
    # --------------------------------------------------------------------------
    # solve_min_var_target : résout le problème QP
    # "minimiser la variance sous contrainte de rendement cible"
    #
    # min  0.5 * w' Σ w
    # s.c. sum(w) = 1          (budget)
    #      μ' w   = target     (rendement cible)
    #      w >= 0              (pas de vente à découvert)
    #      w <= w_max          (concentration max)
    # --------------------------------------------------------------------------
    solve_min_var_target <- function(mu, Sigma, target_return, w_max = 1) {
      n <- length(mu)
      Dmat <- Sigma
      dvec <- rep(0, n)
      
      # Construction de la matrice de contraintes pour solve.QP
      # solve.QP attend : t(Amat) %*% w >= bvec
      # Les `meq` premières contraintes sont des égalités.
      Amat <- cbind(
        rep(1, n),   # sum(w) = 1
        mu,          # mu'w = target
        diag(n),     # w_i >= 0
        -diag(n)     # -w_i >= -w_max  <=>  w_i <= w_max
      )
      bvec <- c(1, target_return, rep(0, n), rep(-w_max, n))
      
      sol <- tryCatch(
        solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 2),
        error = function(e) NULL
      )
      
      if (is.null(sol)) return(rep(NA_real_, n))
      
      w <- sol$solution
      # Arrondit les poids très petits à zéro (bruit numérique)
      w[w < 1e-10] <- 0
      w
    }
    
    # --------------------------------------------------------------------------
    # compute_frontier : calcule la frontière efficiente complète
    # en balayant des rendements cibles de min(mu) à max(mu).
    # Retourne aussi l'indice du portefeuille tangent (max Sharpe).
    # --------------------------------------------------------------------------
    compute_frontier <- function(mu, Sigma, rf, n_grid = 40, w_max = 1) {
      # Grille de rendements cibles
      targets <- seq(min(mu), max(mu), length.out = n_grid)
      
      # Résout le QP pour chaque cible
      W <- lapply(targets, function(tr) {
        solve_min_var_target(mu, Sigma, tr, w_max = w_max)
      })
      W <- do.call(rbind, W)
      
      # Retire les solutions infaisables (NA)
      ok <- apply(W, 1, function(x) all(is.finite(x)))
      W <- W[ok, , drop = FALSE]
      targets <- targets[ok]
      
      if (nrow(W) < 5) stop("Frontière non calculable (trop de points infaisables).")
      
      # Calcul rendement / volatilité / Sharpe pour chaque portefeuille
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
    # pick_tangency : retourne l'indice du portefeuille tangent (max Sharpe)
    # --------------------------------------------------------------------------
    pick_tangency <- function(frontier) which.max(frontier$sharpe)
    
    # -------------------------------------------------------------------------
    # EXPORT : liste des fonctions publiques du module
    # -------------------------------------------------------------------------
    list(
      parse_tickers      = parse_tickers,
      get_prices_yahoo   = get_prices_yahoo,
      calc_log_returns   = calc_log_returns,
      estimate_mu_sigma  = estimate_mu_sigma,
      compute_frontier   = compute_frontier,
      pick_tangency      = pick_tangency
    )
  }
}
