"tool/MarkowitzCore/markowitz_core_provider.R" %provides% {
  
  library(quantmod)
  library(PerformanceAnalytics)
  library(quadprog)
  library(Matrix)
  library(xts)
  
  function() {
    
    parse_tickers <- function(x) {
      t <- unlist(strsplit(x, "[,;\\s]+"))
      t <- t[nchar(t) > 0]
      unique(toupper(t))
    }
    
    get_prices_yahoo <- function(tickers, from, to) {
      lst <- lapply(tickers, function(tk) {
        tryCatch({
          x <- suppressWarnings(getSymbols(tk, src="yahoo", from=from, to=to, auto.assign=FALSE))
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
    
    calc_log_returns <- function(prices_xts) {
      r <- Return.calculate(prices_xts, method="log")
      r <- na.omit(r)
      if (nrow(r) < 60) stop("Pas assez de données après nettoyage (min ~60 obs).")
      r
    }
    
    estimate_mu_sigma <- function(rets_xts, freq=252) {
      mu <- colMeans(rets_xts) * freq
      Sigma <- cov(rets_xts) * freq
      Sigma <- as.matrix(nearPD(Sigma)$mat)
      list(mu = as.numeric(mu), Sigma = Sigma, tickers = colnames(rets_xts))
    }
    
    solve_min_var_target <- function(mu, Sigma, target_return, w_max=1) {
      n <- length(mu)
      Dmat <- Sigma
      dvec <- rep(0, n)
      
      # solve.QP: t(Amat) %*% w >= bvec ; meq=2 (sum=1, return=target)
      Amat <- cbind(
        rep(1, n),  # sum(w)=1
        mu,         # mu'w=target
        diag(n),    # w >= 0
        -diag(n)    # -w >= -w_max  => w <= w_max
      )
      bvec <- c(1, target_return, rep(0, n), rep(-w_max, n))
      
      sol <- tryCatch(
        solve.QP(Dmat=Dmat, dvec=dvec, Amat=Amat, bvec=bvec, meq=2),
        error = function(e) NULL
      )
      if (is.null(sol)) return(rep(NA_real_, n))
      
      w <- sol$solution
      w[w < 1e-10] <- 0
      w
    }
    
    compute_frontier <- function(mu, Sigma, rf, n_grid=40, w_max=1) {
      targets <- seq(min(mu), max(mu), length.out=n_grid)
      
      W <- lapply(targets, function(tr) solve_min_var_target(mu, Sigma, tr, w_max=w_max))
      W <- do.call(rbind, W)
      
      ok <- apply(W, 1, function(x) all(is.finite(x)))
      W <- W[ok, , drop=FALSE]
      targets <- targets[ok]
      if (nrow(W) < 5) stop("Frontière non calculable (trop de points infeasibles).")
      
      port_ret <- as.numeric(W %*% mu)
      port_vol <- apply(W, 1, function(w) sqrt(drop(t(w) %*% Sigma %*% w)))
      sharpe <- (port_ret - rf) / port_vol
      
      list(targets=targets, W=W, ret=port_ret, vol=port_vol, sharpe=sharpe)
    }
    
    pick_tangency <- function(frontier) which.max(frontier$sharpe)
    
    list(
      parse_tickers = parse_tickers,
      get_prices_yahoo = get_prices_yahoo,
      calc_log_returns = calc_log_returns,
      estimate_mu_sigma = estimate_mu_sigma,
      compute_frontier = compute_frontier,
      pick_tangency = pick_tangency
    )
  }
}
