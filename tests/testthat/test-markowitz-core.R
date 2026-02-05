# Unit tests for MarkowitzCore module
# Run with: Rscript tests/testthat.R

library(testthat)
library(modulr)

# Source the module
source(file.path(getwd(), "tool/MarkowitzCore/markowitz_core_provider.R"))
Core <- make("tool/MarkowitzCore/markowitz_core_provider")()

# =============================================================================
# TEST: parse_tickers
# =============================================================================
test_that("parse_tickers handles various input formats", {
  # Space-separated
  expect_equal(Core$parse_tickers("AAPL MSFT GOOGL"), c("AAPL", "MSFT", "GOOGL"))

  # Comma-separated
  expect_equal(Core$parse_tickers("AAPL,MSFT,GOOGL"), c("AAPL", "MSFT", "GOOGL"))

  # Mixed separators
  expect_equal(Core$parse_tickers("AAPL, MSFT; GOOGL"), c("AAPL", "MSFT", "GOOGL"))

  # Lowercase to uppercase
  expect_equal(Core$parse_tickers("aapl msft"), c("AAPL", "MSFT"))

  # Removes duplicates
  expect_equal(Core$parse_tickers("AAPL MSFT AAPL"), c("AAPL", "MSFT"))

  # Empty string
  expect_equal(length(Core$parse_tickers("")), 0)
})

# =============================================================================
# TEST: normalize_tickers
# =============================================================================
test_that("normalize_tickers applies Swiss aliases correctly", {
  # Swiss aliases enabled
  expect_equal(Core$normalize_tickers("UBS", use_aliases = TRUE), "UBSG.SW")
  expect_equal(Core$normalize_tickers("NESTLE", use_aliases = TRUE), "NESN.SW")
  expect_equal(Core$normalize_tickers("NOVARTIS", use_aliases = TRUE), "NOVN.SW")

  # Swiss aliases disabled
  expect_equal(Core$normalize_tickers("UBS", use_aliases = FALSE), "UBS")

  # Non-Swiss tickers unchanged
  expect_equal(Core$normalize_tickers("AAPL", use_aliases = TRUE), "AAPL")
})

# =============================================================================
# TEST: calc_drift
# =============================================================================
test_that("calc_drift computes drift correctly", {
  w_current <- c(0.3, 0.3, 0.4)
  w_target <- c(0.25, 0.35, 0.40)

  result <- Core$calc_drift(w_current, w_target)

  # Max drift should be 0.05 (for both first two assets)
  expect_equal(result$max_drift, 0.05)

  # Total drift (turnover) = sum(abs(delta))/2 = (0.05 + 0.05 + 0) / 2 = 0.05
  expect_equal(result$total_drift, 0.05)

  # Per asset drift
  expect_equal(result$drift_per_asset, c(0.05, 0.05, 0.00))
})

test_that("calc_drift handles identical weights", {
  w <- c(0.25, 0.25, 0.25, 0.25)
  result <- Core$calc_drift(w, w)

  expect_equal(result$max_drift, 0)
  expect_equal(result$total_drift, 0)
})

# =============================================================================
# TEST: needs_rebalancing
# =============================================================================
test_that("needs_rebalancing respects threshold", {
  w_current <- c(0.3, 0.3, 0.4)
  w_target <- c(0.25, 0.35, 0.40)

  # Drift is 5%, threshold 5% -> should NOT trigger
  expect_false(Core$needs_rebalancing(w_current, w_target, threshold = 0.05))

  # Drift is 5%, threshold 4% -> should trigger
  expect_true(Core$needs_rebalancing(w_current, w_target, threshold = 0.04))

  # Zero threshold -> always rebalance
  expect_true(Core$needs_rebalancing(w_current, w_target, threshold = 0))
})

# =============================================================================
# TEST: apply_turnover_cap
# =============================================================================
test_that("apply_turnover_cap limits turnover", {
  w_current <- c(0.50, 0.50)
  w_target <- c(0.30, 0.70)  # Turnover = 20%

  # Cap at 10% -> should scale changes
  result <- Core$apply_turnover_cap(w_current, w_target, turnover_cap = 0.10)

  # Changes should be halved: 0.50 -> 0.40, 0.50 -> 0.60
  expect_equal(sum(result), 1, tolerance = 0.001)  # Still sums to 1
  expect_equal(result[1], 0.40, tolerance = 0.01)
  expect_equal(result[2], 0.60, tolerance = 0.01)

  # No cap (100%) -> return target as-is
  result_no_cap <- Core$apply_turnover_cap(w_current, w_target, turnover_cap = 1.0)
  expect_equal(result_no_cap, w_target, tolerance = 0.001)
})

test_that("apply_turnover_cap handles zero cap", {
  w_current <- c(0.50, 0.50)
  w_target <- c(0.30, 0.70)

  # Zero or negative cap -> return target (no restriction)
  result <- Core$apply_turnover_cap(w_current, w_target, turnover_cap = 0)
  expect_equal(result, w_target)
})

# =============================================================================
# TEST: estimate_mu_sigma
# =============================================================================
test_that("estimate_mu_sigma returns correct structure", {
  # Create mock returns data
  set.seed(42)
  n <- 252
  rets <- matrix(rnorm(n * 3, mean = 0.0004, sd = 0.02), nrow = n, ncol = 3)
  colnames(rets) <- c("A", "B", "C")
  rets_xts <- xts::xts(rets, order.by = seq(Sys.Date() - n + 1, Sys.Date(), by = "day"))

  result <- Core$estimate_mu_sigma(rets_xts)

  # Check structure
  expect_true("mu" %in% names(result))
  expect_true("Sigma" %in% names(result))
  expect_true("tickers" %in% names(result))

  # mu should be length 3

  expect_equal(length(result$mu), 3)

  # Sigma should be 3x3
  expect_equal(dim(result$Sigma), c(3, 3))

  # Sigma should be positive definite (all eigenvalues > 0)
  eigenvalues <- eigen(result$Sigma)$values
  expect_true(all(eigenvalues > 0))
})

# =============================================================================
# TEST: shrink_covariance
# =============================================================================
test_that("shrink_covariance handles different methods", {
  # Create sample covariance matrix
  set.seed(42)
  Sigma <- matrix(c(0.04, 0.01, 0.01,
                    0.01, 0.09, 0.02,
                    0.01, 0.02, 0.16), nrow = 3, ncol = 3)

  # Method: none
  result_none <- Core$shrink_covariance(Sigma, method = "none", lambda = 0.2)
  expect_true(all(dim(result_none) == dim(Sigma)))

  # Method: diag
  result_diag <- Core$shrink_covariance(Sigma, method = "diag", lambda = 0.5)
  expect_true(all(dim(result_diag) == dim(Sigma)))
  # Off-diagonal elements should be reduced
  expect_true(abs(result_diag[1, 2]) < abs(Sigma[1, 2]))

  # Method: constcor
  result_constcor <- Core$shrink_covariance(Sigma, method = "constcor", lambda = 0.5)
  expect_true(all(dim(result_constcor) == dim(Sigma)))

  # All results should be positive definite
  expect_true(all(eigen(result_diag)$values > 0))
  expect_true(all(eigen(result_constcor)$values > 0))
})

# =============================================================================
# TEST: compute_frontier (basic structure)
# =============================================================================
test_that("compute_frontier returns valid structure", {
  # Simple 2-asset case
  mu <- c(0.08, 0.12)
  Sigma <- matrix(c(0.04, 0.01, 0.01, 0.09), nrow = 2)
  rf <- 0.02

  result <- Core$compute_frontier(mu, Sigma, rf = rf, n_grid = 20, w_max = 1)

  # Check structure
  expect_true("W" %in% names(result))
  expect_true("ret" %in% names(result))
  expect_true("vol" %in% names(result))
  expect_true("sharpe" %in% names(result))

  # W should have n_grid rows (or fewer if some are infeasible)
  expect_true(nrow(result$W) > 0)

  # Weights should sum to 1
  weight_sums <- rowSums(result$W)
  expect_true(all(abs(weight_sums - 1) < 0.001))

  # Sharpe ratios should be finite
  expect_true(all(is.finite(result$sharpe)))
})

# =============================================================================
# TEST: calc_rebalancing_trades
# =============================================================================
test_that("calc_rebalancing_trades computes trades correctly", {
  holdings <- data.frame(
    ticker = c("AAPL", "MSFT"),
    shares = c(10, 5),
    price = c(150, 300),
    stringsAsFactors = FALSE
  )

  # Current values: AAPL=1500, MSFT=1500 -> Total=3000
  # Current weights: 50%, 50%
  w_target <- c(0.40, 0.60)  # Target: AAPL=1200, MSFT=1800

  result <- Core$calc_rebalancing_trades(
    holdings = holdings,
    w_target = w_target,
    portfolio_value = 3000,
    tc_bps = 10
  )

  # Check structure
  expect_true("ticker" %in% colnames(result))
  expect_true("trade_value" %in% colnames(result))
  expect_true("trade_shares" %in% colnames(result))

  # AAPL: sell 300 (1500 -> 1200)
  expect_equal(result$trade_value[result$ticker == "AAPL"], -300, tolerance = 1)

  # MSFT: buy 300 (1500 -> 1800)
  expect_equal(result$trade_value[result$ticker == "MSFT"], 300, tolerance = 1)
})

# =============================================================================
# TEST: drift_weights
# =============================================================================
test_that("drift_weights computes drifted weights correctly", {
  w_initial <- c(0.50, 0.50)

  # Asset 1 returns 10%, Asset 2 returns 0%
  # After: 0.50*1.1 = 0.55, 0.50*1.0 = 0.50
  # Normalized: 0.55/1.05 ≈ 0.524, 0.50/1.05 ≈ 0.476
  rets_period <- matrix(c(0.10, 0.00), nrow = 1, ncol = 2)

  result <- Core$drift_weights(w_initial, rets_period)

  expect_equal(sum(result), 1, tolerance = 0.001)
  expect_true(result[1] > 0.50)  # First asset grew more
  expect_true(result[2] < 0.50)  # Second asset is now smaller share
})
