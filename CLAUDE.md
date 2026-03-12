# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Markowitz-Shiny is a R Shiny application for portfolio optimization using Modern Portfolio Theory (Markowitz). It features efficient frontier calculation, Monte-Carlo simulation, walk-forward backtesting, stress testing, and real portfolio rebalancing with FX conversion to CHF.

## Commands

### Run locally (development)
```bash
Rscript scripts/run_local.R
# App available at http://localhost:3838
# Auto-reload enabled
```

### Run tests
```bash
Rscript tests/testthat.R
```

### Docker (production)
```bash
docker-compose up -d
# App available at http://localhost:3838
```

## Architecture

### Framework: modulr (dependency injection)

The app uses the `modulr` package with `%requires%` / `%provides%` syntax for module dependency injection. This is **not** a golem app.

**Module dependency chain:**
```
app.R
  └─ tool/MarkowitzShiny/markowitz_shiny_app_provider  (UI + Server)
       └─ tool/MarkowitzCore/markowitz_core_provider    (quantitative engine)
            └─ tool/MarkowitzCore/logger                (structured JSON logging)
```

### Key Files

| File | Role | Lines |
|------|------|-------|
| `app.R` | Entry point: login overlay + admin FAB + modulr bootstrap | ~580 |
| `tool/MarkowitzCore/markowitz_core_provider.R` | Quantitative engine (optimization, risk, FX, persistence) | ~1130 |
| `tool/MarkowitzShiny/markowitz_shiny_app_provider.R` | Full Shiny UI + server (bslib, plotly, ggplot2) | ~2900 |
| `tool/MarkowitzCore/logger.R` | JSON structured logging | ~70 |
| `tests/testthat/test-markowitz-core.R` | Unit tests for core functions | ~260 |
| `docs/documentation.md` | Mathematical reference (MathJax) | - |

### Core Engine Functions (markowitz_core_provider.R)

The core module exposes functions via a named list. Key function groups:

- **Data**: `parse_tickers()`, `normalize_tickers()`, `get_prices_yahoo()`, `get_fx_series()`, `convert_prices_to_base()`
- **Estimation**: `calc_log_returns()`, `estimate_mu_sigma()`, `shrink_covariance()` (none/diag/constcor)
- **Optimization**: `compute_frontier()`, `compute_frontier_sectors()`, `pick_tangency()`, `solve_min_var_target()`
- **Risk**: `calc_var()`, `calc_cvar()`, `run_stress_test()`, `run_all_stress_tests()`
- **Portfolio management**: `calc_drift()`, `needs_rebalancing()`, `calc_rebalancing_trades()`, `apply_turnover_cap()`
- **Persistence**: `save_settings()`, `load_settings()` (JSON in `data/` folder)

### Shiny App Structure (markowitz_shiny_app_provider.R)

Five main tabs: Optimisation, Monte-Carlo, Backtest, Stress Test, Holdings, Documentation.

The provider returns a function that produces `list(ui, server)`. The UI uses `bslib` (Bootstrap 5) with custom CSS and a `theme_markowitz()` ggplot2 theme. Interactive plots use plotly.

Reactive values (`rv`) hold state: tickers, prices (raw + FX-converted), returns, mu/Sigma estimates, frontier weights, and user settings.

### Authentication (app.R)

Custom login overlay with hardcoded credentials (plain text in `app.R`). Admin users see a floating action button (FAB) for ops documentation (SSH commands, server management).

## Dependencies

No renv.lock or DESCRIPTION file. Packages are installed directly (see Dockerfile):

**Core**: `modulr`, `shiny`, `bslib`, `quantmod`, `PerformanceAnalytics`, `quadprog`, `Matrix`, `xts`
**UI**: `ggplot2`, `plotly`, `scales`, `DT`, `dplyr`, `tibble`, `markdown`
**Infra**: `jsonlite`, `rlang`

## Conventions

- All financial calculations use log-returns annualized with factor 252
- Prices are converted to CHF base currency via FX series before any computation
- Covariance matrices are forced positive-definite via `Matrix::nearPD()`
- The Quadratic Programming solver is Goldfarb-Idnani (`quadprog::solve.QP`)
- User settings are persisted as JSON files in `data/` directory
- Logs are written as JSON to `logs/markowitz_YYYYMMDD.log` (level configurable via `SHINY_LOG_LEVEL` env var)
- Swiss tickers use alias normalization (e.g., ABB -> ABBN.SW, UBS -> UBSG.SW)

## Docker Environment Variables

- `SHINY_LOG_LEVEL`: DEBUG, INFO, WARN, ERROR (default: INFO)
- `YAHOO_RATE_LIMIT`: Rate limit for Yahoo Finance API calls
