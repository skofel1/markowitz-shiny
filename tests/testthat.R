# Test runner for Markowitz Portfolio Optimizer
# Run with: Rscript tests/testthat.R

# Ensure user library is on the search path
user_lib <- Sys.getenv("R_LIBS_USER")
if (nzchar(user_lib) && dir.exists(path.expand(user_lib))) {
  .libPaths(c(path.expand(user_lib), .libPaths()))
}

library(testthat)
library(modulr)

# Set working directory to project root
setwd(dirname(dirname(normalizePath(commandArgs(trailingOnly = FALSE)[grep("--file=", commandArgs(trailingOnly = FALSE))]))))

# Source the modules
source("tool/MarkowitzCore/markowitz_core_provider.R")

# Run tests
test_dir("tests/testthat", reporter = "summary")
