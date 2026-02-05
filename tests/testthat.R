# Test runner for Markowitz Portfolio Optimizer
# Run with: Rscript tests/testthat.R

library(testthat)
library(modulr)

# Set working directory to project root
setwd(dirname(dirname(normalizePath(commandArgs(trailingOnly = FALSE)[grep("--file=", commandArgs(trailingOnly = FALSE))]))))

# Source the modules
source("tool/MarkowitzCore/markowitz_core_provider.R")

# Run tests
test_dir("tests/testthat", reporter = "summary")
