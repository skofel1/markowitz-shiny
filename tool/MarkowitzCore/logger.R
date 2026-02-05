# Simple structured logging module for Markowitz Portfolio Optimizer
# Provides consistent logging with timestamps, levels, and context

library(jsonlite)

# Log levels
LOG_LEVELS <- c(DEBUG = 1, INFO = 2, WARN = 3, ERROR = 4)

# Configuration
.log_config <- new.env()
.log_config$level <- "INFO"
.log_config$file <- NULL
.log_config$console <- TRUE

#' Set log level
#' @param level One of: DEBUG, INFO, WARN, ERROR
set_log_level <- function(level = "INFO") {
  level <- toupper(level)
  if (!(level %in% names(LOG_LEVELS))) {
    warning("Invalid log level. Using INFO.")
    level <- "INFO"
  }
  .log_config$level <- level
}

#' Set log file path
#' @param path Path to log file (NULL for no file logging)
set_log_file <- function(path = NULL) {
  if (!is.null(path)) {
    dir <- dirname(path)
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  }
  .log_config$file <- path
}

#' Set console logging
#' @param enabled TRUE/FALSE
set_console_logging <- function(enabled = TRUE) {
  .log_config$console <- enabled
}

#' Format log message as JSON
format_log_json <- function(level, message, context = NULL) {
  entry <- list(
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    level = level,
    message = message
  )

  if (!is.null(context) && length(context) > 0) {
    entry$context <- context
  }

  jsonlite::toJSON(entry, auto_unbox = TRUE)
}

#' Format log message for console
format_log_console <- function(level, message, context = NULL) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  prefix <- sprintf("[%s] [%s]", ts, level)

  ctx_str <- ""
  if (!is.null(context) && length(context) > 0) {
    ctx_parts <- sapply(names(context), function(k) {
      sprintf("%s=%s", k, as.character(context[[k]]))
    })
    ctx_str <- paste0(" {", paste(ctx_parts, collapse = ", "), "}")
  }

  paste0(prefix, " ", message, ctx_str)
}

#' Core logging function
#' @param level Log level
#' @param message Log message
#' @param context Named list of context values
log_message <- function(level, message, context = NULL) {
  level <- toupper(level)

  # Check if we should log this level
  if (LOG_LEVELS[[level]] < LOG_LEVELS[[.log_config$level]]) {
    return(invisible(NULL))
  }

  # Console output
  if (isTRUE(.log_config$console)) {
    cat(format_log_console(level, message, context), "\n")
  }

  # File output
  if (!is.null(.log_config$file)) {
    tryCatch({
      cat(format_log_json(level, message, context), "\n",
          file = .log_config$file, append = TRUE)
    }, error = function(e) {
      warning("Failed to write to log file: ", e$message)
    })
  }

  invisible(NULL)
}

#' Log debug message
log_debug <- function(message, ...) {
  context <- list(...)
  log_message("DEBUG", message, if (length(context) > 0) context else NULL)
}

#' Log info message
log_info <- function(message, ...) {
  context <- list(...)
  log_message("INFO", message, if (length(context) > 0) context else NULL)
}

#' Log warning message
log_warn <- function(message, ...) {
  context <- list(...)
  log_message("WARN", message, if (length(context) > 0) context else NULL)
}

#' Log error message
log_error <- function(message, ...) {
  context <- list(...)
  log_message("ERROR", message, if (length(context) > 0) context else NULL)
}

# Export functions
list(
  set_log_level = set_log_level,
  set_log_file = set_log_file,
  set_console_logging = set_console_logging,
  log_debug = log_debug,
  log_info = log_info,
  log_warn = log_warn,
  log_error = log_error,
  LOG_LEVELS = LOG_LEVELS
)
