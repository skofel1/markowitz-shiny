# Markowitz Portfolio Optimizer - Dockerfile for AWS Deployment
# Base image: rocker/shiny with R 4.3
FROM rocker/shiny:4.3

# Maintainer
LABEL maintainer="Endreas"
LABEL description="Markowitz Portfolio Optimizer Shiny Application"

# System dependencies for R packages
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c( \
    'shiny', \
    'bslib', \
    'dplyr', \
    'ggplot2', \
    'scales', \
    'tibble', \
    'xts', \
    'zoo', \
    'quantmod', \
    'PerformanceAnalytics', \
    'quadprog', \
    'Matrix', \
    'modulr', \
    'markdown', \
    'jsonlite', \
    'logger' \
), repos='https://cloud.r-project.org/')"

# Create app directory structure
RUN mkdir -p /srv/shiny-server/markowitz-app/tool/MarkowitzCore \
    && mkdir -p /srv/shiny-server/markowitz-app/tool/MarkowitzShiny \
    && mkdir -p /srv/shiny-server/markowitz-app/docs \
    && mkdir -p /srv/shiny-server/markowitz-app/data

# Copy application files
COPY tool/MarkowitzCore/markowitz_core_provider.R /srv/shiny-server/markowitz-app/tool/MarkowitzCore/
COPY tool/MarkowitzShiny/markowitz_shiny_app_provider.R /srv/shiny-server/markowitz-app/tool/MarkowitzShiny/
COPY docs/documentation.md /srv/shiny-server/markowitz-app/docs/
COPY scripts/run_local.R /srv/shiny-server/markowitz-app/

# Create app.R entry point
RUN echo 'library(modulr) \n\
setwd("/srv/shiny-server/markowitz-app") \n\
source("tool/MarkowitzCore/markowitz_core_provider.R") \n\
source("tool/MarkowitzShiny/markowitz_shiny_app_provider.R") \n\
app <- make("tool/MarkowitzShiny/markowitz_shiny_app_provider")() \n\
shiny::runApp(app, host = "0.0.0.0", port = 3838)' > /srv/shiny-server/markowitz-app/app.R

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server/markowitz-app

# Create volume for persistent data (user settings, logs)
VOLUME /srv/shiny-server/markowitz-app/data

# Expose port
EXPOSE 3838

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=30s --retries=3 \
    CMD curl -f http://localhost:3838/ || exit 1

# Set working directory
WORKDIR /srv/shiny-server/markowitz-app

# Run as shiny user
USER shiny

# Start the application
CMD ["R", "-e", "source('app.R')"]
