library(modulr)

"tool/MarkowitzShiny/markowitz_shiny_app_provider" %requires% list(
  Core_ = "tool/MarkowitzCore/markowitz_core_provider"
) %provides% {
  
  # ============================================================================
  # DÉPENDANCES UI / DATA
  # ============================================================================
  library(shiny)
  library(bslib)
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(tibble)
  library(xts)
  
  # ============================================================================
  # PROVIDER
  # ============================================================================
  function() {
    
    Core <- Core_()
    
    # ==========================================================================
    # PALETTE DE COULEURS
    # ==========================================================================
    colors <- list(
      primary     = "#2C3E50",
      secondary   = "#18BC9C",
      tangent     = "#E74C3C",
      mvp         = "#27AE60",
      selection   = "#F39C12",
      frontier    = "#3498DB",
      inefficient = "#BDC3C7",
      grid        = "#ECF0F1",
      text        = "#2C3E50",
      muted       = "#95A5A6",
      purple      = "#9B59B6",
      orange      = "#E67E22",
      drawdown    = "#C0392B",
      benchmark1  = "#1ABC9C",
      benchmark2  = "#9B59B6"
    )
    
    # ==========================================================================
    # THÈME GGPLOT2
    # ==========================================================================
    theme_markowitz <- function(base_size = 13) {
      theme_minimal(base_size = base_size) +
        theme(
          plot.title = element_text(face = "bold", color = colors$primary, size = 16),
          plot.subtitle = element_text(color = colors$text, size = 11, margin = margin(b = 10)),
          plot.caption = element_text(color = colors$muted, size = 9, hjust = 1),
          axis.title = element_text(face = "bold", color = colors$text),
          axis.text = element_text(color = colors$text),
          panel.grid.minor = element_line(color = colors$grid, linetype = "dotted", size = 0.3),
          panel.grid.major = element_line(color = colors$grid, size = 0.5),
          legend.position = "bottom",
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)
        )
    }
    
    # ==========================================================================
    # CSS PERSONNALISÉ
    # ==========================================================================
    custom_css <- "
      .navbar { box-shadow: 0 2px 10px rgba(0,0,0,0.1); }
      .navbar-brand { font-weight: 700 !important; font-size: 1.3rem !important; }
      
      .card {
        transition: transform 0.2s ease, box-shadow 0.2s ease;
        border: none !important;
        box-shadow: 0 2px 12px rgba(0,0,0,0.08);
        border-radius: 0.75rem !important;
        overflow: hidden;
      }
      .card:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(0,0,0,0.12);
      }
      .card-header {
        background: linear-gradient(135deg, #2C3E50 0%, #34495E 100%) !important;
        color: white !important;
        font-weight: 600;
        padding: 1rem 1.25rem;
        border: none !important;
      }
      .card-body { padding: 1.25rem; }
      
      .sidebar {
        background-color: #F8F9FA !important;
        border-right: 1px solid #E9ECEF;
      }
      .sidebar .form-label {
        font-weight: 600;
        color: #2C3E50;
        margin-bottom: 0.5rem;
      }
      .sidebar hr { border-color: #DEE2E6; margin: 1.25rem 0; }
      
      .btn-primary {
        background: linear-gradient(135deg, #18BC9C 0%, #1ABC9C 100%) !important;
        border: none !important;
        font-weight: 600;
        padding: 12px 24px;
        transition: all 0.3s ease;
        box-shadow: 0 4px 15px rgba(24, 188, 156, 0.3);
      }
      .btn-primary:hover {
        background: linear-gradient(135deg, #16A085 0%, #1ABC9C 100%) !important;
        transform: translateY(-1px);
        box-shadow: 0 6px 20px rgba(24, 188, 156, 0.4);
      }
      
      .btn-default, .btn-secondary {
        background-color: #fff !important;
        border: 2px solid #2C3E50 !important;
        color: #2C3E50 !important;
        font-weight: 500;
        transition: all 0.2s ease;
      }
      .btn-default:hover, .btn-secondary:hover {
        background-color: #2C3E50 !important;
        color: white !important;
      }
      
      .table { font-size: 0.9rem; margin-bottom: 0; }
      .table thead th {
        background-color: #2C3E50 !important;
        color: white !important;
        font-weight: 600;
        border: none;
        padding: 0.75rem;
      }
      .table tbody tr:hover { background-color: rgba(24, 188, 156, 0.05); }
      .table td { vertical-align: middle; padding: 0.6rem 0.75rem; }
      
      .kpi-box {
        background: linear-gradient(135deg, #fff 0%, #f8f9fa 100%);
        border-radius: 0.5rem;
        padding: 1rem;
        margin-bottom: 0.75rem;
        border-left: 4px solid;
        box-shadow: 0 2px 8px rgba(0,0,0,0.04);
        transition: all 0.2s ease;
      }
      .kpi-box:hover { box-shadow: 0 4px 12px rgba(0,0,0,0.08); }
      .kpi-tangent { border-left-color: #E74C3C; }
      .kpi-mvp { border-left-color: #27AE60; }
      .kpi-selection { border-left-color: #F39C12; }
      
      .kpi-value { font-size: 1.1rem; font-weight: 700; color: #2C3E50; }
      .kpi-label { font-size: 0.75rem; color: #95A5A6; text-transform: uppercase; letter-spacing: 0.5px; }
      
      .form-control, .form-select {
        border: 1px solid #DEE2E6;
        border-radius: 0.5rem;
        transition: all 0.2s ease;
      }
      .form-control:focus, .form-select:focus {
        border-color: #18BC9C;
        box-shadow: 0 0 0 3px rgba(24, 188, 156, 0.15);
      }
      
      textarea.form-control { font-family: 'Monaco', 'Menlo', monospace; font-size: 0.9rem; }
      
      .irs--shiny .irs-bar {
        background: linear-gradient(90deg, #18BC9C, #1ABC9C) !important;
        border: none !important;
      }
      .irs--shiny .irs-handle {
        background: #2C3E50 !important;
        border: 3px solid white !important;
        box-shadow: 0 2px 8px rgba(0,0,0,0.2);
      }
      .irs--shiny .irs-single { background: #2C3E50 !important; border-radius: 4px; }
      
      .form-check-input:checked {
        background-color: #18BC9C !important;
        border-color: #18BC9C !important;
      }
      
      .section-header {
        display: flex;
        align-items: center;
        gap: 0.5rem;
        margin-bottom: 1rem;
        padding-bottom: 0.5rem;
        border-bottom: 2px solid #E9ECEF;
      }
      .section-header i { color: #18BC9C; }
      .section-header h5 { margin: 0; color: #2C3E50; font-weight: 600; }
      
      .form-text, .help-block {
        font-size: 0.8rem;
        color: #6C757D;
        font-style: italic;
        margin-top: 0.25rem;
      }
      
      .download-row { display: flex; gap: 0.5rem; margin-bottom: 0.5rem; }
      .download-row .btn { flex: 1; font-size: 0.85rem; padding: 0.5rem; }
      
      .alert { border: none; border-radius: 0.5rem; }
      .alert-info { background-color: rgba(52, 152, 219, 0.1); color: #2980B9; }
      .alert-warning { background-color: rgba(243, 156, 18, 0.1); color: #D68910; }
      .alert-success { background-color: rgba(39, 174, 96, 0.1); color: #1E8449; }
      
      .cash-summary {
        background: linear-gradient(135deg, #E8F6F3 0%, #D5F5E3 100%);
        border-radius: 0.5rem;
        padding: 1rem;
        margin-top: 1rem;
      }
      .cash-summary .label { color: #1E8449; font-weight: 500; }
      .cash-summary .value { color: #145A32; font-weight: 700; }
      
      .nav-link { font-weight: 500; transition: all 0.2s ease; }
      .nav-link:hover { background-color: rgba(255,255,255,0.1); }
      .nav-link.active { font-weight: 600; }
      
      .progress { border-radius: 0.5rem; overflow: hidden; }
      .progress-bar { background: linear-gradient(90deg, #18BC9C, #1ABC9C); }
      
      .goal-box {
        background: linear-gradient(135deg, #E8F8F5 0%, #D1F2EB 100%);
        border-radius: 0.75rem;
        padding: 1.25rem;
        text-align: center;
        border: 2px solid #1ABC9C;
      }
      .goal-box.success { border-color: #27AE60; background: linear-gradient(135deg, #E8F6F3 0%, #D5F5E3 100%); }
      .goal-box.warning { border-color: #F39C12; background: linear-gradient(135deg, #FEF9E7 0%, #FCF3CF 100%); }
      .goal-box.danger { border-color: #E74C3C; background: linear-gradient(135deg, #FDEDEC 0%, #FADBD8 100%); }
      
      .prob-value { font-size: 2.5rem; font-weight: 700; }
      .prob-label { font-size: 0.9rem; color: #5D6D7E; margin-top: 0.5rem; }
      
      @media (max-width: 768px) {
        .kpi-box { padding: 0.75rem; }
        .card-header { padding: 0.75rem 1rem; }
      }
    "
    
    # ==========================================================================
    # UI
    # ==========================================================================
    ui <- page_navbar(
      title = tags$span(
        tags$i(class = "fas fa-chart-line", style = "margin-right: 10px;"),
        "Markowitz Portfolio Optimizer V10"
      ),
      
      theme = bs_theme(
        version = 5,
        bootswatch = "flatly",
        primary = "#2C3E50",
        secondary = "#18BC9C",
        success = "#27AE60",
        warning = "#F39C12",
        danger = "#E74C3C",
        "navbar-bg" = "#2C3E50"
      ),
      
      header = tags$head(
        tags$link(
          rel = "stylesheet",
          href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
        ),
        tags$link(
          rel = "stylesheet",
          href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"
        ),
        tags$style(HTML(custom_css)),
        tags$style(HTML("body { font-family: 'Inter', sans-serif; }"))
      ),
      
      # =========================================================================
      # ONGLET 1 : OPTIMISATION
      # =========================================================================
      nav_panel(
        title = tags$span(tags$i(class = "fas fa-calculator"), " Optimisation"),
        
        layout_sidebar(
          sidebar = sidebar(
            width = 380,
            
            tags$div(
              class = "section-header",
              tags$i(class = "fas fa-tags"),
              tags$h5("Actifs")
            ),
            
            textAreaInput(
              "tickers", 
              label = NULL,
              value = "AAPL MSFT AMZN GOOGL NVDA",
              rows = 3,
              placeholder = "Ex: AAPL, MSFT, NOVN, UBS..."
            ),
            
            checkboxInput(
              "use_aliases", 
              tags$span(
                tags$i(class = "fas fa-flag", style = "color: #E74C3C;"),
                " Activer alias CH (.SW)"
              ), 
              TRUE
            ),
            tags$div(class = "form-text", "ABBN → ABBN.SW, NOVN → NOVN.SW, UBS → UBSG.SW"),
            
            tags$hr(),
            
            tags$div(
              class = "section-header",
              tags$i(class = "fas fa-calendar-alt"),
              tags$h5("Période d'analyse")
            ),
            
            dateRangeInput(
              "dates", 
              label = NULL,
              start = Sys.Date() - 365 * 5,
              end = Sys.Date(),
              language = "fr",
              separator = " au "
            ),
            tags$div(class = "form-text", "Recommandé: 3-10 ans pour une estimation stable."),
            
            tags$hr(),
            
            tags$div(
              class = "section-header",
              tags$i(class = "fas fa-wallet"),
              tags$h5("Capital & Devise")
            ),
            
            numericInput(
              "capital", 
              tags$span(tags$i(class = "fas fa-coins", style = "color: #F39C12;"), " Capital total (CHF)"),
              value = 10000, 
              min = 0,
              step = 1000
            ),
            
            tags$div(
              class = "alert alert-info",
              style = "padding: 0.75rem; margin-top: 0.5rem;",
              tags$i(class = "fas fa-info-circle"),
              " Devise base: CHF. Tous les prix sont convertis via FX Yahoo."
            ),
            
            radioButtons(
              "rounding_mode",
              tags$span(tags$i(class = "fas fa-calculator"), " Conversion en actions"),
              choices = c(
                "Floor (arrondi inférieur)" = "floor", 
                "Greedy (optimise le cash)" = "greedy"
              ),
              selected = "greedy",
              inline = FALSE
            ),
            
            tags$hr(),
            
            tags$div(
              class = "section-header",
              tags$i(class = "fas fa-sliders-h"),
              tags$h5("Paramètres")
            ),
            
            numericInput(
              "rf", 
              tags$span(tags$i(class = "fas fa-percentage"), " Taux sans risque (annuel)"),
              value = 0.02, 
              min = 0,
              max = 0.20,
              step = 0.005
            ),
            
            sliderInput(
              "wmax", 
              tags$span(tags$i(class = "fas fa-balance-scale"), " Poids max par titre"),
              min = 0.1, max = 1, value = 0.40, step = 0.05
            ),
            tags$div(class = "form-text", "Limite la concentration. Ex: 0.30 = max 30% par actif."),
            
            sliderInput(
              "ngrid", 
              tags$span(tags$i(class = "fas fa-th"), " Points sur la frontière"),
              min = 20, max = 120, value = 60, step = 10
            ),
            
            tags$hr(),
            
            tags$div(
              class = "section-header",
              tags$i(class = "fas fa-shield-alt"),
              tags$h5("Robustesse (Σ)")
            ),
            
            selectInput(
              "shrink_method",
              tags$span(tags$i(class = "fas fa-compress-arrows-alt"), " Shrinkage covariance"),
              choices = c(
                "Aucun (Σ brute)" = "none",
                "Diagonal (vers variances)" = "diag",
                "Corrélation constante" = "constcor"
              ),
              selected = "constcor"
            ),
            
            conditionalPanel(
              condition = "input.shrink_method != 'none'",
              sliderInput(
                "shrink_lambda",
                "Intensité λ",
                min = 0, max = 1, value = 0.20, step = 0.05
              ),
              tags$div(class = "form-text", "λ=0: Σ brute | λ=1: Σ cible. Reco: 0.15-0.30")
            ),
            
            tags$hr(),
            
            tags$div(
              class = "section-header",
              tags$i(class = "fas fa-crosshairs"),
              tags$h5("Robustesse (μ)")
            ),
            
            selectInput(
              "mu_method",
              tags$span(tags$i(class = "fas fa-chart-bar"), " Ajustement rendements"),
              choices = c(
                "Aucun (μ historique)" = "none",
                "Winsorize (coupe extrêmes)" = "winsor",
                "Shrink vers moyenne" = "shrink_mean",
                "Shrink vers 0 (conservateur)" = "shrink_zero"
              ),
              selected = "shrink_mean"
            ),
            
            conditionalPanel(
              condition = "input.mu_method == 'winsor'",
              sliderInput("mu_winsor", "Percentile coupé (p)", min = 0, max = 0.10, value = 0.02, step = 0.01),
              tags$div(class = "form-text", "p=0.02: coupe les 2% extrêmes.")
            ),
            
            conditionalPanel(
              condition = "input.mu_method == 'shrink_mean' || input.mu_method == 'shrink_zero'",
              sliderInput("mu_lambda", "Intensité λ", min = 0, max = 1, value = 0.40, step = 0.05),
              tags$div(class = "form-text", "λ=0: μ brut | λ=1: μ cible. Reco: 0.30-0.60")
            ),
            
            tags$hr(),
            
            actionButton(
              "run", 
              tags$span(
                tags$i(class = "fas fa-play-circle", style = "margin-right: 8px;"),
                "Calculer l'optimisation"
              ),
              class = "btn-primary w-100",
              style = "font-size: 1.1rem; margin-top: 0.5rem;"
            )
          ),
          
          layout_columns(
            col_widths = c(7, 5),
            
            card(
              card_header(
                tags$span(tags$i(class = "fas fa-chart-area", style = "margin-right: 8px;"), "Frontière efficiente")
              ),
              card_body(
                uiOutput("pick_ui"),
                plotOutput("frontier_plot", height = 480)
              )
            ),
            
            card(
              card_header(
                tags$span(tags$i(class = "fas fa-table", style = "margin-right: 8px;"), "Résultats (base CHF)")
              ),
              card_body(
                style = "max-height: 85vh; overflow-y: auto;",
                
                uiOutput("kpi_box"),
                tags$hr(),
                
                tags$div(
                  class = "section-header",
                  tags$i(class = "fas fa-bullseye", style = "color: #E74C3C;"),
                  tags$h5("Portefeuille Tangent", style = "color: #E74C3C;")
                ),
                tableOutput("tab_tan"),
                tags$hr(),
                
                tags$div(
                  class = "section-header",
                  tags$i(class = "fas fa-hand-pointer", style = "color: #F39C12;"),
                  tags$h5("Portefeuille Sélectionné", style = "color: #F39C12;")
                ),
                tableOutput("tab_sel"),
                tags$hr(),
                
                tags$div(
                  class = "section-header",
                  tags$i(class = "fas fa-shopping-cart"),
                  tags$h5("Ordres (actions entières)")
                ),
                
                tags$h6(tags$span(style = "color: #E74C3C;", tags$i(class = "fas fa-circle"), " Tangent")),
                tableOutput("orders_tan"),
                
                tags$h6(tags$span(style = "color: #F39C12;", tags$i(class = "fas fa-circle"), " Sélection"), style = "margin-top: 1rem;"),
                tableOutput("orders_sel"),
                
                uiOutput("cash_box"),
                tags$hr(),
                
                tags$div(
                  class = "section-header",
                  tags$i(class = "fas fa-chart-bar"),
                  tags$h5("Benchmarks (comparaison)")
                ),
                tableOutput("benchmarks_table"),
                tags$hr(),
                
                tags$div(
                  class = "section-header",
                  tags$i(class = "fas fa-exchange-alt"),
                  tags$h5("Devises & Taux FX")
                ),
                tableOutput("ccy_fx_table"),
                uiOutput("fx_note"),
                tags$hr(),
                
                tags$div(
                  class = "section-header",
                  tags$i(class = "fas fa-download"),
                  tags$h5("Exports CSV")
                ),
                tags$div(
                  class = "download-row",
                  downloadButton("dl_alloc_sel", tags$span(tags$i(class = "fas fa-file-csv"), " Alloc Sélection")),
                  downloadButton("dl_orders_sel", tags$span(tags$i(class = "fas fa-file-csv"), " Ordres Sélection"))
                ),
                tags$div(
                  class = "download-row",
                  downloadButton("dl_alloc_tan", tags$span(tags$i(class = "fas fa-file-csv"), " Alloc Tangent")),
                  downloadButton("dl_ccy_fx", tags$span(tags$i(class = "fas fa-file-csv"), " Devises & FX"))
                )
              )
            )
          )
        )
      ),
      
      # =========================================================================
      # ONGLET 2 : PROJECTION (AMÉLIORÉ V10)
      # =========================================================================
      nav_panel(
        title = tags$span(tags$i(class = "fas fa-chart-line"), " Projection"),
        
        layout_sidebar(
          sidebar = sidebar(
            width = 380,
            
            tags$div(
              class = "section-header",
              tags$i(class = "fas fa-clock"),
              tags$h5("Horizon")
            ),
            
            numericInput(
              "proj_years", 
              tags$span(tags$i(class = "fas fa-calendar"), " Durée (années)"),
              value = 10, min = 1, max = 50
            ),
            
            numericInput(
              "proj_monthly", 
              tags$span(tags$i(class = "fas fa-piggy-bank"), " Investissement mensuel (CHF)"),
              value = 500, min = 0, step = 50
            ),
            
            tags$hr(),
            
            # NOUVEAU V10: Objectif financier
            tags$div(
              class = "section-header",
              tags$i(class = "fas fa-bullseye"),
              tags$h5("Objectif financier")
            ),
            
            numericInput(
              "proj_goal",
              tags$span(tags$i(class = "fas fa-flag-checkered", style = "color: #27AE60;"), " Montant cible (CHF)"),
              value = 100000, min = 0, step = 10000
            ),
            tags$div(class = "form-text", "L'app calculera la probabilité d'atteindre cet objectif."),
            
            tags$hr(),
            
            tags$div(
              class = "section-header",
              tags$i(class = "fas fa-cogs"),
              tags$h5("Méthode")
            ),
            
            radioButtons(
              "proj_mode", NULL,
              choices = c(
                "Déterministe (moyenne)" = "det", 
                "Monte Carlo (simulations)" = "mc"
              ),
              selected = "mc"
            ),
            
            conditionalPanel(
              condition = "input.proj_mode == 'mc'",
              numericInput(
                "proj_nsims", 
                tags$span(tags$i(class = "fas fa-random"), " Nombre de simulations"),
                value = 5000, min = 500, max = 50000, step = 500
              ),
              numericInput(
                "proj_seed", 
                tags$span(tags$i(class = "fas fa-seedling"), " Seed (reproductibilité)"),
                value = 42, min = 1, step = 1
              )
            ),
            
            tags$div(
              class = "alert alert-warning",
              style = "margin-top: 1rem;",
              tags$i(class = "fas fa-exclamation-triangle"),
              " Basé sur μ/σ historiques. Ce n'est pas une prédiction."
            )
          ),
          
          layout_columns(
            col_widths = c(7, 5),
            
            card(
              card_header(tags$span(tags$i(class = "fas fa-chart-area"), " Projection de valeur")),
              card_body(plotOutput("proj_plot", height = 480))
            ),
            
            card(
              card_header(tags$span(tags$i(class = "fas fa-list-ol"), " Résumé & Objectif")),
              card_body(
                # NOUVEAU V10: Probabilité d'atteindre l'objectif
                uiOutput("goal_probability_box"),
                tags$hr(),
                tableOutput("proj_summary"),
                tags$hr(),
                uiOutput("proj_note")
              )
            )
          )
        )
      ),
      
      # =========================================================================
      # ONGLET 3 : BACKTEST (AMÉLIORÉ V10)
      # =========================================================================
      nav_panel(
        title = tags$span(tags$i(class = "fas fa-history"), " Backtest"),
        
        layout_sidebar(
          sidebar = sidebar(
            width = 380,
            
            tags$div(
              class = "section-header",
              tags$i(class = "fas fa-cog"),
              tags$h5("Configuration")
            ),
            
            numericInput(
              "bt_train_years", 
              tags$span(tags$i(class = "fas fa-graduation-cap"), " Fenêtre d'apprentissage (années)"),
              value = 3, min = 1, max = 15, step = 1
            ),
            
            selectInput(
              "bt_rebal_months", 
              tags$span(tags$i(class = "fas fa-sync"), " Fréquence de rebalancement"),
              choices = c("Mensuel" = 1, "Trimestriel" = 3, "Semestriel" = 6, "Annuel" = 12),
              selected = 3
            ),
            
            numericInput(
              "bt_tc_bps", 
              tags$span(tags$i(class = "fas fa-hand-holding-usd"), " Coût transaction (bps)"),
              value = 10, min = 0, max = 200, step = 5
            ),
            tags$div(class = "form-text", "10 bps = 0.10% par transaction."),
            
            tags$hr(),
            
            tags$div(
              class = "section-header",
              tags$i(class = "fas fa-crosshairs"),
              tags$h5("Stratégie")
            ),
            
            selectInput(
              "bt_pick_mode",
              tags$span(tags$i(class = "fas fa-bullseye"), " Portefeuille à chaque période"),
              choices = c(
                "Tangent (max Sharpe)" = "tangency",
                "MVP (variance minimale)" = "mvp",
                "Même profil que le slider" = "profile"
              ),
              selected = "tangency"
            ),
            
            tags$div(
              class = "alert alert-info",
              style = "margin-top: 1rem;",
              tags$i(class = "fas fa-info-circle"),
              " Walk-forward: ré-estime μ/Σ sur la fenêtre passée, puis applique sur la période suivante."
            ),
            
            tags$hr(),
            
            downloadButton(
              "dl_backtest", 
              tags$span(tags$i(class = "fas fa-file-csv"), " Télécharger le backtest"),
              class = "btn-secondary w-100"
            )
          ),
          
          # NOUVEAU V10: Layout avec graphique performance ET drawdown
          layout_column_wrap(
            width = 1,
            
            layout_columns(
              col_widths = c(7, 5),
              
              card(
                card_header(tags$span(tags$i(class = "fas fa-chart-line"), " Performance vs Benchmarks")),
                card_body(plotOutput("bt_plot", height = 350))
              ),
              
              card(
                card_header(tags$span(tags$i(class = "fas fa-tachometer-alt"), " Métriques détaillées")),
                card_body(
                  tableOutput("bt_summary"),
                  tags$hr(),
                  uiOutput("bt_note")
                )
              )
            ),
            
            # NOUVEAU V10: Graphique Drawdown
            card(
              card_header(tags$span(tags$i(class = "fas fa-chart-area"), " Drawdowns historiques")),
              card_body(plotOutput("bt_drawdown_plot", height = 250))
            )
          )
        )
      ),
      
      # =========================================================================
      # ONGLET 4 : DOCUMENTATION
      # =========================================================================
      nav_panel(
        title = tags$span(tags$i(class = "fas fa-book"), " Documentation"),
        
        layout_column_wrap(
          width = 1,
          card(
            card_header(tags$span(tags$i(class = "fas fa-info-circle"), " Guide d'utilisation")),
            card_body(
              style = "max-height: 80vh; overflow-y: auto;",
              uiOutput("doc_ui")
            )
          )
        )
      )
    )
    
    # ==========================================================================
    # SERVER
    # ==========================================================================
    server <- function(input, output, session) {
      
      base_ccy <- "CHF"
      
      # ------------------------------------------------------------------------
      # Reactive values
      # ------------------------------------------------------------------------
      rv <- reactiveValues(
        frontier   = NULL,
        tickers    = NULL,
        W          = NULL,
        tan_idx    = NULL,
        mvp_idx    = NULL,
        px_last    = NULL,
        tick_ccy   = NULL,
        fx_last    = NULL,
        px_chf     = NULL,
        benchmarks = NULL,
        ew_stats   = NULL,
        
        # V10: Stocker les données pour le backtest benchmark
        rets_chf   = NULL
      )
      
      # ------------------------------------------------------------------------
      # Clamp index
      # ------------------------------------------------------------------------
      clamp_idx <- function(i, n) {
        if (is.null(i) || !is.finite(i) || n < 1) return(1L)
        i <- as.integer(i)
        if (i < 1) return(1L)
        if (i > n) return(as.integer(n))
        i
      }
      
      # ------------------------------------------------------------------------
      # Robustesse μ
      # ------------------------------------------------------------------------
      apply_mu_robustness <- function(rets_xts, mu, method,
                                      winsor_p = 0.02, lambda = 0.30, freq = 252) {
        
        method <- match.arg(method, c("none", "winsor", "shrink_mean", "shrink_zero"))
        mu <- as.numeric(mu)
        n  <- length(mu)
        if (n == 0) return(mu)
        
        nm <- colnames(rets_xts)
        if (!is.null(nm) && length(nm) == n) names(mu) <- nm
        
        if (method == "none") return(mu)
        
        if (method == "winsor") {
          p <- as.numeric(winsor_p)
          if (!is.finite(p)) p <- 0.02
          p <- max(min(p, 0.10), 0)
          
          R <- tryCatch(as.matrix(xts::coredata(rets_xts)), error = function(e) NULL)
          if (is.null(R)) return(mu)
          
          for (j in seq_len(ncol(R))) {
            x <- R[, j]
            x <- x[is.finite(x)]
            if (length(x) < 10) next
            qlo <- as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE))
            qhi <- as.numeric(stats::quantile(x, probs = 1 - p, na.rm = TRUE, names = FALSE))
            R[, j] <- pmax(pmin(R[, j], qhi), qlo)
          }
          
          mu_w <- colMeans(R, na.rm = TRUE) * freq
          mu_w <- as.numeric(mu_w)
          if (!is.null(nm) && length(nm) == length(mu_w)) names(mu_w) <- nm
          return(mu_w)
        }
        
        lam <- as.numeric(lambda)
        if (!is.finite(lam)) lam <- 0
        lam <- max(min(lam, 1), 0)
        
        if (method == "shrink_mean") {
          target <- rep(mean(mu, na.rm = TRUE), n)
          return((1 - lam) * mu + lam * target)
        }
        
        if (method == "shrink_zero") {
          target <- rep(0, n)
          return((1 - lam) * mu + lam * target)
        }
        
        mu
      }
      
      # ------------------------------------------------------------------------
      # Helper : position relative du slider
      # ------------------------------------------------------------------------
      slider_profile_frac <- reactive({
        req(rv$frontier, input$pick_idx)
        n <- nrow(rv$frontier)
        if (n <= 1) return(0)
        (clamp_idx(input$pick_idx, n) - 1) / (n - 1)
      })
      
      # ------------------------------------------------------------------------
      # V10: Métriques avancées
      # ------------------------------------------------------------------------
      calc_sortino <- function(returns, rf = 0, freq = 252) {
        r <- as.numeric(returns)
        r <- r[is.finite(r)]
        if (length(r) < 10) return(NA_real_)
        
        excess <- r - rf/freq
        downside <- r[r < 0]
        if (length(downside) < 2) return(NA_real_)
        
        downside_vol <- sd(downside) * sqrt(freq)
        if (downside_vol == 0) return(NA_real_)
        
        mean(excess) * freq / downside_vol
      }
      
      calc_calmar <- function(equity, rf = 0) {
        v <- as.numeric(equity)
        v <- v[is.finite(v)]
        if (length(v) < 2) return(NA_real_)
        
        n <- length(v)
        cagr <- (v[n] / v[1])^(252 / n) - 1
        
        peak <- cummax(v)
        dd <- v / peak - 1
        mdd <- abs(min(dd, na.rm = TRUE))
        
        if (mdd == 0) return(NA_real_)
        (cagr - rf) / mdd
      }
      
      calc_win_rate <- function(returns) {
        r <- as.numeric(returns)
        r <- r[is.finite(r)]
        if (length(r) == 0) return(NA_real_)
        sum(r > 0) / length(r)
      }
      
      max_drawdown <- function(v) {
        v <- as.numeric(v)
        v <- v[is.finite(v)]
        if (length(v) < 2) return(NA_real_)
        peak <- cummax(v)
        dd <- v / peak - 1
        min(dd, na.rm = TRUE)
      }
      
      # ========================================================================
      # OBSERVER : bouton "Calculer"
      # ========================================================================
      observeEvent(input$run, {
        
        withProgress(message = "Optimisation en cours...", value = 0, {
          
          tryCatch({
            
            incProgress(0.05, detail = "Parsing des tickers")
            tks <- Core$normalize_tickers(input$tickers, use_aliases = input$use_aliases)
            
            validate(need(length(tks) >= 2, "Entrez au moins 2 tickers."))
            validate(need(
              input$wmax >= 1 / length(tks),
              paste0("wmax trop bas. Avec ", length(tks), " titres, il faut wmax ≥ ", round(1 / length(tks), 3))
            ))
            
            showNotification(paste0("Tickers analysés: ", paste(tks, collapse = ", ")), type = "message", duration = 4)
            
            incProgress(0.15, detail = "Récupération devises & prix (Yahoo)")
            tick_ccy <- Core$get_ticker_currency_yahoo(tks)
            px_native <- Core$get_prices_yahoo(tks, from = input$dates[1], to = input$dates[2])
            
            incProgress(0.35, detail = paste0("Conversion FX → ", base_ccy))
            conv <- Core$convert_prices_to_base(
              px         = px_native,
              ticker_ccy = tick_ccy,
              base_ccy   = base_ccy,
              from       = input$dates[1],
              to         = input$dates[2]
            )
            
            px <- conv$px_base
            rv$px_chf   <- px
            rv$tick_ccy <- conv$tick_ccy
            rv$fx_last  <- conv$fx_last
            
            px_last <- as.numeric(xts::last(px))
            names(px_last) <- colnames(px)
            rv$px_last <- px_last
            
            incProgress(0.55, detail = "Calcul rendements & estimation μ/Σ")
            rets <- Core$calc_log_returns(px)
            rv$rets_chf <- rets
            est  <- Core$estimate_mu_sigma(rets)
            
            mu_method <- if (!is.null(input$mu_method)) input$mu_method else "none"
            if (!is.null(mu_method) && mu_method != "none") {
              winsor_p <- if (!is.null(input$mu_winsor)) input$mu_winsor else 0.02
              mu_lam   <- if (!is.null(input$mu_lambda)) input$mu_lambda else 0.30
              est$mu <- apply_mu_robustness(
                rets_xts = rets, mu = est$mu, method = mu_method,
                winsor_p = winsor_p, lambda = mu_lam, freq = 252
              )
            }
            
            if (!is.null(input$shrink_method) && input$shrink_method != "none") {
              lam <- if (!is.null(input$shrink_lambda)) input$shrink_lambda else 0.20
              est$Sigma <- Core$shrink_covariance(est$Sigma, method = input$shrink_method, lambda = lam)
            }
            
            incProgress(0.75, detail = "Construction frontière efficiente")
            f <- Core$compute_frontier(est$mu, est$Sigma, rf = input$rf, n_grid = input$ngrid, w_max = input$wmax)
            
            df <- tibble::tibble(ret = f$ret, vol = f$vol, sharpe = f$sharpe)
            ord      <- order(df$vol, df$ret)
            df       <- df[ord, , drop = FALSE]
            W_sorted <- f$W[ord, , drop = FALSE]
            
            tan_idx <- which.max(df$sharpe)
            mvp_idx <- which.min(df$vol)
            df$idx  <- seq_len(nrow(df))
            
            rv$frontier <- df
            rv$tickers  <- est$tickers
            rv$W        <- W_sorted
            rv$tan_idx  <- tan_idx
            rv$mvp_idx  <- mvp_idx
            
            # V10: Benchmarks
            incProgress(0.85, detail = "Chargement benchmarks...")
            
            benchmarks <- tryCatch(
              Core$get_benchmarks(from = input$dates[1], to = input$dates[2], base_ccy = base_ccy),
              error = function(e) list()
            )
            
            ew_w <- rep(1 / length(est$tickers), length(est$tickers))
            ew_mu <- sum(ew_w * est$mu)
            ew_vol <- sqrt(as.numeric(t(ew_w) %*% est$Sigma %*% ew_w))
            ew_sharpe <- (ew_mu - input$rf) / ew_vol
            
            ew_stats <- list(name = "Equal-Weight", mu = ew_mu, vol = ew_vol, sharpe = ew_sharpe)
            
            rv$benchmarks <- benchmarks
            rv$ew_stats <- ew_stats
            
            incProgress(1.00, detail = "Terminé!")
            showNotification("Optimisation terminée ✅", type = "message", duration = 3)
            
          }, error = function(e) {
            rv$frontier <- NULL
            rv$tickers  <- NULL
            rv$W        <- NULL
            rv$tan_idx  <- NULL
            rv$mvp_idx  <- NULL
            rv$px_last  <- NULL
            rv$tick_ccy <- NULL
            rv$fx_last  <- NULL
            rv$px_chf   <- NULL
            rv$benchmarks <- NULL
            rv$ew_stats <- NULL
            rv$rets_chf <- NULL
            
            showNotification(paste("Erreur:", e$message), type = "error", duration = NULL)
          })
        })
      })
      
      # ========================================================================
      # UI : slider sélection
      # ========================================================================
      output$pick_ui <- renderUI({
        req(rv$frontier, rv$tan_idx)
        tags$div(
          style = "margin-bottom: 1rem;",
          sliderInput(
            "pick_idx", 
            tags$span(tags$i(class = "fas fa-hand-pointer", style = "color: #F39C12;"), " Sélectionner un portefeuille"),
            min = 1, max = nrow(rv$frontier), value = rv$tan_idx, step = 1, width = "100%"
          )
        )
      })
      
      # ========================================================================
      # Plot frontière efficiente
      # ========================================================================
      output$frontier_plot <- renderPlot({
        req(rv$frontier, rv$tan_idx, rv$mvp_idx, input$pick_idx)
        
        df  <- rv$frontier
        n   <- nrow(df)
        sel_i <- clamp_idx(input$pick_idx, n)
        
        tan <- df[rv$tan_idx, , drop = FALSE]
        sel <- df[sel_i, , drop = FALSE]
        mvp <- df[rv$mvp_idx, , drop = FALSE]
        
        mvp_ret <- mvp$ret
        
        df_efficient <- df %>%
          dplyr::filter(ret >= mvp_ret - 1e-9) %>%
          dplyr::arrange(vol)
        
        if (nrow(df_efficient) > 1) {
          keep <- rep(TRUE, nrow(df_efficient))
          max_ret_so_far <- df_efficient$ret[1]
          for (i in 2:nrow(df_efficient)) {
            if (df_efficient$ret[i] < max_ret_so_far - 1e-9) {
              keep[i] <- FALSE
            } else {
              max_ret_so_far <- max(max_ret_so_far, df_efficient$ret[i])
            }
          }
          df_efficient <- df_efficient[keep, ]
        }
        
        df_inefficient <- df %>%
          dplyr::filter(ret < mvp_ret - 1e-9) %>%
          dplyr::arrange(vol)
        
        rf_val <- input$rf
        cml_df <- NULL
        if (tan$vol > 0) {
          cml_slope <- (tan$ret - rf_val) / tan$vol
          vol_max <- max(df$vol) * 1.2
          cml_df <- data.frame(vol = c(0, vol_max), ret = c(rf_val, rf_val + cml_slope * vol_max))
        }
        
        bench_df <- NULL
        if (!is.null(rv$ew_stats)) {
          bench_df <- data.frame(name = rv$ew_stats$name, vol = rv$ew_stats$vol, ret = rv$ew_stats$mu, stringsAsFactors = FALSE)
        }
        if (!is.null(rv$benchmarks) && length(rv$benchmarks) > 0) {
          for (b in rv$benchmarks) {
            bench_df <- rbind(bench_df, data.frame(name = b$name, vol = b$vol, ret = b$mu, stringsAsFactors = FALSE))
          }
        }
        
        p <- ggplot()
        
        if (!is.null(cml_df)) {
          p <- p + geom_line(data = cml_df, aes(vol, ret), color = colors$muted, linetype = "dashed", size = 0.8, alpha = 0.6)
        }
        
        if (nrow(df_inefficient) > 0) {
          p <- p + 
            geom_line(data = df_inefficient, aes(vol, ret), color = colors$inefficient, size = 1, alpha = 0.4) +
            geom_point(data = df_inefficient, aes(vol, ret), color = colors$inefficient, size = 2, alpha = 0.3)
        }
        
        p <- p +
          geom_ribbon(data = df_efficient, aes(x = vol, ymin = min(df$ret) - 0.02, ymax = ret), fill = colors$frontier, alpha = 0.08) +
          geom_line(data = df_efficient, aes(vol, ret), color = colors$frontier, size = 2.5) +
          geom_point(data = df_efficient, aes(vol, ret), color = colors$frontier, size = 3.5, alpha = 0.8)
        
        if (!is.null(bench_df) && nrow(bench_df) > 0) {
          bench_colors <- c("Equal-Weight" = "#9B59B6", "S&P 500" = "#1ABC9C", "MSCI World" = "#3498DB", "Nasdaq 100" = "#E67E22")
          for (i in seq_len(nrow(bench_df))) {
            bname <- bench_df$name[i]
            bcol <- ifelse(bname %in% names(bench_colors), bench_colors[[bname]], "#7F8C8D")
            p <- p +
              geom_point(data = bench_df[i, ], aes(vol, ret), color = "white", size = 8, shape = 18) +
              geom_point(data = bench_df[i, ], aes(vol, ret), color = bcol, size = 6, shape = 18) +
              annotate("text", x = bench_df$vol[i], y = bench_df$ret[i], label = bname, hjust = 0.5, vjust = -1.2, color = bcol, fontface = "bold", size = 3.2)
          }
        }
        
        p <- p +
          geom_point(data = mvp, aes(vol, ret), color = "white", size = 11) +
          geom_point(data = mvp, aes(vol, ret), color = colors$mvp, size = 7, shape = 15) +
          geom_point(data = tan, aes(vol, ret), color = "white", size = 11) +
          geom_point(data = tan, aes(vol, ret), color = colors$tangent, size = 7, shape = 16) +
          geom_point(data = sel, aes(vol, ret), color = "white", size = 11) +
          geom_point(data = sel, aes(vol, ret), color = colors$selection, size = 7, shape = 17) +
          annotate("text", x = tan$vol, y = tan$ret, label = paste0("  Tangent (SR: ", round(tan$sharpe, 2), ")"), hjust = 0, vjust = -1.5, color = colors$tangent, fontface = "bold", size = 4) +
          annotate("text", x = mvp$vol, y = mvp$ret, label = paste0("  MVP (σ: ", scales::percent(mvp$vol, 0.1), ")"), hjust = 0, vjust = 2.2, color = colors$mvp, fontface = "bold", size = 4)
        
        if (sel_i != rv$tan_idx && sel_i != rv$mvp_idx) {
          p <- p + annotate("text", x = sel$vol, y = sel$ret, label = "  Sélection", hjust = 0, vjust = -1.5, color = colors$selection, fontface = "bold", size = 4)
        }
        
        p <- p +
          scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0.05, 0)) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0.08, 0)) +
          theme_markowitz() +
          labs(
            x = "Volatilité annualisée (σ)",
            y = "Rendement attendu annualisé (μ)",
            title = "Frontière efficiente de Markowitz",
            subtitle = "● Tangent   ■ MVP   ▲ Sélection   ◆ Benchmarks",
            caption = paste0("Base: ", base_ccy, " | rf = ", scales::percent(rf_val, 0.1), " | Source: Yahoo Finance")
          )
        
        p
      })
      
      # ========================================================================
      # KPI Box
      # ========================================================================
      output$kpi_box <- renderUI({
        req(rv$frontier, rv$tan_idx, rv$mvp_idx, input$pick_idx)
        
        df <- rv$frontier
        n <- nrow(df)
        sel_i <- clamp_idx(input$pick_idx, n)
        
        tan <- df[rv$tan_idx, , drop = FALSE]
        mvp <- df[rv$mvp_idx, , drop = FALSE]
        sel <- df[sel_i, , drop = FALSE]
        
        make_kpi_card <- function(label, icon, data, css_class, border_color) {
          tags$div(
            class = paste("kpi-box", css_class),
            tags$div(
              class = "d-flex justify-content-between align-items-center mb-2",
              tags$span(class = "fw-bold", style = paste0("color: ", border_color, "; font-size: 0.95rem;"),
                        tags$i(class = icon, style = "margin-right: 6px;"), label
              ),
              tags$span(class = "badge", style = paste0("background-color: ", border_color, "; color: white;"),
                        paste0("SR: ", round(data$sharpe, 2))
              )
            ),
            tags$div(
              class = "row text-center",
              tags$div(class = "col-6", tags$div(class = "kpi-label", "Rendement"), tags$div(class = "kpi-value", scales::percent(data$ret, accuracy = 0.1))),
              tags$div(class = "col-6", tags$div(class = "kpi-label", "Volatilité"), tags$div(class = "kpi-value", scales::percent(data$vol, accuracy = 0.1)))
            )
          )
        }
        
        tags$div(
          tags$div(class = "section-header", tags$i(class = "fas fa-chart-pie"), tags$h5("KPIs (annualisés)")),
          make_kpi_card("Tangent (Max Sharpe)", "fas fa-bullseye", tan, "kpi-tangent", colors$tangent),
          make_kpi_card("MVP (Min Variance)", "fas fa-shield-alt", mvp, "kpi-mvp", colors$mvp),
          make_kpi_card("Sélection", "fas fa-hand-pointer", sel, "kpi-selection", colors$selection)
        )
      })
      
      # ========================================================================
      # Tables allocations / ordres
      # ========================================================================
      build_alloc_table <- function(w) {
        cap <- input$capital
        tibble::tibble(Ticker = rv$tickers, Poids = w, Montant = w * cap) %>%
          dplyr::mutate(
            Poids = scales::percent(Poids, accuracy = 0.01),
            Montant = paste0("CHF ", scales::comma(Montant, accuracy = 0.01))
          )
      }
      
      greedy_fill_cash <- function(df, cash_left) {
        if (!is.finite(cash_left) || cash_left <= 0) return(df)
        guard <- 0
        while (cash_left > 0 && guard < 10000) {
          guard <- guard + 1
          buyable <- which(df$Prix <= cash_left & is.finite(df$Prix) & df$Prix > 0)
          if (length(buyable) == 0) break
          invested_now <- sum(df$Shares * df$Prix, na.rm = TRUE)
          if (!is.finite(invested_now) || invested_now <= 0) {
            j <- buyable[which.min(df$Prix[buyable])]
          } else {
            w_cur <- (df$Shares * df$Prix) / invested_now
            gap <- df$Poids - w_cur
            gap[!is.finite(gap)] <- -Inf
            gap[-buyable] <- -Inf
            j <- which.max(gap)
            if (!is.finite(gap[j]) || gap[j] == -Inf) j <- buyable[which.min(df$Prix[buyable])]
          }
          df$Shares[j] <- df$Shares[j] + 1
          cash_left <- cash_left - df$Prix[j]
        }
        df
      }
      
      build_orders_table <- function(w, px_last_chf) {
        cap <- input$capital
        df <- tibble::tibble(
          Ticker = rv$tickers, Poids = as.numeric(w), Prix = as.numeric(px_last_chf[rv$tickers]),
          Montant_cible = as.numeric(w) * cap
        ) %>%
          dplyr::mutate(Shares = floor(Montant_cible / Prix), Shares = ifelse(is.finite(Shares) & Shares >= 0, Shares, 0))
        
        invested0 <- sum(df$Shares * df$Prix, na.rm = TRUE)
        cash_left <- cap - invested0
        
        if (identical(input$rounding_mode, "greedy")) df <- greedy_fill_cash(df, cash_left)
        
        df <- df %>% dplyr::mutate(Montant_investi = Shares * Prix)
        invested <- sum(df$Montant_investi, na.rm = TRUE)
        cash_left2 <- cap - invested
        
        df_disp <- df %>%
          dplyr::mutate(
            Poids = scales::percent(Poids, accuracy = 0.01),
            Prix = round(Prix, 2),
            Montant_cible = paste0("CHF ", scales::comma(Montant_cible, accuracy = 0.01)),
            Montant_investi = paste0("CHF ", scales::comma(Montant_investi, accuracy = 0.01))
          ) %>%
          dplyr::select(Ticker, Poids, Prix, Shares, Montant_cible, Montant_investi)
        
        list(table = df_disp, cash_left = cash_left2, invested = invested)
      }
      
      output$tab_tan <- renderTable({
        req(rv$W, rv$tan_idx, rv$tickers)
        build_alloc_table(as.numeric(rv$W[rv$tan_idx, ]))
      })
      
      output$tab_sel <- renderTable({
        req(rv$W, rv$tickers, input$pick_idx)
        n <- nrow(rv$W)
        sel_i <- clamp_idx(input$pick_idx, n)
        build_alloc_table(as.numeric(rv$W[sel_i, ]))
      })
      
      output$orders_tan <- renderTable({
        req(rv$W, rv$tan_idx, rv$tickers, rv$px_last)
        res <- build_orders_table(as.numeric(rv$W[rv$tan_idx, ]), rv$px_last)
        res$table
      })
      
      output$orders_sel <- renderTable({
        req(rv$W, rv$tickers, rv$px_last, input$pick_idx)
        n <- nrow(rv$W)
        sel_i <- clamp_idx(input$pick_idx, n)
        res <- build_orders_table(as.numeric(rv$W[sel_i, ]), rv$px_last)
        res$table
      })
      
      output$cash_box <- renderUI({
        req(rv$W, rv$tickers, rv$px_last, input$pick_idx)
        n <- nrow(rv$W)
        sel_i <- clamp_idx(input$pick_idx, n)
        res <- build_orders_table(as.numeric(rv$W[sel_i, ]), rv$px_last)
        cap <- input$capital
        pct_inv <- if (cap > 0) res$invested / cap else NA_real_
        
        tags$div(
          class = "cash-summary",
          tags$div(
            class = "row",
            tags$div(class = "col-4 text-center",
                     tags$div(class = "label", tags$i(class = "fas fa-check-circle"), " Investi"),
                     tags$div(class = "value", paste0("CHF ", scales::comma(res$invested, accuracy = 0.01)))
            ),
            tags$div(class = "col-4 text-center",
                     tags$div(class = "label", tags$i(class = "fas fa-coins"), " Cash"),
                     tags$div(class = "value", paste0("CHF ", scales::comma(res$cash_left, accuracy = 0.01)))
            ),
            tags$div(class = "col-4 text-center",
                     tags$div(class = "label", tags$i(class = "fas fa-percentage"), " Taux"),
                     tags$div(class = "value", scales::percent(pct_inv, accuracy = 0.1))
            )
          )
        )
      })
      
      # ========================================================================
      # Benchmarks table
      # ========================================================================
      output$benchmarks_table <- renderTable({
        req(rv$ew_stats)
        
        rows <- list()
        rows[[1]] <- data.frame(
          Benchmark = "Equal-Weight (vos actifs)",
          Rendement = scales::percent(rv$ew_stats$mu, accuracy = 0.1),
          Volatilite = scales::percent(rv$ew_stats$vol, accuracy = 0.1),
          Sharpe = round(rv$ew_stats$sharpe, 2),
          stringsAsFactors = FALSE
        )
        
        if (!is.null(rv$benchmarks) && length(rv$benchmarks) > 0) {
          for (b in rv$benchmarks) {
            sharpe_b <- (b$mu - input$rf) / b$vol
            rows[[length(rows) + 1]] <- data.frame(
              Benchmark = b$name,
              Rendement = scales::percent(b$mu, accuracy = 0.1),
              Volatilite = scales::percent(b$vol, accuracy = 0.1),
              Sharpe = round(sharpe_b, 2),
              stringsAsFactors = FALSE
            )
          }
        }
        
        do.call(rbind, rows)
      }, striped = TRUE, hover = TRUE)
      
      output$ccy_fx_table <- renderTable({
        req(rv$tickers, rv$tick_ccy, rv$fx_last, rv$px_last)
        tibble::tibble(
          Ticker = rv$tickers,
          Devise = unname(rv$tick_ccy[rv$tickers]),
          FX_vers_CHF = unname(rv$fx_last[rv$tickers]),
          Prix_CHF = unname(rv$px_last[rv$tickers])
        ) %>%
          dplyr::mutate(
            Devise = ifelse(is.na(Devise) | !nzchar(Devise), base_ccy, Devise),
            FX_vers_CHF = ifelse(is.na(FX_vers_CHF), 1, FX_vers_CHF),
            FX_vers_CHF = round(FX_vers_CHF, 4),
            Prix_CHF = round(Prix_CHF, 2)
          )
      })
      
      output$fx_note <- renderUI({
        req(rv$tickers, rv$tick_ccy)
        if (any(is.na(rv$tick_ccy[rv$tickers]) | !nzchar(rv$tick_ccy[rv$tickers]))) {
          tags$div(class = "alert alert-warning mt-2", style = "padding: 0.5rem;",
                   tags$i(class = "fas fa-exclamation-triangle"), " Certains tickers n'ont pas de devise Yahoo → CHF par défaut."
          )
        } else {
          tags$div(class = "form-text mt-2",
                   tags$i(class = "fas fa-check-circle", style = "color: #27AE60;"), " Conversion FX appliquée sur toute la période."
          )
        }
      })
      
      # ========================================================================
      # DOWNLOAD HANDLERS
      # ========================================================================
      output$dl_alloc_sel <- downloadHandler(
        filename = function() paste0("allocation_selection_", Sys.Date(), ".csv"),
        content = function(file) {
          req(rv$W, rv$tickers, input$pick_idx)
          n <- nrow(rv$W)
          sel_i <- clamp_idx(input$pick_idx, n)
          w <- as.numeric(rv$W[sel_i, ])
          df <- tibble::tibble(Ticker = rv$tickers, Weight = w)
          utils::write.csv(df, file, row.names = FALSE)
        }
      )
      
      output$dl_alloc_tan <- downloadHandler(
        filename = function() paste0("allocation_tangent_", Sys.Date(), ".csv"),
        content = function(file) {
          req(rv$W, rv$tan_idx, rv$tickers)
          w <- as.numeric(rv$W[rv$tan_idx, ])
          df <- tibble::tibble(Ticker = rv$tickers, Weight = w)
          utils::write.csv(df, file, row.names = FALSE)
        }
      )
      
      output$dl_orders_sel <- downloadHandler(
        filename = function() paste0("orders_selection_", Sys.Date(), ".csv"),
        content = function(file) {
          req(rv$W, rv$tickers, rv$px_last, input$pick_idx)
          n <- nrow(rv$W)
          sel_i <- clamp_idx(input$pick_idx, n)
          res <- build_orders_table(as.numeric(rv$W[sel_i, ]), rv$px_last)
          utils::write.csv(res$table, file, row.names = FALSE)
        }
      )
      
      output$dl_ccy_fx <- downloadHandler(
        filename = function() paste0("currencies_fx_", Sys.Date(), ".csv"),
        content = function(file) {
          req(rv$tickers, rv$tick_ccy, rv$fx_last, rv$px_last)
          df <- tibble::tibble(
            Ticker = rv$tickers,
            Currency = unname(rv$tick_ccy[rv$tickers]),
            FX_to_CHF = unname(rv$fx_last[rv$tickers]),
            LastPrice_CHF = unname(rv$px_last[rv$tickers])
          )
          utils::write.csv(df, file, row.names = FALSE)
        }
      )
      
      # ========================================================================
      # PROJECTION (V10 - avec objectif)
      # ========================================================================
      selected_portfolio <- reactive({
        req(rv$frontier, rv$W, rv$tickers, input$pick_idx)
        n <- nrow(rv$W)
        sel_i <- clamp_idx(input$pick_idx, n)
        list(
          idx = sel_i,
          w   = as.numeric(rv$W[sel_i, ]),
          mu  = as.numeric(rv$frontier$ret[sel_i]),
          vol = as.numeric(rv$frontier$vol[sel_i])
        )
      })
      
      project_det <- function(cap0, mu_annual, monthly_add, years) {
        n_months <- years * 12
        r_m <- (1 + mu_annual)^(1/12) - 1
        v <- numeric(n_months + 1)
        v[1] <- cap0
        for (t in 1:n_months) v[t + 1] <- (v[t] * (1 + r_m)) + monthly_add
        tibble::tibble(month = 0:n_months, value = v)
      }
      
      project_mc <- function(cap0, mu_annual, vol_annual, monthly_add, years, nsims, seed = 42) {
        set.seed(seed)
        n_months <- years * 12
        dt <- 1/12
        drift <- (mu_annual - 0.5 * vol_annual^2) * dt
        shock_sd <- vol_annual * sqrt(dt)
        
        Z <- matrix(stats::rnorm(n_months * nsims), nrow = n_months, ncol = nsims)
        R <- exp(drift + shock_sd * Z)
        
        V <- matrix(NA_real_, nrow = n_months + 1, ncol = nsims)
        V[1, ] <- cap0
        for (t in 1:n_months) V[t + 1, ] <- (V[t, ] * R[t, ]) + monthly_add
        
        qs <- c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)
        qmat <- apply(V, 1, stats::quantile, probs = qs, na.rm = TRUE)
        qdf <- tibble::as_tibble(t(qmat))
        colnames(qdf) <- paste0("p", qs * 100)
        qdf <- dplyr::mutate(qdf, month = 0:n_months, .before = 1)
        
        keep <- min(30, nsims)
        idx <- sample.int(nsims, keep)
        paths <- V[, idx, drop = FALSE]
        paths_df <- tibble::tibble(
          month = rep(0:n_months, times = keep),
          sim = rep(seq_len(keep), each = n_months + 1),
          value = as.numeric(paths)
        )
        
        final_values <- V[nrow(V), ]
        
        list(q = qdf, paths = paths_df, final_values = final_values)
      }
      
      # V10: Probabilité d'atteindre l'objectif
      output$goal_probability_box <- renderUI({
        req(rv$frontier, input$proj_years, input$proj_monthly, input$proj_goal)
        
        sp <- selected_portfolio()
        cap0 <- input$capital
        years <- as.integer(input$proj_years)
        addm <- as.numeric(input$proj_monthly)
        goal <- as.numeric(input$proj_goal)
        
        if (identical(input$proj_mode, "det")) {
          df <- project_det(cap0, sp$mu, addm, years)
          final <- df$value[nrow(df)]
          prob <- if (final >= goal) 1 else 0
          prob_text <- if (final >= goal) "Objectif atteint ✅" else "Objectif non atteint ❌"
          prob_class <- if (final >= goal) "success" else "danger"
        } else {
          ns <- as.integer(input$proj_nsims)
          seed <- as.integer(input$proj_seed)
          res <- project_mc(cap0, sp$mu, sp$vol, addm, years, nsims = ns, seed = seed)
          
          prob <- sum(res$final_values >= goal) / length(res$final_values)
          prob_text <- paste0(scales::percent(prob, accuracy = 0.1), " de chance")
          prob_class <- if (prob >= 0.7) "success" else if (prob >= 0.4) "warning" else "danger"
        }
        
        tags$div(
          class = paste("goal-box", prob_class),
          tags$div(
            tags$i(class = "fas fa-flag-checkered", style = "font-size: 1.5rem; margin-bottom: 0.5rem;")
          ),
          tags$div(class = "prob-value", style = paste0("color: ", 
                                                        if (prob_class == "success") colors$mvp else if (prob_class == "warning") colors$selection else colors$tangent, ";"),
                   if (identical(input$proj_mode, "det")) {
                     if (prob == 1) "100%" else "0%"
                   } else {
                     scales::percent(prob, accuracy = 0.1)
                   }
          ),
          tags$div(class = "prob-label", 
                   paste0("Probabilité d'atteindre ", scales::comma(goal), " CHF")
          ),
          if (identical(input$proj_mode, "mc")) {
            tags$div(
              class = "mt-2",
              style = "font-size: 0.8rem; color: #5D6D7E;",
              paste0("Basé sur ", scales::comma(input$proj_nsims), " simulations Monte Carlo")
            )
          }
        )
      })
      
      output$proj_plot <- renderPlot({
        req(rv$frontier, input$proj_years, input$proj_monthly)
        sp <- selected_portfolio()
        cap0 <- input$capital
        years <- as.integer(input$proj_years)
        addm <- as.numeric(input$proj_monthly)
        goal <- as.numeric(input$proj_goal)
        
        if (identical(input$proj_mode, "det")) {
          df <- project_det(cap0, sp$mu, addm, years)
          
          p <- ggplot(df, aes(month/12, value)) +
            geom_area(fill = colors$secondary, alpha = 0.2) +
            geom_line(color = colors$secondary, size = 1.5) +
            geom_point(data = df[nrow(df), ], aes(month/12, value), color = colors$secondary, size = 4)
          
          if (!is.null(goal) && is.finite(goal) && goal > 0) {
            p <- p + geom_hline(yintercept = goal, linetype = "dashed", color = colors$tangent, size = 1)
          }
          
          p <- p +
            theme_markowitz() +
            scale_y_continuous(labels = scales::comma) +
            scale_x_continuous(breaks = seq(0, years, by = max(1, years/10)), expand = c(0.02, 0)) +
            labs(
              x = "Années", y = "Valeur du portefeuille (CHF)",
              title = "Projection déterministe",
              subtitle = paste0("μ = ", scales::percent(sp$mu, 0.1), " | σ = ", scales::percent(sp$vol, 0.1), " | +", scales::comma(addm), " CHF/mois"),
              caption = "Hypothèse: rendement moyen constant"
            )
          
        } else {
          ns <- as.integer(input$proj_nsims)
          seed <- as.integer(input$proj_seed)
          res <- project_mc(cap0, sp$mu, sp$vol, addm, years, nsims = ns, seed = seed)
          q <- res$q
          
          p <- ggplot() +
            geom_ribbon(data = q, aes(x = month/12, ymin = p5, ymax = p95), fill = colors$secondary, alpha = 0.1) +
            geom_ribbon(data = q, aes(x = month/12, ymin = p10, ymax = p90), fill = colors$secondary, alpha = 0.15) +
            geom_ribbon(data = q, aes(x = month/12, ymin = p25, ymax = p75), fill = colors$secondary, alpha = 0.25) +
            geom_line(data = res$paths, aes(month/12, value, group = sim), alpha = 0.06, color = colors$primary) +
            geom_line(data = q, aes(month/12, p50), size = 1.5, color = colors$secondary) +
            geom_line(data = q, aes(month/12, p10), linetype = "dashed", color = colors$muted, size = 0.8) +
            geom_line(data = q, aes(month/12, p90), linetype = "dashed", color = colors$muted, size = 0.8) +
            geom_point(data = q[nrow(q), ], aes(month/12, p50), color = colors$secondary, size = 4)
          
          if (!is.null(goal) && is.finite(goal) && goal > 0) {
            p <- p + geom_hline(yintercept = goal, linetype = "dashed", color = colors$tangent, size = 1) +
              annotate("text", x = years * 0.02, y = goal, label = paste0(" Objectif: ", scales::comma(goal), " CHF"), hjust = 0, vjust = -0.5, color = colors$tangent, fontface = "bold", size = 3.5)
          }
          
          p <- p +
            theme_markowitz() +
            scale_y_continuous(labels = scales::comma, expand = c(0.05, 0)) +
            scale_x_continuous(breaks = seq(0, years, by = max(1, years/10)), expand = c(0.02, 0)) +
            labs(
              x = "Années", y = "Valeur du portefeuille (CHF)",
              title = "Projection Monte Carlo",
              subtitle = paste0("Médiane + bandes p10/p90 | μ = ", scales::percent(sp$mu, 0.1), " | σ = ", scales::percent(sp$vol, 0.1), " | ", scales::comma(ns), " simulations"),
              caption = paste0("+", scales::comma(addm), " CHF/mois | Hypothèse: log-returns GBM")
            )
        }
        
        p
      })
      
      output$proj_summary <- renderTable({
        sp <- selected_portfolio()
        cap0 <- input$capital
        years <- as.integer(input$proj_years)
        addm <- as.numeric(input$proj_monthly)
        n_months <- years * 12
        total_contrib <- cap0 + addm * n_months
        
        if (identical(input$proj_mode, "det")) {
          df <- project_det(cap0, sp$mu, addm, years)
          final <- df$value[nrow(df)]
          gain <- final - total_contrib
          
          tibble::tibble(
            Metric = c("Capital initial", "Contributions totales", "Total investi", "Valeur finale", "Plus-value"),
            Value = c(
              paste0("CHF ", scales::comma(cap0)),
              paste0("CHF ", scales::comma(addm * n_months)),
              paste0("CHF ", scales::comma(total_contrib)),
              paste0("CHF ", scales::comma(final, accuracy = 1)),
              paste0("CHF ", scales::comma(gain, accuracy = 1))
            )
          )
        } else {
          res <- project_mc(cap0, sp$mu, sp$vol, addm, years, nsims = as.integer(input$proj_nsims), seed = as.integer(input$proj_seed))
          q_last <- res$q[nrow(res$q), ]
          
          tibble::tibble(
            Metric = c("Capital initial", "Contributions totales", "Total investi", "Valeur p10 (pessimiste)", "Valeur p50 (médiane)", "Valeur p90 (optimiste)"),
            Value = c(
              paste0("CHF ", scales::comma(cap0)),
              paste0("CHF ", scales::comma(addm * n_months)),
              paste0("CHF ", scales::comma(total_contrib)),
              paste0("CHF ", scales::comma(q_last$p10, accuracy = 1)),
              paste0("CHF ", scales::comma(q_last$p50, accuracy = 1)),
              paste0("CHF ", scales::comma(q_last$p90, accuracy = 1))
            )
          )
        }
      })
      
      output$proj_note <- renderUI({
        sp <- selected_portfolio()
        tags$div(
          class = "small text-muted",
          tags$p(tags$i(class = "fas fa-info-circle", style = "color: #3498DB;"), " Hypothèses: μ/σ issus de l'historique, appliqués au futur."),
          tags$p(tags$i(class = "fas fa-chart-line", style = "color: #18BC9C;"), paste0(" Portefeuille sélectionné: μ = ", scales::percent(sp$mu, 0.1), ", σ = ", scales::percent(sp$vol, 0.1))),
          tags$p(tags$i(class = "fas fa-exclamation-triangle", style = "color: #F39C12;"), " Ceci n'est pas une prédiction. Les performances passées ne garantissent pas les résultats futurs.")
        )
      })
      
      # ========================================================================
      # BACKTEST V10 (avec benchmarks et drawdown)
      # ========================================================================
      
      run_walk_forward <- function(px_chf, cap0, rf, n_grid, w_max,
                                   train_years, rebal_months, tc_bps,
                                   pick_mode, profile_frac,
                                   shrink_method, shrink_lambda,
                                   mu_method, mu_winsor, mu_lambda) {
        
        rets <- px_chf / xts::lag.xts(px_chf, 1) - 1
        rets <- na.omit(rets)
        
        train_obs <- max(60, as.integer(round(train_years * 252)))
        k_months  <- as.integer(rebal_months)
        if (!is.finite(k_months) || k_months < 1) k_months <- 3
        
        ep <- xts::endpoints(rets, on = "months", k = k_months)
        ep <- ep[ep > 0]
        ep <- unique(c(ep, nrow(rets)))
        ep <- ep[ep >= train_obs]
        
        if (length(ep) < 2) stop("Backtest impossible: fenêtre d'apprentissage trop longue vs historique.")
        
        n_assets <- ncol(rets)
        w_prev <- rep(0, n_assets)
        
        dates_all <- c()
        values_all <- c()
        
        V <- cap0
        tc_rate <- as.numeric(tc_bps) / 10000
        if (!is.finite(tc_rate) || tc_rate < 0) tc_rate <- 0
        
        turnover_acc <- 0
        
        first_date_idx <- ep[1] + 1
        
        for (i in seq_len(length(ep) - 1)) {
          
          reb_pos  <- ep[i]
          next_pos <- ep[i + 1]
          
          train_slice <- rets[(reb_pos - train_obs + 1):reb_pos, , drop = FALSE]
          
          est <- Core$estimate_mu_sigma(train_slice)
          
          if (!is.null(mu_method) && mu_method != "none") {
            est$mu <- apply_mu_robustness(
              rets_xts = train_slice, mu = est$mu, method = mu_method,
              winsor_p = mu_winsor, lambda = mu_lambda, freq = 252
            )
          }
          
          if (!is.null(shrink_method) && shrink_method != "none") {
            est$Sigma <- Core$shrink_covariance(est$Sigma, method = shrink_method, lambda = shrink_lambda)
          }
          
          f <- Core$compute_frontier(est$mu, est$Sigma, rf = rf, n_grid = n_grid, w_max = w_max)
          
          if (pick_mode == "tangency") {
            j <- which.max(f$sharpe)
          } else if (pick_mode == "mvp") {
            j <- which.min(f$vol)
          } else {
            m <- nrow(f$W)
            j <- 1 + as.integer(round(profile_frac * (m - 1)))
            j <- clamp_idx(j, m)
          }
          
          w_new <- as.numeric(f$W[j, ])
          
          turnover <- 0.5 * sum(abs(w_new - w_prev))
          turnover_acc <- turnover_acc + turnover
          
          cost <- V * tc_rate * turnover
          V <- V - cost
          if (!is.finite(V) || V <= 0) V <- 0
          
          seg <- rets[(reb_pos + 1):next_pos, , drop = FALSE]
          if (nrow(seg) == 0) next
          
          rp <- as.numeric(as.matrix(seg) %*% w_new)
          v_path <- V * cumprod(1 + rp)
          
          dates_all  <- c(dates_all, as.Date(zoo::index(seg)))
          values_all <- c(values_all, as.numeric(v_path))
          
          V <- as.numeric(tail(v_path, 1))
          w_prev <- w_new
        }
        
        eq <- xts::xts(values_all, order.by = as.Date(dates_all))
        colnames(eq) <- "value_chf"
        
        # V10: Equal-Weight benchmark
        rets_bt_period <- rets[first_date_idx:nrow(rets), , drop = FALSE]
        ew_w <- rep(1/n_assets, n_assets)
        rets_ew <- as.numeric(as.matrix(rets_bt_period) %*% ew_w)
        eq_ew <- cap0 * cumprod(1 + rets_ew)
        eq_ew_xts <- xts::xts(eq_ew, order.by = as.Date(zoo::index(rets_bt_period)))
        
        r_eq <- eq / xts::lag.xts(eq, 1) - 1
        r_eq <- na.omit(r_eq)
        
        n_days <- nrow(r_eq)
        cagr <- if (n_days > 0) (as.numeric(xts::last(eq)) / cap0)^(252 / n_days) - 1 else NA_real_
        vol  <- if (n_days > 1) stats::sd(as.numeric(r_eq), na.rm = TRUE) * sqrt(252) else NA_real_
        shp  <- if (is.finite(vol) && vol > 0) (cagr - rf) / vol else NA_real_
        mdd  <- max_drawdown(eq)
        
        sortino <- calc_sortino(r_eq, rf = rf)
        calmar <- calc_calmar(eq, rf = rf)
        win_rate <- calc_win_rate(r_eq)
        
        # EW metrics
        r_ew <- eq_ew_xts / xts::lag.xts(eq_ew_xts, 1) - 1
        r_ew <- na.omit(r_ew)
        n_days_ew <- nrow(r_ew)
        cagr_ew <- if (n_days_ew > 0) (as.numeric(xts::last(eq_ew_xts)) / cap0)^(252 / n_days_ew) - 1 else NA_real_
        vol_ew <- if (n_days_ew > 1) stats::sd(as.numeric(r_ew), na.rm = TRUE) * sqrt(252) else NA_real_
        shp_ew <- if (is.finite(vol_ew) && vol_ew > 0) (cagr_ew - rf) / vol_ew else NA_real_
        mdd_ew <- max_drawdown(eq_ew_xts)
        
        list(
          equity = eq,
          equity_ew = eq_ew_xts,
          metrics = list(
            CAGR = cagr, Vol = vol, Sharpe = shp, MaxDrawdown = mdd,
            Sortino = sortino, Calmar = calmar, WinRate = win_rate, Turnover = turnover_acc
          ),
          metrics_ew = list(
            CAGR = cagr_ew, Vol = vol_ew, Sharpe = shp_ew, MaxDrawdown = mdd_ew
          )
        )
      }
      
      backtest_res <- reactive({
        req(rv$px_chf, rv$frontier, rv$W, rv$tickers)
        
        cap0 <- input$capital
        train_years <- as.numeric(input$bt_train_years)
        rebal_months <- as.integer(input$bt_rebal_months)
        tc_bps <- as.numeric(input$bt_tc_bps)
        
        pick_mode <- input$bt_pick_mode
        prof <- slider_profile_frac()
        
        shrink_method <- input$shrink_method
        shrink_lambda <- if (!is.null(input$shrink_lambda)) input$shrink_lambda else 0.20
        
        mu_method <- input$mu_method
        mu_winsor <- if (!is.null(input$mu_winsor)) input$mu_winsor else 0.02
        mu_lambda <- if (!is.null(input$mu_lambda)) input$mu_lambda else 0.30
        
        run_walk_forward(
          px_chf = rv$px_chf, cap0 = cap0, rf = input$rf, n_grid = input$ngrid, w_max = input$wmax,
          train_years = train_years, rebal_months = rebal_months, tc_bps = tc_bps,
          pick_mode = pick_mode, profile_frac = prof,
          shrink_method = shrink_method, shrink_lambda = shrink_lambda,
          mu_method = mu_method, mu_winsor = mu_winsor, mu_lambda = mu_lambda
        )
      })
      
      # V10: Graphique avec benchmarks
      output$bt_plot <- renderPlot({
        res <- backtest_res()
        eq <- res$equity
        eq_ew <- res$equity_ew
        
        # Aligner les dates
        common_dates <- intersect(as.Date(zoo::index(eq)), as.Date(zoo::index(eq_ew)))
        
        df_strat <- data.frame(date = as.Date(zoo::index(eq)), value = as.numeric(eq$value_chf), type = "Stratégie Markowitz")
        df_ew <- data.frame(date = as.Date(zoo::index(eq_ew)), value = as.numeric(eq_ew), type = "Equal-Weight")
        
        df_all <- rbind(df_strat, df_ew)
        
        ggplot(df_all, aes(date, value, color = type)) +
          geom_line(size = 1.2) +
          scale_color_manual(values = c("Stratégie Markowitz" = colors$frontier, "Equal-Weight" = colors$purple)) +
          theme_markowitz() +
          theme(legend.position = "bottom", legend.title = element_blank()) +
          scale_y_continuous(labels = scales::comma) +
          scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
          labs(
            x = NULL, y = "Valeur du portefeuille (CHF)",
            title = "Performance: Stratégie vs Equal-Weight",
            subtitle = paste0("Fenêtre: ", input$bt_train_years, " ans | Rebal: ", 
                              c("1" = "mensuel", "3" = "trimestriel", "6" = "semestriel", "12" = "annuel")[as.character(input$bt_rebal_months)],
                              " | Coûts: ", input$bt_tc_bps, " bps"),
            caption = "Walk-forward: ré-estimation périodique μ/Σ"
          )
      })
      
      # V10: Graphique Drawdown
      output$bt_drawdown_plot <- renderPlot({
        res <- backtest_res()
        eq <- res$equity
        eq_ew <- res$equity_ew
        
        # Drawdown stratégie
        v_strat <- as.numeric(eq)
        peak_strat <- cummax(v_strat)
        dd_strat <- (v_strat / peak_strat - 1) * 100
        
        # Drawdown EW
        v_ew <- as.numeric(eq_ew)
        peak_ew <- cummax(v_ew)
        dd_ew <- (v_ew / peak_ew - 1) * 100
        
        df_strat <- data.frame(date = as.Date(zoo::index(eq)), dd = dd_strat, type = "Stratégie Markowitz")
        df_ew <- data.frame(date = as.Date(zoo::index(eq_ew)), dd = dd_ew, type = "Equal-Weight")
        
        df_all <- rbind(df_strat, df_ew)
        
        ggplot(df_all, aes(date, dd, fill = type)) +
          geom_area(alpha = 0.4, position = "identity") +
          geom_line(aes(color = type), size = 0.8) +
          scale_fill_manual(values = c("Stratégie Markowitz" = colors$drawdown, "Equal-Weight" = colors$purple)) +
          scale_color_manual(values = c("Stratégie Markowitz" = colors$drawdown, "Equal-Weight" = colors$purple)) +
          theme_markowitz() +
          theme(legend.position = "bottom", legend.title = element_blank()) +
          scale_y_continuous(labels = function(x) paste0(x, "%")) +
          scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
          labs(
            x = NULL, y = "Drawdown (%)",
            title = "Drawdowns historiques",
            subtitle = "Perte depuis le plus haut historique"
          )
      })
      
      # V10: Métriques étendues
      output$bt_summary <- renderTable({
        res <- backtest_res()
        m <- res$metrics
        m_ew <- res$metrics_ew
        
        tibble::tibble(
          Métrique = c(
            "CAGR (rendement annualisé)",
            "Volatilité annualisée",
            "Ratio de Sharpe",
            "Ratio de Sortino",
            "Ratio de Calmar",
            "Drawdown maximum",
            "Win Rate (jours positifs)",
            "Turnover cumulé"
          ),
          Stratégie = c(
            scales::percent(m$CAGR, accuracy = 0.1),
            scales::percent(m$Vol, accuracy = 0.1),
            round(m$Sharpe, 2),
            round(m$Sortino, 2),
            round(m$Calmar, 2),
            scales::percent(m$MaxDrawdown, accuracy = 0.1),
            scales::percent(m$WinRate, accuracy = 0.1),
            paste0(round(m$Turnover * 100, 1), "%")
          ),
          `Equal-Weight` = c(
            scales::percent(m_ew$CAGR, accuracy = 0.1),
            scales::percent(m_ew$Vol, accuracy = 0.1),
            round(m_ew$Sharpe, 2),
            "-",
            "-",
            scales::percent(m_ew$MaxDrawdown, accuracy = 0.1),
            "-",
            "0%"
          )
        )
      }, striped = TRUE, hover = TRUE)
      
      output$bt_note <- renderUI({
        tags$div(
          class = "small",
          tags$div(
            class = "alert alert-info", style = "padding: 0.75rem;",
            tags$p(tags$i(class = "fas fa-lightbulb"), tags$strong(" Interprétation:")),
            tags$ul(
              style = "margin-bottom: 0; padding-left: 1.2rem;",
              tags$li("Sharpe > 1: bon ratio rendement/risque"),
              tags$li("Sortino: comme Sharpe mais ne pénalise que la vol négative"),
              tags$li("Calmar: CAGR / Max Drawdown"),
              tags$li("Win Rate > 50%: majorité de jours positifs")
            )
          ),
          tags$p(class = "text-muted mt-2",
                 tags$i(class = "fas fa-exclamation-triangle", style = "color: #F39C12;"),
                 " Les performances passées ne garantissent pas les résultats futurs."
          )
        )
      })
      
      output$dl_backtest <- downloadHandler(
        filename = function() paste0("walkforward_backtest_", Sys.Date(), ".csv"),
        content = function(file) {
          res <- backtest_res()
          eq <- res$equity
          eq_ew <- res$equity_ew
          
          df <- tibble::tibble(
            date = as.Date(zoo::index(eq)),
            strategy_chf = as.numeric(eq$value_chf)
          )
          
          df_ew <- tibble::tibble(
            date = as.Date(zoo::index(eq_ew)),
            equalweight_chf = as.numeric(eq_ew)
          )
          
          df_merged <- merge(df, df_ew, by = "date", all = TRUE)
          utils::write.csv(df_merged, file, row.names = FALSE)
        }
      )
      
      # ========================================================================
      # DOCUMENTATION
      # ========================================================================
      output$doc_ui <- renderUI({
        if (!requireNamespace("markdown", quietly = TRUE)) {
          return(tags$div(
            class = "alert alert-warning",
            tags$i(class = "fas fa-exclamation-triangle"),
            " Le package 'markdown' est requis. Installe-le avec: ", tags$code("install.packages('markdown')")
          ))
        }
        
        doc_path <- normalizePath(file.path("docs", "documentation.md"), mustWork = FALSE)
        if (!file.exists(doc_path)) {
          return(tags$div(
            class = "alert alert-warning",
            tags$i(class = "fas fa-file-alt"),
            " Fichier ", tags$code("docs/documentation.md"), " introuvable.",
            tags$br(), "Crée ce fichier pour afficher la documentation."
          ))
        }
        
        shiny::withMathJax(shiny::includeMarkdown(doc_path))
      })
      
    } # fin server
    
    shinyApp(ui, server)
  }
}
