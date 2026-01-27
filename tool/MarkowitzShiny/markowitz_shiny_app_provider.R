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
  library(stringr)
  library(tibble)
  
  # ============================================================================
  # PROVIDER : renvoie une fonction qui construit et lance l'application Shiny
  # ============================================================================
  function() {
    
    # --------------------------------------------------------------------------
    # Instancie le module Core (récupère la liste de fonctions métier)
    # IMPORTANT : Core_() renvoie une LISTE de fonctions (pas une closure)
    # --------------------------------------------------------------------------
    Core <- Core_()
    
    # ==========================================================================
    # UI
    # ==========================================================================
    ui <- page_navbar(
      title = "Markowitz Shiny (V5) — Base CHF + FX",
      theme = bs_theme(version = 5, bootswatch = "flatly"),
      
      # =========================================================================
      # ONGLET 1 : OPTIMISATION
      # =========================================================================
      nav_panel(
        "Optimisation",
        layout_sidebar(
          
          # --------------------------------------------------------------------
          # SIDEBAR : paramètres utilisateur
          # --------------------------------------------------------------------
          sidebar = sidebar(
            width = 360,
            
            textAreaInput(
              "tickers", "Tickers (Yahoo)",
              value = "AAPL MSFT AMZN GOOGL NVDA",
              rows = 3
            ),
            
            checkboxInput("use_aliases", "Aide tickers CH (alias .SW)", TRUE),
            helpText("Ex: ABBN → ABBN.SW, NOVARTIS → NOVN.SW, UBS → UBSG.SW, NESTLE → NESN.SW"),
            
            dateRangeInput(
              "dates", "Période",
              start = Sys.Date() - 365 * 3,
              end   = Sys.Date()
            ),
            
            numericInput(
              "capital", "Capital total (CHF)",
              value = 10000, min = 0
            ),
            
            helpText("Devise base FIXE: CHF (les prix et rendements sont convertis en CHF via FX Yahoo)."),
            
            radioButtons(
              "rounding_mode",
              "Conversion en actions",
              choices = c("Floor (simple)" = "floor", "Floor + optimisation cash" = "greedy"),
              selected = "greedy",
              inline = TRUE
            ),
            
            numericInput(
              "rf", "Taux sans risque annuel (ex: 0.02)",
              value = 0.02, step = 0.005
            ),
            
            # ---------------------------
            # Robustesse (Σ : covariance)
            # ---------------------------
            selectInput(
              "shrink_method",
              "Robustesse (shrinkage Σ)",
              choices = c(
                "Aucun" = "none",
                "Diagonal (vers variances)" = "diag",
                "Corrélation constante" = "constcor"
              ),
              selected = "constcor"
            ),
            
            # Affiche λ uniquement si shrink activé
            conditionalPanel(
              condition = "input.shrink_method != 'none'",
              sliderInput(
                "shrink_lambda",
                "Intensité shrinkage (λ)",
                min = 0, max = 1, value = 0.20, step = 0.05
              ),
              helpText("λ=0 : Σ brute ; λ=1 : Σ cible. Recommandé: Corrélation constante, λ≈0.1–0.3")
            ),
            
            # ---------------------------
            # Robustesse (μ : rendements)
            # ---------------------------
            selectInput(
              "mu_method",
              "Robustesse (μ : rendements attendus)",
              choices = c(
                "Aucun (μ historique)" = "none",
                "Winsorize (coupe extrêmes)" = "winsor",
                "Shrink vers moyenne" = "shrink_mean",
                "Shrink vers 0 (conservateur)" = "shrink_zero"
              ),
              selected = "shrink_mean"
            ),
            
            # Winsor → on montre p
            conditionalPanel(
              condition = "input.mu_method == 'winsor'",
              sliderInput(
                "mu_winsor",
                "Winsor tail (p)",
                min = 0, max = 0.10, value = 0.02, step = 0.01
              ),
              helpText("p=0.02 coupe les 2% extrêmes (bas/haut) des rendements journaliers.")
            ),
            
            # Shrink μ → on montre λ
            conditionalPanel(
              condition = "input.mu_method == 'shrink_mean' || input.mu_method == 'shrink_zero'",
              sliderInput(
                "mu_lambda",
                "Intensité shrink μ (λ)",
                min = 0, max = 1, value = 0.30, step = 0.05
              ),
              helpText("λ=0 : μ brut ; λ=1 : μ cible (moyenne ou 0). Reco: 0.3–0.6.")
            ),
            
            sliderInput(
              "wmax", "Poids max par titre",
              min = 0.1, max = 1, value = 1, step = 0.05
            ),
            sliderInput(
              "ngrid", "Nb points frontière",
              min = 20, max = 100, value = 50, step = 5
            ),
            
            actionButton("run", "Calculer", class = "btn-primary w-100")
          ),
          
          # --------------------------------------------------------------------
          # MAIN PANEL : graphique + tables d'allocation
          # --------------------------------------------------------------------
          layout_columns(
            col_widths = c(7, 5),
            
            # Carte graphique
            card(
              card_header("Frontière efficiente + tangence"),
              uiOutput("pick_ui"),
              plotOutput("frontier_plot", height = 450)
            ),
            
            # Carte allocations
            card(
              card_header("Allocations (base CHF)"),
              uiOutput("kpi_box"),
              hr(),
              
              h5("Portefeuille tangent (max Sharpe)"),
              tableOutput("tab_tan"),
              hr(),
              
              h5("Portefeuille sélectionné"),
              tableOutput("tab_sel"),
              hr(),
              
              h5("Ordres (actions entières) — Prix en CHF"),
              h6("Tangent"),
              tableOutput("orders_tan"),
              h6("Sélection"),
              tableOutput("orders_sel"),
              uiOutput("cash_box"),
              hr(),
              
              h5("Devises & FX (Yahoo)"),
              tableOutput("ccy_fx_table"),
              uiOutput("fx_note")
            )
          )
        )
      ),
      
      # =========================================================================
      # ONGLET 2 : PROJECTION
      # =========================================================================
      nav_panel(
        "Projection du portefeuille sélectionné",
        layout_sidebar(
          sidebar = sidebar(
            width = 360,
            
            numericInput("proj_years", "Horizon (années)", value = 5, min = 1, max = 50),
            numericInput("proj_monthly", "Investissement mensuel (CHF)", value = 500, min = 0, step = 50),
            
            radioButtons(
              "proj_mode", "Méthode",
              choices = c("Déterministe (moyenne)" = "det", "Simulation (Monte Carlo)" = "mc"),
              selected = "mc",
              inline = TRUE
            ),
            
            conditionalPanel(
              condition = "input.proj_mode == 'mc'",
              numericInput("proj_nsims", "Nb simulations", value = 2000, min = 200, max = 20000, step = 200),
              numericInput("proj_seed", "Seed (reproductible)", value = 42, min = 1, step = 1)
            ),
            
            helpText("Note: la projection se base sur μ/Σ estimés sur la période historique choisie. Ce n’est pas une prédiction.")
          ),
          
          layout_columns(
            col_widths = c(7, 5),
            card(
              card_header("Projection de la valeur du portefeuille (base CHF)"),
              plotOutput("proj_plot", height = 450)
            ),
            card(
              card_header("Résumé"),
              tableOutput("proj_summary"),
              hr(),
              uiOutput("proj_note")
            )
          )
        )
      ),
      
      # =========================================================================
      # ONGLET 3 : DOCUMENTATION (Markdown)
      # =========================================================================
      nav_panel(
        "Documentation",
        layout_column_wrap(
          width = 1,
          card(
            card_header("Documentation"),
            uiOutput("doc_ui")
          )
        )
      )
    )
    
    # ==========================================================================
    # SERVER
    # ==========================================================================
    server <- function(input, output, session) {
      
      # Devise base fixée (tu peux rendre ça paramétrable plus tard)
      base_ccy <- "CHF"
      
      # ------------------------------------------------------------------------
      # Reactive values : résultats entre calcul et affichage
      # ------------------------------------------------------------------------
      rv <- reactiveValues(
        frontier = NULL,  # tibble ret/vol/sharpe/idx
        tickers  = NULL,  # tickers effectifs (après nettoyage)
        W        = NULL,  # matrice poids (n_points x n_tickers)
        tan_idx  = NULL,  # indice portefeuille tangent
        mvp_idx  = NULL,  # indice portefeuille variance minimale
        
        px_last  = NULL,  # dernier prix EN CHF (par ticker)
        tick_ccy = NULL,  # devise détectée par ticker
        fx_last  = NULL   # dernier FX (devise->CHF) utilisé par ticker
      )
      
      # ------------------------------------------------------------------------
      # Helper : clamp un indice entre 1 et n (évite "subscript out of bounds")
      # ------------------------------------------------------------------------
      clamp_idx <- function(i, n) {
        if (is.null(i) || !is.finite(i) || n < 1) return(1L)
        i <- as.integer(i)
        if (i < 1) return(1L)
        if (i > n) return(as.integer(n))
        i
      }
      
      # ------------------------------------------------------------------------
      # V5 CORRECTION : appliquer réellement l’input mu_method (robustesse μ)
      # => à faire juste après estimate_mu_sigma(rets) et avant compute_frontier()
      # ------------------------------------------------------------------------
      apply_mu_robustness <- function(rets_xts, mu, method,
                                      winsor_p = 0.02, lambda = 0.30, freq = 252) {
        
        method <- match.arg(
          method,
          c("none", "winsor", "shrink_mean", "shrink_zero")
        )
        
        mu <- as.numeric(mu)
        n  <- length(mu)
        
        # Sécu
        if (n == 0) return(mu)
        
        # Noms (utile pour debug / cohérence)
        nm <- colnames(rets_xts)
        if (!is.null(nm) && length(nm) == n) names(mu) <- nm
        
        if (method == "none") {
          return(mu)
        }
        
        # ---- 1) Winsorize : on coupe les extrêmes des rendements journaliers
        if (method == "winsor") {
          p <- as.numeric(winsor_p)
          if (!is.finite(p)) p <- 0.02
          p <- max(min(p, 0.10), 0)
          
          R <- tryCatch(as.matrix(xts::coredata(rets_xts)), error = function(e) NULL)
          if (is.null(R)) return(mu)
          
          # Clamp colonne par colonne
          for (j in seq_len(ncol(R))) {
            x <- R[, j]
            x <- x[is.finite(x)]
            if (length(x) < 10) next
            
            qlo <- as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE))
            qhi <- as.numeric(stats::quantile(x, probs = 1 - p, na.rm = TRUE, names = FALSE))
            
            # Applique winsor sur la colonne complète
            R[, j] <- pmax(pmin(R[, j], qhi), qlo)
          }
          
          mu_w <- colMeans(R, na.rm = TRUE) * freq
          mu_w <- as.numeric(mu_w)
          if (!is.null(nm) && length(nm) == length(mu_w)) names(mu_w) <- nm
          return(mu_w)
        }
        
        # ---- 2) Shrink μ vers moyenne
        if (method == "shrink_mean") {
          lam <- as.numeric(lambda)
          if (!is.finite(lam)) lam <- 0
          lam <- max(min(lam, 1), 0)
          
          target <- rep(mean(mu, na.rm = TRUE), n)
          mu_s   <- (1 - lam) * mu + lam * target
          return(mu_s)
        }
        
        # ---- 3) Shrink μ vers 0 (conservateur)
        if (method == "shrink_zero") {
          lam <- as.numeric(lambda)
          if (!is.finite(lam)) lam <- 0
          lam <- max(min(lam, 1), 0)
          
          target <- rep(0, n)
          mu_s   <- (1 - lam) * mu + lam * target
          return(mu_s)
        }
        
        mu
      }
      
      # ========================================================================
      # OBSERVER : déclenché au clic sur "Calculer"
      # ========================================================================
      observeEvent(input$run, {
        
        withProgress(message = "Calcul en cours...", value = 0, {
          
          tryCatch({
            
            # ------------------------------------------------------------------
            # 1) Parse / valide les tickers
            # ------------------------------------------------------------------
            incProgress(0.05, detail = "Parsing tickers")
            tks <- Core$normalize_tickers(input$tickers, use_aliases = input$use_aliases)
            
            validate(need(length(tks) >= 2, "Entre au moins 2 tickers."))
            validate(need(
              input$wmax >= 1 / length(tks),
              paste0("wmax trop bas. Avec ", length(tks),
                     " titres, il faut wmax ≥ ", round(1 / length(tks), 3))
            ))
            
            showNotification(
              paste0("Tickers: ", paste(tks, collapse = ", ")),
              type = "message", duration = 4
            )
            
            # ------------------------------------------------------------------
            # 2) Devise par ticker + download prix NATIFS (Adjusted)
            # ------------------------------------------------------------------
            incProgress(0.20, detail = "Devises tickers + prix (Yahoo)")
            
            tick_ccy <- Core$get_ticker_currency_yahoo(tks)
            
            # Prix ajustés dans la devise native du ticker
            px_native <- Core$get_prices_yahoo(tks, from = input$dates[1], to = input$dates[2])
            
            # ------------------------------------------------------------------
            # 3) Conversion des prix en CHF (sur toute la période)
            # ------------------------------------------------------------------
            incProgress(0.35, detail = paste0("Conversion FX vers ", base_ccy))
            
            conv <- Core$convert_prices_to_base(
              px         = px_native,
              ticker_ccy = tick_ccy,
              base_ccy   = base_ccy,
              from       = input$dates[1],
              to         = input$dates[2]
            )
            
            px <- conv$px_base  # PRIX EN CHF (séries)
            
            rv$tick_ccy <- conv$tick_ccy
            rv$fx_last  <- conv$fx_last
            
            # Dernier prix en CHF par ticker (pour orders)
            px_last <- as.numeric(xts::last(px))
            names(px_last) <- colnames(px)
            rv$px_last <- px_last
            
            # Notification devises détectées
            ccy_used <- unique(unname(rv$tick_ccy[colnames(px)]))
            ccy_used <- ccy_used[!is.na(ccy_used) & nzchar(ccy_used)]
            showNotification(
              paste0("Base: ", base_ccy, " | Devises détectées: ", paste(ccy_used, collapse = ", ")),
              type = "message", duration = 4
            )
            
            # ------------------------------------------------------------------
            # 4) Rendements + estimation μ/Σ (EN CHF)
            # ------------------------------------------------------------------
            incProgress(0.55, detail = "Rendements + estimation μ/Σ (base CHF)")
            rets <- Core$calc_log_returns(px)
            est  <- Core$estimate_mu_sigma(rets)
            
            # ------------------------------------------------------------------
            # 4b) V5 CORRECTION : robustesse μ (rendements attendus)
            # ------------------------------------------------------------------
            mu_method <- if (!is.null(input$mu_method)) input$mu_method else "none"
            
            if (!is.null(mu_method) && mu_method != "none") {
              
              # valeurs de fallback si input caché / NULL
              winsor_p <- if (!is.null(input$mu_winsor)) input$mu_winsor else 0.02
              mu_lam   <- if (!is.null(input$mu_lambda)) input$mu_lambda else 0.30
              
              est$mu <- apply_mu_robustness(
                rets_xts  = rets,
                mu        = est$mu,
                method    = mu_method,
                winsor_p  = winsor_p,
                lambda    = mu_lam,
                freq      = 252
              )
              
              # Notification claire
              if (mu_method == "winsor") {
                showNotification(
                  paste0("Robust μ: winsor (p=", winsor_p, ")"),
                  type = "message", duration = 3
                )
              } else {
                showNotification(
                  paste0("Robust μ: ", mu_method, " (λ=", mu_lam, ")"),
                  type = "message", duration = 3
                )
              }
            }
            
            # ------------------------------------------------------------------
            # 5) Shrinkage Σ (robustesse covariance)
            # ------------------------------------------------------------------
            if (!is.null(input$shrink_method) && input$shrink_method != "none") {
              lam <- if (!is.null(input$shrink_lambda)) input$shrink_lambda else 0.20
              
              est$Sigma <- Core$shrink_covariance(
                est$Sigma,
                method = input$shrink_method,
                lambda = lam
              )
              
              showNotification(
                paste0("Shrinkage Σ: ", input$shrink_method, " (λ=", lam, ")"),
                type = "message", duration = 3
              )
            }
            
            # ------------------------------------------------------------------
            # 6) Frontière efficiente
            # ------------------------------------------------------------------
            incProgress(0.80, detail = "Frontière + tangence")
            f <- Core$compute_frontier(
              est$mu, est$Sigma,
              rf     = input$rf,
              n_grid = input$ngrid,
              w_max  = input$wmax
            )
            
            # ------------------------------------------------------------------
            # 7) Tibble de résultats + indices clés
            # ------------------------------------------------------------------
            df <- tibble::tibble(
              ret    = f$ret,
              vol    = f$vol,
              sharpe = f$sharpe
            )
            
            # Trie par volatilité croissante
            ord      <- order(df$vol, df$ret)
            df       <- df[ord, , drop = FALSE]
            W_sorted <- f$W[ord, , drop = FALSE]
            
            tan_idx <- which.max(df$sharpe)
            mvp_idx <- which.min(df$vol)
            
            df$idx <- seq_len(nrow(df))
            
            # ------------------------------------------------------------------
            # 8) Stocke dans reactive values
            # ------------------------------------------------------------------
            rv$frontier <- df
            rv$tickers  <- est$tickers
            rv$W        <- W_sorted
            rv$tan_idx  <- tan_idx
            rv$mvp_idx  <- mvp_idx
            
            incProgress(1.00, detail = "Terminé")
            showNotification("Calcul terminé ✅", type = "message", duration = 3)
            
          }, error = function(e) {
            
            # Reset safe
            rv$frontier <- NULL
            rv$tickers  <- NULL
            rv$W        <- NULL
            rv$tan_idx  <- NULL
            rv$mvp_idx  <- NULL
            rv$px_last  <- NULL
            rv$tick_ccy <- NULL
            rv$fx_last  <- NULL
            
            showNotification(
              paste("Erreur:", e$message),
              type = "error", duration = NULL
            )
          })
          
        })
      })
      
      # ========================================================================
      # OUTPUT : slider de sélection sur la frontière
      # ========================================================================
      output$pick_ui <- renderUI({
        req(rv$frontier, rv$tan_idx)
        sliderInput(
          "pick_idx",
          "Choisir un portefeuille sur la frontière",
          min   = 1,
          max   = nrow(rv$frontier),
          value = rv$tan_idx,
          step  = 1
        )
      })
      
      # ========================================================================
      # OUTPUT : graphique frontière (ligne propre = branche efficiente)
      # ========================================================================
      output$frontier_plot <- renderPlot({
        req(rv$frontier, rv$tan_idx, rv$mvp_idx, input$pick_idx)
        
        df  <- rv$frontier
        n   <- nrow(df)
        sel_i <- clamp_idx(input$pick_idx, n)
        
        tan <- df[rv$tan_idx, , drop = FALSE]
        sel <- df[sel_i, , drop = FALSE]
        mvp <- df[rv$mvp_idx, , drop = FALSE]
        
        # Branche efficiente : on garde pour chaque vol_bucket le max ret
        df_efficient <- df %>%
          dplyr::mutate(vol_bucket = round(vol, 5)) %>%
          dplyr::group_by(vol_bucket) %>%
          dplyr::filter(ret == max(ret)) %>%
          dplyr::slice(1) %>%
          dplyr::ungroup() %>%
          dplyr::arrange(vol)
        
        # Branche inefficiente : les autres points
        df_inefficient <- df %>%
          dplyr::anti_join(df_efficient, by = c("vol", "ret"))
        
        ggplot() +
          geom_point(
            data = df_inefficient,
            aes(x = vol, y = ret),
            color = "grey70", size = 1.5, alpha = 0.6
          ) +
          geom_path(
            data = df_efficient,
            aes(x = vol, y = ret),
            color = "steelblue", size = 1.2
          ) +
          geom_point(
            data = df_efficient,
            aes(x = vol, y = ret),
            color = "steelblue", size = 2
          ) +
          geom_point(
            data = mvp,
            aes(x = vol, y = ret),
            color = "darkgreen", size = 5, shape = 15
          ) +
          geom_point(
            data = tan,
            aes(x = vol, y = ret),
            color = "firebrick", size = 5, shape = 16
          ) +
          geom_point(
            data = sel,
            aes(x = vol, y = ret),
            color = "darkorange", size = 5, shape = 17
          ) +
          scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
          theme_minimal(base_size = 13) +
          theme(
            plot.title    = element_text(face = "bold"),
            plot.subtitle = element_text(color = "grey40")
          ) +
          labs(
            x        = "Volatilité (annuelle)",
            y        = "Rendement attendu (annuel)",
            title    = "Frontière efficiente de Markowitz (base CHF)",
            subtitle = paste0(
              "● Tangent (max Sharpe)\n",
              "■ Variance min (MVP)\n",
              "▲ Sélection\n",
              "Points gris = branche inefficiente"
            )
          )
      })
      
      # ========================================================================
      # HELPER : greedy fill cash (optionnel)
      # ========================================================================
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
            if (!is.finite(gap[j]) || gap[j] == -Inf) {
              j <- buyable[which.min(df$Prix[buyable])]
            }
          }
          
          df$Shares[j] <- df$Shares[j] + 1
          cash_left <- cash_left - df$Prix[j]
        }
        
        df
      }
      
      # ========================================================================
      # HELPER : orders table (prix EN CHF)
      # ========================================================================
      build_orders_table <- function(w, px_last_chf) {
        cap <- input$capital
        
        df <- tibble::tibble(
          Ticker = rv$tickers,
          Poids  = as.numeric(w),
          Prix   = as.numeric(px_last_chf[rv$tickers]),  # PRIX EN CHF
          Montant_cible = as.numeric(w) * cap
        ) %>%
          dplyr::mutate(
            Shares = floor(Montant_cible / Prix),
            Shares = ifelse(is.finite(Shares) & Shares >= 0, Shares, 0)
          )
        
        invested0 <- sum(df$Shares * df$Prix, na.rm = TRUE)
        cash_left <- cap - invested0
        
        if (identical(input$rounding_mode, "greedy")) {
          df <- greedy_fill_cash(df, cash_left)
        }
        
        df <- df %>% dplyr::mutate(Montant_investi = Shares * Prix)
        
        invested  <- sum(df$Montant_investi, na.rm = TRUE)
        cash_left <- cap - invested
        
        df_disp <- df %>%
          dplyr::mutate(
            Poids = scales::percent(Poids, accuracy = 0.01),
            Prix  = round(Prix, 2),
            Montant_cible   = paste0("CHF ", scales::comma(Montant_cible, accuracy = 0.01)),
            Montant_investi = paste0("CHF ", scales::comma(Montant_investi, accuracy = 0.01))
          ) %>%
          dplyr::select(Ticker, Poids, Prix, Shares, Montant_cible, Montant_investi)
        
        list(table = df_disp, cash_left = cash_left, invested = invested)
      }
      
      # ========================================================================
      # HELPER : allocation table (CHF)
      # ========================================================================
      build_alloc_table <- function(w) {
        cap <- input$capital
        tibble::tibble(
          Ticker  = rv$tickers,
          Poids   = w,
          Montant = w * cap
        ) %>%
          dplyr::mutate(
            Poids   = scales::percent(Poids, accuracy = 0.01),
            Montant = paste0("CHF ", scales::comma(Montant, accuracy = 0.01))
          )
      }
      
      # ========================================================================
      # OUTPUT : tables allocations
      # ========================================================================
      output$tab_tan <- renderTable({
        req(rv$W, rv$tan_idx, rv$tickers)
        w <- as.numeric(rv$W[rv$tan_idx, ])
        build_alloc_table(w)
      })
      
      output$tab_sel <- renderTable({
        req(rv$W, rv$tickers, input$pick_idx)
        n     <- nrow(rv$W)
        sel_i <- clamp_idx(input$pick_idx, n)
        w     <- as.numeric(rv$W[sel_i, ])
        build_alloc_table(w)
      })
      
      # ========================================================================
      # OUTPUT : KPI box
      # ========================================================================
      output$kpi_box <- renderUI({
        req(rv$frontier, rv$tan_idx, rv$mvp_idx, input$pick_idx)
        
        df    <- rv$frontier
        n     <- nrow(df)
        sel_i <- clamp_idx(input$pick_idx, n)
        
        tan <- df[rv$tan_idx, , drop = FALSE]
        mvp <- df[rv$mvp_idx, , drop = FALSE]
        sel <- df[sel_i, , drop = FALSE]
        
        mk_row <- function(lbl, x, color = "black") {
          tags$tr(
            tags$td(tags$strong(lbl, style = paste0("color:", color))),
            tags$td(scales::percent(x$ret, accuracy = 0.01)),
            tags$td(scales::percent(x$vol, accuracy = 0.01)),
            tags$td(round(x$sharpe, 3))
          )
        }
        
        tags$div(
          tags$h6("KPIs (annualisés)"),
          tags$table(
            class = "table table-sm table-striped",
            tags$thead(
              tags$tr(
                tags$th("Portefeuille"),
                tags$th("Rendement"),
                tags$th("Volatilité"),
                tags$th("Sharpe")
              )
            ),
            tags$tbody(
              mk_row("● Tangent", tan, "firebrick"),
              mk_row("■ Variance min", mvp, "darkgreen"),
              mk_row("▲ Sélection", sel, "darkorange")
            )
          )
        )
      })
      
      # ========================================================================
      # OUTPUT : orders + cash
      # ========================================================================
      output$orders_tan <- renderTable({
        req(rv$W, rv$tan_idx, rv$tickers, rv$px_last)
        w <- as.numeric(rv$W[rv$tan_idx, ])
        res <- build_orders_table(w, rv$px_last)
        res$table
      })
      
      output$orders_sel <- renderTable({
        req(rv$W, rv$tickers, rv$px_last, input$pick_idx)
        n     <- nrow(rv$W)
        sel_i <- clamp_idx(input$pick_idx, n)
        w <- as.numeric(rv$W[sel_i, ])
        res <- build_orders_table(w, rv$px_last)
        res$table
      })
      
      output$cash_box <- renderUI({
        req(rv$W, rv$tickers, rv$px_last, input$pick_idx)
        n     <- nrow(rv$W)
        sel_i <- clamp_idx(input$pick_idx, n)
        w <- as.numeric(rv$W[sel_i, ])
        res <- build_orders_table(w, rv$px_last)
        
        cap <- input$capital
        pct_inv <- if (cap > 0) res$invested / cap else NA_real_
        
        tags$div(
          class = "mt-2",
          tags$div(tags$strong("Investi (sélection): "), paste0("CHF ", scales::comma(res$invested, accuracy = 0.01))),
          tags$div(tags$strong("Cash restant: "), paste0("CHF ", scales::comma(res$cash_left, accuracy = 0.01))),
          tags$div(tags$strong("% investi: "), scales::percent(pct_inv, accuracy = 0.01))
        )
      })
      
      # ========================================================================
      # OUTPUT : table devises & FX
      # ========================================================================
      output$ccy_fx_table <- renderTable({
        req(rv$tickers, rv$tick_ccy, rv$fx_last, rv$px_last)
        
        tibble::tibble(
          Ticker = rv$tickers,
          Devise = unname(rv$tick_ccy[rv$tickers]),
          FX_vers_CHF = unname(rv$fx_last[rv$tickers]),
          Dernier_prix_CHF = unname(rv$px_last[rv$tickers])
        ) %>%
          dplyr::mutate(
            Devise = ifelse(is.na(Devise) | !nzchar(Devise), base_ccy, Devise),
            FX_vers_CHF = ifelse(is.na(FX_vers_CHF), 1, FX_vers_CHF),
            FX_vers_CHF = round(FX_vers_CHF, 6),
            Dernier_prix_CHF = round(Dernier_prix_CHF, 2)
          )
      })
      
      output$fx_note <- renderUI({
        req(rv$tickers, rv$tick_ccy)
        if (any(is.na(rv$tick_ccy[rv$tickers]) | !nzchar(rv$tick_ccy[rv$tickers]))) {
          tags$div(
            class = "text-muted mt-2",
            "Note: certains tickers n'ont pas renvoyé de devise via Yahoo ; on suppose CHF par défaut."
          )
        } else {
          tags$div(class = "text-muted mt-2", "Conversion FX appliquée sur toute la période (prix et rendements en CHF).")
        }
      })
      
      # ========================================================================
      # OUTPUT : Documentation (Markdown)
      # ========================================================================
      output$doc_ui <- renderUI({
        if (!requireNamespace("markdown", quietly = TRUE)) {
          return(tags$div(
            class = "alert alert-warning",
            "Le package 'markdown' est requis. Installe-le avec: install.packages('markdown')"
          ))
        }
        
        doc_path <- normalizePath(file.path("docs", "documentation.md"), mustWork = FALSE)
        
        if (!file.exists(doc_path)) {
          return(tags$div(
            class = "alert alert-warning",
            "Fichier docs/documentation.md introuvable. Crée-le pour afficher la documentation."
          ))
        }
        
        shiny::withMathJax(
          shiny::includeMarkdown(doc_path)
        )
      })
      
      # ========================================================================
      # Projection : portefeuille sélectionné
      # ========================================================================
      selected_portfolio <- reactive({
        req(rv$frontier, rv$W, rv$tickers, input$pick_idx)
        
        n <- nrow(rv$W)
        sel_i <- clamp_idx(input$pick_idx, n)
        
        list(
          idx = sel_i,
          w   = as.numeric(rv$W[sel_i, ]),
          mu  = as.numeric(rv$frontier$ret[sel_i]),  # annualisé
          vol = as.numeric(rv$frontier$vol[sel_i])   # annualisé
        )
      })
      
      # Projection déterministe (moyenne)
      project_det <- function(cap0, mu_annual, monthly_add, years) {
        n_months <- years * 12
        r_m <- (1 + mu_annual)^(1/12) - 1  # taux mensuel équivalent
        
        v <- numeric(n_months + 1)
        v[1] <- cap0
        
        for (t in 1:n_months) {
          v[t + 1] <- (v[t] * (1 + r_m)) + monthly_add
        }
        
        tibble::tibble(month = 0:n_months, value = v)
      }
      
      # Projection Monte Carlo (GBM simplifié)
      project_mc <- function(cap0, mu_annual, vol_annual, monthly_add, years, nsims, seed = 42) {
        set.seed(seed)
        
        n_months <- years * 12
        dt <- 1/12
        
        drift    <- (mu_annual - 0.5 * vol_annual^2) * dt
        shock_sd <- vol_annual * sqrt(dt)
        
        Z <- matrix(stats::rnorm(n_months * nsims), nrow = n_months, ncol = nsims)
        R <- exp(drift + shock_sd * Z)  # multiplicateurs mensuels
        
        V <- matrix(NA_real_, nrow = n_months + 1, ncol = nsims)
        V[1, ] <- cap0
        
        for (t in 1:n_months) {
          V[t + 1, ] <- (V[t, ] * R[t, ]) + monthly_add
        }
        
        qs <- c(0.10, 0.25, 0.50, 0.75, 0.90)
        qmat <- apply(V, 1, stats::quantile, probs = qs, na.rm = TRUE)
        
        qdf <- tibble::as_tibble(t(qmat))
        colnames(qdf) <- paste0("p", qs * 100)
        qdf <- dplyr::mutate(qdf, month = 0:n_months, .before = 1)
        
        keep <- min(30, nsims)
        idx  <- sample.int(nsims, keep)
        paths <- V[, idx, drop = FALSE]
        paths_df <- tibble::tibble(
          month = rep(0:n_months, times = keep),
          sim   = rep(seq_len(keep), each = n_months + 1),
          value = as.numeric(paths)
        )
        
        list(q = qdf, paths = paths_df)
      }
      
      # ========================================================================
      # OUTPUT : plot projection
      # ========================================================================
      output$proj_plot <- renderPlot({
        req(rv$frontier, input$proj_years, input$proj_monthly)
        
        sp    <- selected_portfolio()
        cap0  <- input$capital
        years <- as.integer(input$proj_years)
        addm  <- as.numeric(input$proj_monthly)
        
        if (identical(input$proj_mode, "det")) {
          
          df <- project_det(cap0, sp$mu, addm, years)
          
          ggplot(df, aes(x = month/12, y = value)) +
            geom_line() +
            theme_minimal() +
            scale_y_continuous(labels = scales::comma) +
            labs(
              x = "Années",
              y = "Valeur (CHF)",
              title = "Projection déterministe (moyenne)",
              subtitle = paste0(
                "μ=", scales::percent(sp$mu, accuracy = 0.1),
                " ; vol=", scales::percent(sp$vol, accuracy = 0.1),
                " ; +", scales::comma(addm), " CHF/mois"
              )
            )
          
        } else {
          
          ns   <- as.integer(input$proj_nsims)
          seed <- as.integer(input$proj_seed)
          
          res <- project_mc(cap0, sp$mu, sp$vol, addm, years, nsims = ns, seed = seed)
          q   <- res$q
          
          ggplot() +
            geom_line(
              data = res$paths,
              aes(x = month/12, y = value, group = sim),
              alpha = 0.15
            ) +
            geom_line(data = q, aes(x = month/12, y = p50), size = 1.1) +
            geom_line(data = q, aes(x = month/12, y = p10), linetype = "dashed") +
            geom_line(data = q, aes(x = month/12, y = p90), linetype = "dashed") +
            theme_minimal() +
            scale_y_continuous(labels = scales::comma) +
            labs(
              x = "Années",
              y = "Valeur (CHF)",
              title = "Projection Monte Carlo",
              subtitle = paste0(
                "Médiane + bandes p10/p90 ; μ=", scales::percent(sp$mu, accuracy = 0.1),
                " ; vol=", scales::percent(sp$vol, accuracy = 0.1),
                " ; +", scales::comma(addm), " CHF/mois ; ", ns, " sims"
              )
            )
        }
      })
      
      # ========================================================================
      # OUTPUT : résumé projection
      # ========================================================================
      output$proj_summary <- renderTable({
        sp    <- selected_portfolio()
        cap0  <- input$capital
        years <- as.integer(input$proj_years)
        addm  <- as.numeric(input$proj_monthly)
        n_months <- years * 12
        
        if (identical(input$proj_mode, "det")) {
          df <- project_det(cap0, sp$mu, addm, years)
          final <- df$value[nrow(df)]
          
          tibble::tibble(
            Metric = c("Capital initial", "Contributions totales", "Valeur finale (moyenne)"),
            Value = c(
              scales::comma(cap0),
              scales::comma(addm * n_months),
              scales::comma(final)
            )
          )
        } else {
          res <- project_mc(
            cap0, sp$mu, sp$vol, addm, years,
            nsims = as.integer(input$proj_nsims),
            seed  = as.integer(input$proj_seed)
          )
          q_last <- res$q[nrow(res$q), ]
          
          tibble::tibble(
            Metric = c("Capital initial", "Contributions totales", "Valeur finale p10", "Valeur finale médiane", "Valeur finale p90"),
            Value = c(
              scales::comma(cap0),
              scales::comma(addm * n_months),
              scales::comma(q_last$p10),
              scales::comma(q_last$p50),
              scales::comma(q_last$p90)
            )
          )
        }
      })
      
      # ========================================================================
      # OUTPUT : note projection
      # ========================================================================
      output$proj_note <- renderUI({
        sp <- selected_portfolio()
        tags$div(
          class = "small text-muted",
          tags$p("Hypothèses : μ/vol calculés à partir des données historiques sur la période sélectionnée, et appliqués au futur."),
          tags$p("Comme tu utilises les prix ajustés (Adjusted), les dividendes sont déjà inclus dans le rendement historique (approx total return)."),
          tags$p(paste0(
            "Portefeuille sélectionné: μ=", scales::percent(sp$mu, accuracy = 0.1),
            ", vol=", scales::percent(sp$vol, accuracy = 0.1), "."
          ))
        )
      })
      
    } # fin server
    
    # ==========================================================================
    # LANCE L'APPLICATION
    # ==========================================================================
    shinyApp(ui, server)
  }
}
