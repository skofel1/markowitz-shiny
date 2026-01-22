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
    
    # Instancie le module Core (récupère la liste de fonctions métier)
    Core <- Core_()
    
    # ==========================================================================
    # UI
    # ==========================================================================
    ui <- page_navbar(
      title = "Markowitz Shiny (V2)",
      theme = bs_theme(version = 5, bootswatch = "flatly"),
      
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
            sliderInput(
              "shrink_lambda",
              "Intensité shrinkage (λ)",
              min = 0, max = 1, value = 0.20, step = 0.05
            ),
            helpText("λ=0 : Σ brute ; λ=1 : Σ cible. Recommandé: constcor, λ≈0.1–0.3"),
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
              card_header("Allocations"),
              uiOutput("kpi_box"),
              hr(),
              h5("Portefeuille tangent (max Sharpe)"),
              tableOutput("tab_tan"),
              hr(),
              h5("Portefeuille sélectionné"),
              tableOutput("tab_sel"),
              hr(),
              h5("Ordres (actions entières)"),
              h6("Tangent"),
              tableOutput("orders_tan"),
              h6("Sélection"),
              tableOutput("orders_sel"),
              uiOutput("cash_box")
            )
          )
        )
      )
    )
    
    # ==========================================================================
    # SERVER
    # ==========================================================================
    server <- function(input, output, session) {
      
      # ------------------------------------------------------------------------
      # Reactive values : stocke les résultats entre le calcul et l'affichage
      # ------------------------------------------------------------------------
      rv <- reactiveValues(
        frontier = NULL,   # tibble avec ret, vol, sharpe, idx
        tickers  = NULL,   # vecteur de noms de tickers
        W        = NULL,   # matrice des poids (n_points x n_tickers)
        tan_idx  = NULL,   # indice du portefeuille tangent
        mvp_idx  = NULL,    # indice du portefeuille variance minimale
        px_last = NULL
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
      
      # ========================================================================
      # OBSERVER : déclenché au clic sur "Calculer"
      # ========================================================================
      observeEvent(input$run, {
        
        withProgress(message = "Calcul en cours...", value = 0, {
          
          tryCatch({
            
            # ------------------------------------------------------------------
            # 1) Parse et valide les tickers
            # ------------------------------------------------------------------
            incProgress(0.05, detail = "Parsing tickers")
            
            tks <- Core$normalize_tickers(input$tickers, use_aliases = input$use_aliases)
            
            validate(need(
              length(tks) >= 2,
              "Entre au moins 2 tickers."
            ))
            validate(need(
              input$wmax >= 1 / length(tks),
              paste0(
                "wmax trop bas. Avec ", length(tks),
                " titres, il faut wmax ≥ ", round(1 / length(tks), 3)
              )
            ))
            
            showNotification(
              paste0("Tickers: ", paste(tks, collapse = ", ")),
              type = "message", duration = 4
            )
            
            # ------------------------------------------------------------------
            # 2) Télécharge les prix depuis Yahoo Finance
            # ------------------------------------------------------------------
            incProgress(0.30, detail = "Téléchargement prix (Yahoo)")
            px <- Core$get_prices_yahoo(tks, from = input$dates[1], to = input$dates[2])
            px_last <- as.numeric(xts::last(px))
            names(px_last) <- colnames(px)
            
            rv$px_last <- px_last
            # ------------------------------------------------------------------
            # 3) Calcule les log-rendements et estime μ / Σ
            # ------------------------------------------------------------------
            incProgress(0.50, detail = "Rendements + estimation μ/Σ")
            rets <- Core$calc_log_returns(px)
            est  <- Core$estimate_mu_sigma(rets)
            
            # ------------------------------------------------------------------
            # V6: Shrinkage de Σ (robustesse)
            # ------------------------------------------------------------------
            if (!is.null(input$shrink_method) && input$shrink_method != "none") {
              est$Sigma <- Core$shrink_covariance(
                est$Sigma,
                method = input$shrink_method,
                lambda = input$shrink_lambda
              )
              
              showNotification(
                paste0("Shrinkage Σ: ", input$shrink_method, " (λ=", input$shrink_lambda, ")"),
                type = "message", duration = 3
              )
            }
            
            # ------------------------------------------------------------------
            # 4) Calcule la frontière efficiente complète
            # ------------------------------------------------------------------
            incProgress(0.75, detail = "Frontière + tangence")
            f <- Core$compute_frontier(
              est$mu, est$Sigma,
              rf     = input$rf,
              n_grid = input$ngrid,
              w_max  = input$wmax
            )
            
            # ------------------------------------------------------------------
            # 5) Construit le tibble de résultats
            # ------------------------------------------------------------------
            df <- tibble::tibble(
              ret    = f$ret,
              vol    = f$vol,
              sharpe = f$sharpe
            )
            
            # Trie par volatilité croissante (ordre naturel de la frontière)
            ord       <- order(df$vol, df$ret)
            df        <- df[ord, , drop = FALSE]
            W_sorted  <- f$W[ord, , drop = FALSE]
            
            # Indice du portefeuille tangent (max Sharpe) après tri
            tan_idx <- which.max(df$sharpe)
            
            # Indice du portefeuille de variance minimale (MVP)
            mvp_idx <- which.min(df$vol)
            
            # Ajoute une colonne idx (1, 2, 3, ...) pour le slider
            df$idx <- seq_len(nrow(df))
            
            # ------------------------------------------------------------------
            # 6) Stocke tout dans les reactive values
            # ------------------------------------------------------------------
            rv$frontier <- df
            rv$tickers  <- est$tickers
            rv$W        <- W_sorted
            rv$tan_idx  <- tan_idx
            rv$mvp_idx  <- mvp_idx
            
            incProgress(1.00, detail = "Terminé")
            showNotification("Calcul terminé ✅", type = "message", duration = 3)
            
          }, error = function(e) {
            # En cas d'erreur, reset pour éviter des affichages incohérents
            rv$frontier <- NULL
            rv$tickers  <- NULL
            rv$W        <- NULL
            rv$tan_idx  <- NULL
            rv$mvp_idx  <- NULL
            
            showNotification(
              paste("Erreur:", e$message),
              type = "error", duration = NULL
            )
          })
          
        })
      })
      
      # ========================================================================
      # OUTPUT : slider de sélection d'un portefeuille sur la frontière
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
      # OUTPUT : graphique de la frontière efficiente
      # ========================================================================
      output$frontier_plot <- renderPlot({
        req(rv$frontier, rv$tan_idx, rv$mvp_idx, input$pick_idx)
        
        df  <- rv$frontier
        n   <- nrow(df)
        sel_i <- clamp_idx(input$pick_idx, n)
        
        # Points clés
        tan <- df[rv$tan_idx, , drop = FALSE]
        sel <- df[sel_i, , drop = FALSE]
        mvp <- df[rv$mvp_idx, , drop = FALSE]
        
        # ----------------------------------------------------------------------
        # CONSTRUCTION DE LA LIGNE "PROPRE" DE LA FRONTIÈRE
        # La frontière a deux branches :
        #   - Branche INEFFICIENTE : sous le MVP (même vol, rendement inférieur)
        #   - Branche EFFICIENTE   : au-dessus du MVP (on veut tracer celle-ci)
        #
        # Stratégie : pour chaque niveau de vol (arrondi), on garde le point
        # avec le rendement MAX. Ensuite on trace uniquement ces points,
        # triés par volatilité croissante.
        # ----------------------------------------------------------------------
        df_efficient <- df %>%
          # Arrondit la vol pour regrouper les points quasi-identiques
          dplyr::mutate(vol_bucket = round(vol, 5)) %>%
          dplyr::group_by(vol_bucket) %>%
          # Garde uniquement le meilleur rendement pour chaque bucket de vol
          dplyr::filter(ret == max(ret)) %>%
          dplyr::slice(1) %>%
          dplyr::ungroup() %>%
          # Trie par volatilité croissante => ligne continue sans zigzag
          dplyr::arrange(vol)
        
        # Points de la branche inefficiente (optionnel, pour affichage grisé)
        df_inefficient <- df %>%
          dplyr::anti_join(df_efficient, by = c("vol", "ret"))
        
        # ----------------------------------------------------------------------
        # TRACÉ GGPLOT
        # ----------------------------------------------------------------------
        ggplot() +
          # Branche inefficiente en gris clair (points uniquement)
          geom_point(
            data = df_inefficient,
            aes(x = vol, y = ret),
            color = "grey70", size = 1.5, alpha = 0.6
          ) +
          # Branche efficiente : ligne + points
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
          # Point MVP (carré vert)
          geom_point(
            data = mvp,
            aes(x = vol, y = ret),
            color = "darkgreen", size = 5, shape = 15
          ) +
          # Point tangent (cercle rouge)
          geom_point(
            data = tan,
            aes(x = vol, y = ret),
            color = "firebrick", size = 5, shape = 16
          ) +
          # Point sélectionné (triangle orange)
          geom_point(
            data = sel,
            aes(x = vol, y = ret),
            color = "darkorange", size = 5, shape = 17
          ) +
          # Axes en pourcentage
          scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
          # Thème et labels
          theme_minimal(base_size = 13) +
          theme(
            plot.title    = element_text(face = "bold"),
            plot.subtitle = element_text(color = "grey40")
          ) +
          labs(
            x        = "Volatilité (annuelle)",
            y        = "Rendement attendu (annuel)",
            title    = "Frontière efficiente de Markowitz",
            subtitle = paste0(
              "● Tangent (max Sharpe)
 ",
              "■ Variance min (MVP)
 ",
              "▲ Sélection
 ",
              "Points gris = branche inefficiente"
            )
          )
      })
      
      # ========================================================================
      # HELPER : construit la table d'allocation pour un vecteur de poids
      # ========================================================================
      greedy_fill_cash <- function(df, cash_left) {
        # df doit contenir: Ticker, Prix, Shares, Poids (num), Montant_cible (num)
        # Objectif: ajouter des shares tant que cash permet,
        # en privilégiant le titre le plus sous-pondéré (vs cible)
        
        # sécurité
        if (!is.finite(cash_left) || cash_left <= 0) return(df)
        
        # boucle avec garde-fou
        guard <- 0
        while (cash_left > 0 && guard < 10000) {
          guard <- guard + 1
          
          # titres achetables
          buyable <- which(df$Prix <= cash_left & is.finite(df$Prix) & df$Prix > 0)
          if (length(buyable) == 0) break
          
          # poids actuel vs cible
          invested_now <- sum(df$Shares * df$Prix, na.rm = TRUE)
          if (!is.finite(invested_now) || invested_now <= 0) {
            # si rien investi, on prend le moins cher achetable
            j <- buyable[which.min(df$Prix[buyable])]
          } else {
            w_cur <- (df$Shares * df$Prix) / invested_now
            gap <- df$Poids - w_cur           # positif = sous-pondéré
            gap[!is.finite(gap)] <- -Inf
            gap[-buyable] <- -Inf             # pas achetable => interdit
            
            j <- which.max(gap)
            if (!is.finite(gap[j]) || gap[j] == -Inf) {
              # fallback: moins cher achetable
              j <- buyable[which.min(df$Prix[buyable])]
            }
          }
          
          # acheter 1 action du titre j
          df$Shares[j] <- df$Shares[j] + 1
          cash_left <- cash_left - df$Prix[j]
        }
        
        df
      }
      
      build_orders_table <- function(w, px_last) {
        cap <- input$capital
        
        df <- tibble::tibble(
          Ticker = rv$tickers,
          Poids = as.numeric(w),
          Prix = as.numeric(px_last[rv$tickers]),
          Montant_cible = as.numeric(w) * cap
        )
        
        df <- df %>%
          dplyr::mutate(
            Shares = floor(Montant_cible / Prix),
            Shares = ifelse(is.finite(Shares) & Shares >= 0, Shares, 0)
          )
        
        invested0 <- sum(df$Shares * df$Prix, na.rm = TRUE)
        cash_left <- cap - invested0
        
        if (identical(input$rounding_mode, "greedy")) {
          df <- greedy_fill_cash(df, cash_left)
        }
        
        df <- df %>%
          dplyr::mutate(Montant_investi = Shares * Prix)
        
        invested <- sum(df$Montant_investi, na.rm = TRUE)
        cash_left2 <- cap - invested
        
        df_disp <- df %>%
          dplyr::mutate(
            Poids = scales::percent(Poids, accuracy = 0.01),
            Prix = round(Prix, 2),
            Montant_cible = scales::dollar(Montant_cible, accuracy = 0.01),
            Montant_investi = scales::dollar(Montant_investi, accuracy = 0.01)
          ) %>%
          dplyr::select(Ticker, Poids, Prix, Shares, Montant_cible, Montant_investi)
        
        list(table = df_disp, cash_left = cash_left2, invested = invested)
      }
      
      
      build_alloc_table <- function(w) {
        cap <- input$capital
        tibble::tibble(
          Ticker  = rv$tickers,
          Poids   = w,
          Montant = w * cap
        ) %>%
          dplyr::mutate(
            # Formate en pourcentage
            Poids   = scales::percent(Poids, accuracy = 0.01),
            # Formate en CHF (préfixe personnalisé)
            Montant = paste0("CHF ", scales::comma(Montant, accuracy = 0.01))
          )
      }
      
      # ========================================================================
      # OUTPUT : table du portefeuille tangent
      # ========================================================================
      output$tab_tan <- renderTable({
        req(rv$W, rv$tan_idx, rv$tickers)
        w <- as.numeric(rv$W[rv$tan_idx, ])
        build_alloc_table(w)
      })
      
      # ========================================================================
      # OUTPUT : table du portefeuille sélectionné par le slider
      # ========================================================================
      output$tab_sel <- renderTable({
        req(rv$W, rv$tickers, input$pick_idx)
        n     <- nrow(rv$W)
        sel_i <- clamp_idx(input$pick_idx, n)
        w     <- as.numeric(rv$W[sel_i, ])
        build_alloc_table(w)
      })
      
      # ========================================================================
      # OUTPUT : encadré KPI (rendement, volatilité, Sharpe)
      # ========================================================================
      output$kpi_box <- renderUI({
        req(rv$frontier, rv$tan_idx, rv$mvp_idx, input$pick_idx)
        
        df    <- rv$frontier
        n     <- nrow(df)
        sel_i <- clamp_idx(input$pick_idx, n)
        
        tan <- df[rv$tan_idx, , drop = FALSE]
        mvp <- df[rv$mvp_idx, , drop = FALSE]
        sel <- df[sel_i, , drop = FALSE]
        
        # Fonction helper pour créer une ligne de tableau HTML
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
      # OUTPUT : Ordres + cash
      # ========================================================================
      output$orders_tan <- renderTable({
        req(rv$W, rv$tan_idx, rv$tickers, rv$px_last)
        w <- as.numeric(rv$W[rv$tan_idx, ])
        res <- build_orders_table(w, rv$px_last)
        res$table
      })
      
      output$orders_sel <- renderTable({
        req(rv$W, rv$tickers, rv$px_last, input$pick_idx)
        n <- nrow(rv$W)
        sel_i <- clamp_idx(input$pick_idx, n)
        w <- as.numeric(rv$W[sel_i, ])
        res <- build_orders_table(w, rv$px_last)
        res$table
      })
      
      output$cash_box <- renderUI({
        req(rv$W, rv$tickers, rv$px_last, input$pick_idx)
        n <- nrow(rv$W)
        sel_i <- clamp_idx(input$pick_idx, n)
        w <- as.numeric(rv$W[sel_i, ])
        res <- build_orders_table(w, rv$px_last)
        
        cap <- input$capital
        pct_inv <- if (cap > 0) res$invested / cap else NA_real_
        
        tags$div(
          class = "mt-2",
          tags$div(tags$strong("Investi (sélection): "), scales::dollar(res$invested, accuracy = 0.01)),
          tags$div(tags$strong("Cash restant: "), scales::dollar(res$cash_left, accuracy = 0.01)),
          tags$div(tags$strong("% investi: "), scales::percent(pct_inv, accuracy = 0.01))
        )
      })
      
    }
    
    # ==========================================================================
    # LANCE L'APPLICATION
    # ==========================================================================
    shinyApp(ui, server)
  }
}
