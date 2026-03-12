# app.R — Point d'entrée avec splash screen futuriste
# Overlay animé + lancement de l'app Markowitz

library(modulr)
library(shiny)

# ---------------------------------------------------------------------------
# Sources modulr
# ---------------------------------------------------------------------------
source("tool/MarkowitzCore/markowitz_core_provider.R")
source("tool/MarkowitzShiny/markowitz_shiny_app_provider.R")

# ---------------------------------------------------------------------------
# Build original app — intercepter UI et server avant shinyApp()
# ---------------------------------------------------------------------------
captured <- new.env(parent = emptyenv())
original_shinyApp <- shiny::shinyApp

#1 Override temporaire pour capturer ui + server
assign("shinyApp", function(ui, server, ...) {
  captured$ui     <- ui
  captured$server <- server
  original_shinyApp(ui, server, ...)
}, envir = globalenv())

make("tool/MarkowitzShiny/markowitz_shiny_app_provider")()

# Restaurer
assign("shinyApp", original_shinyApp, envir = globalenv())

app_ui     <- captured$ui
app_server <- captured$server

# ---------------------------------------------------------------------------
# Splash screen overlay (press Escape to continue)
# ---------------------------------------------------------------------------
splash_overlay_html <- tags$div(

  # Google Fonts
  tags$head(
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Orbitron:wght@400;700;900&family=Rajdhani:wght@300;400;500;600;700&display=swap",
      rel = "stylesheet"
    )
  ),

  # Full-screen overlay
  tags$div(
    id = "login-overlay",
    style = "position:fixed; top:0; left:0; width:100%; height:100%;
             background:#0a0e17; z-index:99999; display:flex;
             align-items:center; justify-content:center;
             transition: opacity 0.6s ease;",

    # Canvas particules
    tags$canvas(
      id = "particles",
      style = "position:absolute;top:0;left:0;width:100%;height:100%;z-index:0;"
    ),

    # Card centrale
    tags$div(
      id = "login-card",
      style = "position:relative; z-index:10; width:400px; padding:40px 35px;
               background:rgba(12,16,30,0.92); backdrop-filter:blur(25px);
               -webkit-backdrop-filter:blur(25px);
               border:1px solid rgba(0,255,170,0.15); border-radius:20px;
               box-shadow: 0 0 60px rgba(0,255,170,0.08),
                           0 30px 80px rgba(0,0,0,0.6),
                           inset 0 1px 0 rgba(255,255,255,0.04);
               animation: fadeInUp 0.8s ease-out;",

      # Logo SVG anime
      HTML('
        <div style="text-align:center; margin-bottom:25px;">
          <svg width="80" height="50" viewBox="0 0 80 50" style="margin-bottom:12px;">
            <defs>
              <linearGradient id="lineGrad" x1="0%" y1="0%" x2="100%" y2="0%">
                <stop offset="0%" style="stop-color:#00ffaa;stop-opacity:0.3"/>
                <stop offset="50%" style="stop-color:#00ffaa;stop-opacity:1"/>
                <stop offset="100%" style="stop-color:#00cc88;stop-opacity:0.3"/>
              </linearGradient>
              <filter id="glow">
                <feGaussianBlur stdDeviation="2" result="blur"/>
                <feMerge><feMergeNode in="blur"/><feMergeNode in="SourceGraphic"/></feMerge>
              </filter>
            </defs>
            <polyline points="2,40 12,35 22,38 30,20 38,25 46,10 54,15 62,5 70,12 78,8"
              fill="none" stroke="url(#lineGrad)" stroke-width="2.5"
              filter="url(#glow)" stroke-linecap="round" stroke-linejoin="round">
              <animate attributeName="stroke-dasharray" from="0,200" to="200,0" dur="2s" fill="freeze"/>
            </polyline>
            <circle r="3" fill="#00ffaa" filter="url(#glow)">
              <animateMotion dur="2s" fill="freeze"
                path="M2,40 L12,35 L22,38 L30,20 L38,25 L46,10 L54,15 L62,5 L70,12 L78,8"/>
            </circle>
          </svg>
          <div style="font-family:Orbitron,sans-serif; font-size:30px; font-weight:900;
                       background:linear-gradient(135deg,#00ffaa,#00cc88,#00ffaa);
                       -webkit-background-clip:text; -webkit-text-fill-color:transparent;
                       letter-spacing:4px; margin-bottom:6px;">MARKOWITZ</div>
          <div style="font-family:Rajdhani,sans-serif; font-size:13px; color:rgba(0,255,170,0.45);
                       letter-spacing:6px; text-transform:uppercase; margin-bottom:15px;">Portfolio Optimizer</div>
          <div style="width:60px; height:2px;
                       background:linear-gradient(90deg,transparent,#00ffaa,transparent);
                       margin:0 auto; border-radius:1px;"></div>
        </div>
      '),

      # "Appuyez sur Echap" message
      tags$div(
        style = "text-align:center; margin-top:20px;",
        tags$div(
          id = "press-escape",
          style = "font-family:Rajdhani,sans-serif; font-size:15px; color:rgba(0,255,170,0.5);
                   letter-spacing:3px; text-transform:uppercase;
                   animation: escapePulse 2s ease-in-out infinite;",
          HTML('Appuyez sur <span style="color:#00ffaa; font-weight:600;">Enter</span> pour continuer')
        )
      )
    )
  ),

  # CSS
  tags$style(HTML("
    @keyframes fadeInUp {
      from { opacity:0; transform:translateY(30px); }
      to   { opacity:1; transform:translateY(0); }
    }
    @keyframes gridScroll {
      0%   { transform:translate(0,0); }
      100% { transform:translate(50px,50px); }
    }
    @keyframes glowPulse {
      0%,100% { opacity:.4; transform:translate(-50%,-50%) scale(1); }
      50%     { opacity:.8; transform:translate(-50%,-50%) scale(1.15); }
    }
    @keyframes escapePulse {
      0%,100% { opacity:0.4; }
      50%     { opacity:1; }
    }

    /* Grille animee */
    #login-overlay::before {
      content:''; position:absolute; top:-50px; left:-50px;
      width:calc(100% + 100px); height:calc(100% + 100px);
      background:
        linear-gradient(rgba(0,255,170,.03) 1px, transparent 1px),
        linear-gradient(90deg, rgba(0,255,170,.03) 1px, transparent 1px);
      background-size:50px 50px;
      animation:gridScroll 25s linear infinite;
      pointer-events:none;
    }
    /* Lueur centrale */
    #login-overlay::after {
      content:''; position:absolute; top:50%; left:50%;
      width:700px; height:700px;
      background:radial-gradient(circle, rgba(0,255,170,.1) 0%, transparent 70%);
      animation:glowPulse 5s ease-in-out infinite;
      pointer-events:none;
    }
  ")),

  # JavaScript
  tags$script(HTML("
    // Particules
    (function() {
      var c = document.getElementById('particles');
      if (!c) return;
      var ctx = c.getContext('2d'), ps = [], N = 60;
      function resize() { c.width = window.innerWidth; c.height = window.innerHeight; }
      window.addEventListener('resize', resize); resize();
      for (var i=0;i<N;i++) ps.push({
        x:Math.random()*c.width, y:Math.random()*c.height,
        r:Math.random()*1.8+0.3, dx:(Math.random()-0.5)*0.4,
        dy:(Math.random()-0.5)*0.4, o:Math.random()*0.4+0.1
      });
      (function draw() {
        ctx.clearRect(0,0,c.width,c.height);
        for(var i=0;i<N;i++) for(var j=i+1;j<N;j++){
          var d=Math.hypot(ps[i].x-ps[j].x, ps[i].y-ps[j].y);
          if(d<150){ctx.beginPath();ctx.strokeStyle='rgba(0,255,170,'+(0.08*(1-d/150))+')';
          ctx.lineWidth=0.5;ctx.moveTo(ps[i].x,ps[i].y);ctx.lineTo(ps[j].x,ps[j].y);ctx.stroke();}
        }
        ps.forEach(function(p){
          ctx.beginPath();ctx.arc(p.x,p.y,p.r,0,Math.PI*2);
          ctx.fillStyle='rgba(0,255,170,'+p.o+')';ctx.fill();
          p.x+=p.dx;p.y+=p.dy;
          if(p.x<0||p.x>c.width)p.dx*=-1;
          if(p.y<0||p.y>c.height)p.dy*=-1;
        });
        requestAnimationFrame(draw);
      })();
    })();

    // Escape to dismiss
    document.addEventListener('keydown', function(e) {
      if (e.key === 'Enter') {
        var overlay = document.getElementById('login-overlay');
        if (!overlay || overlay.style.display === 'none') return;

        // Flash effect + fade out
        var flash = document.createElement('div');
        flash.style.cssText =
          'position:fixed;top:0;left:0;width:100%;height:100%;z-index:100000;' +
          'background:radial-gradient(circle,rgba(0,255,170,0.4),transparent 70%);' +
          'pointer-events:none;opacity:1;transition:opacity 0.5s ease;';
        document.body.appendChild(flash);

        setTimeout(function() {
          flash.style.opacity = '0';
          overlay.style.opacity = '0';
        }, 100);

        setTimeout(function() {
          overlay.style.display = 'none';
          flash.remove();
        }, 700);
      }
    });
  "))
)

# ---------------------------------------------------------------------------
# Wrap UI + Server
# ---------------------------------------------------------------------------
ui <- htmltools::tagAppendChildren(app_ui, splash_overlay_html)

server <- function(input, output, session) {
  # Run original app server directly — no auth needed
  app_server(input, output, session)
}

shinyApp(ui, server)
