# R/css.R ─────────────────────────────────────────────────────
# Tema bslib y CSS glassmorphism
# ─────────────────────────────────────────────────────────────

# Fallbacks para evitar errores de carga si globals.R aún no fue evaluado
css_get <- function(name, default) get0(name, envir = .GlobalEnv, inherits = TRUE, ifnotfound = default)

CSS_ACCENT     <- css_get("ACCENT", "#1A73E8")
CSS_ACCENT2    <- css_get("ACCENT2", "#E8F0FE")
CSS_ACCENT_RGB <- css_get("ACCENT_RGB", "26,115,232")
CSS_BG         <- css_get("BG", "#F0F2F5")
CSS_CARD_BG    <- css_get("CARD_BG", "rgba(255,255,255,0.72)")
CSS_CARD_SOLID <- css_get("CARD_SOLID", "#FFFFFF")
CSS_BORDER     <- css_get("BORDER", "rgba(218,220,224,0.55)")
CSS_TXT        <- css_get("TXT", "#1A1D21")
CSS_TXT_SEC    <- css_get("TXT_SEC", "#5F6368")
CSS_MUTED      <- css_get("MUTED", "#80868B")

app_theme <- bs_theme(
  bootswatch = "flatly",
  primary    = CSS_ACCENT,
  base_font  = font_google("DM Sans"),
  font_scale = 0.94,
  bg         = CSS_BG,
  fg         = CSS_TXT
)

app_css <- paste0('
/* ── Variables ─────────────────────────────────── */
:root {
  --accent:     ', CSS_ACCENT, ';
  --accent2:    ', CSS_ACCENT2, ';
  --accent-rgb: ', CSS_ACCENT_RGB, ';
  --bg:         ', CSS_BG, ';
  --card:       ', CSS_CARD_BG, ';
  --card-solid: ', CSS_CARD_SOLID, ';
  --border:     ', CSS_BORDER, ';
  --txt:        ', CSS_TXT, ';
  --txt-sec:    ', CSS_TXT_SEC, ';
  --muted:      ', CSS_MUTED, ';
  --radius:     16px;
  --radius-sm:  10px;
  --shadow:     0 2px 8px rgba(0,0,0,.06), 0 0px 1px rgba(0,0,0,.08);
  --shadow-lg:  0 8px 32px rgba(0,0,0,.08), 0 2px 8px rgba(0,0,0,.06);
  --blur:       saturate(1.6) blur(16px);
}

/* ── Animaciones ───────────────────────────────── */
@keyframes fadeUp {
  from { opacity:0; transform:translateY(10px); }
  to   { opacity:1; transform:translateY(0); }
}
@keyframes shimmer {
  0%   { background-position: -200% 0; }
  100% { background-position: 200% 0; }
}
@keyframes pulse-glow {
  0%, 100% { box-shadow: 0 0 4px rgba(var(--accent-rgb),.15); }
  50%      { box-shadow: 0 0 16px rgba(var(--accent-rgb),.25); }
}
@keyframes gradient-shift {
  0%   { background-position: 0% 50%; }
  50%  { background-position: 100% 50%; }
  100% { background-position: 0% 50%; }
}

/* ── Scrollbar ────────────────────────────────── */
::-webkit-scrollbar { width: 6px; height: 6px; }
::-webkit-scrollbar-track { background: transparent; }
::-webkit-scrollbar-thumb {
  background: rgba(var(--accent-rgb),.2);
  border-radius: 10px;
}
::-webkit-scrollbar-thumb:hover { background: rgba(var(--accent-rgb),.4); }

/* ── Base ──────────────────────────────────────── */
body {
  background: var(--bg) !important;
  color: var(--txt) !important;
  font-family: "DM Sans", system-ui, -apple-system, sans-serif !important;
}

/* ── Glassmorphism cards ───────────────────────── */
.card-panel {
  background: var(--card);
  backdrop-filter: var(--blur);
  -webkit-backdrop-filter: var(--blur);
  border: 1px solid var(--border);
  border-radius: var(--radius);
  box-shadow: var(--shadow);
  padding: 18px;
  transition: box-shadow 0.25s ease, transform 0.25s ease;
  animation: fadeUp 0.4s ease both;
}
.card-panel:hover {
  box-shadow: var(--shadow-lg);
  transform: translateY(-1px);
}

/* ── KPIs ──────────────────────────────────────── */
.kpiRow {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 10px;
}
.kpi {
  padding: 14px 16px;
  background: var(--card);
  backdrop-filter: var(--blur);
  -webkit-backdrop-filter: var(--blur);
  border: 1px solid var(--border);
  border-radius: var(--radius);
  box-shadow: var(--shadow);
  animation: fadeUp 0.4s ease both;
  transition: box-shadow 0.2s ease, transform 0.2s ease;
  position: relative;
  overflow: hidden;
}
.kpi::before {
  content: "";
  position: absolute;
  top: 0; left: 0; right: 0;
  height: 3px;
  background: linear-gradient(90deg, var(--accent), #9B59B6, rgba(var(--accent-rgb),.15));
  background-size: 200% 100%;
  animation: gradient-shift 4s ease infinite;
  border-radius: var(--radius) var(--radius) 0 0;
}
.kpi:hover { box-shadow: var(--shadow-lg); transform: translateY(-2px); }
.kpi .t { font-size: 10.5px; color: var(--muted); letter-spacing: 0.1em;
           text-transform: uppercase; font-weight: 700; }
.kpi .v { font-size: 22px; color: var(--txt); font-weight: 800;
           margin-top: 5px; line-height: 1.1; }
.kpi .v img { filter: drop-shadow(0 1px 3px rgba(0,0,0,.15)); }
.kpi .s { font-size: 11.5px; color: var(--txt-sec); margin-top: 6px; line-height: 1.4; }
.kpi .s img { filter: drop-shadow(0 1px 2px rgba(0,0,0,.1)); }

/* ── Sidebar ───────────────────────────────────── */
.bslib-sidebar-layout > .sidebar {
  background: var(--card) !important;
  backdrop-filter: var(--blur) !important;
  -webkit-backdrop-filter: var(--blur) !important;
  border-right: 1px solid var(--border) !important;
}
.sidebar-panel-inner { background: transparent !important; }

/* ── Tipografía ────────────────────────────────── */
.blockTitle {
  font-weight: 800;
  color: var(--txt);
  letter-spacing: -0.02em;
  font-size: 15px;
}
.smallHelp { color: var(--muted); font-size: 11.5px; line-height: 1.4; }

/* ── Inputs ────────────────────────────────────── */
.form-control, .form-select, .selectize-input {
  border-radius: var(--radius-sm) !important;
  border-color: var(--border) !important;
  background: rgba(255,255,255,0.6) !important;
  color: var(--txt) !important;
  font-size: 13px !important;
  transition: border-color 0.2s ease, box-shadow 0.2s ease;
}
.form-control:focus, .form-select:focus, .selectize-input.focus {
  border-color: var(--accent) !important;
  box-shadow: 0 0 0 3px rgba(var(--accent-rgb), .12) !important;
}
.selectize-input { box-shadow: none !important; }
.selectize-dropdown {
  background: #fff !important;
  border: 1px solid #DADCE0 !important;
  border-radius: 10px !important;
  box-shadow: 0 8px 24px rgba(0,0,0,.12) !important;
  z-index: 9999 !important;
}
.selectize-dropdown .option {
  background: #fff !important;
  color: var(--txt) !important;
  padding: 8px 12px !important;
  font-size: 13px !important;
}
.selectize-dropdown .option:hover,
.selectize-dropdown .active {
  background: var(--accent-bg) !important;
  color: var(--accent) !important;
}

/* ── Botones ───────────────────────────────────── */
.btn-accent {
  background: linear-gradient(135deg, var(--accent), #4285F4, #9B59B6) !important;
  background-size: 200% 200%;
  animation: gradient-shift 6s ease infinite;
  border: none !important;
  color: #fff !important;
  font-weight: 700 !important;
  border-radius: var(--radius-sm) !important;
  padding: 10px 16px !important;
  font-size: 13.5px !important;
  letter-spacing: 0.05em;
  text-transform: uppercase;
  transition: all 0.25s ease;
  box-shadow: 0 2px 8px rgba(var(--accent-rgb), .3);
}
.btn-accent:hover {
  filter: brightness(1.08);
  box-shadow: 0 4px 24px rgba(var(--accent-rgb), .45), 0 0 40px rgba(var(--accent-rgb), .15);
  transform: translateY(-2px) scale(1.01);
}
.btn-accent:active {
  transform: translateY(0) scale(0.98);
  box-shadow: 0 1px 4px rgba(var(--accent-rgb), .3);
}
.btn-outline-secondary {
  border-radius: var(--radius-sm) !important;
  border-color: var(--border) !important;
  color: var(--txt-sec) !important;
  font-weight: 600 !important;
  font-size: 13px !important;
  transition: all 0.15s ease;
}
.btn-outline-secondary:hover {
  background: rgba(var(--accent-rgb), .06) !important;
  border-color: var(--accent) !important;
  color: var(--accent) !important;
}

/* ── Leaflet ───────────────────────────────────── */
.leaflet-container {
  border-radius: var(--radius);
  border: 1px solid var(--border);
  box-shadow: var(--shadow);
  overflow: hidden;
}

/* ── Sliders ───────────────────────────────────── */
.irs-bar, .irs-bar-edge { background: var(--accent) !important; border-color: var(--accent) !important; }
.irs-single, .irs-from, .irs-to { background: var(--accent) !important; border-radius: 6px !important; }
.irs-slider { border-color: var(--accent) !important; }

/* ── Tabs ──────────────────────────────────────── */
.nav-tabs { border-bottom: 2px solid var(--border) !important; }
.nav-tabs .nav-link {
  color: var(--muted) !important;
  font-weight: 700 !important;
  font-size: 13px !important;
  padding: 10px 20px !important;
  border: none !important;
  border-bottom: 2.5px solid transparent !important;
  margin-bottom: -2px;
  transition: all 0.2s ease;
  letter-spacing: 0.01em;
}
.nav-tabs .nav-link:hover {
  color: var(--txt) !important;
  border-bottom-color: rgba(var(--accent-rgb), .3) !important;
}
.nav-tabs .nav-link.active {
  color: var(--accent) !important;
  background: transparent !important;
  border-bottom: 2.5px solid var(--accent) !important;
}

/* ── Tablas ─────────────────────────────────────── */
table.dataTable thead th {
  background: rgba(var(--accent-rgb), .03) !important;
  color: var(--txt-sec) !important;
  font-size: 11.5px !important;
  font-weight: 700 !important;
  border-bottom: 2px solid var(--border) !important;
  letter-spacing: 0.04em;
  text-transform: uppercase;
  white-space: nowrap;
}
table.dataTable thead th img {
  vertical-align: middle;
  margin-right: 3px;
  border-radius: 3px;
  box-shadow: 0 1px 3px rgba(0,0,0,.1);
}
table.dataTable tbody td {
  font-size: 13px !important;
  border-bottom: 1px solid rgba(0,0,0,.04) !important;
}
table.dataTable tbody tr:hover td {
  background: rgba(var(--accent-rgb), .03) !important;
}
.dt-buttons .dt-button {
  border-radius: 8px !important;
  padding: 5px 14px !important;
  font-weight: 700 !important;
  font-size: 11.5px !important;
  letter-spacing: 0.03em;
  border: 1px solid var(--border) !important;
  background: var(--card) !important;
  backdrop-filter: var(--blur);
  color: var(--txt) !important;
  transition: all .2s ease;
}
.dt-buttons .dt-button:hover {
  background: rgba(var(--accent-rgb),.08) !important;
  border-color: var(--accent) !important;
  color: var(--accent) !important;
  transform: translateY(-1px);
  box-shadow: 0 2px 8px rgba(var(--accent-rgb),.15);
}

/* ── Plotly ─────────────────────────────────────── */
.plotly .modebar { display: none !important; }

/* ── Badges ────────────────────────────────────── */
.buffer-badge {
  display: inline-flex; align-items: center; gap: 6px;
  padding: 4px 12px; border-radius: 20px;
  font-size: 11.5px; font-weight: 700;
}
.buffer-badge.inside  { background: #E6F4EA; color: #1E8E3E; }
.buffer-badge.outside { background: #FCE8E6; color: #D93025; }

/* ── Accordion ─────────────────────────────────── */
.accordion-button { font-weight: 700 !important; font-size: 13px !important; }

/* ── Login customization ───────────────────────── */
#shinymanager-auth {
  font-family: "DM Sans", system-ui, -apple-system, sans-serif !important;
}
.panel-auth {
  border-radius: 24px !important;
  background: rgba(255,255,255,0.75) !important;
  backdrop-filter: saturate(1.8) blur(24px);
  -webkit-backdrop-filter: saturate(1.8) blur(24px);
  border: 1px solid rgba(255,255,255,0.4) !important;
  box-shadow:
    0 24px 80px rgba(0,0,0,.08),
    0 8px 32px rgba(0,0,0,.04),
    0 0 120px rgba(var(--accent-rgb),.06),
    inset 0 1px 0 rgba(255,255,255,0.6) !important;
  padding: 36px 32px !important;
  max-width: 380px !important;
  margin: 0 auto !important;
  animation: fadeUp 0.6s ease both !important;
}
.panel-auth .panel-body { padding: 0 !important; }
#shinymanager-auth .form-control {
  border-radius: 12px !important;
  border: 1.5px solid rgba(218,220,224,0.6) !important;
  background: rgba(255,255,255,0.8) !important;
  padding: 12px 16px !important;
  font-size: 14px !important;
  font-weight: 500 !important;
  transition: all 0.25s ease !important;
  box-shadow: 0 1px 3px rgba(0,0,0,.04) !important;
}
#shinymanager-auth .form-control:focus {
  border-color: var(--accent) !important;
  box-shadow: 0 0 0 4px rgba(var(--accent-rgb),.12), 0 2px 8px rgba(var(--accent-rgb),.08) !important;
  background: #fff !important;
}
#shinymanager-auth .form-control::placeholder {
  color: #9AA0A6 !important; font-weight: 400 !important;
}
#shinymanager-auth label {
  font-size: 11px !important; font-weight: 700 !important;
  letter-spacing: 0.08em !important; text-transform: uppercase !important;
  color: var(--txt-sec) !important; margin-bottom: 4px !important;
}
#shinymanager-auth .btn-primary {
  background: linear-gradient(135deg, var(--accent), #4285F4, #7C3AED) !important;
  background-size: 200% 200% !important;
  animation: gradient-shift 5s ease infinite !important;
  border: none !important;
  border-radius: 14px !important;
  font-weight: 800 !important; font-size: 14px !important;
  letter-spacing: 0.1em !important; text-transform: uppercase !important;
  padding: 13px 24px !important; width: 100% !important;
  margin-top: 8px !important;
  transition: all 0.25s ease !important;
  box-shadow: 0 4px 16px rgba(var(--accent-rgb),.25) !important;
}
#shinymanager-auth .btn-primary:hover {
  transform: translateY(-2px) !important;
  box-shadow: 0 8px 32px rgba(var(--accent-rgb),.35), 0 0 40px rgba(var(--accent-rgb),.1) !important;
  filter: brightness(1.05) !important;
}
#shinymanager-auth .btn-primary:active {
  transform: translateY(0) scale(0.98) !important;
}

/* ── Radio / Check ─────────────────────────────── */
.form-check-input:checked {
  background-color: var(--accent) !important;
  border-color: var(--accent) !important;
}

/* ── Separator ─────────────────────────────────── */
.sep {
  height: 1px;
  background: linear-gradient(90deg, transparent, var(--border), transparent);
  margin: 10px 0;
  border: none;
}

/* ── Section label ─────────────────────────────── */
.sec-label {
  font-size: 10px;
  font-weight: 800;
  letter-spacing: 0.14em;
  text-transform: uppercase;
  background: linear-gradient(90deg, var(--accent), #9B59B6);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  background-clip: text;
  margin-bottom: 4px;
}
')
