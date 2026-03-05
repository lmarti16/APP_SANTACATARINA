# app.R ───────────────────────────────────────────────────────
# SANTA CATARINA, NL · Explorador electoral por sección
# ── v3.0 modular ─────────────────────────────────────────────
# Punto de entrada principal. Carga módulos desde R/.
# ─────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(shiny)
  library(data.table)
  library(sf)
  library(leaflet)
  library(bslib)
  library(plotly)
  library(DT)
  library(stringr)
  library(htmltools)
  library(dplyr)
  library(ggmap)
  library(shinymanager)
})

sf::sf_use_s2(FALSE)

# ── Cargar módulos ──────────────────────────────────────────
source("R/globals.R",         local = FALSE)
source("R/utils.R",           local = FALSE)
source("R/data.R",            local = FALSE)
source("R/palettes.R",        local = FALSE)
source("R/leaflet_helpers.R", local = FALSE)
source("R/css.R",             local = FALSE)
source("R/mod_explorar.R",    local = FALSE)
source("R/mod_tiempo.R",      local = FALSE)
source("R/mod_pauta.R",       local = FALSE)

# ── Cargar datos ────────────────────────────────────────────
dat    <- load_data()
sf_all <- dat$sf
dl_sf  <- dat$dl

# ── TRADUCTOR: catálogo de variables INEGI ────────
TRADUCTOR <- tryCatch({
  trad <- read_csv_flex(PATH_TRAD)
  if (!all(c("Eje", "Indicador", "VARIABLE") %in% names(trad)))
    stop("Falta columna Eje/Indicador/VARIABLE")
  as.data.frame(trad)
}, error = function(e) {
  message("Aviso: no se pudo cargar TRADUCTOR: ", conditionMessage(e))
  data.frame(Eje = character(0), Indicador = character(0), VARIABLE = character(0),
             stringsAsFactors = FALSE)
})

INEGI_COLS <- grep("_INEGI$", names(sf_all), value = TRUE)
INEGI_VARS <- sub("_INEGI$", "", INEGI_COLS)
INEGI_COL_MAP <- setNames(INEGI_COLS, INEGI_VARS)

if (NROW(TRADUCTOR) > 0) {
  TRADUCTOR <- TRADUCTOR[TRADUCTOR$VARIABLE %in% INEGI_VARS, ]
  TRADUCTOR$COL_NAME <- INEGI_COL_MAP[TRADUCTOR$VARIABLE]
}

EJES_DISPONIBLES <- if (NROW(TRADUCTOR) > 0) unique(TRADUCTOR$Eje) else character(0)
message(">> INEGI: ", length(INEGI_COLS), " columnas, ", NROW(TRADUCTOR),
        " en traductor, ", length(EJES_DISPONIBLES), " ejes")

INIT_BBOX <- sf::st_bbox(sf_all)
INIT_LNG  <- as.numeric((INIT_BBOX["xmin"] + INIT_BBOX["xmax"]) / 2)
INIT_LAT  <- as.numeric((INIT_BBOX["ymin"] + INIT_BBOX["ymax"]) / 2)
INIT_ZOOM <- 12L

SECC_DL_COL      <- if ("DISTRITO_L" %in% names(sf_all)) "DISTRITO_L" else NA_character_
SECC_DL_NAME_COL <- pick_col(names(sf_all), c("NOMBRE_DISTRITO_LOCAL", "NOM_DISTRITO_LOCAL"))
SECC_MUN_COL     <- pick_col(names(sf_all), c("NOMBRE_MUNICIPIO", "NOM_MUN", "MUNICIPIO"))
SECC_DF_COL      <- pick_col(names(sf_all), c("DISTRITO_FEDERAL", "DISTRITO_F", "DIST_F", "DF_NUM"))
SECC_DF_NAME_COL <- pick_col(names(sf_all), c("NOMBRE_DISTRITO_FEDERAL", "NOM_DF"))

mun_sf <- load_aux_layer(PATH_MUN, "municipios")
dfe_sf <- load_aux_layer(PATH_DF, "distritos federales")

# ── Elecciones ──────────────────────────────────────────────
elex <- detect_elections(names(sf_all))
if (nrow(elex) == 0L)
  stop("No detect\u00e9 columnas de elecci\u00f3n con patr\u00f3n *_ALC_24, etc.", call. = FALSE)

elex[, year := mapply(resolve_real_year, office, yr2,
                      MoreArgs = list(df = as.data.frame(st_drop_geometry(sf_all))))]
elex[, year := as.integer(year)]
elex[, office_lbl := vapply(office, label_office, character(1))]
elex[, key        := paste0(office, "_", yr2)]
elex[, label      := paste0(year, " \u00b7 ", office_lbl)]
elex[, office_ord := match(office, c("PRES", "GOB", "SEN", "DF", "DL", "ALC"))]
setorder(elex, year, office_ord)

ELECTION_CHOICES <- setNames(elex$key, elex$label)

DEFAULT_ELECTION <- {
  if ("ALC_24" %in% elex$key) "ALC_24"
  else if ("DL_24" %in% elex$key) "DL_24"
  else tail(elex$key, 1L)
}

# ==========================================================
# UI
# ==========================================================
ui_base <- page_fillable(
  title = APP_TITLE,
  theme = app_theme,
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = ""),
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=DM+Sans:ital,opsz,wght@0,9..40,400;0,9..40,500;0,9..40,600;0,9..40,700;0,9..40,800;0,9..40,900;1,9..40,400&display=swap",
      rel  = "stylesheet"
    )
  ),
  tags$style(HTML(app_css)),

  layout_sidebar(
    fillable = TRUE,

    sidebar = sidebar(
      width = 320,
      style = "padding:14px; height:calc(100vh - 24px); overflow:auto;",

      # ── Header ──
      div(
        style = "text-align:center; margin-bottom:6px;",
        if (file.exists("www/LOGO.PNG.png")) {
          tags$img(src = "LOGO.PNG.png", height = "38px",
                   style = "display:block; margin:0 auto 4px auto;")
        } else {
          tags$span(
            style = paste0(
              "font-size:16px; font-weight:900; letter-spacing:-0.03em; ",
              "background:linear-gradient(135deg,", ACCENT, ",#4285F4,#9B59B6); ",
              "-webkit-background-clip:text; -webkit-text-fill-color:transparent; ",
              "background-clip:text;"
            ),
            "electrend"
          )
        },
        div(style = "font-size:12px; font-weight:800; color:var(--txt); margin-top:1px;", APP_TITLE),
        div(class = "smallHelp", paste0(APP_SUB, " \u00b7 ", nrow(sf_all), " secciones"))
      ),
      div(class = "sep"),

      # ── Elección ──
      div(class = "sec-label", "Elecci\u00f3n"),
      selectInput("election", NULL, choices = ELECTION_CHOICES, selected = DEFAULT_ELECTION),

      div(class = "sec-label", "Filtros geogr\u00e1ficos"),
      div(class = "smallHelp", "Municipio"),
      selectizeInput("mun_sel", NULL, choices = NULL, selected = NULL, multiple = TRUE,
                     options = list(plugins = list("remove_button"), placeholder = "Todos\u2026")),

      div(class = "smallHelp", "Distrito local"),
      selectizeInput("dl_sel", NULL, choices = NULL, selected = NULL, multiple = TRUE,
                     options = list(plugins = list("remove_button"), placeholder = "Todos\u2026")),

      div(class = "smallHelp", "Distrito federal"),
      selectizeInput("df_sel", NULL, choices = NULL, selected = NULL, multiple = TRUE,
                     options = list(plugins = list("remove_button"), placeholder = "Todos\u2026")),

      div(class = "smallHelp", "Secci\u00f3n"),
      selectizeInput("secciones", NULL, choices = NULL, selected = NULL, multiple = TRUE,
                     options = list(plugins = list("remove_button"), placeholder = "Todas\u2026")),

      div(class = "sep"),

      div(class = "sec-label", "Mapa"),
      bslib::input_switch("map_variable", "Choropleth por partido", value = FALSE),

      conditionalPanel(
        condition = "!input.map_variable",
        selectInput("map_view", NULL,
                    choices = c("Ganador" = "winner", "Participaci\u00f3n (%)" = "part",
                                "Total votos" = "tot", "Lista nominal" = "ln"),
                    selected = "winner"),
        conditionalPanel(
          condition = "input.map_view == 'winner'",
          radioButtons("winner_vote_type", NULL,
                       choices  = c("DISTRIBUIDO" = "DISTRIBUIDO", "PURO" = "PURO", "CANDIDATURAS" = "CAND"),
                       selected = "DISTRIBUIDO", inline = TRUE)
        )
      ),

      conditionalPanel(
        condition = "input.map_variable",
        radioButtons("choro_vote_type", "Votos",
                     choices = c("DISTRIBUIDO" = "DISTRIBUIDO", "PURO" = "PURO"),
                     selected = "DISTRIBUIDO", inline = TRUE),
        uiOutput("ui_choro_party"),
        radioButtons("choro_metric", NULL,
                     choices = c("Votos" = "votes", "%" = "pct"),
                     selected = "pct", inline = TRUE),
        radioButtons("choro_scale", NULL,
                     choices = c("Lineal" = "linear", "Cuantiles" = "quantile"),
                     selected = "linear", inline = TRUE),
        sliderInput("choro_opacity", "Opacidad", min = 0.20, max = 0.90, value = 0.65, step = 0.05)
      ),

      div(class = "sep"),

      actionButton("generar", "GENERAR", class = "btn btn-accent w-100", icon = icon("bolt")),
      div(style = "height:6px;"),
      actionButton("reset", "Limpiar", class = "btn btn-outline-secondary w-100", icon = icon("rotate-left")),
      div(style = "height:6px;"),
      downloadButton("download_csv", "CSV", class = "btn btn-outline-secondary w-100"),
      div(style = "height:8px;"),
      uiOutput("ui_status_run")
    ),

    navset_tab(
      id = "main_tabs",

      nav_panel("Explorar",  mod_explorar_ui("explorar")),
      nav_panel("Tiempo",    mod_tiempo_ui("tiempo")),
      nav_panel("PAUTA",     mod_pauta_ui("pauta")),

      # ══ TAB: LÉEME ══════════════════════════════════
      nav_panel(
        "L\u00c9EME",
        div(
          style = "padding:12px; max-width:720px;",
          div(class = "card-panel",
              tags$h4("C\u00f3mo usar", class = "blockTitle", style = "margin-top:0;"),
              tags$p("Explorador de resultados electorales por secci\u00f3n en Santa Catarina, NL.",
                     style = "color:var(--txt-sec);"),
              tags$ul(style = "color:var(--txt-sec); line-height:1.8;",
                      tags$li("Secciones del CSV. Distritos locales construidos autom\u00e1ticamente."),
                      tags$li(tags$b("GENERAR"), " fija la selecci\u00f3n y alimenta Explorar + Tiempo."),
                      tags$li(tags$b("PAUTA:"), " Elecci\u00f3n, partido, radio \u2192 ",
                              tags$b("Optimizar"), "."),
                      tags$li("Overlays (DL, MUN, DF) filtrados por atributo. Geometr\u00edas simplificadas."),
                      tags$li("A\u00f1os reales desde columnas ANIO_* del CSV."),
                      tags$li(tags$b("Credenciales:"), " admin/123 \u00f3 user/User2025!#")
              )
          )
        )
      )
    )
  )
)

# ── Wrap UI con login ──────────────────────────────
ui <- secure_app(
  ui_base,
  enable_admin = FALSE,
  theme        = app_theme,
  tags_top     = tags$div(
    style = "text-align:center; padding:20px 0 8px 0;",
    if (file.exists("www/LOGO.PNG.png")) {
      tags$img(src = "LOGO.PNG.png", height = "56px",
               style = paste0(
                 "display:block; margin:0 auto 10px auto; ",
                 "filter:drop-shadow(0 2px 8px rgba(0,0,0,.12));"))
    } else {
      tags$span(
        style = paste0(
          "font-size:28px; font-weight:900; letter-spacing:-0.03em; ",
          "background:linear-gradient(135deg,", ACCENT, ",#4285F4,#7C3AED); ",
          "background-size:200% 200%; animation:gradient-shift 5s ease infinite; ",
          "-webkit-background-clip:text; -webkit-text-fill-color:transparent; ",
          "background-clip:text; display:block; margin-bottom:6px;"
        ),
        "electrend"
      )
    },
    tags$p(
      style = paste0("color:", TXT, "; font-size:15px; font-weight:800; margin:0; letter-spacing:-0.01em;"),
      APP_TITLE),
    tags$p(
      style = paste0("color:", MUTED, "; font-size:11.5px; margin:4px 0 0 0; letter-spacing:0.04em;"),
      APP_SUB),
    tags$div(
      style = "margin:12px auto 0 auto; width:40px; height:3px; border-radius:2px; background:linear-gradient(90deg, #1A73E8, #7C3AED);")
  ),
  background   = paste0(
    "linear-gradient(135deg, #F8F9FC 0%, ", ACCENT2, " 35%, #F3E8FF 65%, #F8F9FC 100%)"
  ),
  language     = "es"
)

# ==========================================================
# SERVER
# ==========================================================
server <- function(input, output, session) {

  # ── Login ───────────────────────────────────────
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )

  # ─── Selectores geográficos ─────────────────────
  observe({
    if (is.na(SECC_MUN_COL)) {
      updateSelectizeInput(session, "mun_sel", choices = list(), selected = NULL, server = TRUE)
      return()
    }
    muns <- sort(unique(na.omit(sf_all[[SECC_MUN_COL]])))
    ch   <- as.list(setNames(muns, muns))
    updateSelectizeInput(session, "mun_sel", choices = ch, selected = NULL, server = TRUE)
  })

  observe({
    if (is.na(SECC_DL_COL)) {
      updateSelectizeInput(session, "dl_sel", choices = list(), selected = NULL, server = TRUE)
      return()
    }
    dls <- sort(unique(na.omit(sf_all[[SECC_DL_COL]])))
    if (!is.na(SECC_DL_NAME_COL)) {
      labels <- vapply(dls, function(d) {
        nm <- unique(na.omit(sf_all[[SECC_DL_NAME_COL]][sf_all[[SECC_DL_COL]] == d]))
        if (length(nm) > 0 && nzchar(nm[1])) paste0(d, " - ", nm[1]) else as.character(d)
      }, character(1))
    } else labels <- as.character(dls)
    ch <- as.list(setNames(as.character(dls), labels))
    updateSelectizeInput(session, "dl_sel", choices = ch, selected = NULL, server = TRUE)
  })

  observe({
    if (is.na(SECC_DF_COL)) {
      updateSelectizeInput(session, "df_sel", choices = list(), selected = NULL, server = TRUE)
      return()
    }
    dfs <- sort(unique(na.omit(as.integer(sf_all[[SECC_DF_COL]]))))
    if (!is.na(SECC_DF_NAME_COL)) {
      labels <- vapply(dfs, function(d) {
        nm <- unique(na.omit(sf_all[[SECC_DF_NAME_COL]][as.integer(sf_all[[SECC_DF_COL]]) == d]))
        if (length(nm) > 0 && nzchar(nm[1])) paste0(d, " - ", nm[1]) else as.character(d)
      }, character(1))
    } else labels <- as.character(dfs)
    ch <- as.list(setNames(as.character(dfs), labels))
    updateSelectizeInput(session, "df_sel", choices = ch, selected = NULL, server = TRUE)
  })

  observe({
    df0 <- sf_all
    if (!is.na(SECC_MUN_COL)) {
      muns_in <- input$mun_sel %||% character(0)
      if (length(muns_in) > 0) df0 <- df0[df0[[SECC_MUN_COL]] %in% muns_in, ]
    }
    if (!is.na(SECC_DL_COL)) {
      dls_in <- suppressWarnings(as.integer(input$dl_sel %||% character(0)))
      dls_in <- dls_in[!is.na(dls_in)]
      if (length(dls_in) > 0) df0 <- df0[df0[[SECC_DL_COL]] %in% dls_in, ]
    }
    if (!is.na(SECC_DF_COL)) {
      dfs_in <- suppressWarnings(as.integer(input$df_sel %||% character(0)))
      dfs_in <- dfs_in[!is.na(dfs_in)]
      if (length(dfs_in) > 0) df0 <- df0[as.integer(df0[[SECC_DF_COL]]) %in% dfs_in, ]
    }
    secs <- sort(unique(df0$SECCION))
    ch   <- as.list(setNames(as.character(secs), as.character(secs)))
    cur  <- intersect(isolate(input$secciones) %||% character(0), as.character(secs))
    updateSelectizeInput(session, "secciones", choices = ch, selected = cur, server = TRUE)
  })

  winner_vt <- reactive(toupper(input$winner_vote_type %||% "DISTRIBUIDO"))

  # ─── Estado aplicado ─────────────────────────────
  applied <- reactiveVal(NULL)

  observeEvent(input$generar, {
    ap <- list(
      election         = input$election %||% DEFAULT_ELECTION,
      winner_vote_type = winner_vt(),
      map_variable     = isTRUE(input$map_variable),
      map_view         = input$map_view %||% "winner",
      choro_vote_type  = input$choro_vote_type %||% "DISTRIBUIDO",
      choro_party      = input$choro_party %||% "",
      choro_metric     = input$choro_metric %||% "pct",
      choro_scale      = input$choro_scale %||% "linear",
      choro_opacity    = input$choro_opacity %||% 0.65,
      dl_sel           = input$dl_sel   %||% character(0),
      mun_sel          = input$mun_sel  %||% character(0),
      df_sel           = input$df_sel   %||% character(0),
      secciones        = input$secciones %||% character(0),
      ts               = Sys.time()
    )
    applied(ap)
    showNotification("\u2713 Generado", type = "message", duration = 1.2)
  }, ignoreInit = TRUE)

  has_applied <- reactive(!is.null(applied()) && !is.null(applied()$ts))

  output$ui_status_run <- renderUI({
    if (!has_applied())
      div(class = "smallHelp", HTML("Configura y presiona <b>GENERAR</b>"))
    else
      div(class = "smallHelp", style = "color:#1E8E3E;",
          paste0("\u2713 ", format(applied()$ts, "%H:%M:%S")))
  })

  observeEvent(input$reset, {
    updateSelectizeInput(session, "mun_sel",   selected = NULL, server = TRUE)
    updateSelectizeInput(session, "dl_sel",    selected = NULL, server = TRUE)
    updateSelectizeInput(session, "df_sel",    selected = NULL, server = TRUE)
    updateSelectizeInput(session, "secciones", selected = NULL, server = TRUE)
    updateSelectInput(session, "election",     selected = DEFAULT_ELECTION)
    updateSelectInput(session, "map_view",     selected = "winner")
    updateRadioButtons(session, "winner_vote_type", selected = "DISTRIBUIDO")
    bslib::update_switch(session, "map_variable", value = FALSE)
    updateRadioButtons(session, "choro_vote_type", selected = "DISTRIBUIDO")
    updateRadioButtons(session, "choro_metric",    selected = "pct")
    updateRadioButtons(session, "choro_scale",     selected = "linear")
    updateCheckboxGroupInput(session, "tiempo-ts_offices", selected = sort(unique(elex$office)))
    updateSelectInput(session, "tiempo-ts_map_election",
                      selected = head(elex$key, 1) %||% DEFAULT_ELECTION)
  }, ignoreInit = TRUE)

  # ─── Datos aplicados ─────────────────────────────
  apply_filters <- function(df, ap) {
    if (!is.na(SECC_MUN_COL)) {
      muns <- ap$mun_sel %||% character(0)
      if (length(muns) > 0) df <- df[df[[SECC_MUN_COL]] %in% muns, ]
    }
    if (!is.na(SECC_DL_COL)) {
      dls <- suppressWarnings(as.integer(ap$dl_sel %||% character(0)))
      dls <- dls[!is.na(dls)]
      if (length(dls) > 0) df <- df[df[[SECC_DL_COL]] %in% dls, ]
    }
    if (!is.na(SECC_DF_COL)) {
      dfs <- suppressWarnings(as.integer(ap$df_sel %||% character(0)))
      dfs <- dfs[!is.na(dfs)]
      if (length(dfs) > 0) df <- df[as.integer(df[[SECC_DF_COL]]) %in% dfs, ]
    }
    if (length(ap$secciones) > 0) {
      s <- suppressWarnings(as.integer(ap$secciones)); s <- s[!is.na(s)]
      if (length(s) > 0) df <- df[df$SECCION %in% s, ]
    }
    df
  }

  has_active_filter <- reactive({
    if (!has_applied()) return(FALSE)
    ap <- applied()
    length(ap$mun_sel) > 0 || length(ap$dl_sel) > 0 ||
      length(ap$df_sel) > 0 || length(ap$secciones) > 0
  })

  df_applied <- reactive({
    req(has_applied())
    apply_filters(sf_all, applied())
  })

  clipped_overlays <- reactive({
    df <- df_applied()
    if (is.null(df) || NROW(df) == 0) return(list(dl = NULL, mun = NULL, df = NULL))
    if (!has_active_filter()) return(list(dl = dl_sf, mun = mun_sf, df = dfe_sf))
    list(
      dl  = clip_dl_by_attribute(dl_sf, df),
      mun = clip_mun_by_attribute(mun_sf, df),
      df  = clip_df_by_attribute(dfe_sf, df)
    )
  })

  dl_applied <- reactive({ clipped_overlays()$dl })

  current_overlay_groups <- reactive({
    co <- clipped_overlays()
    og <- character(0)
    if (!is.null(co$dl)  && NROW(co$dl)  > 0) og <- c(og, DL_GROUP)
    if (!is.null(co$mun) && NROW(co$mun) > 0) og <- c(og, MUN_GROUP)
    if (!is.null(co$df)  && NROW(co$df)  > 0) og <- c(og, DF_GROUP)
    og
  })

  # ─── UI choropleth partido (sidebar) ──────────────
  output$ui_choro_party <- renderUI({
    req(has_applied())
    if (!isTRUE(input$map_variable)) return(NULL)
    ap <- applied(); df <- df_applied(); req(nrow(df) > 0)
    parties <- get_valid_distribuido_parties(df, list(ap$election))
    validate(need(length(parties) > 0L, "Sin partidos para este tipo de voto."))
    sel <- input$choro_party %||% if ("PRI" %in% parties) "PRI" else parties[1]
    if (!(sel %in% parties)) sel <- parties[1]
    selectizeInput("choro_party", "Partido",
                   choices = as.list(setNames(parties, parties)), selected = sel)
  })

  # ─── Módulos ──────────────────────────────────────
  explorar_out <- mod_explorar_server(
    "explorar",
    has_applied        = has_applied,
    applied            = applied,
    df_applied         = df_applied,
    clipped_overlays   = clipped_overlays,
    current_overlay_groups = current_overlay_groups,
    sf_all = sf_all, dl_sf = dl_sf, mun_sf = mun_sf, dfe_sf = dfe_sf
  )

  tiempo_out <- mod_tiempo_server(
    "tiempo",
    has_applied        = has_applied,
    applied            = applied,
    df_applied         = df_applied,
    clipped_overlays   = clipped_overlays,
    current_overlay_groups = current_overlay_groups,
    main_tabs          = reactive(input$main_tabs),
    election_input     = reactive(input$election)
  )

  mod_pauta_server(
    "pauta",
    has_applied = has_applied,
    applied     = applied,
    df_applied  = df_applied,
    sf_all = sf_all, dl_sf = dl_sf, mun_sf = mun_sf, dfe_sf = dfe_sf,
    main_tabs   = reactive(input$main_tabs)
  )

  # ─── Download CSV (sidebar) ──────────────────────
  output$download_csv <- downloadHandler(
    filename = function() {
      ap <- applied()
      if (is.null(ap)) return("santa_catarina_sin_generar.csv")
      paste0("santa_catarina_", ap$election, "_explorar.csv")
    },
    content = function(file) {
      d <- tryCatch(explorar_out$tbl_data(), error = function(e) data.frame())
      write.csv(d, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
